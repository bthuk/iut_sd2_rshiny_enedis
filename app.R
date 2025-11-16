library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(sf)
library(bslib)
library(ggplot2)
library(plotly)
library(corrplot)
library(broom)
library(janitor)
library(glue)
library(viridis)
library(rsconnect) 
# Définition des thèmes
theme_clair <- bs_theme(bootswatch = "flatly")
theme_sombre <- bs_theme(bootswatch = "darkly")

# --- UI ---
ui <- fluidPage(
  theme = theme_clair,
  tags$head(tags$style(HTML("
    .kpi { background:#f7fff7; border-radius:8px; padding:12px; margin-bottom:8px; }
    .kpi h3 { margin:0; color:#0b6623; }
    .kpi p { margin:0; font-size:12px; color:#333; }
    .titre-app { font-weight:700; }
  "))),
  titlePanel(div("Analyse DPE - Occitanie", class = "titre-app")),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      h4("Filtres"),
      uiOutput("ui_type_logement"),
      uiOutput("ui_code_postal"),
      uiOutput("ui_surface"),
      hr(),
      radioButtons("theme_choice", "Thème", choices = c("Clair" = "clair", "Sombre" = "sombre"), selected = "clair"),
      actionButton("refresh_btn", "Rafraîchir les données", icon = icon("sync")),
      br(), br(),
      selectInput("main_plot_choice", "Graphique à exporter (.png)", choices = c(
        "Histogramme - consommation" = "hist_conso",
        "Boîte à moustaches - conso par DPE" = "box_dpe",
        "Diagramme - répartition DPE" = "bar_dpe",
        "Nuage de points - surface vs conso" = "scatter"
      ), selected = "hist_conso"),
      downloadButton("download_plot_png", "Exporter le graphique (.png)"),
      downloadButton("download_csv", "Exporter les données filtrées (.csv)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tableau de bord",
                 fluidRow(
                   column(3, div(class="kpi", h3(textOutput("kpi_count")), p("Logements sélectionnés"))),
                   column(3, div(class="kpi", h3(textOutput("kpi_mean_conso")), p("Conso moyenne (kWhep/m²/an)"))),
                   column(3, div(class="kpi", h3(textOutput("kpi_mean_ges")), p("GES moyen (kgCO₂/m²)"))),
                   column(3, div(class="kpi", h3(textOutput("kpi_mean_surface")), p("Surface moyenne (m²)")))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("plot_hist_conso")),
                   column(6, plotlyOutput("plot_box_dpe"))
                 ),
                 fluidRow(column(12, plotlyOutput("plot_bar_dpe")))
        ),
        tabPanel("Graphiques",
                 fluidRow(
                   column(6, plotlyOutput("plot_scatter")),
                   column(6, plotlyOutput("plot_hist_ges"))
                 ),
                 hr(),
                 fluidRow(
                   column(6, plotOutput("plot_box_isolation")),
                   column(6, plotOutput("plot_costs"))
                 )
        ),
        tabPanel("Tableau", DTOutput("table_filtered")),
        tabPanel("Carte", leafletOutput("map", height = "700px"))
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  load_data_from_github <- function() {
    url <- "https://raw.githubusercontent.com/bthuk/iut_sd2_rshiny_enedis/47aff6b2d89bf4d6b8fb3e8a8ca290ba9c4cd803/dpe_data.rds"
    
    temp <- tempfile(fileext = ".rds")
    download.file(url, temp, mode = "wb")
    readRDS(temp)
  }
  
  # --- Chargement initial et reactive ---
  donnees <- reactiveVal(NULL)
  
  observe({
    tryCatch({
      donnees(load_data_from_github())
    }, error = function(e) {
      showModal(modalDialog(title = "Erreur", paste("Impossible de charger 'dpe_data.rds':", e$message), easyClose = TRUE))
    })
  })
  
  observeEvent(input$refresh_btn, {
    tryCatch({
      donnees(load_data_from_github())
      showNotification("Données rechargées", type = "message")
    }, error = function(e) {
      showNotification(glue("Erreur de rafraîchissement : {e$message}"), type = "error")
    })
  })
  
  # --- Changement de thème ---
  observeEvent(input$theme_choice, {
    if(input$theme_choice == "clair") {
      session$setCurrentTheme(theme_clair)
    } else {
      session$setCurrentTheme(theme_sombre)
    }
  })
  
  # --- Widgets filtres dynamiques ---
  output$ui_type_logement <- renderUI({
    df <- donnees()
    req(df)
    if("type_batiment" %in% names(df)) {
      types <- sort(unique(na.omit(df$type_batiment)))
      selectInput("type_logement", "Type de logement", choices = c("Tous" = "", types), selected = "")
    } else {
      helpText("Champ 'type_batiment' absent")
    }
  })
  
  output$ui_code_postal <- renderUI({
    df <- donnees()
    req(df)
    if("code_postal_ban" %in% names(df)) {
      cps <- sort(unique(na.omit(as.character(df$code_postal_ban))))
      selectInput("code_postal", "Code postal", choices = c("Tous" = "", cps), selected = "")
    } else {
      helpText("Champ 'code_postal_ban' absent")
    }
  })
  
  output$ui_surface <- renderUI({
    df <- donnees()
    req(df)
    if("surface_habitable_logement" %in% names(df)) {
      rng <- range(df$surface_habitable_logement, na.rm = TRUE)
      sliderInput("surface_range", "Surface habitable (m²)",
                  min = floor(rng[1]), max = ceiling(rng[2]),
                  value = c(floor(rng[1]), ceiling(rng[2])))
    } else {
      helpText("Champ 'surface_habitable_logement' absent")
    }
  })
  
  # --- Jeu filtré ---
  donnees_filtrees <- reactive({
    df <- donnees()
    req(df)
    # Filtre type
    if(!is.null(input$type_logement) && input$type_logement != "" && "type_batiment" %in% names(df)) {
      df <- df %>% filter(type_batiment == input$type_logement)
    }
    # Filtre code postal
    if(!is.null(input$code_postal) && input$code_postal != "" && "code_postal_ban" %in% names(df)) {
      df <- df %>% filter(as.character(code_postal_ban) == as.character(input$code_postal))
    }
    # Filtre surface
    if(!is.null(input$surface_range) && "surface_habitable_logement" %in% names(df)) {
      sr <- input$surface_range
      df <- df %>% filter(!is.na(surface_habitable_logement) &
                            surface_habitable_logement >= sr[1] &
                            surface_habitable_logement <= sr[2])
    }
    df
  })
  
  # --- KPI ---
  output$kpi_count <- renderText({ nrow(donnees_filtrees()) })
  output$kpi_mean_conso <- renderText({
    df <- donnees_filtrees()
    if("conso_5_usages_par_m2_ep" %in% names(df)) {
      val <- mean(df$conso_5_usages_par_m2_ep, na.rm = TRUE)
      if(is.na(val)) "N/A" else format(round(val,2), nsmall = 2)
    } else "N/D"
  })
  output$kpi_mean_ges <- renderText({
    df <- donnees_filtrees()
    if("emission_ges_5_usages_par_m2" %in% names(df)) {
      val <- mean(df$emission_ges_5_usages_par_m2, na.rm = TRUE)
      if(is.na(val)) "N/A" else format(round(val,2), nsmall = 2)
    } else "N/D"
  })
  output$kpi_mean_surface <- renderText({
    df <- donnees_filtrees()
    if("surface_habitable_logement" %in% names(df)) {
      val <- mean(df$surface_habitable_logement, na.rm = TRUE)
      if(is.na(val)) "N/A" else format(round(val,1), nsmall = 1)
    } else "N/D"
  })
  
  # --- Graphiques ---
  output$plot_hist_conso <- renderPlotly({
    df <- donnees_filtrees()
    req(df)
    if(!("conso_5_usages_par_m2_ep" %in% names(df))) return(NULL)
    p <- ggplot(df, aes(x = conso_5_usages_par_m2_ep)) +
      geom_histogram(bins = 30, fill = "#2E86AB", color = "white") +
      labs(title = "Histogramme — consommation primaire (kWhep/m²/an)",
           x = "kWhep/m²/an", y = "Nombre")
    ggplotly(p)
  })
  
  output$plot_box_dpe <- renderPlotly({
    df <- donnees_filtrees()
    req(df)
    if(!all(c("etiquette_dpe","conso_5_usages_par_m2_ep") %in% names(df))) return(NULL)
    p <- ggplot(df, aes(x = factor(etiquette_dpe), y = conso_5_usages_par_m2_ep)) +
      geom_boxplot(fill = "#66C2A5") +
      labs(title = "Boîte à moustaches — consommation par classe DPE",
           x = "Classe DPE", y = "kWhep/m²/an")
    ggplotly(p)
  })
  
  output$plot_bar_dpe <- renderPlotly({
    df <- donnees_filtrees()
    req(df)
    if(!("etiquette_dpe" %in% names(df))) return(NULL)
    p <- df %>% count(etiquette_dpe) %>%
      ggplot(aes(x = reorder(etiquette_dpe, -n), y = n, fill = etiquette_dpe)) +
      geom_col() + theme(legend.position = "none") +
      labs(title = "Répartition des classes DPE", x = "Classe DPE", y = "Nombre")
    ggplotly(p)
  })
  
  output$plot_scatter <- renderPlotly({
    df <- donnees_filtrees()
    req(df)
    if(!all(c("surface_habitable_logement","conso_5_usages_par_m2_ep","etiquette_dpe") %in% names(df))) return(NULL)
    p <- ggplot(df, aes(x = surface_habitable_logement, y = conso_5_usages_par_m2_ep, color = etiquette_dpe)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Nuage de points — Surface vs consommation",
           x = "Surface (m²)", y = "kWhep/m²/an")
    ggplotly(p)
  })
  
  output$plot_hist_ges <- renderPlotly({
    df <- donnees_filtrees()
    req(df)
    if(!("emission_ges_5_usages_par_m2" %in% names(df))) return(NULL)
    p <- ggplot(df, aes(x = emission_ges_5_usages_par_m2)) +
      geom_histogram(bins = 30, fill = "#F39C12", color = "white") +
      labs(title = "Histogramme — émissions GES (kgCO₂/m²)", x = "kgCO₂/m²", y = "Nombre")
    ggplotly(p)
  })
  
  output$plot_box_isolation <- renderPlot({
    df <- donnees_filtrees()
    req(df)
    if(!all(c("qualite_isolation_murs","conso_5_usages_par_m2_ep") %in% names(df))) {
      plot.new(); text(0.5,0.5,"Données isolation ou conso manquantes", cex=1.1); return()
    }
    ggplot(df, aes(x = factor(qualite_isolation_murs), y = conso_5_usages_par_m2_ep)) +
      geom_boxplot() + coord_flip() +
      labs(title = "Consommation par qualité d'isolation murs", x = "", y = "kWhep/m²/an")
  })
  
  output$plot_costs <- renderPlot({
    df <- donnees_filtrees()
    req(df)
    cost_cols <- intersect(c("cout_chauffage","cout_ecs","cout_eclairage","cout_total_5_usages"), names(df))
    if(length(cost_cols) == 0) {
      plot.new(); text(0.5,0.5,"Aucune donnée de coûts disponible", cex=1.1); return()
    }
    df_long <- df %>% select(all_of(cost_cols)) %>% pivot_longer(everything(), names_to = "poste", values_to = "valeur") %>% drop_na()
    ggplot(df_long, aes(x = poste, y = valeur)) +
      geom_boxplot(fill = "#A569BD") +
      labs(title = "Distribution des coûts (€/an)", x = "", y = "€") +
      coord_flip()
  })
  
  # --- Table interactive ---
  output$table_filtered <- renderDT({
    df <- donnees_filtrees()
    datatable(df, extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy','csv','excel')), filter = 'top', rownames = FALSE)
  })
  
  # --- Download CSV ---
  output$download_csv <- downloadHandler(
    filename = function() paste0("dpe_filtre_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(donnees_filtrees(), file)
    }
  )
  
  # --- Download plot PNG ---
  output$download_plot_png <- downloadHandler(
    filename = function() paste0("graphique_", Sys.Date(), ".png"),
    content = function(file) {
      df <- donnees_filtrees()
      p <- NULL
      if(input$main_plot_choice == "hist_conso") {
        req("conso_5_usages_par_m2_ep" %in% names(df))
        p <- ggplot(df, aes(x = conso_5_usages_par_m2_ep)) +
          geom_histogram(bins = 30, fill = "#2E86AB", color = "white") +
          labs(title = "Histogramme — consommation primaire")
      } else if(input$main_plot_choice == "box_dpe") {
        req(all(c("etiquette_dpe","conso_5_usages_par_m2_ep") %in% names(df)))
        p <- ggplot(df, aes(x = factor(etiquette_dpe), y = conso_5_usages_par_m2_ep)) +
          geom_boxplot(fill = "#66C2A5") +
          labs(title = "Boîte à moustaches — conso par DPE")
      } else if(input$main_plot_choice == "bar_dpe") {
        req("etiquette_dpe" %in% names(df))
        p <- df %>% count(etiquette_dpe) %>%
          ggplot(aes(x = reorder(etiquette_dpe, -n), y = n, fill = etiquette_dpe)) +
          geom_col() + theme(legend.position = "none") +
          labs(title = "Répartition DPE")
      } else if(input$main_plot_choice == "scatter") {
        req(all(c("surface_habitable_logement","conso_5_usages_par_m2_ep") %in% names(df)))
        p <- ggplot(df, aes(x = surface_habitable_logement, y = conso_5_usages_par_m2_ep)) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(title = "Surface vs consommation")
      }
      if(is.null(p)) stop("Le graphique sélectionné n'est pas disponible pour les données filtrées.")
      ggsave(file, plot = p, device = "png", width = 12, height = 7)
    }
  )
  
  # --- Corrélations ---
  output$corr_vars_ui <- renderUI({
    df <- donnees_filtrees()
    req(df)
    nums <- df %>% select(where(is.numeric)) %>% names()
    selectizeInput("corr_vars", "Choisir des variables numériques", choices = nums,
                   multiple = TRUE, selected = nums[1:min(6,length(nums))])
  })
  
  observeEvent(input$btn_corr, {
    output$corr_plot <- renderPlot({
      df <- donnees_filtrees()
      req(input$corr_vars)
      df2 <- df %>% select(all_of(input$corr_vars)) %>% mutate_all(as.numeric)
      cor_mat <- cor(df2, use = "pairwise.complete.obs")
      corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8)
    })
  })
  
  # --- Régression linéaire simple ---
  output$reg_x_ui <- renderUI({
    df <- donnees_filtrees()
    req(df)
    nums <- df %>% select(where(is.numeric)) %>% names()
    selectInput("reg_x", "Variable explicative X", choices = nums, selected = nums[1])
  })
  output$reg_y_ui <- renderUI({
    df <- donnees_filtrees()
    req(df)
    nums <- df %>% select(where(is.numeric)) %>% names()
    selectInput("reg_y", "Variable réponse Y", choices = nums, selected = nums[min(2,length(nums))])
  })
  
  reg_fit <- eventReactive(input$run_reg, {
    df <- donnees_filtrees()
    req(input$reg_x, input$reg_y)
    if(!(input$reg_x %in% names(df)) || !(input$reg_y %in% names(df))) return(NULL)
    df2 <- df %>% select(x = all_of(input$reg_x), y = all_of(input$reg_y)) %>% drop_na()
    if(nrow(df2) < 5) return(NULL)
    lm(y ~ x, data = df2)
  })
  
  output$reg_plot <- renderPlot({
    fit <- reg_fit()
    req(fit)
    df_aug <- broom::augment(fit)
    plot(df_aug$x, df_aug$y, pch = 20, xlab = input$reg_x, ylab = input$reg_y,
         main = paste("Régression linéaire : ", input$reg_y, " ~ ", input$reg_x))
    if(isTRUE(input$reg_show_lm)) abline(fit, col = "red", lwd = 2)
  })
  
  output$reg_summary <- renderPrint({
    fit <- reg_fit()
    if(is.null(fit)) "Modèle non estimé : données manquantes ou insuffisantes." else summary(fit)
  })
  
  # --- Carte Leaflet ---
  output$map <- renderLeaflet({
    df <- donnees_filtrees()
    req(df)
    if(!all(c("coordonnee_cartographique_x_ban","coordonnee_cartographique_y_ban") %in% names(df))) {
      leaflet() %>% addTiles() %>% addPopups(0,0,"Coordonnées cartographiques absentes")
    } else {
      pts <- df %>% filter(!is.na(coordonnee_cartographique_x_ban) & !is.na(coordonnee_cartographique_y_ban))
      if(nrow(pts) == 0) return(leaflet() %>% addTiles())
      sfpts <- tryCatch({
        st_as_sf(pts, coords = c("coordonnee_cartographique_x_ban","coordonnee_cartographique_y_ban"), crs = 2154, remove = FALSE) %>% st_transform(4326)
      }, error = function(e) {
        st_as_sf(pts, coords = c("coordonnee_cartographique_x_ban","coordonnee_cartographique_y_ban"), crs = 4326, remove = FALSE)
      })
      pal <- colorFactor(viridis::viridis(7), domain = sfpts$etiquette_dpe)
      leaflet(sfpts) %>% addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(radius = 5, color = ~pal(etiquette_dpe), stroke = FALSE, fillOpacity = 0.8,
                         popup = ~paste0("<b>", nom_commune_ban, "</b><br/>DPE: ", etiquette_dpe,
                                         "<br/>Conso: ", conso_5_usages_par_m2_ep, " kWhep/m²/an")) %>%
        addLegend("bottomright", pal = pal, values = ~etiquette_dpe, title = "Classe DPE")
    }
  })
  
} # fin server

# --- Lancer l'application ---
shinyApp(ui = ui, server = server)

