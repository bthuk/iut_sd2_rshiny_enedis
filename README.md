

# 🚀 Projet RShiny : Analyse DPE pour Enedis

**Auteurs :** Meryem Chouki, Ulrick Berthon, Sharon Guedj
**Client :** Enedis (fictif)
**Contexte :** Projet universitaire – IUT Informatique (SD2 – Octobre 2025)

---

## 1. 🎯 Objectif du Projet

Ce projet analyse l’impact du **Diagnostic de Performance Énergétique (DPE)** sur la **consommation énergétique des logements** dans le département 31.

Il s’appuie sur deux livrables principaux :

1. Une **application Shiny interactive** permettant d’explorer les données DPE.
2. Un **rapport statistique** présentant une analyse approfondie basée sur le fichier `dpe_data.rds`.

---

## 2. 🔗 Accès aux Livrables

### 🖥️ Application Shiny

L'application est déployée en ligne sur shinyapps.io :

👉 **[https://meryem124.shinyapps.io/projet_r/](https://meryem124.shinyapps.io/projet_r/)**

---

### 📄 Rapport Statistique (HTML)

Le rapport d’étude complet est disponible ici :

👉 **[https://htmlpreview.github.io/?https://github.com/bthuk/iut_sd2_rshiny_enedis/blob/main/rapportstat.html](https://htmlpreview.github.io/?https://github.com/bthuk/iut_sd2_rshiny_enedis/blob/main/rapportstat.html)**

---

## 3. 📁 Contenu du Dépôt

| Fichier / Dossier               | Description                                                      |
| ------------------------------- | ---------------------------------------------------------------- |
| `app.R`                         | Application Shiny (UI + Server dans un seul fichier).            |
| `rapportstat.Rmd`               | Code source du rapport statistique (génère `rapportstat.html`).  |
| `rapportstat.html`              | Rapport statique déjà compilé (prévisualisable via htmlpreview). |
| `Rapport_fonctionnelle.Rmd`     | Documentation fonctionnelle destinée aux utilisateurs.           |
| `documentation_technique.md`    | Documentation technique pour développeurs.                       |
| `dpe_data.rds`                  | Jeu de données final utilisé par l’application.                  |
| `dpe_existants.R`, `dpe_neuf.R` | Scripts de préparation des données brutes.                       |
| `fusion_existant_neuf.R`        | Fusion et nettoyage pour créer `dpe_data.rds`.                   |

---

## 4. ▶️ Lancer l'application en local

### 4.1. Prérequis

Installer :

* R
* RStudio
* Les packages listés dans `documentation_technique.md`

### 4.2. Démarrage local

1. Cloner le dépôt :

   ```bash
   git clone https://github.com/bthuk/iut_sd2_rshiny_enedis.git
   ```
2. Ouvrir RStudio
3. Charger **`app.R`**
4. Cliquer sur **Run App**

L’application téléchargera automatiquement `dpe_data.rds` depuis GitHub.

---

## 5. 📬 Contact

Pour toute question concernant ce projet :

* [meryem.chouki@etu.iut.fr](mailto:meryem.chouki@etu.iut.fr)
* [ulrick.berthon@etu.iut.fr](mailto:ulrick.berthon@etu.iut.fr)
* [sharon.guedj@etu.iut.fr](mailto:sharon.guedj@etu.iut.fr)


