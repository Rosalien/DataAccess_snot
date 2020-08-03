ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Chargement des librairies
# library(lubridate) Ne pas utiliser, provoque un conflit qui empêche le lancement de l'application
listpaquets <- c("shiny","RPostgreSQL","DT","pool","leaflet","stringr","sp","dygraphs","xts","data.table","anytime",
"curl","dplyr","wesanderson","shinyjs","shinycssloaders","shinyWidgets","plotly",
"reshape","shinythemes","tableHTML","gridExtra","grid","shiny.i18n",
"RColorBrewer","shinyalert","gsubfn","suncalc","rintrojs","esquisse","shinydashboard","readr")

ipak(listpaquets)

# Lecture des dépendances spécifiques
listDependancies <- c("dependanciesCSS.R","dependanciesQuery.R","dependanciesGraph.R","dependanciesData.R")
sapply(listDependancies,source)


