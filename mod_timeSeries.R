translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"

mod_timeSeriesUI <- function(id){
  ns <- NS(id)

  fluidRow(style="padding-left:15px;",
      withSpinner(uiOutput(ns("timetrendSNOT")),type=5)
    )
}

mod_timeSeries <- function(input, output, session,sqlOutputQuery,dayNightSelected,
  siteSelected,frequenceSelected,variableSelected){
  ns <- session$ns

output$timetrendSNOT <- renderUI({
      withProgress(message = translator$t("Préparation des données ..."),{
        
        incProgress(0.3,translator$t("Requête de la base de données ..."))       
        
        # Requête sur les données
        data <- sqlOutputQuery

       # Calcul de la séquence jour et nuit pour filtrer les données
        if(dayNightSelected!="day/night"){
          incProgress(0.1,translator$t("Calcul des paramètres jour/nuit ..."))
          print("---------Paramètres jour/nuit---------")
          dbDayandNight <- dbDayNight(data)
        }else{
        dbDayandNight <- data
      }
        incProgress(0.3,translator$t("Aggrégation des données ..."))
        subsetoutbdSNOT <- dbselect(dbDayandNight,dayNightSelected,frequenceSelected,siteSelected,variableSelected)
        incProgress(0.3,translator$t("Préparation du graphique ..."))
        
        dy_graphSite <- dygraphSite(subsetoutbdSNOT[(grepl("SWC|TS|G", subsetoutbdSNOT$variable)==FALSE) & subsetoutbdSNOT$variable %!in% c("WTD","TW"),],frequenceSelected)
        dy_graphTypeVariable <- dygraphTypeVariable(subsetoutbdSNOT[grepl("SWC|TS|G", subsetoutbdSNOT$variable)==TRUE,],frequenceSelected)
        dy_graphPiezoWTD <- dygraphPiezo(subsetoutbdSNOT[subsetoutbdSNOT$variable %in% "WTD",],frequenceSelected)
        dy_graphPiezoTW <- dygraphPiezo(subsetoutbdSNOT[subsetoutbdSNOT$variable %in% "TW",],frequenceSelected)
        incProgress(0.1,translator$t("Finalisation ..."))

        tagList(dy_graphSite,dy_graphTypeVariable,dy_graphPiezoWTD,dy_graphPiezoTW)
      })
})


}

#ui <- fluidPage(
  #fluidRow(
  #  mod_mapSensorUI("extraction")
  #)
#)

#server <- function(input, output, session) {
  #callModule(mod_mapSensor, "extraction")
  
#}
#shinyApp(ui = ui, server = server)