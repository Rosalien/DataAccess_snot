translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"

mod_mapSensorUI <- function(id){
  ns <- NS(id)

  fluidRow(style="padding-left:15px;",leafletOutput(ns("sensorMap"),width="100%",height = 700))
}

mod_mapSensor <- function(input, output, session,caracCarto,col_station,siteSelectedVariable){
  ns <- session$ns

  output$sensorMap <- renderLeaflet({
    mapSensorSelected <- unique(caracCarto[code_site_station %in% siteSelectedVariable(),
                                list(zet_coordonnees_bbox,station_description,datatype)])
    # Ajout des couleurs pour les types de stations
    mapSensorSelected <- merge(mapSensorSelected, col_station, by.x = "datatype", by.y = "datatype", all.x = TRUE,all.y=FALSE)
    # Création de la map
    sensorSelectedMap(mapSensorSelected,FALSE)
})

}
