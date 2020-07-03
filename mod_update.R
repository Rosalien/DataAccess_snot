translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"


mod_updateUI <- function(id){
  ns <- NS(id)

  fluidRow(
          # Mise à jour de la sélection
         column(12,uiOutput(ns('updateSelectionPanel'))),
          # Mise à jour de la fréquence et de l'option dayNight
         column(12,offset = 0,style='margin: 0px 0px 0px 300px;top:-31px;',uiOutput(ns('updateFrequencePanel')))
  )
}

mod_update <- function(input, output, session){
  ns <- session$ns

# Reactive pour mettre à jour la sélection globale
  #updateSelection <- reactive({
   # input$updateselection0
  #})

# Reactive pour mettre à jour la sélection globale
  #updateFrequence <- reactive({
    #input$updatefrequence0
  #})

# ActionButton mise à jour de la sélection des variables
output$updateSelectionPanel <- renderUI({
  actionBttn(ns("updateselection0"),icon = icon("sync"),style = "fill", color = "success",
    label = translator$t("Mise à jour de la sélection"),size="sm")
})

# ActionButton mise à jour de la fréquence
output$updateFrequencePanel <- renderUI({
  actionBttn(ns("updatefrequence0"),icon = icon("sync"),style = "fill", color = "success",
    label = translator$t("Mise à jour de la fréquence et du type de jour"),size="sm")
})

toReturn <- reactiveValues(updateFrequence=reactiveValues(),
                           updateSelection=reactiveValues())

observe({
	toReturn$updateFrequence <- input$updatefrequence0
	toReturn$updateSelection <- input$updateselection0
})

return(toReturn)
}

#ui <- fluidPage(
  #fluidRow(
    #mod_updateUI("extraction")
  #)
#)

#server <- function(input, output, session) {
  #callModule(mod_update, "extraction") 
#}
#shinyApp(ui = ui, server = server)