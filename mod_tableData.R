translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"

mod_tableDataUI <- function(id){
  ns <- NS(id)

  fluidRow(style="padding-left:15px;",
    withSpinner(dataTableOutput(ns("Data")),type=5)
    )
}

mod_tableData <- function(input, output, session,aggregateData){

  ns <- session$ns

  aggregateDataR <- reactive({
    print("reactive_modtableData")
    print(aggregateData)
    validate(need(nrow(aggregateData)>0,translator$t("Période d'analyse sans données")))
    aggregateData
  })

# renderUI pour voir les données
  output$Data <- DT::renderDataTable({
    withProgress(message = translator$t("Préparation des données ..."),{
      incProgress(0.7,translator$t("Préparation de la table ..."))       
      tableData <- aggregateDataR()[,list(site,Date,variable,definition,value)]
      retData <- DT::datatable(tableData,rownames= FALSE,filter = 'top')
      incProgress(0.3,translator$t("Fin de la préparation de la table ..."))       
    })
    return(retData)
})#

}

#ui <- fluidPage(
  #fluidRow(
  #  mod_updateui("extraction")
  #)
#)

#server <- function(input, output, session) {
  #callModule(mod_update, "extraction")
  
#}
#shinyApp(ui = ui, server = server)