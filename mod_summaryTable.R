translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"

mod_summaryTableUI <- function(id){
  ns <- NS(id)

  fluidRow(style="padding-left:15px;",
          uiOutput(ns('facetWrapPannel')),
          withSpinner(dataTableOutput(ns("SummaryData")),type=5)# end of "chart" tab panel
    )
}

mod_summaryTable <- function(input, output, session,captionRecap,sqlOutputAndAggregate){
  ns <- session$ns

  output$facetWrapPannel <- renderUI({
          radioButtons(ns('facetWrapOption1'),label="",inline=TRUE,choices = c("none","month","season","year"),selected="none")
  })

  # reactive pour le croisement des données de la table stats
  facetWrapSelectedTable <- reactive({
      input$facetWrapOption1
  })

  output$SummaryData <- DT::renderDataTable({
        if(facetWrapSelectedTable()!="none"){
          retData <- sqlOutputAndAggregate[,list(Min=round(min(value,na.rm=TRUE),2),
                                 Q1=round(quantile(value, .25, na.rm=TRUE),2),
                                 Q2=round(median(value,na.rm=TRUE),2),
                                 M=round(mean(value,na.rm=TRUE),2),
                                 Q3=round(quantile(value, .75, na.rm=TRUE),2),
                                 Max=round(max(value, na.rm=TRUE),2),
                                 sd=round(sd(value, na.rm = TRUE),2)),
                                 by=c('variable','site',facetWrapSelectedTable())]
          names(retData) <- c("Variable","Site",facetWrapSelectedTable(),"Min","Q1",translator$t("Médiane"),translator$t("Moyenne"),"Q3","Max",translator$t("Écart-type"))                      
        }else{
          retData <- sqlOutputAndAggregate[,list(Min=round(min(value,na.rm=TRUE),2),
                                 Q1=round(quantile(value, .25, na.rm=TRUE),2),
                                 Q2=round(median(value,na.rm=TRUE),2),
                                 M=round(mean(value,na.rm=TRUE),2),
                                 Q3=round(quantile(value, .75, na.rm=TRUE),2),
                                 Max=round(max(value, na.rm=TRUE),2),
                                 sd=round(sd(value, na.rm = TRUE),2)),
                                 by=c('variable','site')]
          names(retData) <- c("Variable","Site","Min","Q1",translator$t("Médiane"),translator$t("Moyenne"),"Q3","Max",translator$t("Écart-type"))                      
        }
        retData <- DT::datatable(retData, rownames= FALSE,caption = htmltools::tags$caption(translator$t("Statistiques descriptives"),
          style='font-weight:bold; color:#333'), extensions = 'Buttons',
          options = list(dom = "Blfrtip", buttons = list("copy",list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = "Download")),
                    pageLength = 10, autoWidth = TRUE))
  return(retData)
})#


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