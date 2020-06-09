translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"

mod_boxplotUI <- function(id){
  ns <- NS(id)

  fluidRow(style="padding-left:15px;",
            uiOutput(ns('facetWrapPannel')),
            withSpinner(plotOutput(ns("boxPlotggplotSite")),type=5),
            withSpinner(uiOutput(ns("boxPlotggplotTypeVariable")),type=5)
    )
}

mod_boxplot <- function(input, output, session,sqlOutputAndAggregate,sqlOutputQuery,dayNightSelected,
  siteSelected,frequenceSelected,variableSelected){
  ns <- session$ns

  output$facetWrapPannel <- renderUI({
          radioButtons(ns('facetWrapOption2'),label="",inline=TRUE,choices = c("none","month","season","year"),selected="none")
  })

# reactive pour le croisement des données des boxplots
facetWrapSelectedChart <- reactive({
    	input$facetWrapOption2
})

# renderUI pour les boxplot 
# Comparaisons entre sites (la construction est décomposée pour l'affichage des messages)
  output$boxPlotggplotSite <- renderPlot({  
      withProgress(message = translator$t("Préparation des données ..."),{
        
        incProgress(0.3,translator$t("Requête de la base de données ..."))       
        print("Requête pour boxplot")
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
        print("Aggrégation pour boxplot")
        subsetoutbdSNOT <- dbselect(dbDayandNight,dayNightSelected,frequenceSelected,siteSelected,variableSelected)

        incProgress(0.3,translator$t("Préparation du graphique ..."))
        print("Préparation boxplot")
        
        boxplotSite <- ggplotBoxplotSite(subsetoutbdSNOT[grepl("SWC|TS|G", subsetoutbdSNOT$variable)==FALSE,],facetWrapSelectedChart())
        print("Mise en page des boxplot")
        if(!is.null(boxplotSite))(do.call(grid_arrange_shared_legend,c(boxplotSite,list(position="bottom"))))

        incProgress(0.1,translator$t("FinalifacetWrapSelectedTablesation ..."))
        })
})

  # renderUI pour les Boxplot des variables meteosol
  output$boxPlotggplotTypeVariable <- renderUI({
      subsetoutbdSNOT <- sqlOutputAndAggregate
      plotlyboxplotTypeVariable(subsetoutbdSNOT[grepl("SWC|TS|G", subsetoutbdSNOT$variable)==TRUE,])
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