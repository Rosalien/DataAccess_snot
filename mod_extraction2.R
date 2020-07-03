source("dependancies.R")
source("mod_update.R")
source("mod_loadData.R")
source("mod_description.R")
source("mod_tableData.R")
source("mod_mapSensor.R")
source("mod_timeSeries.R")
source("mod_summaryTable.R")
source("mod_boxplot.R")
source("mod_download.R")

# uiExtraction
mod_extractionui <- function(id){
  ns <- NS(id)

tabPanel(translator$t("Visualiser & Extraire les données"),
         tags$head(tags$style(HTML("hr {border-top: 1px solid #ecf0f1;}"))),
         tags$head(tags$style(HTML('#run{background-color:orange}'))),
         tags$head(HTML(googleAnalyticsParameter())),
         tags$script(HTML("$(document).one('shiny:idle',function() {ga('set','userId', Shiny.user);});")),
  fluidPage(
    fluidRow(
      column(2,
        mod_loadDataUI(ns("loadData"))
      ),
      column(10,style="padding-left:15px;",
        fluidRow(
          column(3,
            mod_updateUI(ns("update")),
            hr(),
            uiOutput(ns('captionRecapLabel')),
            hr()
            )
          ),
        (
          tabsetPanel(
          tabPanel(p(icon("info"),translator$t("Description")),
          	#uiOutput(ns('PR_results_print')),
          	mod_descriptionUI(ns("description"))),
          tabPanel(p(icon("table"),translator$t("Données")),mod_tableDataUI(ns("tableData")))
          #tabPanel(p(icon("map"),translator$t("Carte")),mod_mapSensorUI(ns("sensorMap"))),
   #       tabPanel(p(icon("line-chart"),translator$t("Série temporelle")),mod_timeSeriesUI(ns("timeSeries")))
          #tabPanel(p(icon("calculator"),translator$t("Résumé")),mod_summaryTableUI(ns("SummaryData"))),
          #tabPanel(p(icon("line-chart"),translator$t("Boxplot")),mod_boxplotUI(ns("boxplotData"))),
          #tabPanel(p(icon("download"),translator$t("Télécharger")),mod_downloadUI(ns('downloadData')))
          )
        )
        )
      )
    )
  )
}

mod_extraction <- function(input, output, session){
  ns <- session$ns

  # Création des evenReactive
  siteSelectedVariable <- reactive({
    caracSelection$siteSelectedVariable
  })

  r <- reactiveValues(
    r = reactiveValues()
  )

  observeEvent(update$updateSelection,{
    r$r$siteSelected <- caracSelection$siteSelected
    r$r$periodeSelected <- caracSelection$periodeSelected
    r$r$variableSelected <- caracSelection$variableSelected
  #  r$r$sqlOutputQuery <- sqlOutputQuery()
  })

  observeEvent(update$updateFrequence,{

  if(is.null(caracSelection$dayNightSelected)){
    r$r$dayNightSelected<- "day/night"
  }else{
    r$r$dayNightSelected <- caracSelection$dayNightSelected
}
  if(is.null(caracSelection$frequenceSelected)){
    r$r$frequenceSelected  <- "day"
  }else{
    r$r$frequenceSelected <- caracSelection$frequenceSelected
  }
})  


  # Chargement module update
  update <- callModule(module = mod_update, id = "update")
  
  updateSelection <- reactive({
    update$updateSelection
  })

  updateFrequence <- reactive({
    update$updateFrequence
  })

  # Données descriptives + reactive des variables sélectionnées
  caracSelection <- callModule(module = mod_loadData, id = "loadData")

  siteSelectedVariable <- reactive({
    caracSelection$siteSelectedVariable
  })

  siteSelected <- eventReactive(updateSelection(),{
    print(caracSelection$siteSelected)
    caracSelection$siteSelected
  })

  periodeSelected <- eventReactive(updateSelection(),{
    caracSelection$periodeSelected
  })

  variableSelected <- eventReactive(updateSelection(),{
     print(caracSelection$variableSelected)
    caracSelection$variableSelected
  })

  dayNightSelected <- eventReactive(updateFrequence(),{
    caracSelection$dayNightSelected
  })

  frequenceSelected <- eventReactive(updateFrequence(),{
    caracSelection$frequenceSelected
  })

  # Reactive pour lancer la requête + jointure
  sqlOutputQuery <- eventReactive(update$updateSelection,{
    # Lancement de la requête + melt des données
    print("queryDataSNOT")
    meltvalue <- queryDataSNOT(variableSelected(),siteSelected(),periodeSelected())
    validate(need(nrow(meltvalue)>0,translator$t("Période d'analyse sans données")))
    print("Fin queryDataSNOT")
    print("Jointure avec caracData")
    # Jointure avec data.table (caracData[meltvalue])
    caracSelection$caracData[meltvalue]
  })

# Création de la reactive aggregateData (requête sql + aggrégation)
  aggregateData <- reactive({   
    # Données issues de la requête
    dataSNOT <- sqlOutputQuery()
    
    # Calcul de la séquence jour et nuit pour filtrer les données au besoin
    if(dayNightSelected()!="day/night"){
      dbDayandNight <- dbDayNight(dataSNOT)
    }else{
      dbDayandNight <- dataSNOT
    }
    print("dbselect")
    dbselect(dbDayandNight,dayNightSelected(),frequenceSelected(),siteSelected(),variableSelected())  
  })


output$PR_results_print <- renderPrint({
  	print("----------From eventReactive----------")
  	print(r$r$siteSelected)
  	print(r$r$periodeSelected)
  	print(r$r$variableSelected)
  	print(r$r$dayNightSelected)
  	print(r$r$frequenceSelected)
})                                                                
 
# Création des reactives
  captionRecap <- reactive({
    caption <- paste0("Date : ",format(periodeSelected()[1],"%d-%m-%Y")," to ",format(periodeSelected()[2],"%d-%m-%Y"),
      " | Frequency : ",frequenceSelected()," | Type of day : ",dayNightSelected())
    h5(style="border-top-width: 0px;margin-top: 0px;margin-bottom: 0px;margin-right: -1000px",tags$b(caption))
})

  output$captionRecapLabel <- renderUI({
        captionRecap()
  })

  # Module pour la table description
  callModule(module = mod_description, id = "description",caracData=caracSelection$caracData,
                                                          siteSelected=siteSelected(),
                                                          variableSelected=variableSelected())

  # Module pour voir les données
  callModule(module=mod_tableData,id = "tableData",aggregateData=aggregateData())#,r=r)

  # Chargement du module pour visualiser les instruments
  #callModule(module=mod_mapSensor,id = "sensorMap",caracCarto=caracSelection$caracCarto,
   #                                               col_station=caracSelection$col_station,
    #                                              siteSelectedVariable=reactive(caracSelection$siteSelectedVariable))

  # Chargement du module sur les timeseries
  #callModule(module=mod_timeSeries,id = "timeSeries",sqlOutputQuery=sqlOutputQuery(),
      		                                         #dayNightSelected=r$r$dayNightSelected,
                                                  	 #siteSelected=r$r$siteSelected,
                                                     #frequenceSelected=r$r$frequenceSelected,
                                                     #variableSelected=r$r$variableSelected)

  # Module sur les statistiques descriptives
  #callModule(module=mod_summaryTable,id = "SummaryData",sqlOutputAndAggregate=sqlOutputAndAggregate())
  
  # Module sur les boxplots
  #callModule(module=mod_boxplot,id = "boxplotData",sqlOutputAndAggregate=sqlOutputAndAggregate(),
   #                                                sqlOutputQuery=sqlOutputQuery(),
    #                                               dayNightSelected=dayNightSelected(),
     #                                              siteSelected=siteSelected(),
      #                                             frequenceSelected=frequenceSelected(),
       #                                            variableSelected=variableSelected())
  # Module sur le téléchargement des données
  #callModule(module=mod_download,id = "downloadData",caracData=caracSelection$caracData,
   #                                                  sqlOutputAndAggregate=sqlOutputAndAggregate(),
    #                                                 siteSelected=siteSelected(),
     #                                                frequenceSelected=frequenceSelected(),
      #                                               variableSelected=variableSelected(),
       #                                              variableChecked=reactive(caracSelection$variableChecked))
}#Fin du module


ui <- fluidPage(
  fluidRow(
    mod_extractionui("extraction")
  )
)

server <- function(input, output, session) {
  callModule(mod_extraction, "extraction")
  
}
shinyApp(ui = ui, server = server)

