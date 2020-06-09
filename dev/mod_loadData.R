#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title mod_loadDataui
#' @description This function has to be set in the UI part of a shiny application
#' it adds the load data box.
#' load_data function has to be set in the Server part.
#' @param id An id that will be used to create a namespace
#' @return UI page
#' @examples
#' \dontrun{
#' # In UI :
#' load_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_data, id = "mod1")
#'}

translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"

mod_loadDataUI <- function(id){
  
  ns <- NS(id)

  # Chargement des données 
  caracData <- caracdata(language)[order(variable),]

  # Pour mettre des checkboxs sur différentes colonnes
  tweaks <- list(tags$head(tags$style(HTML(tweaks()))))

  # Fonction pour générer des checkboxGroupInput à la volée 
  dropdowSelectionStation <- function(caracDataStationSelection,nameNS){
  if(nrow(caracDataStationSelection)>0){
    lapply(1:length(unique(caracDataStationSelection$site_nom)),function(x){
    checkboxGroupInput(ns(paste(nameNS,x,sep="")),unique(caracDataStationSelection$site_nom)[x],selected = NULL,   
    choiceNames = caracDataStationSelection[site_nom %in% unique(caracDataStationSelection$site_nom)[x],3][[1]],
    choiceValues = caracDataStationSelection[site_nom %in% unique(caracDataStationSelection$site_nom)[x],]$site)
  })
  }else{}
}

choicesFrequence <- c("30 min", "hour","day","week","month","year")
names(choicesFrequence) <- c(translator$t("Infra-jour"),translator$t("Heure"),translator$t("Jour"),translator$t("Semaine"),translator$t("Mois"),translator$t("Année"))

fluidRow(
  column(12,
        dropdownButton(icon = icon("cloudversify"),width = "150%",circle = FALSE,status = "warning",label = translator$t("Sites & Stations"),
          dropdowSelectionStation(unique(caracData[grepl("pz", caracData$site)==FALSE,list(site,site_nom,station_description)]),nameNS="siteSNOT_")
          ),
        br(),
        dropdownButton(icon = icon("tint"),width = "100%",circle = FALSE,status = "warning",label = translator$t("Piézomètres"),
          dropdowSelectionStation(unique(caracData[grepl("pz", caracData$site)==TRUE,list(site,site_nom,station_nom)]),"sitePiezo_")),
        br(),
        # Panel période
        uiOutput(ns('periodPanel')),
        # Panel Day/Night
        uiOutput(ns('dayNightPanel')),
        # Panel fréquence
        fluidRow(
          column(7,
          pickerInput(ns("frequenceSNOT"), label = translator$t("Fréquence"),
            choices = choicesFrequence,selected = "day",options = list(style = "btn-primary"))),
            #          uiOutput(ns('pickerFrequence'))),
          column(5,offset = 0,style='left:0px;top:3px;',conditionalPanel(condition=paste0("input['",ns("frequenceSNOT"),"']=='hour'"),uiOutput(ns('frequenceHoraire')))),
          column(5,offset = 0,style='left:0px;top:3px;',conditionalPanel(condition=paste0("input['",ns("frequenceSNOT"),"']=='month'"),uiOutput(ns('frequenceMensuelle')))),
          column(5,offset = 0,style='left:0px;top:3px;',conditionalPanel(condition=paste0("input['",ns("frequenceSNOT"),"']=='year'"),uiOutput(ns('frequenceAnnuelle'))))
          ),
          dropdownButton(icon = icon("hand-pointer"),width = "650%",circle = FALSE, status = "warning",label = translator$t("Sélection des variables"),
            column(3,checkboxGroupInput(ns("variableEC"),label=translator$t("Flux de GES par eddy-covariance"),selected = NULL,choiceNames=NULL,choiceValues=NULL)),
            column(3,checkboxGroupInput(ns("variableBM"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL),
              checkboxInput(ns("variableWindRose"),label=translator$t("Rose des vents"),value=NULL)),
            column(4,
              fluidPage(tweaks,fluidRow(column(width = 8, list(h5(tags$b(translator$t("Température du sol"))),tags$div(align = 'left',class = 'multicol',
                checkboxGroupInput(ns("variableTS"),label=NULL,selected = NULL,choiceNames=NULL,choiceValues=NULL)))))),
              fluidPage(tweaks,fluidRow(column(width = 8, list(h5(tags$b(translator$t("Teneur en eau du sol"))),tags$div(align = 'left',class = 'multicol',
                checkboxGroupInput(ns("variableSWC"),label=NULL,selected = NULL,choiceNames=NULL,choiceValues=NULL)))))),
              fluidPage(tweaks,fluidRow(column(width = 8, list(h5(tags$b(translator$t("Flux de chaleur dans le sol"))),tags$div(align = 'left',class = 'multicol',
                checkboxGroupInput(ns("variableG"),label=NULL,selected = NULL,choiceNames=NULL,choiceValues=NULL))))))),
              column(2,checkboxGroupInput(ns("variableWTD"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL)),
              column(2,checkboxGroupInput(ns("variableBiogeo"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL))
          )#,
       )#,
      )
}

#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title mod_loadData
#' @description This function has to be set in the Server part of a shiny application
#'     it adds the load data windows.
#'     load_dataUI function has to be set in the UI part.
#' @param input Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. Done by callModule automatically.
#' @return Server logic
#' @examples
#' \dontrun{
#' # In UI :
#' load_dataUI(id = "mod1")
#' # In Server
#' data_module1 <- callModule(module = load_data, id = "mod1")
#'}
mod_loadData <- function(input, output, session){#,updateFrequence,updateSelection){

  ns <- session$ns

##################################Chargement des données####################################
  col_station <- read.csv("datatype_couleur.csv",sep=";",header=TRUE,stringsAsFactors=FALSE)
  caracData <- caracdata(language)[order(variable)]
  caracCarto <- caracCarto(language)

  ## Sélection de la colonne en fonction de la langue
  col_station <- col_station[,c("datatype","couleur",paste0("type_",language))]
  names(col_station) <- c(names(col_station[1:2]),"type")
############################################################################################


################################################renderUI####################################
# renderUI pour les fréquences
output$frequenceHoraire <- renderUI({
  numericInput(ns("hour"),label=h5(tags$b("")),value=1,min = 1, max = 23,step=1)
})

output$frequenceMensuelle <- renderUI({
  numericInput(ns("month"),label=h5(tags$b("")),value=1,min = 1, max = 12,step=1)
})

output$frequenceAnnuelle <- renderUI({
  numericInput(ns("year"),label=h5(tags$b("")),value=1,min = 1, max = 50,step=1)
})

#output$pickerFrequence <- renderUI({
  #choicesFrequence <- c("30 min", "hour","day","week","month","year")
  #names(choicesFrequence) <- c(translator$t("Infra-jour"),translator$t("Heure"),translator$t("Jour"),translator$t("Semaine"),translator$t("Mois"),translator$t("Année"))
  #pickerInput(ns("frequenceSNOT"), label = translator$t("Fréquence"),choices = choicesFrequence,selected = "day",options = list(style = "btn-primary"))
#})
 
# renderUI pour afficher la période min et max des sites sélectionnée. Filtre sur caracData avec siteSelectedVariable()
output$periodPanel <- renderUI({
  minmaxdate <- caracData[site %in% siteSelectedVariable(),c(min(mindate,na.rm=TRUE),max(maxdate,na.rm=TRUE))]
  dateRangeInput(ns("dateSNOT"),"Période",min = minmaxdate[1],max = minmaxdate[2],start = as.Date(minmaxdate[1],"%Y-%m-%d"),
                 end = as.Date(minmaxdate[2],"%Y-%m-%d"),format = "dd-mm-yyyy",language="fr",separator = "au")
})
# renderUI pour afficher le panel dayNightPanel
output$dayNightPanel <- renderUI({
  # Création de plusieurs listes pour l'internatinoalisation
  choicesDayNight <- c("day/night","day","night")
  names(choicesDayNight) <- c(translator$t("Jour & Nuit"),translator$t("Jour"),translator$t("Nuit"))
  awesomeRadio(ns("dayNight"),label = translator$t("Jour & Nuit"),choices = choicesDayNight,selected = "day/night",inline = TRUE)
})

# eventReactive sur la fréquence
#frequenceSelected <- eventReactive(updateFrequence(),{
frequenceSelected <- reactive({
  if(input$frequenceSNOT=="hour"){
    frequence <- paste(input$hour," hour",sep="")   
  }else if(input$frequenceSNOT=="month"){
    frequence <- paste(input$month," month",sep="")
    print(frequence)
  }else if(input$frequenceSNOT=="year"){
    frequence <- paste(input$year," year",sep="")
    print(frequence)
  }else{
    frequence <- input$frequenceSNOT    
  }
return(frequence)
})

# Reactive utilisée uniquement pour mettre à jour la liste des variables dans les widgets
siteSelectedVariable <- reactive({
  site <- c(input$siteSNOT_1,input$siteSNOT_2,input$siteSNOT_3,input$siteSNOT_4,input$sitePiezo_1,input$sitePiezo_2,input$sitePiezo_3,input$sitePiezo_4)
  site
})

# Reactive pour la sélection de la période de temps. Utilisée pour les graphes, les tables et les extractions
#periodeSelected <- eventReactive(updateSelection(),{
periodeSelected <- reactive({
  periode <- c(input$dateSNOT[1],input$dateSNOT[2])
  validate(need(!is.null(periode),"Aucune date choisie"))
  periode
})

# Reactive utilisée pour les graphes, les tables et les extractions
#siteSelected <- eventReactive(updateSelection(),{
siteSelected <- reactive({
  site <- c(input$siteSNOT_1,input$siteSNOT_2,input$siteSNOT_3,input$siteSNOT_4,input$sitePiezo_1,input$sitePiezo_2,input$sitePiezo_3,input$sitePiezo_4)
  validate(need(!is.null(site),"Aucune station sélectionnée"))
  site
})

#variableSelected <- eventReactive(updateSelection(),{
variableSelected <- reactive({
    variables <- c(input$variableEC,input$variableSWC,input$variableG,input$variableTS,input$variableBM,input$variableWTD,input$variableBiogeo)
    validate(need(!is.null(variables),translator$t("Aucune variable sélectionnée. Veuillez sélectionner au moins une station et variable")))
    variables
  })

variableChecked<- reactiveValues(checked=NULL)

# observeEvent pour mettre à jour les variables checkées

#observeEvent(updateSelection(),{
observe({
    variables <- c(input$variableEC,input$variableSWC,input$variableG,input$variableTS,input$variableBM,input$variableWTD,input$variableBiogeo)
    if(!is.null(variables)){
      variableChecked$checked <- variables
    }
})

# eventReactive lié au bouton de mise à jour fréquence/dayNight
#dayNightSelected <- eventReactive(updateFrequence(),{
dayNightSelected <- reactive({
    input$dayNight
})
######################################Construction des checkin variables####################################

#########################################################################################
checkinEC <- reactiveValues(checked = NULL)
# Variables EC
  observe({
    input$variableEC
    isolate({
      if(!is.null(input$variableEC)){
          checkinEC$checked <- input$variableEC        
      }else{checkinEC$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[site %in% siteSelectedVariable() & grepl("ec1", caracData$site)==TRUE,list(variable,definition)])
    updateCheckboxGroupInput(session,
                         "variableEC", translator$t("Flux de GES par eddy-covariance"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definition,
                         selected = checkinEC$checked)
 })
#########################################################################################
checkinBM <- reactiveValues(checked = NULL)
# Variables meteo
  observe({
    input$variableBM
    isolate({
      if(!is.null(input$variableBM)){
          checkinBM$checked <- input$variableBM
      }else{checkinBM$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[site %in% siteSelectedVariable() & grepl("bm", caracData$site)==TRUE & grepl("SWC|TS|G", caracData$variable)==FALSE,list(variable,definition)])
    updateCheckboxGroupInput(session,
                         "variableBM", translator$t("Météo"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definition,
                         selected = checkinBM$checked)
 })

#########################################################################################
# Liste des variables WTD
checkinWTD <- reactiveValues(checked = NULL)
# Variables piezo
  observe({
    input$variableWTD
    isolate({
      if(!is.null(input$variableWTD)){
          checkinWTD$checked <- input$variableWTD
      }else{checkinWTD$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[site %in% siteSelectedVariable() & caracData$variable %in% c("WTD","TW"),list(variable,definition)])
      updateCheckboxGroupInput(session,
                           "variableWTD", translator$t("Hydrologie"),
                           choiceValues = listVariables$variable,
                           choiceNames = listVariables$definition,
                           selected = checkinWTD$checked)
 })

#########################################################################################
# Liste des variables Biogeo
checkinBiogeo <- reactiveValues(checked = NULL)
# Variables Biogeo
  observe({
    input$variableBiogeo 
    isolate({
      if(!is.null(input$variableBiogeo)){
          checkinBiogeo$checked <- input$variableBiogeo 
      }else{checkinBiogeo$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[site %in% siteSelectedVariable() & caracData$variable %in% c("FDOC","FPOC"),list(variable,definition)])
      updateCheckboxGroupInput(session,
                           "variableBiogeo", translator$t("Biogéochimie"),
                           choiceValues = listVariables$variable,
                           choiceNames = listVariables$definition,
                           selected = checkinBiogeo$checked)
 })

#########################################################################################
 #WindRose <- eventReactive(updateSelection(),{
 WindRose <- reactive({
    input$variableWindRose
  })

  # reactiveValues
  bm1Checked<- reactiveValues(checked=NULL)

  # observe event for updating the reactiveValues
  observe({
    siteBM <- match(siteSelectedVariable()[grepl("bm1",siteSelectedVariable())],siteSelectedVariable())
    isolate({
    if(length(siteBM)>0){
      bm1Checked$checked <- siteBM
    }else{bm1Checked$checked <- NULL}
  })
  })

observe({
    toggleState("variableWindRose", condition = !is.null(bm1Checked$checked))
  })
#########################################################################################
checkinSWC <- reactiveValues(checked = NULL)#reactiveValues(EC = NULL,puts = NULL)

# List des variables SWC
  observe({
    input$variableSWC
    isolate({
      if(!is.null(input$variableSWC)){
        checkinSWC$checked <- input$variableSWC
      }else{checkinSWC$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- checkboxVariablePedo(dbCarac=caracData,codeVariable="SWC",siteBM=siteSelectedVariable())
    updateCheckboxGroupInput(session,
                         "variableSWC", translator$t("Météo"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinSWC$checked)
 })
#########################################################################################
# Liste des variables TS
checkinTS <- reactiveValues(checked = NULL)#reactiveValues(EC = NULL,puts = NULL)

    observe({
    input$variableTS
    isolate({
      if(!is.null(input$variableTS)){
        checkinTS$checked <- input$variableTS
      }else{checkinTS$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- checkboxVariablePedo(dbCarac=caracData,codeVariable="TS",siteBM=siteSelectedVariable())
    updateCheckboxGroupInput(session,
                         "variableTS", translator$t("Météo"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinTS$checked)
 })
#########################################################################################
# Liste des variables G
checkinG <- reactiveValues(checked = NULL)#reactiveValues(EC = NULL,puts = NULL)

observe({
    input$variableG
    isolate({
      if(!is.null(input$variableG)){
        checkinG$checked <- input$variableG
      }else{checkinG$checked <- NULL}
    })
  })

observe({ 
    listVariables <- checkboxVariablePedo(dbCarac=caracData,codeVariable="G",siteBM=siteSelectedVariable())
    updateCheckboxGroupInput(session,
                         "variableG", translator$t("Météo"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinG$checked)
 })

######################################Fin de la construction des checkin variables####################################

toReturn <- reactiveValues(caracData=reactiveValues(),
                           caracCarto=reactiveValues(),
                           col_station=reactiveValues(),
                           siteSelectedVariable=reactiveValues(),
                           siteSelected=reactiveValues(),
                           periodeSelected=reactiveValues(),
                           dayNightSelected=reactiveValues(),
                           frequenceSelected=reactiveValues(),
                           variableChecked=reactiveValues(),
                           variableSelected=reactiveValues())

observe({
  #print("--------------observe mod_loadData--------------")
  #print(paste0("siteSelectedVariable() :", siteSelectedVariable()))
  toReturn$caracData <- caracData
  toReturn$caracCarto <- caracCarto
  toReturn$col_station <- col_station
  toReturn$siteSelectedVariable <- siteSelectedVariable()
  toReturn$siteSelected <- siteSelected()
  toReturn$periodeSelected <- periodeSelected()
  toReturn$variableChecked <- variableChecked$checked
  toReturn$variableSelected <- variableSelected()
  toReturn$frequenceSelected <- frequenceSelected()
  toReturn$dayNightSelected <- input$dayNight

  print(toReturn)
})

#observeEvent(updateFrequence(),{
  #print("--------------observeEvent mod_loadData--------------")
  #print(paste0("dayNightSelected() ",dayNightSelected()))
  #print(paste0("frequenceSelected() : ",frequenceSelected()))
  #toReturn$dayNightSelected <- dayNightSelected()
  #toReturn$frequenceSelected <- frequenceSelected()
#})

#observeEvent(updateSelection(),{
    #print("--------------observeEvent mod_loadData--------------")
    #print(paste0("siteSelected() : ",siteSelected()))
    #print(paste0("periodeSelected() : ",periodeSelected()))
    #print(paste0("variableSelected() : ",variableSelected()))
    #print(paste0("variableChecked$checked : ",variableChecked$checked))
#    toReturn$siteSelected <- siteSelected()
    #toReturn$periodeSelected <- periodeSelected()
    #toReturn$variableChecked <- variableChecked$checked
    #toReturn$variableSelected <- variableSelected()
#})






return(toReturn)

}

ui <- fluidPage(
  fluidRow(
    mod_loadDataUI("extraction")
  )
)

server <- function(input, output, session) {
  callModule(mod_loadData, "extraction")
  
}
shinyApp(ui = ui, server = server)

