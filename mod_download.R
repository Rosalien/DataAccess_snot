translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

language <- "en"

mod_downloadUI <- function(id){
  ns <- NS(id)
  fluidRow(style="padding-left:15px;",
    fluidRow(style="padding-left:15px;",
          column(3,dropdownButton(includeMarkdown("www/Licence.md"),width = "200%",icon=icon("creative-commons"),
                   circle=FALSE,label=translator$t("Résumé de la licence"),status = "info")),
              column(3,dropdownButton(includeMarkdown("www/Citation.md"),width = "200%",icon=icon("quote-right"),
                    circle=FALSE,label=translator$t("Comment citer ?"),status = "info"))),
            br(),
            fluidRow(style="padding-left:15px;",
              column(3,dropdownButton(includeMarkdown("www/Metadata.md"),width = "200%",icon=icon("recycle"),
                    circle=FALSE,label=translator$t("Métadonnées"),status = "info")),
              column(3,
                dropdownButton(includeMarkdown("www/formatTable.md"),width = "200%",icon=icon("table"),
                    circle=FALSE,label=translator$t("Format de la table"),status = "info"))),
            fluidRow(style="padding-left:15px;",
              br(),
              column(3,
                uiOutput(ns("formatTable"))
                ),
              column(3,br(),h6(""),useShinyalert(),actionButton(ns('downloadDataConfirmation'),icon= icon("download"),label=translator$t("Télécharger")))
              ),
            hr(),
            withSpinner(tableOutput(ns("RecapData")),type=5),
            hr(),         
            withSpinner(tableOutput(ns("Citation")),type=5)
  )
}

mod_download <- function(input, output, session,caracData,sqlOutputAndAggregate,
  siteSelected,frequenceSelected,variableSelected,variableChecked){
  ns <- session$ns

  # renderUI pour faire la synthèse de la requête avant le téléchargement
  output$RecapData <- renderTable({
    recapTableDownload <- recapTable()[,list(code_jeu,site_nom,station_nom,variable,definition,unite)]
    names(recapTableDownload) <- c(translator$t("Jeu de données"),"Site","Station","Variable","Description",translator$t("Unité"))
    recapTableDownload
})#

  output$formatTable <- renderUI({
    radioGroupButtons(inputId = ns("tableType"),label = translator$t("Format de la table"),
      choices = c("Verticale","Horizontale"),individual = TRUE)
  })

  # renderUI pour la synthèse des citations à réaliser
  output$Citation <- renderTable({
    CodeJeu <- unique(recapTable()[,code_jeu])
    Jeu <- tableJeu()[code_jeu %in% CodeJeu,list(code_jeu,citation)] 
    names(Jeu) <- c(translator$t("Jeu de données"),"Citation")
    Jeu
})#

# Reactive pour construire une table récapitulative avant téléchargement
  recapTable <- reactive({
    recapTable <- unique(caracData[site %in% siteSelected & variable %in% variableSelected,])
    CodeJeu <- recapTable[,gsub("_[^_]+$","",code_jeu)]
    SiteJeu <- gsub("/[^_]+","",gsub("_[^_]+","",CodeJeu))
    recapTable$code_jeu <- paste(SiteJeu,"-",gsub("^.*?-","",CodeJeu),sep="")
    recapTable
})

# Désactivation du bouton télécharger si aucune variable est sélectionnée
  observe({
    toggleState("downloadDataConfirmation", condition = !is.null(variableChecked))
})

# Désactivation du téléchargement si le bouton confirmation n'est pas sélectionné
  observeEvent(input$readConfirmation,{
    toggleState("downloadData", condition = (input$readConfirmation==TRUE))
})

# observeEvent pour viusalier les conditions de téléchargement lorsqu'on clique sur le bouton
    observeEvent(input$downloadDataConfirmation, {
      showModal(dataModal())
    })

# Reactive pour construire les métadonnées
  metadata_jeu <- reactive({
    # Construction du code Jeu avec caracData
    CodeJeu <- unique(caracData[site %in% siteSelected & variable %in% variableSelected,gsub("_[^_]+$","",code_jeu)])
    SiteJeu <- gsub("/[^_]+","",gsub("_[^_]+","",CodeJeu))
    CodeJeu <- unique(paste(SiteJeu,"-",gsub("^.*?-","",CodeJeu),sep=""))
   # print(CodeJeu)

    # Lancement de la requête curl pour chaque jdd
    lapply(CodeJeu,function(x){
      pivot_metadata <- robustCurl(curl_fetch_memory(paste("http://localhost:8081/rest/resources/pivot?codes_jeu=",x,sep="")))
      pivot_metadata <- ifelse((is.character(pivot_metadata))||(pivot_metadata$status_code==404),"Problème dans la génération des métadonnées",jsonlite::prettify(rawToChar(pivot_metadata$content)))
  })
})#

dataModal <- function(failed = FALSE){
      modalDialog(
        title = "Licence & conditions d'utilisation des données du SNO-T",
        span('Sauf mentions contraires, les données du SNO-T sont diffusées sous'),
        tags$a(href="https://creativecommons.org/licenses/by-sa/4.0/deed.fr", "licence Creative Commons Attribution - Attribution - Partage dans les Mêmes Conditions 4.0 International",target="_blank"),
        tags$br(),tags$br(),
        HTML('<center><a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a></center>'),
        #HTML('<center><img src="https://sourcesup.renater.fr/si-snot/Fig/by-nc-sa.eu.png"></center>'),
        tags$hr(),
        tags$a(href="https://data-snot.cnrs.fr/snot/resources/manual/charte.pdf", "Conditions d'utilisation des données du SNO-T",target="_blank"),
        checkboxInput(ns("readConfirmation"), translator$t("J'accepte les conditions d'utilisation"), FALSE),
        footer = tagList(
          modalButton(translator$t("Annuler")),
          downloadButton(ns("downloadData"), translator$t("Télécharger"))
        ),
        easyClose = TRUE
      )
}

# Téléchargement des données
  output$downloadData <- downloadHandler(

    filename = function() {
      paste("Data_SNOT_",format(Sys.time(), '%d_%m_%Y_%X'),".zip",sep="")
    },
    content = function(fname){

      withProgress(message = translator$t("Début de l'extraction ..."),{
      incProgress(0.1,translator$t("Requête de la base de données ..."))

      setwd(tempdir())
      csvfile <- paste("Data_SNOT_",format(Sys.time(), '%d_%m_%Y_%X'),".csv",sep="")
      
      # Génération des métadonnées
      CodeJeu <- unique(caracData[site %in% siteSelected & variable %in% variableSelected,gsub("_[^_]+$","",code_jeu)])
      SiteJeu <- gsub("/[^_]+","",gsub("_[^_]+","",CodeJeu))
      CodeJeu <- unique(paste(SiteJeu,"-",gsub("^.*?-","",CodeJeu),sep=""))

      # Extraction des données
          if(input$tableType=="Verticale"){
            extractionDataCpt <- sqlOutputAndAggregate
            extractionDataCpt <- extractionDataCpt[,list(site,Date=as.character(Date),value,variable)]
          }else{
            extractionDataCpt <- extracData(sqlOutputAndAggregate,frequenceSelected)
          }
      incProgress(0.6,translator$t("Création de la métadonnée ..."))

      metadataJeu <- metadata_jeu()
      metadata <- lapply(1:length(metadataJeu),function(x){
        pivot <- paste("Metadata_",CodeJeu[x],".json", sep="")
        write(metadataJeu[[x]], pivot)
        return(pivot)
        })
      
      incProgress(0.2,translator$t("Création du fichier ..."))
      write.csv(extractionDataCpt,csvfile,row.names=FALSE)
      incProgress(0.1,translator$t("Finalisation ..."))
      })#Fin de la progression

      if (file.exists(paste0(fname, ".zip")))
        file.rename(paste0(fname, ".zip"), fname)  

    zip(zipfile=fname,files=c(unlist(metadata),csvfile))
    },
  contentType = "application/zip"
)

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