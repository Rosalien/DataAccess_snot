# Caractéristique des données
translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")
language <- "en"
# Voir ensuite pour calculer le nombre de variable et c'est tout...

# uiExtraction
mod_extractiondatasetUI <- function(id){
  ns <- NS(id)
      tabPanel(translator$t("Extraction des jeux de données"),
      fluidPage(
        includeMarkdown(paste0("www/md/datasetExtractorOzcar_",language,".md")),
        hr(),
        fluidRow(
          column(2,
            uiOutput(ns('UIoutputJeuSelected')),
            downloadButton(ns('downloadDataOZCAR'), translator$t("Télécharger au format OZCAR")),
            br(),
            br(),
            downloadButton(ns('downloadDataZENODO'), translator$t("Télécharger au format ZENODO"))
            ),
          column(10,
             tabsetPanel(
              tabPanel(p(icon("table"), translator$t("Description du jeu de données")),dataTableOutput(ns("tableDataset")))# end of "Dataset" tab panel
      )
          )
        )
      )
    )
}
 

mod_extractiondataset <- function(input, output, session){
  my_wd <- getwd()
  ns <- session$ns

  caracJeu <- caracdata(language)
  allDataset <- "all"
  datasetSNOT <- unique(caracJeu[,"code_jeu"][[1]])
  jeuSNOT <- c(allDataset,datasetSNOT)

  output$UIoutputJeuSelected <- renderUI({
    pickerInput(ns('jeuSnot'), translator$t("Jeux de données du SNO-T"), jeuSNOT ,selected=allDataset,options = list(style = "btn-info"))#selectInput
  })

checkinJeu <- reactive({
  input$jeuSnot
})

checkinJeu <- reactiveValues()

  observe({
    input$jeuSnot
    isolate({
      if(is.null(input$jeuSnot)){
          #print("1")
          checkinJeu$checked <- datasetSNOT
      }else if(input$jeuSnot==allDataset){
          #  print("2")
           checkinJeu$checked <- datasetSNOT
      }else{
        #print("3")
        checkinJeu$checked <- input$jeuSnot
      }
    })
  })

metadataJeuPivot <- reactive({
        print(input$jeuSnot)
        metadata_jeu <- robustCurl(httr::GET(paste("http://localhost:8081/rest/resources/pivot?codes_jeu=",input$jeuSnot,sep=""),httr::timeout(300)))
        metadata_jeu <- ifelse((is.character(metadata_jeu))||(metadata_jeu$status_code==404),"Problème dans la génération des métadonnées",jsonlite::prettify(rawToChar(metadata_jeu$content)))
})


sqlOutputdatasetMetadata <- reactiveValues()
  observe({
    input$jeuSnot
    print(checkinJeu$checked)
    isolate({
        sqlOutputdatasetMetadata$siteVariable <- unique(caracdata(language)[code_jeu %in% checkinJeu$checked,list(code_jeu,code_site_station,variable)])
        sqlOutputdatasetMetadata$descriptionDataset <- unique(caracdata(language)[code_jeu %in% checkinJeu$checked,list(titre,descriptionjeu,genealogie,date_debut=min(mindate,na.rm=TRUE),date_fin=max(maxdate,na.rm=TRUE))])
        sqlOutputdatasetMetadata$descriptionMeasurement <- unique(caracdata(language)[code_jeu %in% checkinJeu$checked,list(titre,descriptionjeu,genealogie,date_debut=min(mindate,na.rm=TRUE),date_fin=max(maxdate,na.rm=TRUE))])
      #}
    })
  })

  sqlOutputdataset <- reactive({
    # Détermination des paramètres de la requête sur la base de la réactive jeu()
    caracDataset <- caracdata(language)[code_jeu %in% checkinJeu$checked,]
    variableJeu <- unique(caracDataset[,variable])
    siteJeu <- unique(caracDataset[,code_site_station])
    date_debut <- min(as.Date(unique(caracDataset[,mindate]),"%d-%m-%Y"))
    date_fin <- max(as.Date(unique(caracDataset[,maxdate]),"%d-%m-%Y"))
    periodeJeu <- c(date_debut,date_fin)
  
    # Lancement de la requête
    data <- queryDataSNOT(variableJeu,siteJeu,periodeJeu)
    # Transformation au format horizontal et création de dateEnd
    data <- dcast(data, formula = code_site_station+date+time+datatype~variable, value.var = "value")
    data[,dateEnd:=paste0(date,'T',time,'Z')]
  })

sqlOutputdatasetZENODO <- reactive({
    # Détermination des paramètres de la requête sur la base de la réactive jeu()
    caracDataset <- caracdata(language)[code_jeu %in% checkinJeu$checked,]
    variableJeu <- unique(caracDataset[,variable])
    siteJeu <- unique(caracDataset[,code_site_station])
    date_debut <- min(as.Date(unique(caracDataset[,mindate]),"%d-%m-%Y"))
    date_fin <- max(as.Date(unique(caracDataset[,maxdate]),"%d-%m-%Y"))
    periodeJeu <- c(date_debut,date_fin)
  
    # Lancement de la requête
    dataZENODO <- queryDataSNOT(variableJeu,siteJeu,periodeJeu)
    # Transformation au format horizontal et création de dateEnd
    dataZENODO <- dcast(dataZENODO, formula = code_site_station+date+time+datatype~variable, value.var = "value")
    dataZENODO[,c("datatype"):=NULL]
  })

    # Téléchargement d'une archive OZCAR
    output$downloadDataOZCAR <- downloadHandler(

    filename = function() {
      paste("TOUR_DAT.zip",sep="")
    },
    content = function(fname){    
      withProgress(message = translator$t("Début de l'extraction ..."),{
      incProgress(0.1,translator$t("Requête de la base de données ..."))
      fileNameDataset <- paste("TOUR_DAT_",checkinJeu$checked,".zip",sep="")
      setwd(tempdir())
      pivotMetadata <- paste("TOUR_en.json", sep="")
      print("Création de l'objet pivotHeaderAndData")
      siteVariableJeu <- sqlOutputdatasetMetadata$siteVariable
      #pivotHeaderAndData <- createFilePivot(sqlOutputdataset(),siteVariableJeu,caracJeu)
      incProgress(0.5,translator$t("Création de la métadonnée ..."))
      write(metadataJeuPivot(), pivotMetadata)
      incProgress(0.2,translator$t("Création du fichier ..."))
      print("Création des fichiers")

      filePivotJeu <- lapply(unique(siteVariableJeu[,code_jeu]) ,function(x){
        print(x)
        siteVariable <- unique(caracdata(language)[code_jeu %in% x,list(code_site_station,variable)])
        pivotHeaderAndData <- createFilePivot(sqlOutputdataset(),siteVariable,caracJeu)
          lapply(1:nrow(siteVariable),function(x){
            fileName <- pivotHeaderAndData[[x]]$fileName
            writeHeaderPivot(pivotHeaderAndData[[x]][["data"]],fileName,pivotHeaderAndData[[x]][["header"]])
            return(fileName)
            })
      })

      incProgress(0.2,translator$t("Compression..."))
      
      lapply(1:length(filePivotJeu),function(x){
        zip(zipfile=fileNameDataset[x],files=unlist(filePivotJeu[x]))
      })

      incProgress(0.2,translator$t("Finalisation ..."))

      })#Fin de la progression

      if (file.exists(paste0(fname, ".zip")))
        file.rename(paste0(fname, ".zip"), fname)  
      
      zip(zipfile=fname,files=c(pivotMetadata,unlist(fileNameDataset)))
  
    setwd(my_wd)
    },
  contentType = "application/zip"
  )
  
output$downloadDataZENODO <- downloadHandler(

    filename = function() {
      paste0("TOUR_DAT-",checkinJeu$checked,".zip",sep="")
    },
    content = function(fname){    
      withProgress(message = translator$t("Début de l'extraction ..."),{

      incProgress(0.1,translator$t("Requête de la base de données ..."))
      setwd(tempdir())
      pivotMetadata <- paste0("TOUR_en.json")
      print("Création de dataZENODO")

      siteVariableJeu <- sqlOutputdatasetMetadata$siteVariable
      csvDataFileZENODO <- paste0("TOUR_DAT_",checkinJeu$checked,".csv")
              
      dataZENODO <- sqlOutputdatasetZENODO()
      
      lapply(unique(siteVariableJeu[,code_jeu]) ,function(x){
              csvFile <- paste0("TOUR_DAT_",x,".csv")
              write.csv(dataZENODO[code_site_station %in% siteVariableJeu[code_jeu %in% x,code_site_station],],csvFile,quote=FALSE,row.names=FALSE)
      })

      incProgress(0.5,translator$t("Création de la métadonnée ..."))
      write(metadataJeuPivot(), pivotMetadata)

      incProgress(0.2,translator$t("Finalisation ..."))
      })#Fin de la progression

      if (file.exists(paste0(fname, ".zip")))
        file.rename(paste0(fname, ".zip"), fname)  
      
      zip(zipfile=fname,files=c(pivotMetadata,csvDataFileZENODO))
  
    setwd(my_wd)
    },
  contentType = "application/zip"
  )
  
  output$tableDataset <- DT::renderDataTable({
    descriptionDataset <- sqlOutputdatasetMetadata$descriptionDataset  
    names(descriptionDataset) <- c(translator$t("Titre"),translator$t("Description"),translator$t("Généalogie"),translator$t("Début"),translator$t("Fin"))
    retJeu <- DT::datatable(descriptionDataset,rownames= FALSE)
    return(retJeu)
  })#Fin de la tableJeu

output$tableDatasetMeasurement <- DT::renderDataTable({
    descriptionMeasurement <- sqlOutputdatasetMetadata$descriptionMeasurement 
    names(descriptionMeasurement) <- c(translator$t("Titre"),translator$t("Description"),translator$t("Généalogie"),translator$t("Début"),translator$t("Fin"))
    retJeu <- DT::datatable(descriptionMeasurement,rownames= FALSE)
    return(retJeu)
  })
      

}#Fin du module


