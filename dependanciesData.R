# Configuration du fuseau horaire
attr(ts, "tzone") <- "Africa/Algiers"
Sys.setenv(TZ = "Africa/Algiers")

robustCurl= function(x) {
    tryCatch(x,
            warning = function(w) {print("warning")},
            error = function(e) {print("Problème dans la génération de la métadonnée")}) 
    }

# To creat an html link
createLink <- function(val) {
  paste0('<a href="', val,'" target="_blank">', val ,'</a>')
}

 # Simple fonction pour transformer les variables météo-sol en variable simple (G_1_1_1 >> G)
variableToVariableSimple <- function(x){
  y <- str_replace_all(x, "[[:punct:]]", "")
  y <- str_replace_all(y , "[[1-9+]]", "")
  y
}

# Fontion pour sélectionner l'opposé de '%in%'
'%!in%' <- function(x,y)!('%in%'(x,y))

# Fonction pour calculer le jour et la nuit en fonction d'une date
# 
dbDayNight <- function(dataSelected){
        
      site <- c("lgt","bdz","ldm","frn")
      lat <- c(47.3218442722356,42.8027601354934,48.4418870264623,46.8249915808878)
      lon <- c(2.28190054512825,1.4235536727897,-1.18761537116659,6.16955686517857)
      siteCoord <- setDT(data.frame(site,lat,lon))

# Calcul des paramètres jour/nuit pour chaque site en fonction de leur coordonnées géographiques
aggregateDBDayandNight <- rbindlist(lapply(unique(dataSelected$code_site),function(x){
        dataSelectedSite <- dataSelected[code_site %in% x,]
        dataSelectedSite$Date <- as.POSIXct(paste(dataSelectedSite$date,dataSelectedSite$time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")
        fullSequence <- seq(min(dataSelectedSite$Date),max(dataSelectedSite$Date)+3600,by="day")
        sunSetRiseSeq <- setDT(getSunlightTimes(date = as.Date(fullSequence),keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),lat = siteCoord[site %in% x,lat], lon = siteCoord[site %in% x,lon], tz = "Africa/Algiers"))    
      
        setkey(sunSetRiseSeq, date)
        setkey(dataSelectedSite,date)

        # Jointure avec ifelse pour day and night
        dbDayandNight <- sunSetRiseSeq[dataSelectedSite]
        dbDayandNight[,dayNight:=ifelse(Date > sunrise & Date < sunset, 'day', 'night')]
      }))
return(aggregateDBDayandNight)
}

# Fonction pour calculer la moyenne et/ou la somme (pour les précipitations) des données selon plusieurs fréquences et configurer ensuite la date
# 
dbselect <- function(db,dayNightSelected,frequenceSelected,siteSelected,variableSelected){
  # Préparation générale
  if(dayNightSelected!="day/night"){
      db <- db[dayNight==dayNightSelected,]
    }else{}
   if(frequenceSelected=="30 min"){
     subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected,]
     subsetoutbd[,Date:=as.POSIXct(paste(date,time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")]
   }else if(grepl("hour",frequenceSelected)==TRUE){  
    db[,Date:=cut(as.POSIXct(paste(date,time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers"),frequenceSelected)]
    subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected!="P_1_1_1"],.(value = mean(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight),]#sunrise,sunset
    subsetoutbd <- rbind(subsetoutbd,db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected=="P_1_1_1"] & complete.cases(db$value),.(value = sum(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)])#,sunrise,sunset
    subsetoutbd[,Date:=as.POSIXct(Date, "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")]
   }else if((grepl("day",frequenceSelected))|(grepl("week",frequenceSelected))){
    db[,Date:=cut(as.POSIXct(paste(date,time,sep=" "), "%Y-%m-%d",tz="Africa/Algiers"),frequenceSelected)]
    subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected!="P_1_1_1"],.(value = mean(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)]
    subsetoutbd <- rbind(subsetoutbd,db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected=="P_1_1_1"] & complete.cases(db$value),.(value = sum(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)])
    subsetoutbd[,Date:= as.POSIXct(date, "%Y-%m-%d",tz="Africa/Algiers")]
   }else if(grepl("month",frequenceSelected)|grepl("year",frequenceSelected)){
    db[,Date:=cut(as.POSIXct(paste(date,time,sep=" "),tz="Africa/Algiers"),frequenceSelected)]
    subsetoutbd <- db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected!="P_1_1_1"],.(value = mean(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)]
    subsetoutbd <- rbind(subsetoutbd,db[code_site_station %in% siteSelected & variable %in% variableSelected[variableSelected=="P_1_1_1"] & complete.cases(db$value),.(value = sum(value,na.rm=TRUE)), by = list(date=Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom,dayNight)])
    subsetoutbd[,Date:= as.POSIXct(as.Date(date,"%Y-%m-%d"),tz="Africa/Algiers")]
   }else{}

  # Préparation des variables facetWrapSelected (surveiller dans le cas ou il y a une fréquence year, non sens...)
  subsetoutbd <- subsetoutbd %>% mutate(month = factor(month(Date),1:12,
                                        labels = c("Jan", "Feb", "Mar", "Apr",
                                        "May", "Jun", "Jul", "Aug",
                                        "Sep", "Oct", "Nov", "Dec"),ordered = TRUE),
                                        year= factor(year(Date)),
                                        season=factor(quarters(subsetoutbd$Date), levels = c("Q1", "Q2", "Q3", "Q4"), 
                                        labels = c("winter", "spring", "summer", "fall")))
  setDT(subsetoutbd)

  # Calcul de la moyenne des valeurs dans le cas dayNightSelected=="day/night"
#  if(dayNightSelected=="day/night"){
    #subsetoutbdSNOT <- subsetoutbd[variable !="P_1_1_1",.(value = mean(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)]
    #rbind(subsetoutbdSNOT,subsetoutbd[variable=="P_1_1_1" & complete.cases(value),.(value = sum(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)])  
  #}else{
    #subsetoutbdSNOT <- subsetoutbd
  #}

  return(subsetoutbd)
}

# Fonction pour créer la liste des variables avec une définition simple
checkboxVariablePedo <- function(dbCarac,codeVariable,siteBM){

    listVariables <- unique(dbCarac[code_site_station %in% siteBM & grepl(codeVariable, dbCarac$variable)==TRUE,"variable"])
    listVariables <- listVariables[order(variable),]
    listVariables$definitionsimple <- gsub("[a-zA-Z]", "", listVariables$variable) 
    listVariables$definitionsimple <- substring(listVariables$definitionsimple,2)

    return(listVariables)
}


# Fonction utilisée pour sortir les données au format horizontale lors du téléchargement des données

extracData <- function(db,frequence){
  fullSequence <- seq(min(db$Date),max(db$Date),by=frequence)
  extractionData <- lapply(unique(db$code_site_station),function(x){
    # res <- cast(db[db$site %in% x,c("Date","variable","value")]) (Problème pour une raison inconnue (19/09/2019))
    res <- cast(db[code_site_station %in% x,c("Date","variable","value")],Date~variable,sum)
    names(res) <- c("Date_old",paste(x,"_",names(res[-1]),sep=""))
    res <- setDT(data.frame(Date=fullSequence,with(res,res[match(fullSequence,res$Date_old),])))
    res[,-2]
    })

  # Jointure de la liste
   extractionDataCpt <- Reduce(function(x,y)merge(x, y, by.x="Date", by.y="Date",all=TRUE),extractionData)
      
  # Conversion des NA en -9999
  extractionDataCpt[is.na(extractionDataCpt)] <- -9999
  return(extractionDataCpt)
}


createFilePivot <- function(data,siteVariable,caracDataset){
  
lapply(1:nrow(siteVariable),function(x){
      # Sélection du caracDataSet
      siteInLoop <- siteVariable[x,code_site_station]
      variableInLoop <- siteVariable[x,variable]
      caracDatasetObservation <- unique(caracDataset[variable %in% variableInLoop & code_site_station %in% siteInLoop,list(code_site,code_station,theme,datatype,variable,titre,zet_coordonnees_bbox,zet_altitude)])
      dataInLoop <- data[code_site_station %in% siteInLoop,c("dateEnd",variableInLoop),with=FALSE]
      dataInLoop <- na.omit(dataInLoop,variableInLoop)
      
      # Création des variables pour les en-tête (code caduc car autre table utilisée. A conserver car pratique)
      #patternPath <- "^(.*)\\/(.*),(.*),(.*),(.*)-(.*)-(.*)-(.*)"
      #resRegex <- strapply(caracDatasetObservation$path, patternPath, c)[[1]]
      dateExtraction <- format(Sys.Date(), format='%Y-%m-%dT%H:%M:%SZ')
      
      observationId <- paste(caracDatasetObservation$code_site,"-",caracDatasetObservation$code_station,"-",caracDatasetObservation$theme,"-",caracDatasetObservation$datatype,"-",caracDatasetObservation$variable,sep="")
      datasetTitle <- caracDatasetObservation[,titre]
      coordonnees <- caracDatasetObservation[,"zet_coordonnees_bbox",with=FALSE][[1]]
      longitude <- matrix(as.numeric(unlist(
        str_extract_all(coordonnees, pattern = "-?\\d+\\.?\\d*")
      )), ncol = 2, byrow = T)[,1]
      latitude <- matrix(as.numeric(unlist(
      str_extract_all(coordonnees, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,2]    
      altitude <- caracDatasetObservation[,zet_altitude]
      fileName <- paste("TOUR_OBS_",observationId,".txt",sep="")

      #Construction de l'en-tête
      header <- paste("Date_of_Extraction;",dateExtraction,"\n",
        "Observation_ID;",observationId,"\n",
        "Dataset_title;",datasetTitle,"\n",
        "Variable_name;",variableInLoop,sep="")

      print(header[1])
      # Création de la table
      valueVariable <- dataInLoop[,list(dateEnd)]
      valueVariable$value <- dataInLoop[,variableInLoop,with=FALSE]
      valueVariable$dateBeg <- NA
      valueVariable$longitude <- longitude
      valueVariable$latitude <- latitude
      valueVariable$altitude <- altitude
      valueVariable$qualityFlags <- NA

      valueVariable <- valueVariable[,list(dateBeg,dateEnd,latitude,longitude,altitude,value,qualityFlags)]
    #  valueVariable[is.na(valueVariable$value)] <- -9999
      list(data=valueVariable,header=header,fileName=fileName)
})
}

writeHeaderFile<- function(x, file, header){
# create and open the file connection
  datafile <- file(file, open = 'wt')
# close on exit
  on.exit(close(datafile))
# if a header is defined, write it to the file (@CarlWitthoft's suggestion)
if(!missing(header)) writeLines(header,con=datafile)
# write the file using the defined function and required addition arguments  
  write_csv(x, datafile)
}

sqlOutputDatasetArchive <- function(language,checkinJeu,archiveType="ZENODO")({
    # Load parameters for queryDataSNOT
    caracDataset <- caracdata(language)[code_jeu %in% checkinJeu]
    variableJeu <- unique(caracDataset[,variable])
    siteJeu <- unique(caracDataset[,code_site_station])
    date_debut <- min(as.Date(unique(caracDataset[,mindate]),"%d-%m-%Y"))
    date_fin <- max(as.Date(unique(caracDataset[,maxdate]),"%d-%m-%Y"))
    periodeJeu <- c(date_debut,date_fin)

    # Lancement de la requête
    data <- queryDataSNOT(pool,variableJeu,siteJeu,periodeJeu,melted=FALSE)
    
    if(archiveType=="OZCAR")
      {
        data[,dateEnd:=paste0(date,'T',time,'Z')]
      }else{
        data[,c("datatype"):=NULL]
      }
  })
