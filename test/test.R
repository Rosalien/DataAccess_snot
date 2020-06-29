source("dependancies.R")
source("mod_carto.R")
#source("mod_extractiondataset.R")
source("mod_extraction.R")
source("mod_about.R")
#system.time(caracdata())


jeu <- c("frn-ges-eddycovariance")
siteJeu <- unique(caracDataset[,code_site_station])
caracDataset <- caracdata("en")[code_jeu %in% jeu,]

# Calcul des paramètres pour interroger la base elasticsearch
date_debut <- min(as.Date(unique(caracDataset[,mindate]),"%d-%m-%Y"))
date_fin <- max(as.Date(unique(caracDataset[,maxdate]),"%d-%m-%Y"))
periodeSelected <- c(date_debut,date_fin)

language <- "en"    
caracDataSensor <- caracdata(language)[order(variable)]
  variableCaracData <- c("code_jeu","code_site","code_station","code_site_station","site_nom","theme","datatype","variable","unite","mindate","maxdate","zet_coordonnees_bbox")
  metadataVariable <- c("definition","station_description","site_description","station_nom")
  metadataSensor <- c("fabricant","instrument","description_capteur","station_description")
  metadataMethod <- c("description_methode")

caracData <- unique(caracDataSensor[,c(variableCaracData,metadataVariable),with=FALSE])
caracCarto <- unique(caracDataSensor[,c(variableCaracData,metadataSensor,metadataMethod),with=FALSE])

# Variables pour les tests
input <- list()
siteSelected <- c("frn/ec1")
frequenceSelected <- "day"
variableSelected <- c("FCH4","FCH4_GF")
dayNightSelected <- "day/night"

input$dateBM[1] <- "06-11-2008"
input$dateBM[2] <- "31-07-2019"
frequence <- frequenceSelected
meltvalue <- queryDataSNOT(variableSelected,siteSelected,periodeSelected)
dataSNOT <- caracData[meltvalue]
dataSelected <-    dataSNOT
dbDayandNight <- dbDayNight(dataSNOT)
subsetoutbdAggregate <- dbselect(dbDayandNight,dayNightSelected,frequenceSelected,siteSelected,variableSelected)
subsetoutbdSNOT <- subsetoutbdAggregate[variable !="P_1_1_1",.(value = mean(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)]
subsetoutbdSNOT <- rbind(subsetoutbdSNOT,subsetoutbdAggregate[variable=="P_1_1_1" & complete.cases(value),.(value = sum(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)])  
subsetoutbd <- subsetoutbdSNOT

keyWord <- c("Actual evapotranspiration by eddy-covariance","CO2 flux measurements by eddy-covariance","CH4 flux measurements by eddy-covariance","Sensitive heat flow by eddy-covariance","Latent heat flow by eddy-covariance")
keyWord <- paste(keyWord,collapse = "|")
subsetoutbd[,typeVariable:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

recapTable <- unique(caracData[code_site_station %in% siteSelected & variable %in% variableSelected,])
test <- caracCarto[recapTable]
test[,list(code_site_station,station_nom,variable,unite,fabricant,instrument,description_capteur,zet_coordonnees_bbox,description_methode)]

tableCarac <- unique(caracData[,list(station_nom,theme,variable,definition,mindate,maxdate)])


      figcarac <- tableCarac[,variable:=ifelse(grepl("SWC|TS|G",variable)==TRUE,variableToVariableSimple(variable),variable)]
      figcarac <- unique(figcarac)[order(theme)]
      levels(figcarac$variable) <- unique(rev(figcarac$variable))
      figcarac$variable <- with(figcarac,factor(variable,levels=levels(variable)))
     
tableCarac <- figCaracData


p2 <- unique(tableCarac)
p2 <- unique(tableCarac) %>% ggvis() 


gg <- ggplot(unique(tableCarac),aes(colour=theme,text = 
                                                paste('<br>Begin:', mindate,
                                                      '<br>End: ', maxdate,
                                                      '<br>Variable: ', definition))) +
      geom_segment(aes(x=mindate, xend=maxdate, y=variable, yend=variable),size=1)+
      labs(fill="",x="",y = "")+
      scale_colour_manual(values=wes_palette("Darjeeling1",length(unique(tableCarac$theme)),type="discrete"),name=translator$t("Thème")) +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_chart()+
      facet_wrap(~station_nom)

 ggplotly(gg, tooltip = c("text")) %>%layout(legend = list(orientation = "h", x = 0.4, y = -0.05))


keyWord <- c("Soil temperature","Soil water content","Soil heat flux","Température du sol","Teneur en eau du sol","Flux de chaleur dans le sol")

keyWord <- paste(keyWord,collapse = "|")
 str_extract(figCaracData$definition, "keyWord")

figCaracData$definition
str_replace_all(figCaracData$definition, keyWord, "")          

  figCaracData$definition <- str_replace_all(figCaracData$definition, "[[:punct:]]", "")
  figCaracData$definition <- str_replace_all(figCaracData$definition, "for profil", "")
  figCaracData$definition <- str_replace_all(figCaracData$definition, "[[:digit:]]", "")
  figCaracData$definition <- str_trim(figCaracData$definition,"right")


figCaracData[grepl(keyWord, definition)==TRUE,definition_simple:=str_replace(definition, "for profil", "")]


# A completer avec une fonction
figCaracData[,definition_simple:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

x <- test
test <- figCaracData[grepl(keyWord, definition)==TRUE,definition]

definitionToSimple <- function(x){
  x <- str_replace(x, "for profil", "")  

  return(x)
}
gsub("(NM).*","\\1",a) 



gsub(paste0("(",keyWord,").*"),"\\1",test)


grepl(keyWord, definition)==TRUE,

dayNight:=ifelse(Date > sunrise & Date < sunset, 'day', 'night')
periodeSelected <- c(input$dateBM[1],input$dateBM[2])

meltvalue <- queryDataSNOT(variableSelected,siteSelected,periodeSelected)

dataSNOT <- caracData[meltvalue]
 dataSelected <-    dataSNOT
dbDayandNight <- dbDayNight(dataSNOT)
db <- dbDayandNight

subsetoutbdAggregate <- dbselect(dbDayandNight,dayNightSelected,frequenceSelected,siteSelected,variableSelected)

subsetoutbdSNOT <- subsetoutbdAggregate[variable !="P_1_1_1",.(value = mean(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)]
subsetoutbdSNOT <- rbind(subsetoutbdSNOT,subsetoutbdAggregate[variable=="P_1_1_1" & complete.cases(value),.(value = sum(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)])  


subsetoutbd <- subsetoutbdSNOT

dy_graphSite <- dygraphSite(subsetoutbdSNOT[(grepl("SWC_|TS_|G_", subsetoutbdSNOT$variable)==FALSE) & subsetoutbdSNOT$variable %!in% c("WTD","TW","GPP","RE","NEE"),],frequenceSelected)
dy_graphTypeVariable <- dygraphTypeVariable(subsetoutbdSNOT[grepl("SWC_|TS_|G_", subsetoutbdSNOT$variable)==TRUE,],frequenceSelected())
     






















recapTable <- unique(caracData[code_site_station %in% siteSelected & variable %in% variableSelected,])


  caracDataSensor <- caracdata("en")[order(variable)]





    mapSensorSelected <- unique(caracCarto[code_site_station %in% c("bdz/pz1","lgt/bm1"),
                                list(zet_coordonnees_bbox,station_description,datatype)])
    # Ajout des couleurs pour les types de stations
    mapSensorSelected <- merge(mapSensorSelected, col_station, by.x = "datatype", by.y = "datatype", all.x = TRUE,all.y=FALSE)


    sensorSelectedMap(mapSensorSelected,FALSE)



meltvalue <- queryDataSNOT(variableSelected,siteSelected,periodeSelected)
# Jointure avec data.table (caracData[meltvalue])
dataSelected <- caracData[meltvalue]
dbDayandNight <- dbDayNight(dataSelected)

subsetoutbdSNOT <- dbselect(dbDayandNight,"day/night",frequenceSelected,siteSelected,variableSelected)
frequence <- "day"
subsetoutbd <- subsetoutbdSNOT[subsetoutbdSNOT$variable %in% c("GPP","RE","NEE"),]

db <- subsetoutbdSNOT

extractionDataCpt <- extracData(subsetoutbdSNOT,"day")

dy_graphSite <- dygraphSite(subsetoutbdSNOT[(grepl("SWC|TS|G", variable)==FALSE) & variable %!in% c("WTD","TW"),],"30 min")


  data <- setDT(dbGetQuery(pool, queryValueSNOT))

  # Partie à optimiser
  meltvalue <- setDT(melt(data,id=c("site","date","time","datatype")))

subsetoutbd <- value[(grepl("SWC|TS|G", variable)==FALSE) & variable %!in% c("WTD","TW"),]

    dateWindow <- c(min(subsetoutbd$date),max(subsetoutbd$date))
    fullSequence <- seq(min(subsetoutbd$date),max(subsetoutbd$date),by=frequence)
    variables <- unique(subsetoutbd$variable)



# Comparaison performance
siteSelected <- "bdz/bm1"
variableSelected <- c("TA_1_1_1","P_1_1_1","PA_1_1_1","WD_1_1_1","WS_1_1_1","RH_1_1_1","LW_IN_1_1_1",
  "LW_OUT_1_1_1","SW_IN_1_1_1","SW_OUT_1_1_1")
input$dateBM[1] <- "06-11-2012"
input$dateBM[2] <- "31-07-2019"

periodeSelected <- c(input$dateBM[1],input$dateBM[2])
variableDefinition <- unique(caracData[site %in% siteSelected & variable %in% variableSelected,variable])
variablequery <- as.matrix(paste("max(case when variable='",variableDefinition,"' then value else null end) \"",variableDefinition,"\"",sep=""))
variablecharacter <- apply(variablequery,2,paste,collapse=",")

queryValueSNOT <- paste("select site,date,time,datatype,
      ",variablecharacter,"
      from data_infraj_prod
      where date between '",periodeSelected[1],"' and '",periodeSelected[2],"'
      and site like any ('{",paste(siteSelected,collapse=","),"}' )
      group by site, date, time,datatype",sep="")

dataPG <- dbGetQuery(pool, queryValueSNOT)

queryValueSNOT2 <- paste("select site,date,time,datatype,
      ",variablecharacter,"
      from datainfraj_timescaledb
      where date between '",periodeSelected[1],"' and '",periodeSelected[2],"'
      and site like any ('{",paste(siteSelected,collapse=","),"}' )
      group by site, date, time,datatype",sep="")
system.time(dbGetQuery(pool, queryValueSNOT2))

periodeSelected <- format(as.Date(periodeSelected,"%d-%m-%Y"),"%Y/%m/%d %H:%M:%S")
site <- paste0("[\"",paste(siteSelected,collapse="\",\""),"\"]")
variable <- paste0("[\"",paste(variableSelected,collapse="\",\""),"\"]")
datatype <- paste0("[\"",paste("meteosol_infraj",collapse="\",\""),"\"]")

queryValueSNOT <- query(paste0('{
  "bool":{
    "must":[
      {
        "range": {
          "timestamp": {
            "lte":"',periodeSelected[2],'","gte":"',periodeSelected[1],'"
          }
        }
      },
      {
        "terms": {
          "site.keyword":',site,'
        }
      },
      {
        "terms": {
            "variable.keyword":',variable,'
          }
        }
      ]
    }
}'))
dataEC <- setDT(elastic("http://localhost:9200", "dataprod_infraj", "dataprod") %search% queryValueSNOT)


datainfraj_timescaledb


my_collection = mongo(collection = "dataEC", db = "datainfraj") # create connection, database and collection
# Pour importer
my_collection$insert(dataEC)

query2= my_collection$find('{"variable" : "TA_1_1_1"}')


query1= my_collection$find('{"PrimaryType" : "ASSAULT", "Domestic" : "true" }')



  







meltvalue <- data.table(value,key="variable,site,datatype")
    setkey(meltvalue, variable, site, datatype)
    setkey(caracData, variable, site, datatype)
    dataSelected <- caracData[meltvalue]
  
    # Test en cours sur jour/nuit
    print("---------Paramètres jour/nuit---------")
    dataSelected$Date <- as.POSIXct(paste(dataSelected$date,dataSelected$time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")
    fullSequence <- seq(min(dataSelected$Date),max(dataSelected$Date),by="day")
    sunSetRiseSeq <- setDT(getSunlightTimes(date = as.Date(fullSequence),keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),lat = 42.803965, lon = 1.418299, tz = "Africa/Algiers"))
    
    setkey(sunSetRiseSeq, date)
    setkey(dataSelected,date)

    # Jointure avec ifelse pour day and night
    dbDayandNight <- sunSetRiseSeq[dataSelected]
    dbDayandNight[,dayNight:=ifelse(Date > sunrise & Date < sunset, 'day', 'night')]

    # dbselect avec filtre sur day/night/all
    dayNightSelected <- "day/night"
    frequenceSelected <- "day"
    frequence <- "day"
    # Sélection des variables d'intérêt
    db <- dbDayandNight

        subsetoutbdSNOT <- dbselect(db,dayNightSelected,language,frequenceSelected,siteSelected,variableSelected)  

subsetoutbd <- db






save(dataxts,"test.RData")
outp$dateDay <- format(outp$Date,"%Y-%m-%d")
dayPeriods <- unique(outp[,.(from = max(sunrise),to=min(sunset)),by=dateDay,])
nightPeriods1 <- unique(outp[,.(from = paste0(dateDay," 00:00:00"),to = min(sunrise)-1),by=dateDay,])
nightPeriods2 <- unique(outp[,.(from = max(sunset)+1,to = paste0(dateDay," 23:59:59")),by=dateDay,])

dayPeriods[1]
nightPeriods1[1]
nightPeriods2[1]
p <- dygraph(dataxts,group="groupe",main = paste(unique(outp[,"definition"]),sep=""),ylab = unique(outp[,"unite"])[[1]],height = 250,width="100%")%>%
        dyOptions(stackedGraph = FALSE) %>%
        dyRangeSelector(height = 20,strokeColor = "") %>%  
        dyLegend(show = "onmouseover") %>% 
        dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
        dyCSScool() %>%
        dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))  

for (i in 1:nrow(dayPeriods)) {
  p <- p %>%
       dyShading(from = dayPeriods$from[i], to = dayPeriods$to[i],color = "#d1d166")
       #dyShading(from = nightPeriods1$from[i], to = nightPeriods1$to[i],color = "#424d85")
     # dyShading(from = nightPeriods2$from[i], to = nightPeriods2$to[i],color = "#424d85")
}
print(p)
load("data.RData")
subsetoutbdSNOT <- dbselect(data,dayNightSelected(),language,frequenceSelected(),siteSelected(),variableSelected())  

db <- data
language <- "en"
variables <- "TS_1_1_1"
sites <- "lgt/bm1"
frequence <- "day"
db$Date <- cut(as.POSIXct(paste(db$date,db$time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers"),frequence)





    #https://stackoverflow.com/questions/36915565/r-add-day-night-column-to-data-frame-when-sunrise-sunset-times-change

    library(suncalc)
test <- getSunlightTimes(date = seq.Date(Sys.Date()-9, Sys.Date(), by = 1),keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),lat = 42.803965, lon = 1.418299, tz = "Africa/Algiers")



     db <- caracData[meltvalue]
     subsetoutbd <- setDT(db)[site %in% sites & variable %in% variables,]
     subsetoutbd$Date <- as.POSIXct(paste(subsetoutbd$date,subsetoutbd$time,sep=" "), "%Y-%m-%d %H:%M:%S",tz="Africa/Algiers")
  
    fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by="day")

    sunSetRiseSeq <- setDT(getSunlightTimes(date = as.Date(fullSequence),keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),lat = 42.803965, lon = 1.418299, tz = "Africa/Algiers"))
    setkey(sunSetRiseSeq, date)
    setkey(subsetoutbd,date)

    # Jointure avec ifelse pour day and night
    dbDayandNight <- sunSetRiseSeq[subsetoutbd]
    df$dayNight <- ifelse(df$DateTime > df$sunRise & df$DateTime < df$sunSet, 'day', 'night')
    dbDayandNight[,dayNight:=ifelse(Date > sunrise & Date < sunset, 'day', 'night')]

    db <- caracData[meltvalue]
    frequence <- frequenceSelected

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}
getSeason(as.POSIXct("2016-01-01 12:00:00")+(0:365)*(60*60*24))

frequenceSelected
db <- dbselect(caracData[meltvalue],frequenceSelected,siteSelected,variableSelected)
subsetoutbd <- valuecarac



#subsetoutbd <- valuecarac
frequence <- frequenceSelected
test <- dataxts
str(test)


dygraph(dataxts,group="groupe",main = paste(unique(outp[,"definition"]),sep=""),ylab = unique(outp[,"unite"])[[1]],height = 250,width="100%")%>%
        dyOptions(stackedGraph = FALSE) %>%
        dyRangeSelector(height = 20,strokeColor = "") %>%  
        dyLegend(show = "onmouseover") %>% 
        dyOptions(colors = wes_palette("Zissou1", length(unique(outp$site)),type = "continuous"),plotter=barplot,retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
        dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
        dyCSScool() %>%
        dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))



dygraphSite(valuecarac,frequenceSelected)



    valuecarac <- dbselect(caracData[meltvalue],frequenceSelected,siteSelected,variableSelected)
#Error in vecseq(f__, len__, if (allow.cartesian || notjoin || !anyDuplicated(f__,  : 
  #Join results in 61746 rows; more than 30954 = nrow(x)+nrow(i). Check for duplicate key values in i each of which join to the same group in x over and over again. If that's ok, try by=.EACHI to run j for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice.

subsetoutbd <- valuecarac[valuecarac$variable %in% "WTD",]

    dy_graphSite <- dygraphSite(valuecarac[(grepl("SWC|TS|G", valuecarac$variable)==FALSE) & valuecarac$variable != "WTD",],frequenceSelected)
    dy_graphTypeVariable <- dygraphTypeVariable(subsetoutbdSNOT[grepl("SWC|TS|G", subsetoutbdSNOT$variable)==TRUE,],frequenceSelected())
    dy_graphPiezo <- dygraphPiezo(valuecarac[valuecarac$variable %in% "WTD",],frequenceSelected)





# Test sur dbselect
  db <- caracData[meltvalue]
  sites <- siteSelected
  variables <- variableSelected
  frequence <- "day"

# Test pour les valeurs nulles dans les graphes
subsetoutbd <- valuecarac
dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
fullSequence <- seq(dateWindow,by=frequence)

  subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$definition, "[[:punct:]]", "")
  subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$typeVariable, "pour la position", "")
  subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$typeVariable, "[[:digit:]]", "")
  subsetoutbd$typeVariable <- str_trim(subsetoutbd$typeVariable,"right")

# Génération d'une time series COMPLETE par type de variable et par site  
  typeVariables <- unique(subsetoutbd$typeVariable)




  res <- lapply(1:length(unique(subsetoutbd$typeVariable)),function(z){#Boucle pour chaque type de variable
    # Sélection du type de variable
    outp <- subsetoutbd[subsetoutbd$typeVariable %in% typeVariables[z],]    
    outp <- outp[order(outp$variable),]


    lapply(unique(outp$site),function(y){#Pour chaque site, construction d'un graph
            dataxts <- do.call("cbind", lapply(unique(outp$variable),function(x){#Pour chaque variable, construction d'une dbtimes-series
              if(nrow(outp[site %in% y & variable %in% x,])==0){#revoir ce test
                NULL
              }else{  

                db <- xts(outp[site %in% y & variable %in% x,value],order.by=outp[site %in% y & variable %in% x,Date])
                colnames(db) <- x
                db
              }
            }))

                dygraph(dataxts,group="groupe",main = paste(unique(outp[,"typeVariable"])," (",y,")",sep=""),ylab = unique(outp[,"unite"])[[1]],height = 250,width="100%")%>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 20,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(colors = wes_palette("Zissou1", length(unique(outp$variable)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#fin de res

## Test sur dataxts
z <- 1
x <- "WTD"
y <- "bdz/pz1"
dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))

fullSequence <- seq(min(valuecarac$Date),max(valuecarac$Date),by=frequenceSelected)

    # Sélection du type de variable
    outp <- subsetoutbd[subsetoutbd$typeVariable %in% typeVariables[z],]    
    outp <- outp[order(outp$variable),]

dataxts <- do.call("cbind", lapply(unique(outp$variable),function(x){#Pour chaque variable, construction d'une dbtimes-series
if(nrow(outp[site %in% y & variable %in% x,])==0){#revoir ce test
NULL
}else{  
  tmp <-  outp[site %in% y & variable %in% x,list(Date,value)]
  tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
db <- xts(tmp2[,value],order.by=tmp2[,Date])
colnames(db) <- x
db}
}))

dygraph(dataxts,group="groupe",main = paste(unique(outp[,"typeVariable"])," (",y,")",sep=""),ylab = unique(outp[,"unite"])[[1]],height = 250,width="100%")






# Test sur les données piezo
subsetoutbd <- valuecarac
dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))

  # Génération d'une time series les piézo d'un même site
  sitePiezo <- unique(subsetoutbd$site_description)

  res <- lapply(1:length(sitePiezo),function(z){#Boucle pour chaque site
    # Sélection du type de variable
    outp <- subsetoutbd[site_description %in% sitePiezo[z],]    
    outp <- outp[order(outp$variable),]

    lapply(unique(outp$variable),function(x){#Pour chaque site, construction d'un graph pour les variables
            dataxts <- do.call("cbind", lapply(unique(outp$station_nom),function(y){#Pour chaque variable, construction d'une dbtimes-series
              if(nrow(outp[variable %in% x,])==0){#revoir ce test
                NULL
              }else{  

                db <- xts(outp[station_nom %in% y & variable %in% x,value],order.by=outp[station_nom %in% y & variable %in% x,Date])
                colnames(db) <- y
                db
              }
            }))
                dygraph(dataxts,group="groupe",main = paste(unique(outp[,"definition"])," (",sitePiezo[z],")",sep=""),ylab = unique(outp[,"unite"])[[1]],height = 300,width="100%")%>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 1,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(drawPoints = TRUE,sigFigs=3,colors = wes_palette("Zissou1", length(unique(outp$station_nom)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#fin de res

# Pbl métadonnées
[1] "bdz/pz2-hydrocarto-piezo_sh"          
[2] "lgt/pz_cbdv_amont-hydrocarto-piezo_sh"
[3] "lgt/pz_cbdv_aval-hydrocarto-piezo_sh" 

    SiteJeu <- gsub("/[^[a-zA-Z0-9_]+$","",CodeJeu)


 CodeJeu <- unique(caracData[site %in% siteSelected & variable %in% variableSelected,gsub("_[^_]+$","",code_jeu)])
    SiteJeu <- gsub("/[^_]+","",gsub("_[^_]+","",CodeJeu))
    CodeJeu <- unique(paste(SiteJeu,"-",gsub("^.*?-","",CodeJeu),sep=""))


x <- CodeJeu[1]
      test <- curl_fetch_memory(paste("http://localhost:8081/rest/resources/pivot?codes_jeu=",x,sep=""))
      pivot_metadata <- robustCurl(curl_fetch_memory(paste("http://localhost:8081/rest/resources/pivot?codes_jeu=",x,sep="")))
      pivot_metadata <- ifelse((is.character(pivot_metadata))||(pivot_metadata$status_code==404),"Problème dans la génération des métadonnée",jsonlite::prettify(rawToChar(pivot_metadata$content)))














variableSelected <- c("TS_1_1_1","TS_1_2_1")	
subsetoutbdSNOT <- dbselect(valuecarac,frequenceSelected,siteSelected,variableSelected)
boxplotSite <- ggplotBoxplotSite(subsetoutbdSNOT[grepl("SWC|TS|G|WD", subsetoutbdSNOT$variable)==FALSE,])
if(!is.null(boxplotSite))(do.call(grid_arrange_shared_legend,c(boxplotSite,list(position="bottom"))))



# Préparation des données pour la rose des vents
Bdwind <- dcast(subsetoutbd[variable %in% c("WS_1_1_1","WD_1_1_1"),c("Date","site","variable","value")],formula=Date+site~variable)
Bdwind <- Bdwind[complete.cases(Bdwind[,c("WS_1_1_1","WD_1_1_1")]),]

res <- lapply(unique(Bdwind$site),function(z){
	t<-plot.windrose(data=Bdwind[site %in% z,],
			  spd="WS_1_1_1",
			  dir="WD_1_1_1",
  			  spdres=0.7,
#  			  spdseq = c(0,3,6,12,20)
  			  spdmin=round(min(Bdwind$WS_1_1_1),2),spdmax=round(max(Bdwind$WS_1_1_1),2),
  			  titreLegend="Vitesse du vent (m/s)",
  			  titre=z)
})

do.call(grid_arrange_shared_legend,c(res,list(position="right")))

res2 <- lapply(unique(Bdwind$site),function(z){
	windRose(Bdwind[site %in% z,], ws = "WS_1_1_1", wd = "WD_1_1_1",breaks=6,ws.int=0.5,dig.lab=2,ggtheme='minimal')
	})
do.call(grid_arrange_shared_legend,c(res2,list(position="right")))

select date_debut,date_fin,code_jeu,titre,description,genealogie,definition
from (
select to_char(min(mms_date),'DD-MM-YYYY') as date_debut, to_char(max(mms_date),'DD-MM-YYYY') as date_fin,v.id 
from valeurs_meteo_sh_vms v 
INNER JOIN  mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id 
group by v.id
)a
left join realnode rv ON rv.id = a.id 
left join realnode rd ON rd.id = rv.id_parent_node 
left join realnode rt ON rt.id = rd.id_parent_node 
left join realnode rs ON rs.id = rt.id_parent_node 
left join composite_nodeable cn ON cn.id = rs.id_nodeable 
left join datatype_unite_variable_snot_vdt dvu ON dvu.sdvu_id = rv.id_nodeable 
left join variable ON variable.var_id = dvu.id 
left join jeu on jeu.jeu_id = dvu.jeu_id 



#>> a tester
select site,date,time,definition,value  
from(
select mms_date as date,mms_time as time,v.id,value
from valeurs_meteo_sh_vms as v
INNER JOIN mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id
where mms_date between '16-02-2016' and '28-03-2017'
limit 10000
)a
INNER JOIN 
(
select regPath2[1] as site,regPath2,id
from(select regexp_split_to_array(regPath1[4],'-') as regPath2,id
from(select id,regexp_split_to_array(path, ',') as regPath1
from realnode)a)b
where regPath2[1] similar to 'lgt/bm1|ldm/bm1'
) R
on R.id=a.id
INNER join composite_nodeable as cn on cn.code=regPath2[3]
INNER join variable as v on v.var_id=cn.id
limit 10000



select c[1] as site,date ,time,definition,value  
from(
select regexp_split_to_array(b[4],'-'),date,time,value
from(
select regexp_split_to_array(path, ','),date,time,value
from(
select mms_date as date,mms_time as time,v.id,value
from valeurs_meteo_sh_vms as v
INNER JOIN mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id
where mms_date between '16-02-2016' and '28-03-2017'
)a
INNER JOIN realnode as rn on rn.id=a.id
)as dt(b)
) as dt(c)
INNER join composite_nodeable as cn on cn.code=c[3]
INNER join variable as v on v.var_id=cn.id
where c[1] similar to 'lgt/bm1|ldm/bm1'
limit 10000


# ça donne >> ~30s :(Voir pourquoi cette requête n'est pas plus efficace (elle devrait l'être pourtant)
select site,date,time,
max(case when code_variable='P_1_1_1' then value else null end) "P_1_1_1",
max(case when code_variable='RH_1_1_1' then value else null end) "RH_1_1_1",
max(case when code_variable='SWC_1_1_1' then value else null end) "SWC_1_1_1",
max(case when code_variable='TA_1_1_1' then value else null end) "TA_1_1_1",
max(case when code_variable='TS_1_1_1' then value else null end) "TS_1_1_1",
max(case when code_variable='TS_1_2_1' then value else null end) "TS_1_2_1",
max(case when code_variable='WD_1_1_1' then value else null end) "WD_1_1_1",
max(case when code_variable='WS_1_1_1' then value else null end) "WS_1_1_1"
from(
select site,date,time,value,code_variable
from(
select mms_date as date,mms_time as time,v.id,value
from valeurs_meteo_sh_vms as v
INNER JOIN mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id
where mms_date between '16-02-2016' and '28-03-2017'
)a
INNER JOIN 
(
select regPath2[1] as site,regPath2,id,regPath2[3] as code_variable
from(select regexp_split_to_array(regPath1[4],'-') as regPath2,id
from(select id,regexp_split_to_array(path, ',') as regPath1
from realnode)a)b
where regPath2[1] similar to 'lgt/bm1|ldm/bm1'
) R
on R.id=a.id
)b
group by site, date, time

# contre 6s pour 
select site,date,time,
max(case when definition='Précipitation sous forme de pluie pour la position 1_1_1' then value else null end) "Précipitation sous forme de pluie pour la position 1_1_1",
max(case when definition='Humidité relative de l’air pour la position 1_1_1' then value else null end) "Humidité relative de l’air pour la position 1_1_1"	,
max(case when definition='Teneur en eau du sol pour la position 1_1_1' then value else null end) "Teneur en eau du sol pour la position 1_1_1",
max(case when definition='Température de l’air pour la position 1_1_1' then value else null end) "Température de l’air pour la position 1_1_1",
max(case when definition='Température du sol pour la position 1_1_1' then value else null end) "Température du sol pour la position 1_1_1",
max(case when definition='Température du sol pour la position 1_2_1' then value else null end) "Température du sol pour la position 1_2_1",
max(case when definition='Direction du vent pour la position 1_1_1' then value else null end) "Direction du vent pour la position 1_1_1",
max(case when definition='Vitesse du vent pour la position 1_1_1' then value else null end) "Vitesse du vent pour la position 1_1_1"
from(
select cn.code as site,date ,time,definition,value  
from 
(
select mms_date as date ,mms_time as time,v.id,value
from valeurs_meteo_sh_vms as v
INNER JOIN  mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id  
where mms_date between '16-02-2016' and '28-03-2017'
)a
join realnode rv ON rv.id = a.id    
join realnode rd ON rd.id = rv.id_parent_node
join realnode rt ON rt.id = rd.id_parent_node   
join realnode rs ON rs.id = rt.id_parent_node  
join composite_nodeable cn ON cn.id = rs.id_nodeable  
join datatype_unite_variable_snot_vdt dvu ON dvu.sdvu_id = rv.id_nodeable 
join variable ON variable.var_id = dvu.id 
where cn.code similar to 'lgt/bm1|ldm/bm1' 
UNION    
select cn.code site, mfs_date as date, mfs_time as time,definition,value  
from valeurs_flux_tour_vfs v 
INNER JOIN  mesures_flux_mfs as m on m.mfs_id=v.mesure_id  
join realnode rv ON rv.id = v.id 
join realnode rd ON rd.id = rv.id_parent_node
join realnode rt ON rt.id = rd.id_parent_node  
join realnode rs ON rs.id = rt.id_parent_node  
join composite_nodeable cn ON cn.id = rs.id_nodeable 
join datatype_unite_variable_snot_vdt dvu ON dvu.sdvu_id = rv.id_nodeable   
join variable ON variable.var_id = dvu.id 
where cn.code similar to 'lgt/bm1|ldm/bm1'  
and mfs_date between '16-02-2016' and '28-03-2017'
)a  
group by site, date, time




select site, date, time,
case when code_variable='P_1_1_1' then value else null end "P_1_1_1",
case when code_variable='RH_1_1_1' then value else null end "RH_1_1_1",
case when code_variable='SWC_1_1_1' then value else null end "SWC_1_1_1",
case when code_variable='TA_1_1_1' then value else null end "TA_1_1_1",
case when code_variable='TS_1_1_1' then value else null end "TS_1_1_1",
case when code_variable='TS_1_2_1' then value else null end "TS_1_2_1",
case when code_variable='WD_1_1_1' then value else null end "WD_1_1_1",
case when code_variable='WS_1_1_1' then value else null end "WS_1_1_1"
from
(
select site,date,time,value,code_variable
from(
select mms_date as date,mms_time as time,v.id,value
from valeurs_meteo_sh_vms as v
INNER JOIN mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id
where mms_date between '16-02-2016' and '28-03-2017'
)a
INNER JOIN 
(
select regPath2[1] as site,regPath2,id,regPath2[3] as code_variable
from(select regexp_split_to_array(regPath1[4],'-') as regPath2,id
from(select id,regexp_split_to_array(path, ',') as regPath1
from realnode)a)b
where regPath2[1] similar to 'lgt/bm1|ldm/bm1'
)R
on R.id=a.id
limit 24528
--group by site, date, time, value, code_variable
)a
limit 24528


queryDataSNOT <- function(variablecharacter,siteSelected,periodeSelected,limit){

if(limit==FALSE){
  queryValueSNOT <- paste("select site,date,time, 
      ",variablecharacter,"
    from data_sh2
    where site similar to '",paste(siteSelected,collapse="|"),"'
    and date between '",periodeSelected[1],"' and '",periodeSelected[2],"'
    group by site, date, time",sep="")

  data <- setDT(dbGetQuery(pool, queryValueSNOT))
}else{
  queryValueSNOT <- paste("select site,date,time, 
      ",variablecharacter,"
    from data_sh2
    where site similar to '",paste(siteSelected,collapse="|"),"'
    and date between '",periodeSelected[1],"' and '",periodeSelected[2],"'
    group by site, date, time
    limit 100",sep="")
  data <- setDT(dbGetQuery(pool, queryValueSNOT))
}
  return(data)
}



