

dygraphTypeVariable <- function(subsetoutbd,frequence){
  if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
  dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
  fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)
  variables <- unique(subsetoutbd$variable)

  keyWord <- c("Soil temperature","Soil water content","Soil heat flux","Température du sol","Teneur en eau du sol","Flux de chaleur dans le sol")
  keyWord <- paste(keyWord,collapse = "|")
  subsetoutbd[,typeVariable:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

  # A améliorer
  #subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$definition, "[[:punct:]]", "")
  #subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$typeVariable, "for profil", "")
  #subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$typeVariable, "[[:digit:]]", "")
  #subsetoutbd$typeVariable <- str_trim(subsetoutbd$typeVariable,"right")

  # Génération d'une time series par type de variable et par site  
  typeVariables <- unique(subsetoutbd$typeVariable)

  res <- lapply(1:length(unique(subsetoutbd$typeVariable)),function(z){#Boucle pour chaque type de variable
    # Sélection du type de variable
    outp <- subsetoutbd[subsetoutbd$typeVariable %in% typeVariables[z],]    
    outp <- outp[order(outp$variable),]

    lapply(unique(outp$code_site_station),function(y){#Pour chaque site, construction d'un graph
            dataxts <- do.call("cbind", lapply(unique(outp$variable),function(x){#Pour chaque variable, construction d'une dbtimes-series
              if(nrow(outp[code_site_station %in% y & variable %in% x,])==0){#revoir ce test
                NULL
              }else{  
                tmp <-  outp[code_site_station %in% y & variable %in% x,list(Date,value)]
                tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
                db <- xts(tmp2[,value],order.by=tmp2[,Date])
                #db <- xts(outp[site %in% y & variable %in% x,value],order.by=outp[site %in% y & variable %in% x,Date])
                colnames(db) <- x
                db
              }
            }))
                if(grepl("year",frequence)){
                  axisYears<-"function(d){ return d.getFullYear() }"
                  tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                                  return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
                  DataTimezone <- FALSE
                }else{
                axisYears<-NULL
                tickerYears <- NULL
                DataTimezone <- TRUE
                }

                dygraph(dataxts,group="groupe",main = paste(unique(outp[,"typeVariable"])," (",y,")",sep=""),ylab = unique(outp[,"unite"])[[1]],height = 250,width="100%")%>%
                dyAxis("x", axisLabelFormatter=axisYears,
                ticker= tickerYears) %>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 20,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(colors = wes_palette("Zissou1", length(unique(outp$variable)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#fin de res
  }
    dy_graph <- tagList(res)
    return(dy_graph)
}

dygraphSite <- function(subsetoutbd,frequence){
  if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
    dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
    fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)
    variables <- unique(subsetoutbd$variable)

    res <- lapply(1:length(variables),function(z){
      outp <- subsetoutbd[subsetoutbd$variable == variables[z],]   
      # Génération de la time serie pour tous les sites
      dataxts <- do.call("cbind", lapply(unique(outp$code_site_station),function(x){
        if(nrow(outp[code_site_station %in% x,])==0){
        NULL
        }else{
        #Construction d'une time-series complète
          tmp <-  outp[code_site_station %in% x,list(Date,value)]
          tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
          db <- xts(tmp2[,value],order.by=tmp2[,Date])
        colnames(db) <- x
        db
        }
        }))

      if(grepl("year",frequence)){
        axisYears<-"function(d){ return d.getFullYear() }"
        tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                          return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
        DataTimezone <- FALSE
      }else{
        axisYears<-NULL
        tickerYears <- NULL
        DataTimezone <- TRUE
      }
      # https://stackoverflow.com/questions/33885817/r-dygraphs-x-axis-granularity-from-monthly-to-yearly
      graph <- dygraph(dataxts,group="groupe",main = unique(outp[,definition]),ylab = unique(outp[,unite]),height = 250,width="100%") %>%
               dyAxis("x", axisLabelFormatter=axisYears,ticker= tickerYears) %>%
               dyOptions(stackedGraph = FALSE) %>%
               dyRangeSelector(height = 20,strokeColor = "") %>%  
               dyLegend(show = "onmouseover") %>% 
               dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
               dyCSScool() %>%
               dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))    %>%   
               dyOptions(colors = wes_palette("Zissou1", length(unique(outp$code_site_station)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=DataTimezone)

    # Condition pour générer un barplot pour les précipitations
    if(unique(outp$variable)=="P_1_1_1" & length(unique(outp$code_site_station)) >=1){graph<-graph %>% dyBarChart()}else{graph}
    if(unique(outp$variable)=="P_1_1_1" & length(unique(outp$code_site_station)) >1){graph<- graph %>% dyMultiColumn()}else{graph}
    graph
    })
  }

    dy_graph <- tagList(res)
    return(dy_graph)
}


dygraphPiezo <- function(subsetoutbd,frequence){
if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
  dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
  fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)

  sitePiezo <- unique(subsetoutbd$site_description)
  
  res <- lapply(1:length(sitePiezo),function(z){#Boucle pour chaque site

    # Sélection du type de piezo
    outp <- subsetoutbd[subsetoutbd$site_description %in% sitePiezo[z],]    
    outp <- outp[order(outp$variable),]

    lapply(unique(outp$variable),function(x){#Pour chaque site, construction d'un graph pour les variables
            dataxts <- do.call("cbind", lapply(unique(outp$station_nom),function(y){#Pour chaque piezo, construction d'une dbtimes-series
              if(nrow(outp[variable %in% x,])==0){
                NULL
              }else{  
                #Construction d'une time-series complète
                tmp <-  outp[station_nom %in% y & variable %in% x,list(Date,value)]
                tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
                db <- xts(tmp2[,value],order.by=tmp2[,Date])
                colnames(db) <- y
                db
              }
            }))
            if(grepl("year",frequence)){
              axisYears<-"function(d){ return d.getFullYear() }"
              tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                              return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
              DataTimezone <- FALSE
                }else{
                  axisYears<-NULL
                  tickerYears <- NULL
                  DataTimezone <- TRUE
                }

                dygraph(dataxts,group=z,main = paste(unique(outp[variable %in% x,"definition"])," (",sitePiezo[z],")",sep=""),ylab = unique(outp[variable %in% x,"unite"])[[1]],height = 250,width="100%")%>%
                dyAxis("x", axisLabelFormatter=axisYears,
                ticker= tickerYears) %>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 20,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(sigFigs=3,colors = wes_palette("Zissou1", length(unique(outp$station_nom)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#fin de res
}
    dy_graph <- tagList(res)
    return(dy_graph)
}

plotlyboxplotTypeVariable  <- function(subsetoutbd){
if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
  subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$definition, "[[:punct:]]", "")
  subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$typeVariable, "pour la position", "")
  subsetoutbd$typeVariable <- str_replace_all(subsetoutbd$typeVariable, "[[:digit:]]", "")
  subsetoutbd$typeVariable <- str_trim(subsetoutbd$typeVariable,"right")

  # Génération d'une time series par type de variable et par site  
  typeVariables <- unique(subsetoutbd$typeVariable)
  res <- lapply(1:length(unique(subsetoutbd$typeVariable)),function(z){#Boucle pour chaque type de variable
      # Sélection du type de variable
      outp <- subsetoutbd[subsetoutbd$typeVariable %in% typeVariables[z],]    
      outp <- outp[order(outp$variable),]
      lapply(unique(outp$code_site_station),function(y){#Pour chaque site, construction d'un graph
        outp <- outp[code_site_station %in% y,]
          gg <- ggplot(outp)+
          geom_boxplot(aes(x=variable,y=value,fill=variable))+
          labs(title=y,fill="",x="",y = unique(outp[,"unite"])[[1]])+
          scale_fill_manual(values=wes_palette("Zissou1",length(unique(outp$variable)),type="continuous"),name="")+
          theme_chart2()
          ggplotly(gg)%>%layout(boxmode = "group")#width=1000,height=300
      })
  })
}
  plotly_graph <- list(res)
  return(plotly_graph)
}

# Construction des graphiques pour les données disponibles (partie welcome)
timelineDataAvailable <- function(tableCarac,allSite,facetWrapOption){
  keyWord <- c("Soil temperature","Soil water content","Soil heat flux","Température du sol","Teneur en eau du sol","Flux de chaleur dans le sol")
  keyWord <- paste(keyWord,collapse = "|")
  tableCarac[,definition_simple:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

# Construction du graph
gg <- ggplot(unique(tableCarac),aes(colour=theme,text = 
                                                paste('<br>Begin:', mindate,
                                                      '<br>End: ', maxdate,
                                                      '<br>Variable: ', definition_simple)))+
      geom_segment(aes(x=mindate, xend=maxdate, y=variable, yend=variable),size=1)+
      labs(fill="",x="",y = "")+
      scale_colour_manual(values=wes_palette("Darjeeling1",length(unique(tableCarac$theme)),type="discrete"),name=translator$t("Thème")) +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_chart()
      
      {if(allSite==TRUE & facetWrapOption==TRUE) gg <- gg+facet_wrap(~site_nom) else gg}
      {if(allSite==FALSE & facetWrapOption==TRUE) gg <- gg+facet_wrap(~station_nom) else gg}
    
return(ggplotly(gg,tooltip = c("text"))%>%layout(legend = list(orientation = "h", x = 0.4, y = -0.05))
)

}


ggplotBoxplotSite <- function(subsetoutbd,facetWrapSelected){
  if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
    variables <- unique(subsetoutbd$variable)
    res <- lapply(1:length(variables),function(z){
      outp <- subsetoutbd[subsetoutbd$variable == variables[z],]
      gg <- ggplot(outp)+
      geom_boxplot(aes(x=variable,y=value,fill=code_site_station))+
      labs(fill="",x="",y = unique(outp[,"unite"])[[1]])+
      scale_fill_manual(values=wes_palette("Zissou1",length(unique(outp$code_site_station)),type="continuous"),name="Site/Station")+
      theme_chart2()

      {if(facetWrapSelected=="month") gg <- gg+facet_wrap(~month) else gg}
      {if(facetWrapSelected=="season") gg <- gg+facet_wrap(~season) else gg}
      {if(facetWrapSelected=="year") gg <- gg+facet_wrap(~year) else gg}
      {if(facetWrapSelected=="dayNight") gg <- gg+facet_wrap(~dayNight) else gg}

    })
    }
    return(res)
}

graphWindRose <- function(subsetoutbd){
if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{

  Bdwind <- dcast(subsetoutbd[variable %in% c("WS_1_1_1","WD_1_1_1"),c("Date","code_site_station","variable","value")],formula=Date+code_site_station~variable)
  Bdwind <- Bdwind[complete.cases(Bdwind[,c("WS_1_1_1","WD_1_1_1")]),]
 
  res <- lapply(unique(Bdwind$code_site_station),function(z){
    plot.windrose(data=Bdwind[code_site_station %in% z,],
        spd="WS_1_1_1",
        dir="WD_1_1_1",
          spdres=0.7,
#         spdseq = c(0,3,6,12,20)
          spdmin=round(min(Bdwind$WS_1_1_1),2),spdmax=round(max(Bdwind$WS_1_1_1),2),
          titreLegend="Vitesse du vent (m/s)",
          titre=z)
  })
}
  return(res)
}

sensorSelectedMap <- function(mapSensorSelected,allSite=FALSE){

    # Extraction des coordonnées des stations 
    mapSensorSelected$lng <- matrix(as.numeric(unlist(
      str_extract_all(mapSensorSelected$zet_coordonnees_bbox, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,1]
    
    mapSensorSelected$lat <- matrix(as.numeric(unlist(
      str_extract_all(mapSensorSelected$zet_coordonnees_bbox, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,2]

    description_station <- paste0("<b>Site/Station : </b>",mapSensorSelected$code_site_station,"<br/>",
                                 "<b>Station : </b>",mapSensorSelected$station_description,"<br/>")
    
#    code_station <- paste(sep = "",
 #                                "<b>Code Site/Station : </b>",mapSensorSelected$code_site_station,"<br/>")
    # Condition pour afficher tous les sites
    if(allSite==TRUE){
      lng <- 2.067
      lat <- 46
      zoom <- 5 

    mapSiteSelected <- mapSensorSelected[,.(lngSite = mean(lng,na.rm=TRUE),
                     latSite = mean(lat,na.rm=TRUE)),by=list(site_nom)]
    description_site <- paste0("<b>Site SNO-Tourbières : </b>",mapSiteSelected$site_nom,"<br/>")
    }else{
      # lng et lat sur la dernière ligne
      lng <- mapSensorSelected[,.SD[.N]]$lng
      lat <- mapSensorSelected[,.SD[.N]]$lat
      zoom <- 16
    }

map <- leaflet(mapSensorSelected)%>%
      setView(lng = lng, lat = lat,zoom=zoom)%>%
      
      # Ajout des tuiles (pour en rajouter voir http://leaflet-extras.github.io/leaflet-providers/preview/)
      addProviderTiles("OpenStreetMap.DE",group="Open Street Map")%>%
      addProviderTiles("Esri.WorldImagery",group = "Image satellite") %>%
      
      # Ajout des stations 
      addCircleMarkers(lng = mapSensorSelected$lng, lat = mapSensorSelected$lat, 
                       radius=6,
                       fillColor = mapSensorSelected$couleur,color="#000000",weight = 1.7,opacity = 0.8,
                       stroke = TRUE, fillOpacity = 0.8, popup = description_station)%>%

      # Ajout des bouton pour revenir à la carte de france et sur les différentes tourbières
      addEasyButton(easyButton(
        icon="fa-globe", title="France",
        onClick=JS("function(btn, map){map.setZoom(6); }")))  %>%

      #addEasyButton(easyButton(
        #'<font size="1">
        #<strong>Bdz</strong>
        #</font>', title="Bernadouze",
        #onClick=JS("function(btn, map){map.setView([42.8015,1.4218], 17);}"))) %>%

      #addEasyButton(easyButton(
        #'<font size="1">
        #<strong>Frn</strong>
        #</font>', title="Frasne",
        #onClick=JS("function(btn, map){map.setView([46.82695,6.17297], 16);}"))) %>%

#      addEasyButton(easyButton(
 #       '<font size="1">
  #      <strong>Ldm</strong>
   #     </font>', title="Landemarais",
    #    onClick=JS("function(btn, map){map.setView([48.442,-1.186], 16);}"))) %>%

     # addEasyButton(easyButton(
      #  '<font size="1">
       # <strong>Lgt</strong>
        #</font>', title="la Guette",
        #onClick=JS("function(btn, map){map.setView([47.32405,2.286589], 16);}"))) %>%
   
     # addMeasure(position = "topleft",
      #           primaryLengthUnit = "meters",
       #          primaryAreaUnit = "sqmeters",
        #         activeColor = "#3D535D",
         #        completedColor = "#7D4479")%>%
      
      # Pour contrôler les couches
      addLayersControl(
        baseGroups = c("Open Street Map", "Image satellite"),
        options = layersControlOptions(collapsed = FALSE),position = "topleft"
      )%>%

      leaflet::addLegend(position = 'topright',
                          colors = unique(mapSensorSelected$couleur), 
                          labels = unique(mapSensorSelected$type),title=translator$t("Type de station"))
      
      # Ajout des sites
      {if(allSite==TRUE) map <- addAwesomeMarkers(map,lng = mapSiteSelected$lngSite,
       lat = mapSiteSelected$latSite,popup = description_site) else map}

      return(map)
}

