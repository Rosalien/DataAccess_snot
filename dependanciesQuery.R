# Chargement des paramètres de connexion
source("confConnexion.R")
pool <- confConnexion(dbWork="local")#prod, prod_local,local (voir configuration dans confConnexion.R)

tableJeu <- function(x){
  query <- paste("select * from jeu",sep="")
  jeu <- setDT(dbGetQuery(pool, query))
  return(jeu)
}

tableSousJeu <- function(x){
  query <- paste("select j.code_jeu,j.titre as titre_dataset,sj.citation,date_debut,date_fin,sj.doi,sj.titre_licence,sj.url_licence from sousjeu as sj
inner join jeu as j on j.jeu_id=sj.jeu_id",sep="")
  sousjeu <- setDT(dbGetQuery(pool, query))
  return(sousjeu)
}

caracsite <- function(language){
variableCaracData <- c("code_site","site_id","altitude")
metadataVariable <- "site"
internationalVariable <- paste0(metadataVariable,"_",language,collapse = ",")

querysite <- paste0("
select ",paste0(variableCaracData,collapse=","),",",internationalVariable,"
from(
select description as site_fr,l.localestring as site_en, name as code_site, zet_altitude as altitude,site.site_id
from site 
inner join site_snot on site_snot.site_id=site.site_id
inner join localisation as l on l.defaultstring=description
where parent_site_id is null
and l.colonne like 'name')a
")

site <- setDT(dbGetQuery(pool, querysite))
names(site) <- c(variableCaracData,metadataVariable)
return(site)
}

# Caracteristique des variables et des sites de toutes les datatype du SI
caracdata <- function(language){
variableCaracData <- c("code_jeu","code_site","code_station","code_site_station","site_nom","theme","datatype","variable","unite","mindate","maxdate","fabricant","instrument","zet_coordonnees_bbox")
metadataVariable <- c("definition","station_description","site_description","station_nom")
metadataSensor <- c("description_capteur","station_description")
metadataMethod <- c("description_methode")
metadataJeu <- c("titre","descriptionjeu","genealogie","doi","zet_altitude")

internationalVariable <- paste0(c(metadataVariable,metadataSensor,metadataMethod),"_",language,collapse = ",")
querydata <- paste("
select ",paste0(variableCaracData,collapse=","),",",internationalVariable,",",paste0(metadataJeu,collapse=","),"
from public.carac_data_sensor_method_prod
")
data <- setDT(dbGetQuery(pool, querydata))
names(data) <- c(variableCaracData,metadataVariable,metadataSensor,metadataMethod,metadataJeu)
setkey(data, variable,code_site_station, datatype)

return(data)
}

queryDataSNOT <- function(variableSelected,siteSelected,periodeSelected){
  variablequery <- as.matrix(paste("max(case when variable='",variableSelected,"' then value else null end) \"",variableSelected,"\"",sep=""))
  variablecharacter <- apply(variablequery,2,paste,collapse=",")

  queryValueSNOT <- paste("select code_site_station,date,time,datatype,
      ",variablecharacter,"
      from data_infraj_prod
      where date between '",periodeSelected[1],"' and '",periodeSelected[2],"'
      and code_site_station like any ('{",paste(siteSelected,collapse=","),"}' )
      group by code_site_station, date, time,datatype",sep="")

  data <- setDT(dbGetQuery(pool, queryValueSNOT))

  # Partie à optimiser
  meltvalue <- reshape::melt(data,id=1:4,na.rm=TRUE)
  is.na(meltvalue$value) <- meltvalue$value==-9999  
  meltvalue$variable <- as.character(meltvalue$variable)
  setkey(meltvalue, variable, code_site_station, datatype)

  return(meltvalue)
}

# Requête elasticsearch en test (pour le moment, trop long)
queryDataSNOT_EC <- function(variableSelected,siteSelected,periodeSelected){
#periodeSelected <- format(as.Date(periodeSelected,"%d-%m-%Y"),"%Y/%m/%d %H:%M:%S")
periodeSelected <- format(as.Date(periodeSelected,"%d-%m-%Y"),"%Y/%m/%d")
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

queryValueSNOT2 <- query(paste0(
'{
        "bool":{
            "must":[
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
    ],
    "filter":{
        "range": {
                "timestamp": {
                    "from":"',periodeSelected[1],'","to":"',periodeSelected[2],'"
                    }
                }
            }
        }
}'))

for_everything <- query('{
  "match_all": {}
}')

queryForElastic <- paste0(
'{
"query":
{
        "bool":{
            "must":[
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
    ],
    "filter":{
        "range": {
                "timestamp": {
                    "from":"',periodeSelected[1],'","to":"',periodeSelected[2],'"
                    }
                }
            }
        }
}}')

datatest <- elastic("http://localhost:9200", "dataprod_infraj", "dataprod") %search% for_everything
datatest <- elastic("http://localhost:9200", "dataprod", "dataprod") %search% for_everything


es <- elastic("http://localhost:9200", "dataprod_infraj", "dataprod")
system.time(setDT(elastic("http://localhost:9200", "dataprod_infraj", "dataprod") %search% queryValueSNOT2))
system.time(setDT(elastic("http://localhost:9200", "dataprod_infraj", "dataprod") %search% queryValueSNOT))



es <- elastic("http://localhost:9200", "dataprod_infraj", "dataprod")
results <- scroll_search(es, queryForElastic)

system.time(setDT(scroll_search(es, queryForElastic)))

test <- setDT(elastic("http://localhost:9200", "dataprod_infraj", "dataprod") %search% queryValueSNOT)

test2 <- system.time(setDT(elastic("http://localhost:9200", "dataprod_infraj", "dataprod") %search% queryValueSNOT))

#dataProd <- setDT(elastic("http://localhost:9200", "dataprod", "dataprod") %search% queryValueSNOT)
data <- data[,list(site,date=as.Date(timestamp,"%Y/%m/%d %H:%M:%S"),
                time=format(strptime(timestamp, "%Y/%m/%d %H:%M:%S"),"%H:%M:%S"),
                datatype,variable,value)]
###############################################################################""
query <- "http://localhost:9200/dataprod_infraj/dataprod/_search?&site:100000&q=fields.variable=TA_1_1_1"
library(httr) 

}

# Test sur une dbMongo, non opérationnel
mongodbQuery <- function(variableSelected,siteSelected,periodeSelected){
periodeSelected <- format(as.Date(periodeSelected,"%d-%m-%Y"),"%Y/%m/%d %H:%M:%S")
site <- paste0("[\"",paste(siteSelected,collapse="\",\""),"\"]")
variable <- paste0("[\"",paste(variableSelected,collapse="\",\""),"\"]")
datatype <- paste0("[\"",paste("meteosol_infraj",collapse="\",\""),"\"]")

"OrderDate" : { "$gt" : { "$date" : "2015-01-01"}}

queryMongo <- paste0(
  '{"variable" : {"$in":',variable,'},
   "site" : {"$in":',site,'}}'
  )

query2= my_collection$find(
  query=queryMongo
  )
}

caracCarto <- function(language){

variableCaracData <- c("code_site_station","datatype","variables","unite","fabricant","instrument","infos_calibration","zet_coordonnees_bbox","parent_site_id")
metadataVariable <- c("description_capteur","station_description")
internationalVariable <- paste0(metadataVariable,"_",language,collapse = ",")

queryValueSiteStation <- sqlInterpolate(ANSI(),paste0("select ",paste0(variableCaracData,collapse = ","),",",internationalVariable,"
from(
select b[1] as code_site_station, b[2] as datatype,b[3] as variables,b[4] as unite,
  fabricant, dt.code as instrument, dt.description as description_capteur_fr,l.localestring as description_capteur_en,infos_calibration,zet_coordonnees_bbox,site.description as station_description_fr,l2.localestring as station_description_en,parent_site_id
                                   from(
                                   select regexp_split_to_array(a[4],'-'),fabricant, code, description, infos_calibration
                                   from(
                                   select regexp_split_to_array(path, ','),fabricant, i.code, description, infos_calibration
                                   from periode_utilisation_instrument as pui
                                   inner join instrument as i on i.instr_id=pui.instr_id
                                   inner join realnode as rn on rn.id=stdtvar_id
                                   ) as dt(a)
                                   ) as dt(b)
                                   inner join composite_nodeable as cn on cn.code=b[1]
                                   inner join site on site.site_id= cn.id
                                   inner join site_snot on site_snot.site_id=site.site_id
                                   inner join localisation as l on l.defaultstring=dt.description
                                   inner join localisation as l2 on l2.defaultstring=site.description
                                   where l.colonne like 'description'
                   and l2.colonne like 'description'
  )a"))
outdbmap <- setDT(dbGetQuery(pool, queryValueSiteStation))
names(outdbmap) <- c(variableCaracData,metadataVariable)

return(outdbmap)
}