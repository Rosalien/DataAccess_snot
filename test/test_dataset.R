jeu <- c("lgt-ges-eddycovariance")

#jeu <- "bdz-meteosol-meteosol"


caracDataset <- caracdata("en")[code_jeu %in% jeu,]
filename <- paste("TOUR_DAT_",jeu,".zip",sep="")
# Calcul des paramètres pour interroger la base elasticsearch
date_debut <- min(as.Date(unique(caracDataset[,mindate]),"%d-%m-%Y"))
date_fin <- max(as.Date(unique(caracDataset[,maxdate]),"%d-%m-%Y"))
periodeSelected <- c(date_debut,date_fin)
siteJeu <- unique(caracDataset[,code_site_station])

# Construction de la requête
variableSelected <- unique(caracDataset[,variable])
data <- queryDataSNOT(variableSelected,siteJeu,periodeSelected)
data <- dcast(data, formula = code_site_station+date+time+datatype~variable, value.var = "value")
data[,dateEnd:=paste0(date,'T',time,'Z')]


dataZENODO <- data

siteVariableJeu <- unique(caracDataset[code_jeu %in% jeu,list(code_jeu,code_site_station,variable)])

filePivotJeu <- lapply(unique(siteVariableJeu[,code_jeu]) ,function(x){
  siteVariable <- unique(caracDataset[code_jeu %in% x,list(code_site_station,variable)])
  pivotHeaderAndData <- createFilePivot(data,siteVariable,caracdata("en"))
   lapply(1:nrow(siteVariable),function(x){
    fileName <- pivotHeaderAndData[[x]]$fileName
    writeHeaderPivot(pivotHeaderAndData[[x]][["data"]],fileName,pivotHeaderAndData[[x]][["header"]])
    return(fileName)
  })
})


lapply(1:length(filePivotJeu),function(x){
  zip(zipfile=filename[x],files=unlist(filePivotJeu[x]))
})
  
  zip(zipfile="test.zip",files=c(unlist(filename))
  
    



pivot_metadata <- robustCurl(httr::GET(paste("http://localhost:8081/rest/resources/pivot?codes_jeu=",jeu,sep=""),httr::timeout(60)))
      

      siteVariable <- sqlOutputdataset$siteVariable
