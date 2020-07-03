#' @export
#' @import shiny
#' @importFrom shinyjs enable disable disabled
#' @title mod_descriptionui
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

mod_descriptionUI <- function(id){
  ns <- NS(id)

  fluidRow(style="padding-left:15px;",dataTableOutput(ns("tableData"))
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

mod_description <- function(input, output, session,caracData,siteSelected,variableSelected){

  ns <- session$ns
  
  r <- reactiveValues(
    r = reactiveValues()
  )

  observe({
    r$r$siteSelected <- siteSelected
    r$r$variableSelected <- variableSelected
  })
    
# renderUI pour la table de description des variables sélectionnées
  output$tableData <- DT::renderDataTable({
    descriptionData <- unique(caracData[site %in% r$r$siteSelected & variable %in% r$r$variableSelected,list(site,station_description,variable,definition,unite,mindate,maxdate)])
    names(descriptionData) <- c("Site","Station","Variable","Description",translator$t("Unité"),translator$t("Début"),translator$t("Fin"))
    retData <- DT::datatable(descriptionData,extensions = 'Buttons',rownames= FALSE,filter = 'top',
      options = list(dom = "Blfrtip", buttons = list("copy",list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = "Download")),
                    pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

}
