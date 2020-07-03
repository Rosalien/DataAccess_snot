source("dependancies.R")
source("mod_extraction2.R")
 
translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")

ui <- fluidPage(theme =shinytheme("flatly"),

   #theme="www/css/bootstrap.css",
   #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/css/bootstrap.css")),
    #includeCSS("www/css/bootstrap.css"),#tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  	useShinyjs(),
    inlineCSS(appCSS()),
    # Loading message
    div(id = "loading-content",withSpinner(h4(translator$t("Chargement ...")),type=1)),
  	hidden(
  	div(
  		id = "app-content",
  		fluidPage(       		
           titlePanel(
                  title="", windowTitle=translator$t("Visualisation & Extraction des donnée du SNO-T")
           )
        ),
    		navbarPage(
          fluidRow(       
              div(img(src="https://www.sno-tourbieres.cnrs.fr/wp-content/uploads/2017/11/logo_Tourbieres.jpg",height = "60px",style = "position: relative; top: -20px;left:0px;"), "")
          ),
          mod_extractionui("extraction")
          )
    			)#navbar
    		)#div
  	)

server <- function(input, output, session) {
  #output$selectLanguage <- renderUI({
    #radioButtons('selected_language',
               #label="",
               #choices = translator$languages,
               #selected = input$selected_language)
  #})

      # Simulate work being done for 5 second
    	Sys.sleep(5)  
    	# Hide the loading message when the rest of the server function has executed
    	hide(id = "loading-content", anim = TRUE, animType = "fade")    
  	  show("app-content")
    	# Chargement des modules
    	extraction <- callModule(module=mod_extraction,"extraction")#,translationVariable = reactive(input$selected_language))
  }

  shinyApp(ui, server)


