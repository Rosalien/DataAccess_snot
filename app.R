source("dependancies.R")
#source("mod_welcome.R")
source("mod_extractiondataset.R")
source("global.R")
#source("mod_accessdata.R")
#source("mod_about.R")

ui <- fluidPage(theme =shinytheme("flatly"),

   #theme="www/css/bootstrap.css",
   #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/css/bootstrap.css")),
    #includeCSS("www/css/bootstrap.css"),#tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  	useShinyjs(),
    inlineCSS(appCSS()),
    # Loading message
    div(id = "loading-content",withSpinner(h4(translator$t("Chargement ...")),type=4)),
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
              div(img(src="https://raw.githubusercontent.com/Rosalien/doc_snot/master/Figures/logo_Tourbieres.jpg",height = "60px",style = "position: relative; top: -20px;left:0px;"), "")
              #,           
              #div(uiOutput('selectLanguage'),style="position:relative;top:-90px;left:1500px")
          ),
    #      mod_welcomeUI("welcome"),
     #     mod_accessdataUI("accessdata")
          mod_extractiondatasetUI("extractiondataset")
       #   mod_aboutUI("about")
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

      #rv <- reactiveValues(translationVariable = "en")


      # Simulate work being done for 5 second
    	Sys.sleep(5)  
    	# Hide the loading message when the rest of the server function has executed
    	hide(id = "loading-content", anim = TRUE, animType = "fade")    
  	  show("app-content")

    	# Chargement des modules
  	  #carto <- callModule(mod_welcome,"welcome")
      #extraction <- callModule(module=mod_accessdata,"accessdata")#,translationVariable = reactive(rv$translationVariable))
      extractiondataset <- callModule(module=mod_extractiondataset,"extractiondataset")
      #about <- callModule(module=mod_about,"about")
  }

  shinyApp(ui, server)
