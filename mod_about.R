# uiExtraction
mod_aboutUI <- function(id){
  ns <- NS(id)
             tabPanel("About",
              includeMarkdown("www/md/about_en.md"))
}

mod_about <- function(input, output, session){
  ns <- session$ns
}#Fin du module


