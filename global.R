library(shiny.i18n)

translator <- Translator$new(translation_csvs_path = "translation")
translator$set_translation_language("en")
language <- "en"

# allSite variable for welcome part
allSite <- "All peatlands"
