.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "custom-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "MST") # path to resource in your package
  )
  shiny::addResourcePath(
    prefix = "item_banks", # custom prefix that will be used to reference your directory
    directoryPath = system.file("item_banks", package = "itembankr") # path to resource in your package
  )
}

MST_dict <- musicassessr::dict(additional_dict = MST_dict_df)
