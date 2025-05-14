#' mapping_dialog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mapping_dialog <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' mapping_dialog Server Functions
#'
#' @noRd
mod_mapping_dialog_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_mapping_dialog("mapping_dialog_1")

## To be copied in the server
# mod_mapping_dialog_server("mapping_dialog_1")
