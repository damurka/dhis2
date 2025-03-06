#' population_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_population_page_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' population_page Server Functions
#'
#' @noRd
mod_population_page_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
