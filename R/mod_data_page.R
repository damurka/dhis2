#' data_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_data_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
      fluidPage(
        mod_service_page_ui("service_page_1"),
        mod_completeness_page_ui("completeness_page_1"),
        mod_population_page_ui("population_page_1"),
        mod_countdown_page_ui("countdown_page_1")
      )
  )
}

#' data_page Server Functions
#'
#' @noRd
mod_data_page_server <- function(id, credentials){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    mod_service_page_server("service_page_1")
    mod_completeness_page_server("completeness_page_1")
    mod_population_page_server("population_page_1")
    mod_countdown_page_server("countdown_page_1")

  })
}
