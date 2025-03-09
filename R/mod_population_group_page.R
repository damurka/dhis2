#' population_group_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_population_group_page_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      fill = FALSE,
      col_widths = c(9,3),
      data_elements_select(ns('data_elements'), 'Population Data Elements', 'Select the population elements'),
      div(style = "display: flex; align-items: flex-end; height: 100%;",
          actionButton(ns('retrieve'), 'Retrieve Data')
      )
    ),
    navset_card_tab(
      full_screen = TRUE,
      title = 'Population Page',
      nav_panel(
        'Data Filters',
        mod_filter_page_ui(ns('filter_page_1'))
      ),
      nav_panel(
        'Population Data',
        mod_population_page_ui(ns('population_page_1'))
      ),
      nav_panel(
        'Custom Aggregation Settings',
        mod_custom_service_page_ui(ns('custom_service_page_1'))
      )
    )
  )
}

#' population_group_page Server Functions
#'
#' @noRd
mod_population_group_page_server <- function(id, data_levels, selected_orgs, logout_event, selected_date, credentials){
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(selected_orgs))
  stopifnot(is.reactive(logout_event))
  stopifnot(is.reactive(selected_date))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      data_elements <- data_elements_select_server('data_elements', credentials)

      data_analytics <- eventReactive(c(input$retrieve, logout_event), {
        req(credentials$auth, data_elements()$items, data_elements()$selected, selected_date(), data_levels()$selected)

        get_population_data_analytics(
          data_elements = data_elements()$items,
          element_ids = data_elements()$selected,
          start_date = selected_date()[1],
          end_date = selected_date()[2],
          level = data_levels()$selected,
          auth = credentials$auth
        )
      })

      selected_data_elements <- mod_filter_page_server("filter_page_1", data_elements)
      custom_groups <- mod_custom_service_page_server('custom_service_page_1', data_elements)
      # mod_service_page_server('population_page_1', data_analytics, data_levels, selected_data_elements, selected_orgs, custom_groups)
      mod_population_page_server('population_page_1', data_analytics, data_levels, selected_data_elements, selected_orgs, custom_groups)
    }
  )
}
