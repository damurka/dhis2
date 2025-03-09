#' Filter Page UI Function
#'
#' Creates the UI for the filter page, including an organisation selection input
#' and a data table.
#'
#' @param id Character. The module ID for namespacing UI elements.
#'
#' @return A Shiny UI layout with organisation selection and a `reactableOutput`
#'   table.
#'
#' @noRd
mod_service_group_page_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      fill = FALSE,
      col_widths = c(9,3),
      data_elements_select(ns('data_elements')),
      div(style = "display: flex; align-items: flex-end; height: 100%;",
          actionButton(ns('retrieve'), 'Retrieve Data')
      )
    ),
    navset_card_tab(
      full_screen = TRUE,
      title = 'Service Page',
      nav_panel(
        'Data Filters',
        mod_filter_page_ui(ns('filter_page_1'))
      ),
      nav_panel(
        'Service Data',
        mod_service_page_ui(ns('service_page_1'))
      ),
      nav_panel(
        'Reporting Completeness',
        mod_completeness_page_ui(ns('completeness_page_1'))
      ),
      nav_panel(
        'Custom Aggregation Settings',
        mod_custom_service_page_ui(ns('custom_service_page_1'))
      )
    )
  )
}

#' Filter Page Server Function
#'
#' Handles data filtering based on selected organisations and data elements,
#' providing an interactive table.
#'
#' @param id Character. The module ID for namespacing.
#' @param data_elements Reactive. A reactive expression containing data elements.
#' @param data_levels Reactive. A reactive expression containing available data
#'   levels.
#' @param credentials List. Authentication credentials for accessing organisation
#'   data.
#'
#' @return A reactive list containing:
#'   - orgs Reactive vector of selected organisations.
#'   - data_elements  Reactive filtered dataset including only selected elements.
#'
#' @noRd
mod_service_group_page_server <- function(id, data_levels, selected_orgs, logout_event, selected_date, credentials){
  stopifnot(is.reactive(selected_orgs))
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(logout_event))
  stopifnot(is.reactive(selected_date))

  moduleServer(
    id = id,
    module = function(input, output, session){
      ns <- session$ns

      data_elements <- data_elements_select_server('data_elements', credentials)

      data_analytics <- eventReactive(c(input$retrieve, logout_event()), {
        req(credentials$auth, data_elements()$selected, data_levels()$selected, selected_date())

        tryCatch(
          get_data_analytics_(
            element_ids = data_elements()$selected,
            level = data_levels()$selected,
            start_date = selected_date()[1],
            end_date = selected_date()[2],
            auth = credentials$auth
          ),
          error = function(e) e
        )
      })

      datasets <- eventReactive(c(input$retrieve, logout_event()), {
        req(credentials$auth, data_elements()$items, data_elements()$selected, data_levels()$selected, selected_date())

        tryCatch(
          get_datasets_(
            data_elements = data_elements()$items,
            element_id = data_elements()$selected,
            level = data_levels()$selected,
            start_date = selected_date()[1],
            end_date = selected_date()[2],
            auth = credentials$auth
          ),
          error = function(e) e
        )
      })

      selected_data_elements <- mod_filter_page_server("filter_page_1", data_elements)
      custom_groups <- mod_custom_service_page_server('custom_service_page_1', data_elements)
      mod_service_page_server('service_page_1', data_analytics, data_levels, selected_data_elements, selected_orgs, custom_groups)
      mod_completeness_page_server('completeness_page_1', datasets, data_levels, selected_data_elements, selected_orgs)

      observeEvent(input$retrieve, {
        req(credentials$auth)

        if (is.null(data_elements()$selected)) {
          showNotification("No data elements are selected", type = "error", duration=5)
        }
      })
    }
  )
}
