#' population_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_population_page_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, 12),
      row_heights = list("auto", 1),
      data_elements_select(ns('data_elements'), 'Population Data Elements', 'Select the population elements'),
      card(
        card_header(
          'Population Data',
          actionButton(ns('button'),
                       label = 'Download as CSV',
                       icon = icon("download"),
                       onclick = sprintf("Reactable.downloadDataCSV('%s', 'population-data-%s.csv')", ns('table'), format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

          )
        ),
        withSpinner(
          reactableOutput(ns("table"))
        )
      )
    )
  )
}

#' population_page Server Functions
#'
#' @noRd
mod_population_page_server <- function(id, data_levels, selected_date, credentials){
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(selected_date))

  moduleServer(
    id = id,
    module = function(input, output, session){
      ns <- session$ns

      data_elements <- data_elements_select_server('data_elements', credentials)

      pop_data <- reactive({
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

      output$table <- renderReactable({
        pop_data() %>%
          reactable(
            compact = TRUE,
            highlight = TRUE,
            defaultPageSize = 15,
            searchable = TRUE,
            minRows = 15,
            #wrap = FALSE,
            resizable = TRUE,
            pagination = TRUE
          )
      })
    }
  )
}
