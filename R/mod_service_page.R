#' service_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_service_page_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      'Service Data',
      actionButton(ns('button'),
                   label = 'Download as CSV',
                   icon = icon("download"),
                   onclick = sprintf("Reactable.downloadDataCSV('%s', 'service-data-%s.csv')", ns('table'), format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

      )
    ),
    withSpinner(
      reactableOutput(ns("table"))
    )
  )
}

#' service_page Server Functions
#'
#' @noRd
mod_service_page_server <- function(id, data_nalytics, data_levels, filters){
  stopifnot(is.reactive(data_nalytics))
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(filters))

  moduleServer(
    id = id,
    module = function(input, output, session){
      ns <- session$ns

      category_filter <- reactive({
        filters()$data_elements %>%
          distinct(category) %>%
          pull(category)
      })

      data <- reactive({
        req(data_nalytics())

        dt <- data_nalytics()

        if (inherits(dt, 'error')) {
          showNotification(dt$message, type = "error", duration=15)
          return(tibble(error = dt$message))
        }

        if (is.null(dt) || nrow(dt) == 0) {
          showNotification('No data was returned for the specified period', type = "warning", duration=15)
          return(tibble(error = 'No data was returned for the specified period'))
        }

        dt %>%
          filter(category %in% category_filter()) %>%
          my_summary(
            data_levels = data_levels()$items,
            org_level = data_levels()$selected,
            .by = c('element', 'year', 'month'),
            total = sum(value)
          ) %>%
          pivot_wider(names_from = element, values_from = total, values_fill = 0) %>%
          filter_by_orgs(filters()$orgs, data_levels()$items, data_levels()$selected)
      })

      output$table <- renderReactable(
        reactable(data(),
                  # noData = 'No data was returned',
                  compact = TRUE,
                  highlight = TRUE,
                  defaultPageSize = 15,
                  searchable = TRUE,
                  minRows = 15,
                  #wrap = FALSE,
                  resizable = TRUE,
                  pagination = TRUE
        )
      )

    }
  )
}
