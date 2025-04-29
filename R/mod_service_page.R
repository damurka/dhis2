#' service_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_service_page_ui <- function(id) {
  ns <- NS(id)
  withSpinner(
    tagList(
      uiOutput(ns("download_ui")),
      reactableOutput(ns("table"))
    )
  )
}

#' service_page Server Functions
#'
#' @noRd
mod_service_page_server <- function(id, data_analytics, data_levels, selected_data_elements, selected_orgs, custom_groups){
  stopifnot(is.reactive(data_analytics))
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(selected_data_elements))
  stopifnot(is.reactive(selected_orgs))
  stopifnot(is.reactive(custom_groups))

  moduleServer(
    id = id,
    module = function(input, output, session){
      ns <- session$ns

      category_filter <- reactive({
        selected_data_elements() %>%
          distinct(category) %>%
          pull(category)
      })

      data <- reactive({
        req(data_analytics())

        dt <- data_analytics()

        if (inherits(dt, 'error')) {
          showNotification(dt$message, type = "error", duration=15)
          return(tibble(error = dt$message))
        }

        if (is.null(dt) || nrow(dt) == 0) {
          showNotification('No data was returned for the specified period', type = "warning", duration=15)
          return(tibble(error = 'No data was returned for the specified period'))
        }

        summarised_with_groups <- dt %>%
          left_join(custom_groups(), join_by(element, category)) %>%
          my_summary(
            data_levels = data_levels()$items,
            org_level =data_levels()$selected,
            .by = c('group_name', 'year', 'month'),
            total = sum(value)
          ) %>%
          drop_na(group_name) %>%
          pivot_wider(names_from = group_name, values_from = total)

        summarised_data <- dt %>%
          filter(category %in% category_filter()) %>%
          my_summary(
            data_levels = data_levels()$items,
            org_level =data_levels()$selected,
            .by = c('element', 'year', 'month'),
            total = sum(value)
          ) %>%
          pivot_wider(names_from = element, values_from = total, values_fill = 0) %>%
          filter_by_orgs(selected_orgs(), data_levels()$items, data_levels()$selected)

        org_cols <- get_organisation_cols(data_levels()$items, data_levels()$selected)

        summarised_data %>%
          left_join(summarised_with_groups, by = c(org_cols, 'year', 'month'))
      })

      observe({
        req(data())  # Ensure data is available
        show_button <- nrow(data()) > 0  # Check if table has data

        output$download_ui <- renderUI({
          if (show_button) {
            actionButton(
              ns("button"),
              label = "Download as CSV",
              icon = icon("download"),
              onclick = sprintf(
                "Reactable.downloadDataCSV('%s', 'service-data-%s.csv')",
                ns("table"), format(Sys.time(), "%Y%m%d%H%M%S")
              )
            )
          } else {
            NULL  # Hide button if no data
          }
        })
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
