#' completeness_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_completeness_page_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      'Service Data',
      actionButton(ns('button'),
                   label = 'Download as CSV',
                   icon = icon("download"),
                   onclick = sprintf("Reactable.downloadDataCSV('%s', 'data-completeness-%s.csv')", ns('table'), format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

      )
    ),
    withSpinner(
      reactableOutput(ns("table"))
    )
  )
}

#' completeness_page Server Functions
#'
#' @noRd
mod_completeness_page_server <- function(id, datasets, data_levels, filters) {
  stopifnot(is.reactive(datasets))
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(filters))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      dataset_filter <- reactive({
        filters()$data_elements %>%
          distinct(dataset) %>%
          pull(dataset)
      })

      data <- reactive({
        dt <- datasets()

        if (inherits(dt, 'error')) {
          showNotification(dt$message, type = "error", duration=15)
          return(tibble(error = dt$message))
        }

        if (is.null(dt) || nrow(dt) == 0) {
          showNotification('No data was returned for the specified period', type = "warning", duration=15)
          return(tibble(error = 'No data was returned for the specified period'))
        }

        dt %>%
          # filter(str_detect(dataset, fixed(dataset_filter()))) %>%
          my_summary(
            data_levels = data_levels()$items,
            org_level = data_levels()$selected,
            .by = c('year', 'month', 'dataset'),
            actual = sum(actual_reports),
            expected = sum(expected_reports),
            reporting_rate = actual/expected
          ) %>%
          pivot_wider(
            names_from = dataset,
            values_from = c(actual,expected, reporting_rate),
            names_glue = '{dataset}_{.value}',
            values_fill = 0
          ) %>%
          filter_by_orgs(filters()$orgs, data_levels()$items, data_levels()$selected)
      })

      dataset_names <- reactive({
        dt <- datasets()
        if (inherits(dt, 'error')) {
          return(NULL)
        }
        dt %>%
          distinct(dataset) %>%
          pull(dataset)
      })

      columns <- reactive({
        names(data()) %>%
          str_subset('_actual$|_expected$|_reporting_rate$')
      })

      output$table <- renderReactable(

        reactable(data(),
                  # noData = 'No data was returned',
                  columns = map(columns(), ~ {
                    suffix <- str_extract(.x, "_actual$|_expected$|_reporting_rate$")
                    colDef(
                      name = switch(
                        suffix,
                        "_actual" = "Actual",
                        "_expected" = "Expected",
                        "_reporting_rate" = "Reporting Rate"
                      ),
                      format = switch(
                        suffix,
                        "_actual" = colFormat(digits = 0),  # Format Actual as a number
                        "_expected" = colFormat(digits = 0),  # Format Expected as a number
                        "_reporting_rate" = colFormat(percent = TRUE, digits = 1)  # Format Reporting Rate as percentage
                      )
                    )
                  }
                  ) %>% set_names(columns()),
                  columnGroups = map(dataset_names(), ~ {
                    # Escape special characters in the dataset name
                    dataset_name_escaped <- str_replace_all(.x, "([\\(\\)\\[\\]\\{\\}\\^\\$\\+\\*\\?\\|\\\\])", "\\\\\\1")
                    # Find the columns that match the group pattern
                    columns <- str_subset(names(data()), str_c('^', dataset_name_escaped, "(_actual|_expected|_reporting_rate)$"))
                    # Only create a group if there are matching columns
                    if (length(columns) > 0) {
                      colGroup(name = .x, columns = columns)
                    } else {
                      NULL
                    }
                  }) %>% compact(),
                  compact = TRUE,
                  highlight = TRUE,
                  searchable = TRUE,
                  minRows = 15,
                  pagination = TRUE,
                  defaultPageSize = 15,
                  resizable = TRUE
        )
      )
    }
  )
}
