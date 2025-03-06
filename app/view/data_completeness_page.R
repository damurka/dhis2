box::use(
  reactable[colDef, colFormat, colGroup, reactable, reactableOutput, renderReactable],
  shiny[actionButton, icon, is.reactive, moduleServer, NS, showNotification, reactive],
  dplyr[distinct, filter, pull, `%>%`],
  stringr[fixed, str_detect],
  tidyr[pivot_wider],
  purrr[compact, map, set_names],
  stringr[str_subset, str_c, str_extract, str_replace_all],
  bslib[card, card_header],
  shinycssloaders[withSpinner]
)

box::use(
  app/logic/data_extraction[my_summary, filter_by_orgs]
)

#' @export
ui <- function(id) {
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

#' @export
server <- function(id, datasets, data_levels, selected_level, filters) {
  stopifnot(is.reactive(datasets))
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(selected_level))
  stopifnot(is.reactive(filters))

  moduleServer(id = id, module = function(input, output, session) {

    dataset_filter <- reactive({
      filters()$data_elements %>%
        distinct(dataset) %>%
        pull(dataset)
    })

    data <- reactive({

      dt <-  datasets()
      if (inherits(dt, 'error')) {
        showNotification(dt$message, type = "error", duration=15)
        return(data.frame(error = dt$message))
      }

      if (is.null(dt)) {
        showNotification('No data was returned for the specified period', type = "warning", duration=15)
        return(data.frame(error = 'No data was returned for the specified period'))
      }

      print(dt %>% pull(dataset))
      print(filters()$data_elements)

      dt %>%
        # filter(str_detect(dataset, fixed(dataset_filter()))) %>%
        my_summary(
          data_levels = data_levels(),
          org_level = selected_level(),
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
        filter_by_orgs(filters()$orgs, data_levels(), selected_level())
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

  })
}
