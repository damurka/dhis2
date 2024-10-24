box::use(
  dplyr[distinct, filter, pull],
  reactable[reactable, reactableOutput, renderReactable],
  janitor[make_clean_names],
  tidyr[pivot_wider, `%>%`],
  shiny[actionButton, icon, is.reactive, moduleServer, NS, observeEvent, showNotification, reactive],
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
                   onclick = sprintf("Reactable.downloadDataCSV('%s', 'service-data-%s.csv')", ns('table'), format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

      )
    ),
    withSpinner(
      reactableOutput(ns("table"))
    )
  )
}

#' @export
server <- function(id, data_analytics, data_levels, selected_level, filters) {
  stopifnot(is.reactive(data_analytics))
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(selected_level))
  stopifnot(is.reactive(filters))

  moduleServer(id = id, module = function(input, output, session) {

    category_filter <- reactive({
      filters()$data_elements %>%
        distinct(category) %>%
        pull(category)
    })



    data <- reactive({

      dt <- data_analytics()
      if (inherits(dt, 'error')) {
        showNotification(dt$message, type = "error", duration=15)
        return(data.frame(error = dt$message))
      }

      if (is.null(dt)) {
        showNotification('No data was returned for the specified period', type = "warning", duration=15)
        return(data.frame(error = 'No data was returned for the specified period'))
      }

      dt %>%
        filter(category %in% category_filter()) %>%
        my_summary(
          data_levels = data_levels(),
          org_level = selected_level(),
          .by = c('element', 'year', 'month'),
          total = sum(value)
        ) %>%
        pivot_wider(names_from = element, values_from = total, values_fill = 0) %>%
        filter_by_orgs(filters()$orgs, data_levels(), selected_level())
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
                pagination = FALSE
          )
    )

  })
}
