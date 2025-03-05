box::use(
  shiny[actionButton, icon, is.reactive, moduleServer, NS, observe, reactive, req],
  dplyr[`%>%`, arrange, distinct, filter, select, join_by, left_join, mutate],
  tidyr[pivot_wider],
  reactable[colDef, reactable, reactableOutput, renderReactable],
  bslib[page_fillable, layout_columns, card, card_header],
  lubridate[ymd],
  khisr[`%.d%`, `%.in%`, get_analytics, get_organisations_by_level],
  shinycssloaders[withSpinner]
)

box::use(
  app/view/data_elements_filter,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, 12),
      row_heights = list("auto", 1),
      data_elements_filter$ui(ns('data_elements'), 'Population Data Elements', 'Select the population elements'),
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

#' @export
server <- function(id, data_elements, selected_level, selected_date, credentials) {
  stopifnot(is.reactive(data_elements))
  stopifnot(is.reactive(selected_level))
  stopifnot(is.reactive(selected_date))

  moduleServer(id = id, module = function(input, output, session) {

    selected_elements <- data_elements_filter$server('data_elements', data_elements, credentials)

    output$table <- renderReactable({
      req(selected_level(), selected_elements(), selected_date())

      start_date <- selected_date()[1]
      end_date <- selected_date()[2]

      pop_periods <- format(seq(ymd(start_date), ymd(end_date), by = 'year'), '%Y')

      data <- get_analytics(
        dx %.d% selected_elements(),
        pe %.d% pop_periods,
        ou %.d% paste0('LEVEL-', selected_level()),
        auth = credentials$auth)

      orgs <- get_organisations_by_level(org_ids = data$ou, level = as.integer(selected_level()), auth = credentials$auth)
      data_els <- data_elements() %>%
        select(element_id, element) %>%
        distinct()

      data %>%
        left_join(data_els, join_by(dx == element_id)) %>%
        left_join(orgs, join_by(ou == id)) %>%
        mutate(year = as.integer(pe)) %>%
        select(-dx, -ou, -pe) %>%
        pivot_wider(names_from = element, values_from = value) %>%
        reactable()
    })

  })
}
