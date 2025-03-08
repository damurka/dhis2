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
mod_filter_page_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, 12),
      row_heights = list("auto", 1),
      organisations_select(ns('filter')),
      reactableOutput(ns("table"))
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
mod_filter_page_server <- function(id, data_elements, data_levels, credentials){
  stopifnot(is.reactive(data_elements))
  stopifnot(is.reactive(data_levels))

  moduleServer(
    id = id,
    module = function(input, output, session){
      ns <- session$ns

      orgs <- organisations_select_server('filter', data_levels, credentials)

      data <- reactive({
        data_elements()$items %>%
          filter(element_id %in% data_elements()$selected) %>%
          distinct(element, category, .keep_all = TRUE) %>%
          arrange(category) %>%
          SharedData$new()
      })

      default_selected <- reactive({
        df <- data()$origData()

        rows <- NULL
        if (!is.null(df) && nrow(df) > 0){
          rows = seq_len(nrow(df))
        }
        rows
      })

      output$table <- renderReactable(
        reactable(data(),
                  groupBy = c('element_group', 'element'),
                  selection = "multiple",
                  defaultSelected = default_selected(),
                  onClick = "select",
                  bordered = TRUE,
                  borderless = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  rowStyle = list(cursor = "pointer"),
                  columns = list(
                    element_group = colDef(name = 'Data Element Group'),
                    element = colDef(aggregate = 'unique', name = 'Data Element'),
                    category = colDef(name = 'Category Options'),
                    element_id = colDef(show = FALSE),
                    dataset_id = colDef(show = FALSE),
                    dataset = colDef(show = FALSE),
                    category_id = colDef(show = FALSE),
                    element_group_id = colDef(show = FALSE)
                  )
        )
      )

      return(reactive(
        list(
          orgs = orgs(),
          data_elements = data()$data(withSelection = TRUE) %>%
            filter(selected_ == TRUE | all(is.na(selected_)))
        )
      ))
    }
  )
}
