#' filter_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_page_ui <- function(id) {
  ns <- NS(id)
  withSpinner(
    reactableOutput(ns("table"))
  )
}

#' filter_page Server Functions
#'
#' @noRd
mod_filter_page_server <- function(id, data_elements){
  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

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

      selected_data_elements <- reactive({
        data()$data(withSelection = TRUE) %>%
          filter(selected_ == TRUE | all(is.na(selected_)))
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

      return(selected_data_elements)
    }
  )
}
