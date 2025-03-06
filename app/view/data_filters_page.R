box::use(
  shiny[is.reactive, moduleServer, NS, observe, reactive],
  crosstalk[SharedData],
  dplyr[`%>%`, arrange, distinct, filter, select],
  reactable[colDef, reactable, reactableOutput, renderReactable],
  bslib[page_fillable, layout_columns]
)

box::use(
  app/view/organisation_units_filter
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, 12),
      row_heights = list("auto", 1),
      organisation_units_filter$ui(ns('filter')),
      reactableOutput(ns("table"))
    )
  )
}

#' @export
server <- function(id, data_elements, selected_elements, data_levels, selected_level, credentials) {
  stopifnot(is.reactive(data_elements))
  stopifnot(is.reactive(selected_elements))

  moduleServer(id = id, module = function(input, output, session) {

    orgs <- organisation_units_filter$server('filter', data_levels, selected_level, credentials)

    data <- reactive({
      data_elements() %>%
        filter(element_id %in% selected_elements()) %>%
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
  })
}
