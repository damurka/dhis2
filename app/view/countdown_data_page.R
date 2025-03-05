box::use(
  shiny[actionButton, column, fluidRow, icon, is.reactive, moduleServer, NS, p, reactive, renderUI, tagList, selectizeInput, uiOutput],
  bslib[page_fillable, layout_columns, card, card_header],
  dplyr[`%>%`, select],
  purrr[pmap],
  reactable[colDef, reactable, reactableOutput, renderReactable],
  shinycssloaders[withSpinner]
)

box::use(
  app/logic/data_extraction[get_sheet_data],
  app/view/data_elements_filter
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = c(12, 12),
      row_heights = list("auto", 1),
      selectizeInput(ns('language'), label = 'Language', choices = c('English' = 'english', 'French' = 'french')),
      card(
        card_header(
          'Service Data',
          actionButton(ns('button'),
                       label = 'Download as CSV',
                       icon = icon("download"),
                       onclick = sprintf("Reactable.downloadDataCSV('%s', 'population-data-%s.csv')", ns('table'), format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

          )
        ),
        withSpinner(
          uiOutput(ns("table"))
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

  moduleServer(
    id = id,
    module = function(input, output, session) {

      sheet_data <- reactive({
        get_sheet_data()
      })

      output$table <- renderUI({

        dt <- sheet_data() %>%
          select(hfd_sheet, hfd_id, any_of(paste0('hfd_title_', input$language)))

        tagList(
          pmap(dt, function(hfd_sheet, hfd_id, hfd_title_english) {
            # print(paste("Sheet:", hfd_sheet, "- id:", hfd_id, '- title:', hfd_title_english ))
            fluidRow(
              column(6, p(hfd_title_english)),
              column(6, data_elements_filter$ui(session$ns(hfd_id), NULL))
            )
          })
        )
      })

    }
  )

}
