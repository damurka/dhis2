#' data_elements_select
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
data_elements_select <- function(id, name = 'Data Elements', placeholder = 'Select the data elements') {
  ns <- NS(id)
  selectizeInput(
    ns("data_element"),
    name,
    choices = NULL,
    multiple = TRUE,
    options = list(placeholder = placeholder)
  )
}

#' @noRd
data_elements_select_server <- function(id, credentials, use_dataset = FALSE) {

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      data_elements <- reactive({
        req(credentials$auth)

        iso2 <- session$userData$iso2
        get_cached_data_elements(iso2, credentials$auth)
      })

      observe({
        req(data_elements())

        elements <- data_elements()

        if (use_dataset) {
          els <- elements %>%
            distinct(dataset_id, dataset)

          choices <- els$dataset_id
          names(choices) <- els$dataset
        } else {
          choices <- elements$element_id
          names(choices) <- elements$element
        }

        # Update selectize input with new choices
        updateSelectizeInput(session, "data_element", choices = choices, selected = NULL, server = TRUE)
      })

      observeEvent(credentials$auth, {
        if (is.null(credentials$auth)) {
          invoke_js('resetSelectizeInput', list(id = ns('login')))
        }
      }, ignoreNULL = FALSE)

      return(reactive(
        list(
          selected = input$data_element,
          items = data_elements()
        )
      ))
    })
}
