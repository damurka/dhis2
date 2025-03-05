box::use(
  shiny[is.reactive, moduleServer, NS, observe, observeEvent, reactive, req, selectizeInput, updateSelectizeInput]
)

#' @export
ui <- function(id, name = 'Data Elements', placeholder = 'Select the data elements') {
  ns <- NS(id)
  selectizeInput(
    ns("data_element"),
    name,
    choices = NULL,
    multiple = TRUE,
    options = list(placeholder = placeholder)
  )
}

#' @export
server <- function(id, data_elements, credentials) {

  moduleServer(id = id, module = function(input, output, session) {
    stopifnot(is.reactive(data_elements))

    observe({
      req(credentials$auth)
      elements <- data_elements()

      if (!is.null(elements) && !is.null(credentials$auth)) {
        choices <- elements$element_id
        names(choices) <- elements$element

        # Update selectize input with new choices
        updateSelectizeInput(session, "data_element", choices = choices, selected = NULL, server = TRUE)
      }
    })

    observeEvent(credentials$auth, {
      if (is.null(credentials$auth)) {
        #updateSelectizeInput(session, "data_element", choices = NULL, selected = NULL, server = TRUE)
        session$sendCustomMessage("resetSelectizeInput", list(id = session$ns("data_element")))
      }
    }, ignoreNULL = FALSE)

    return(reactive(input$data_element))
  })
}
