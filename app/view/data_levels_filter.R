box::use(
  shiny[is.reactive, moduleServer, NS, observe, observeEvent, reactive, req, selectizeInput, updateSelectizeInput]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  selectizeInput(
    ns("level"),
    "Data Level",
    choices = NULL,
    options = list(placeholder = 'Select the data level')
  )
}

#' @export
server <- function(id, data_levels, credentials) {
  stopifnot(is.reactive(data_levels))

  moduleServer(id = id, module = function(input, output, session) {

    observe({
      req(credentials$auth)
      levels <- data_levels()

      if (!is.null(levels) && !is.null(credentials$auth)) {
        choices <- levels$level
        names(choices) <- levels$name

        # Update selectize input with new choices
        updateSelectizeInput(session, "level", choices = choices, selected = 2, server = TRUE)
      }
    })

    observeEvent(credentials$auth, {
      if (is.null(credentials$auth)) {
        # Clear the selectize input on logout
        # updateSelectizeInput(session, "level", choices = NULL, selected = NULL, server = TRUE)
        session$sendCustomMessage("resetSelectizeInput", list(id = session$ns("level")))
      }
    }, ignoreNULL = FALSE)

    return(reactive(input$level))
  })
}
