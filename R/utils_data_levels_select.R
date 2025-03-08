#' Data Levels Select Input
#'
#' @description Creates a `selectizeInput` UI component for selecting a data level.
#'
#' @param id A unique module ID used for namespacing in Shiny modules.
#'
#' @return A `selectizeInput` UI element with dynamically populated choices.
#'
#' @details
#' - The dropdown starts empty and is populated dynamically based on authentication.
#' - The available data levels are retrieved using `get_data_levels()`.
#' - The function is meant to be used inside a **Shiny module**.
#'
#' @noRd
data_levels_select <- function(id) {
  ns <- NS(id)
  selectizeInput(
    ns('level'),
    'Data Level',
    choices = NULL,
    options = list(placeholder = 'Select the data level')
  )
}

#' Server Logic for Data Levels Select Module
#'
#' Server-side logic for dynamically updating a data level dropdown based on
#' authentication.
#'
#' @param id A unique module ID used for namespacing in Shiny modules.
#' @param credentials A reactive values list containing authentication credentials.
#'
#' @return A reactive expression containing the selected data level.
#'
#' @details
#' - The function listens for changes in `credentials$auth` to update available data levels.
#' - When authentication is valid, `get_data_levels()` retrieves data levels for the authenticated country.
#' - If the user logs out, the dropdown is reset.
#' - The default selection is set to **level 2** (if available).
#'
#' @noRd
data_levels_select_server <- function(id, credentials) {

  moduleServer(
    id,
    module = function(input, output, session) {

      ns <- session$ns

      data_levels <- reactive({
        req(credentials$auth)

        iso2 <- session$userData$iso2
        get_data_levels(iso2, credentials$auth)
      })

      observe({
        req(data_levels())

        choices <- data_levels()$level
        names(choices) <- data_levels()$name

        updateSelectInput(session, 'level', choices = choices, selected = 2)
      })

      observeEvent(credentials$auth, {
        if (is.null(credentials$auth)) {
          # Clear the selectize input on logout
          invoke_js('resetSelectizeInput', list(id = ns('login')))
        }
      }, ignoreNULL = FALSE)

      return(reactive(
        list(
          selected = input$level,
          items = data_levels()
        )
      ))
    }
  )
}
