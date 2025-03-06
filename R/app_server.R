#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {

  credentials <- rv(auth = NULL)

  output$loggedIn <- reactive({
    not_null(credentials$auth) && khis_has_cred(credentials$auth)
  })

  outputOptions(output, "loggedIn", suspendWhenHidden = FALSE)

  mod_login_page_server("login_page_1", credentials)
  mod_data_page_server("data_page_1", credentials)
}
