box::use(
  shiny[conditionalPanel, moduleServer, NS, observe, outputOptions, reactive, reactiveValues, tags],
  khisr[khis_has_cred],
  bslib[page_fillable, bs_theme],
)

box::use(
  app/view/login,
  app/view/data
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    theme = bs_theme(version = 5),
    tags$script(
      src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
    ),
    conditionalPanel(
      ns = ns,
      condition = "output.loggedIn === false",
      style = "display: none;",
      login$ui(ns('login'))
    ),
    conditionalPanel(
      ns = ns,
      condition = "output.loggedIn === true",
      style = "display: none;",
      class = 'bslib-page-sidebar html-fill-container',
      data$ui(ns('data'))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    credentials <- reactiveValues(auth = NULL)

    output$loggedIn <- reactive({
      !is.null(credentials$auth) && khis_has_cred(credentials$auth)
    })

    outputOptions(output, "loggedIn", suspendWhenHidden = FALSE)

    login$server('login', credentials)
    data$server('data', credentials)
  })
}
