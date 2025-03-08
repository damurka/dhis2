#' login_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_login_page_ui <- function(id, supported_countries) {
  ns <- NS(id)
  page_fillable(
    tags$script(src = "https://cdn.tailwindcss.com"),
    div(class = "flex min-h-full flex-col justify-center px-6 py-12 lg:px-8",
        # logo,
        div(class = "sm:mx-auto sm:w-full sm:max-w-sm",
            tags$img(
              class="mx-auto h-auto w-4/5",
              src = "/www/img/logo.svg", alt = "CD2030 Data Extractor Logo"
            ),
            tags$h2(
              "Enter your DHIS2 account",
              class="mt-10 text-center text-2xl font-bold leading-9 tracking-tight text-gray-900"
            )
        ),
        div(class = "mt-10 sm:mx-auto sm:w-full sm:max-w-sm",
            tags$form(class = "space-y-6",
                      uiOutput(ns("statusMessage")),
                      tailTextInput(ns('username'), 'DHIS2 username'),
                      tailTextInput(ns('password'), 'DHIS2 password', 'password'),
                      tailwind_select(ns('country'), 'Country', supported_countries),
                      tailActionButton(ns('login'), 'Sign in', sprintf("onLogin(this, '%s')", ns('login')))
            )
        )
    )
  )
}

#' login_page Server Functions
#'
#' @noRd
mod_login_page_server <- function(id, credentials){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    country <- tailwind_select_server('country')

    dhis2_url <- reactive({
      req(country())
      country()$url
    })

    observeEvent(input$login, {
      req(dhis2_url())

      if (is.null(input$username) || nchar(input$username) == 0) {
        output$statusMessage <- renderTailText('Username is missing.', TRUE)
        invoke_js('loginStatus', list(id = ns('login')))
        return(invisible(FALSE))
      }

      if (is.null(input$password) || nchar(input$password) == 0) {
        output$statusMessage <- renderTailText('Password is missing.', TRUE)
        invoke_js('loginStatus', list(id = ns('login')))
        return(invisible(FALSE))
      }

      creds <- tryCatch(
        khis_cred(username = input$username, password = input$password, server = dhis2_url()),
        error = function(e) e
      )

      if (inherits(creds, 'error')) {
        output$statusMessage <- renderTailText(creds$message, TRUE)
        invoke_js('loginStatus', list(id = ns('login')))
        return(invisible(FALSE))
      }

      session$userData$iso2 <- if (not_null(country()$country)) country()$country$iso2 else NULL
      session$userData$country <- if (not_null(country()$country)) country()$country$country else dhis2_url()

      if (khis_has_cred(creds)) {
        credentials$auth <- creds$clone()
        updateTextInput(session, "password", value = "")
        output$statusMessage <- renderTailText('')
        invoke_js('loginStatus', list(id = ns('login')))
      } else {
        output$statusMessage <- renderTailText("Login failed. Check your credentials.", TRUE)
        invoke_js('loginStatus', list(id = ns('login')))
      }

      khis_cred_clear()
    })
  })
}
