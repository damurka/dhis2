#' login_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_login_page_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    tags$script(src = "https://cdn.tailwindcss.com"),
    div(class = "flex min-h-full flex-col justify-center px-6 py-12 lg:px-8",
        # logo,
        div(class = "sm:mx-auto sm:w-full sm:max-w-sm",
            tags$img(
              class="mx-auto h-auto w-4/5",
              src = "/r/img/logo.svg", alt = "CD2030 Data Extractor Logo"
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
                      tailSelectInput(ns('country'), 'Country', c('Kenya')),
                      uiOutput(ns('select_url')),
                      tailActionButton(ns('login'), 'Sign in', sprintf("App.onLogin(this, '%s')", ns('login')))
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

  })
}
