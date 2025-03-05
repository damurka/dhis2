box::use(
  shiny[div, h2, img, moduleServer, NS, observeEvent, tags, uiOutput, updateTextInput],
  bslib[page_fillable],
  khisr[khis_cred, khis_has_cred, khis_cred_clear]
)

box::use(
  app/logic/tailwind[...],
  app/logic/data_extraction[get_base_url, get_supported_countries],
)

countries <- get_supported_countries()

#' @export
ui <- function(id, condition) {
  ns <- NS(id)

  page_fillable(
    tags$script(src = "https://cdn.tailwindcss.com"),
    div(class = "flex min-h-full flex-col justify-center px-6 py-12 lg:px-8",
        # logo,
        div(class = "sm:mx-auto sm:w-full sm:max-w-sm",
            tags$img(
              class="mx-auto h-auto w-4/5",
              src = "static/img/logo.svg", alt = "CD2030 Data Extractor Logo"
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
                 tailSelectInput(ns('country'), 'Country', countries),
                 tailActionButton(ns('login'), 'Sign in', sprintf("App.onLogin(this, '%s')", ns('login')))
            )
        )
    )
  )
}

#' @export
server <- function(id, credentials) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      observeEvent(input$login, {
        use_manual <- input[[paste0("country-manual-toggle")]]
        manual_url <- input[[paste0("country-text")]]
        country_input <- input[[paste0("country-select")]]
        dhis2_url <- if (use_manual) manual_url else get_base_url(country_input)$dhis2_url

        country <- if (!use_manual) get_base_url(country_input) else NULL

        creds <- tryCatch(
          khis_cred(username = input$username, password = input$password, server = dhis2_url),
          error = function(e) e
        )

        if (inherits(creds, 'error')) {
          output$statusMessage <- renderTailText(creds$message, TRUE)
          session$sendCustomMessage("loginStatus", list(status = "error"))
          return(invisible(FALSE))
        }

        session$userData$iso3 <- if (!is.null(country)) country$iso3
        session$userData$country <- if (!is.null(country)) country$country else dhis2_url

        if (khis_has_cred(creds)) {
          credentials$auth <- creds$clone()
          updateTextInput(session, "password", value = "")
          output$statusMessage <- renderTailText('')
          session$sendCustomMessage("loginStatus", list(status = "success"))
        } else {
          output$statusMessage <- renderTailText("Login failed. Check your credentials.", TRUE)
          session$sendCustomMessage("loginStatus", list(status = "error"))
        }

        khis_cred_clear()
      })

    }
  )
}
