#' Tailwind-Styled Select Input with Manual Entry Toggle
#'
#' Creates a `select` UI component styled with Tailwind CSS, allowing users to
#' choose a predefined DHIS2 country URL or manually enter one.
#'
#' @param id A unique module ID used for namespacing in Shiny modules.
#' @param label A character string specifying the label for the select input.
#' @param choices A named vector where names represent country with DHIS2 url
#'
#' @return A `div` container containing a Tailwind-styled select input with:
#'   - A dropdown menu populated with `choices`.
#'   - A checkbox to toggle between selection and manual entry.
#'   - A text input field that appears when manual entry is enabled.
#'   - A display section showing the selected or manually entered value.
#'
#' @details
#' - The function generates a UI component where users can either select from a dropdown
#'   or manually enter a URL.
#' - When toggled to manual entry, the dropdown is hidden, and a text input appears.
#' - The selected value is sent to Shiny via `Shiny.setInputValue()`.
#'
#' @noRd
tailwind_select <- function(id, label, choices) {
  ns <- NS(id)

  div(
    div(
      class = 'mt-2 flex items-center justify-between',
      div(
        class = 'flex items-start',
        tailInputLabel(ns('select'), label),
      ),
      div(
        class = 'flex items-end',
        div(
          class = 'flex items-center h-5',
          tags$input(
            type = 'checkbox',
            id = ns("manual_toggle"),
            class = 'w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-3 focus:ring-primary-300 dark:bg-gray-700 dark:border-gray-600 dark:focus:ring-primary-600 dark:ring-offset-gray-800',
            onchange = sprintf("
              let isChecked = this.checked;
              document.getElementById('%s').style.display = isChecked ? 'none' : 'block';
              document.getElementById('%s').style.display = isChecked ? 'block' : 'none';
              document.getElementById('%s-label').innerText = isChecked ? 'Enter DHIS2 Base Url' : '%s';
              let urlText = isChecked ? document.getElementById('%s').value : document.getElementById('%s').value;
              Shiny.setInputValue('%s', urlText);
            ", ns("select"), ns("text_container"), ns("select"), label, ns("text"), ns("select"), ns("selected_url"))
          )
        ),
        div(
          class = 'ml-3 text-sm',
          tags$label("Enter URL manually", `for` = ns("manual_toggle"), class = "text-gray-500")
        )
      )
    ),
    div(
      class = 'mt-2',
      tags$select(
        id = ns("select"),
        class = 'block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 focus:ring-green-500 focus:border-green-500 p-2.5',
        onchange = sprintf("
          document.getElementById('%s-text').value = this.value;
          Shiny.setInputValue('%s', this.value);",
          ns('text'), ns("text_display")),
        lapply(names(choices), function(name) {
          tags$option(value = choices[[name]], name)
        })
      ),
    ),
    div(
      id = ns("text_container"),
      class = "mt-2 hidden",
      tailTextInput(
        inputId = ns("text"),
        label = NULL # No need for duplicate label
      )
    ),
    div(
      class = "mt-2 text-sm text-gray-500",
      textOutput(ns("text_display"))
    )
  )
}

#' Server Logic for Tailwind Select Module
#'
#' Manages the backend logic for the `tailwind_select` UI component. Listens for
#' user selection changes and updates the corresponding value.
#'
#' @param id A unique module ID used for namespacing in Shiny modules.
#'
#' @return A reactive expression returning:
#'   - The selected value if an option is chosen from the dropdown.
#'   - The manually entered URL if the user toggles manual entry.
#'
#' @details
#' - If a selection is made from the dropdown, the corresponding value is returned.
#' - If the manual input is enabled, the entered text is returned instead.
#' - The selected value is displayed dynamically in the UI.
#'
#' @noRd
tailwind_select_server <- function(id) {

  moduleServer(
    id = id,
    module = function(input, output, session) {

      country <- reactive({
        req(input$select)
        if (isTRUE(input$manual_toggle)) return(NULL)
        get_country(input$select)
      })

      selected_url <- reactive({
        if (isTRUE(input$manual_toggle)) {
          input$text
        } else {
          country()$dhis2_url
        }
      })

      # Display the selected URL dynamically
      output$text_display <- renderText({
        req(selected_url())
        selected_url()
      })

      return(
        reactive({list(
            country = country(),
            url = selected_url()
          )
        })
      )
    }
  )
}
