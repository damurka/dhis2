#' tailwind_ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
tailSelectInput <- function(inputId, label, choices) {
  div(
    div(
      class = 'mt-2 flex items-center justify-between',
      div(
        class = 'flex items-start',
        tailInputLabel(inputId, label),
      ),
      div(
        class = 'flex items-end',
        div(
          class = 'flex items-center h-5',
          tags$input(
            type = 'checkbox',
            id = paste0(inputId, "-manual-toggle"),
            class = 'w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-3 focus:ring-primary-300 dark:bg-gray-700 dark:border-gray-600 dark:focus:ring-primary-600 dark:ring-offset-gray-800',
            onchange = sprintf("
            let isChecked = this.checked;
            document.getElementById('%s-select').style.display = isChecked ? 'none' : 'block';
            document.getElementById('%s-text-container').style.display = isChecked ? 'block' : 'none';
            document.getElementById('%s-label').innerText = isChecked ? 'Enter DHIS2 Base Url' : '%s';
          ", inputId, inputId, inputId, label)
          )
        ),
        div(
          class = 'ml-3 text-sm',
          tags$label("Enter URL manually", `for` = paste0(inputId, "-manual-toggle"), class = "text-gray-500")
        )
      )
    ),
    div(
      class = 'mt-2',
      tags$select(
        id = paste0(inputId, "-select"),
        class = 'block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 focus:ring-green-500 focus:border-green-500 p-2.5',
        onchange = sprintf("document.getElementById('%s-text').value = this.value;", inputId),
        lapply(names(choices), function(name) {
          tags$option(value = choices[[name]], name)
        })
      ),
    ),
    div(
      id = paste0(inputId, "-text-container"),
      class = "mt-2 hidden",
      tailTextInput(
        inputId = paste0(inputId, "-text"),
        label = NULL # No need for duplicate label
      )
    )
  )
}

#' @noRd
tailTextInput <- function(inputId, label, type='text') {
  div(
    tailInputLabel(inputId, label),
    div(class = 'mt-2',
        tags$input(id = inputId, type = type,
                   class = 'block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-green-600 sm:text-sm p-2.5')
    )
  )
}

#' @noRd
tailInputLabel <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = 'block text-sm font-medium leading-6 text-gray-900',
    id = paste0(inputId, '-label'),
    `for` = inputId
  )
}

#' @noRd
tailActionButton <- function(inputId, label, onclick) {

  value <- restoreInput(id = inputId, default = NULL)

  div(
    tags$button(id = inputId,
                type = 'button',
                class = 'action-button flex w-full justify-center rounded-md bg-green-600 px-5 py-2.5 text-sm font-semibold leading-6 text-white text-center shadow-sm hover:bg-green-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-green-600 inline-flex items-center',
                `data-val` = value,
                onclick = onclick,
                label
    )
  )
}

#' @noRd
renderTailText <- function(text, isError = FALSE) {
  renderUI({
    class <- 'block text-sm font-medium leading-6 '
    if (isError) {
      class <- paste0(class, 'text-danger')
    } else {
      class <- paste0(class, 'text-success')
    }

    div(class = class, text)
  })
}
