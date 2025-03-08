#' tailwind_ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
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
