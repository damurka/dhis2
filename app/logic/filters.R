box::use(
  dplyr[any_of, distinct, filter, pull, select, `%>%`],
  rlang[sym, `!!`]
)

#' @export
filter_organisation <- function(.data, selected_label, previous_label, previous_selected_value) {

  data <- .data
  if (!is.null(previous_selected_value) && previous_selected_value != '0') {
    data <- data %>%
      filter(!!sym(previous_label) == previous_selected_value)
  }

  choices <- data %>%
    select(any_of(selected_label)) %>%
    distinct(across(any_of(selected_label))) %>%
    pull(any_of(selected_label))

  names(choices) <- choices

  c('Select' = '0', choices)
}

#' @export
get_previous_label <- function(.data_levels, label) {

  # Find the level of the given name
  current_level <- .data_levels %>%
    filter(input_ids == !!label) %>%
    pull(level)

  # Check if the name exists in the dataframe
  if(length(current_level) == 0) {
    return(NULL)
  }

  # Determine the previous level
  previous_level <- current_level - 1

  # Find the name associated with the previous level
  previous_label <- .data_levels %>%
    filter(level == previous_level) %>%
    pull(input_ids)

  # Return the previous name or a message if it's the first level
  if(length(previous_label) == 0) {
    return(NULL)
  } else {
    return(previous_label)
  }
}
