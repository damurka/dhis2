#' Retrieve Data Analytics
#'
#' Fetches analytical data for specified elements within a given date range and
#' organisational level.
#'
#' @param element_ids Character vector. A list of data element IDs to retrieve
#'   analytics for.
#' @param start_date Date or Character. The start date for the data query
#'   (format: "YYYY-MM-DD").
#' @param end_date Date or Character. The end date for the data query
#'   (format: "YYYY-MM-DD").
#' @param level Integer. The hierarchical level of the organisation.
#' @param country_iso2 description
#' @param is_population Is this is population data
#' @param auth Authentication object. Credentials required for API access.
#'
#' @return A data frame containing population analytics data, structured with
#'   years as columns and values for each data element.
#'
#' @noRd
get_data_analytics_ <- function(element_ids,
                                start_date,
                                end_date,
                                level,
                                is_population = FALSE,
                                orgs,
                                data_els,
                                auth) {

  pop_periods <- if (is_population) {
    format(seq(ymd(start_date), ymd(end_date), by = 'year'), '%Y')
  } else {
    format(seq(ymd(start_date), ymd(end_date), by = 'month'), '%Y%m')
  }

  # Limit orgs to only those relevant to our data
  ou_count <- nrow(orgs)
  pe_count <- length(pop_periods)

  # Adjust batch size dynamically based on orgs & periods
  base_batch_size <- 50  # Default batch size for small requests
  max_requests <- 5000   # Avoid hitting API limits

  effective_element_batch_size <- min(base_batch_size, max(4, floor(max_requests / (ou_count * pe_count))))
  effective_pe_batch_size <- min(10, max(1, floor(max_requests / (ou_count * length(element_ids)))))

  # Split `element_ids` dynamically
  element_batches <- split(element_ids, ceiling(seq_along(element_ids) / effective_element_batch_size))

  # Split `pe` dynamically
  pe_batches <- split(pop_periods, ceiling(seq_along(pop_periods) / effective_pe_batch_size))

  # Create batch combinations using `tidyr::expand_grid()`
  batch_combinations <- expand_grid(element_batch = element_batches, pe_batch = pe_batches)

  data <- map2(batch_combinations$element_batch, batch_combinations$pe_batch, ~ {
    print(.x)
    print(.y)
    get_analytics(
      dx %.d% paste(.x, collapse = ";"),
      pe %.d% paste(.y, collapse = ";"),
      ou %.d% paste0('LEVEL-', level),
      co %.d% 'all',
      auth = auth
    )
  }) %>%
    bind_rows() %>%
    distinct()

  data_els <- data_els %>%
    drop_na(element_id)

  data <- if (is_population) {
    data %>%
      filter(dx %in% data_els$element_id) %>%
      left_join(data_els, join_by(dx == element_id))
  } else {
    data %>%
      filter(dx %in% data_els$element_id, co %in% data_els$category_id) %>%
      left_join(data_els, join_by(dx == element_id, co == category_id))
  }

  data %>%
    left_join(orgs, join_by(ou == id)) %>%
    mutate(
      pe = if (is_population) as.integer(pe) else ym(pe),
      year = if (is_population) pe else year(pe),
      month = if (is_population) NA else month(pe, label = TRUE, abbr = FALSE)
    ) %>%
    select(-dx, -ou, -pe, -co)
}
