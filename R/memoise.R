get_cached_data <- function(cached_fn, ..., auth) {

  key_args = splice(dots_list(..., .homonyms = "first", .ignore_empty = "trailing"))

  existing_cache <- do.call(has_cache(cached_fn), c(key_args, list(auth = auth)))

  if (existing_cache) {
    print('Reading cache')
  } else {
    print('Making new cache.')
  }

  req <- do.call(cached_fn, c(key_args, list(auth = auth)))

  cache_result <- tryCatch({
    not_null(req) && nrow(req) > 0
  }, error = function(ex) {
    print(ex)
    warning('Error in cache function', call. = FALSE)
    FALSE
  })

  if (!cache_result) {
    print('Forgetting cache')
    forget(cached_fn)
  } else {
    print('Successfully created a cache.')
  }

  req
}

get_cached_data_elements <- function(country_iso, auth) {
  get_cached_data(cached_fn = get_data_elements_,
                  country_iso = country_iso,
                  auth = auth)
}

get_cached_org_units <- function(level, auth) {
  get_cached_data(cached_fn = get_organisations,
                  level = level,
                  auth = auth)
}
