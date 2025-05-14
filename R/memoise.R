get_cached_data <- function(cached_fn, ..., auth) {

  key_args = dots_list(..., .homonyms = "first", .ignore_empty = "trailing")

  existing_cache <- exec(has_cache(cached_fn), !!!key_args, auth = auth)

  if (existing_cache) {
    print('Reading cache')
  } else {
    print('Making new cache.')
  }

  req <- exec(cached_fn, !!!key_args, auth = auth)

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

get_cached_org_units <- function(country_iso, level, auth) {
  check_required(country_iso)
  check_required(auth)
  if (!is_scalar_integerish(level)) {
    abort(c('x' = 'level must be an integer'))
  }
  get_cached_data(cached_fn = get_organisations,
                  country_iso = country_iso,
                  level = level,
                  auth = auth)
}
