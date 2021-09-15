freq = function(data, ..., weights, use_na = c("show", "drop")) {
  vars = rlang::ensyms(...)
  if (length(vars) > 2) rlang::abort("More than 2 variables supplied.")
  if (length(vars) == 0) rlang::abort("No variables supplied.")
  if (missing(weights)) rlang::abort("No weights supplied.")

  sym_weights = rlang::ensym(weights)

  neg_wgt_count = sum(data[[rlang::as_string(sym_weights)]] < 0)
  if (neg_wgt_count > 0) rlang::abort("Cannot use negative weights.")

  if (missing(use_na) || use_na == "drop") {
    data = dplyr::filter(data, !is.na(!!vars[[1]]))
    if (length(vars) == 2) {
      data = dplyr::filter(data, !is.na(!!vars[[2]]))
    }
  }

  data %>%
    dplyr::group_by(!!!vars) %>%
    dplyr::summarise(freq = sum(!!sym_weights, na.rm = TRUE))
}
