#' Last logged action query
#'
#' @param tbl Database table connection to query (duckdb)
#' @param date_col Date column name
#' @param status_col Status column name
#' @param date Query date
#' @param ... Column names to group by
#' @export
last_logged_action <- function(tbl, date_col, status_col, date, ...) {
  tbl |>
    dplyr::filter({{ date_col }} <= date) |>
    dbplyr::window_order({{ date_col }}) |>
    dplyr::summarise(
      .by = c(...),
      status = last({{ status_col }})
    )
}


#' Filters a the registration_badges table for finalized artifacts that were:
#' 1. Created on/before the specified date
#' 2. Not deleted or deleted after the specified date.
#'
#' @param tbl Registration badges table
#' @param date Query date
#' @param ... Column names to group by
#'
#' @export
reg_badge_status <- function(tbl, date, ...) {
  tbl |>
    dplyr::filter(
      created <= date,
      deleted > date | is.na(deleted),
      finalized == TRUE
    ) |>
    dplyr::summarise(
      .by = c(...),
      n_artifacts = n()
    )
}

#' @export
reg_recipe_status <- function(tbl) {
  tbl |>
    dplyr::summarise(
      .by = node_id,
      n_outcomes = sum(
        artifact_type %in% c("data", "materials", "code", "supplements"),
        na.rm = TRUE
      ),
      n_outputs = sum(artifact_type == "papers", na.rm = TRUE)
    )
}
