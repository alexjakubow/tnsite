# OBJECT ------------------------------------------------------------------------
# Goal: Create a single table of OSF objects and their types

# Define object types using a list
#' @export
TYPES <- list(
  preprint = "P",
  registration = "NR",
  project = "NP",
  component = "NC",
  user = "U",
  draft = "D"
)


#' Assign Node Type
#'
#' @param tbl A tibble or zoomed table (`dm`)
#' @export
assign_node_type <- function(tbl) {
  tbl |>
    dplyr::mutate(
      type = dplyr::case_when(
        type == "osf.node" & root_id == id ~ TYPES$project,
        type == "osf.node" & root_id != id ~ TYPES$component,
        type == "osf.draftnode" ~ TYPES$draft,
        type == "osf.registration" ~ TYPES$registration,
        .default = NA
      )
    ) |>
    dplyr::rename(object = type)
}


# LOGS -------------------------------------------------------------------------
#' @export
logged_actions_at_date <- function(tbl, date, ...) {
  tbl |>
    dplyr::filter(created <= date) |>
    dplyr::group_by(...) |>
    dplyr::count(action, .drop = FALSE)
}

#' @export
last_logged_actions <- function(log_tbl, date, ...) {
  log_tbl |>
    dplyr::filter(created <= date) |>
    dplyr::group_by(...) |>
    dplyr::arrange(dplyr::desc(created)) |>
    dplyr::slice(1)
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
      artifact_created <= date,
      artifact_deleted > date | is.na(artifact_deleted),
      artifact_finalized == TRUE
    ) |>
    dplyr::group_by(...) |>
    dplyr::arrange(dplyr::desc(artifact_created)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}


# CRITERIA ----------------------------------------------------------------

# 1. Existential ("living")

#' Return living OSF objects at a given date
#'
#' This function filters a table for OSF objects that are alive (i.e., have
#' been created and not deleted) at a given date.
#'
#' @param tbl An input lifespan table with `born` and `died` columns.
#' @param date A date in POSIXct format
#' @param summary Logical. Should a table of summary counts be returned.
#' @param ... Additional columns to group by for summary
#'
#' @return A tibble
#' @export
living_at_date <- function(tbl, date, summary = FALSE, ...) {
  tbl <- tbl |>
    dplyr::filter(born < date & (died >= date | is.na(died)))
}


# 2. Visibility ("public vs. private")
#' Public actions
#' @export
PUBLIC_ACTIONS <- c(
  "made_public",
  "embargo_cancelled",
  "embargo_terminated",
  "embargo_completed",
  "published"
)

#' Private actions
#' @export
PRIVATE_ACTIONS <- c(
  "made_private",
  "embargo_approved"
)


#' Determine the visibility status of an OSF object at a given date
visibility_status <- function(x) {
  dplyr::case_when(
    x %in% PRIVATE_ACTIONS ~ "private",
    x %in% PUBLIC_ACTIONS ~ "public"
  )
}


#' @export
visible_at_date <- function(tbl, date, ...) {
  living_tbl <- living_at_date(tbl, date) |>
    dplyr::select(c(id, table, ...))

  logged_tbl <- last_logged_action(tbl, date, id, table) |>
    dplyr::select(id, table, action)

  # Has to be a non-deleted object, so left join on living table
  living_tbl <- living_tbl |>
    dplyr::left_join(logged_tbl, by = c("id", "table")) |>
    dplyr::mutate(
      visibility = ifelse(
        is.na(action),
        "private",
        visibility_status(action)
      )
    )
}


# Projects or registrations
NODE_PUBLIC_ACTIONS <- c(
  "made_public",
  "embargo_cancelled",
  "embargo_terminated",
  "embargo_completed"
)
NODE_PRIVATE_ACTIONS <- c("made_private", "embargo_approved")
PREPRINT_PUBLIC_ACTIONS <- c("published", "made_public")
PREPRINT_PRIVATE_ACTIONS <- c("made_private")

PREPRINT_PUBLIC_STATES <- c("accepted", "pending") # but "pending" only applies if post-moderation model
PREPRINT_PRIVATE_STATES <- c("withdrawn", "deleted")
