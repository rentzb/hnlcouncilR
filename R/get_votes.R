#' Extract Voting Results for Honolulu City Council Measures
#'
#' @param measures A tibble data frame of measures downloaded by `get_council_measures`
#'
#' @returns A tibble data frame in long format with one row per council member per voting event.
#' @export
#'
#' @details
#' This function creates a long tibble data frame with one row per voting event per
#' Honolulu City Council member who participated in the voting event. The output of
#' this function excludes non-voting events such as the introduction of a measure
#' or publishing public hearing notices.
#'
#'
#' @examples
#' measures_df <- get_council_measures(start_measure = 2796, end_measure = 2797)
#' votes_df <- get_votes(measures_df)
get_votes <- function(measures) {
  measures %>%
    tidyr::separate_wider_delim(
      c(AYES, NOES, ABSENT),
      delim = ", ",
      names_sep = "",
      too_few = "align_start"
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("AYES", "NOES", "ABSENT"),ignore.case=FALSE),
      #AYES1:ABSENT4,
      names_to = "vote",
      values_to = "council_member",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(vote = stringr::str_replace(vote, "\\d", "")) %>%
    dplyr::mutate(vote = dplyr::case_when(vote == "AYES" ~ "AYE", vote == "NOES" ~ "NO", .default = vote)) %>%
    dplyr::mutate(
      AYE_WITH_RESERVATION = dplyr::case_when(
        stringr::str_detect(council_member, "\\*") == TRUE ~ "YES",
        vote == "AYE" &
          stringr::str_detect(council_member, "\\*") == FALSE ~ "NO"
      ),
      council_member = stringr::str_remove(council_member, "\\*")
    )
}
