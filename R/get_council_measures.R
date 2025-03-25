#' Download Information About Honolulu City Council Measures
#'
#' @description
#' `get_council_measures` returns a data frame with information about all
#' measures that occur within the specified range of measures.
#'
#' @param start_measure The numeric value of the starting measure.
#' @param end_measure The numeric value of the ending measure
#'
#' @returns A tibble data frame with information about each measure.
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @details
#' This function downloads information from the Honolulu City Council's website
#' about all measures that occur within the range from the start_measure to the
#' end_measure. Those values must be numeric. If only a single measure is wanted,
#' then set the start_measure and end_measure to the same value. The numeric
#' values of the measures differ from the measures' names. For example, RES25-001
#' has a numeric value of 3163, since that is the number given in its url:
#' https://hnldoc.ehawaii.gov/hnldoc/measure/3163. The values appear to occur in
#' sequential order, though there are some occasional exceptions.
#'
#' @examples
#' measures_df <- get_council_measures(start_measure = 2796, end_measure = 2797)
#'
get_council_measures <- function(start_measure = 2796,
                                 end_measure = 3162) {
  suppressWarnings({measures_df <- data.frame()
  for (measure in start_measure:end_measure) {
    # get the page
    page <- rvest::read_html(glue::glue(
      "https://hnldoc.ehawaii.gov/hnldoc/measure/browse/{measure}"
    ))
    # extract metadata
    name <- page %>%
      rvest::html_node("dd") %>%
      rvest::html_text2()
    if (is.na(name) == TRUE)
      next
    introducer <- page %>%
      rvest::html_elements("dd") %>%
      rvest::html_text2() %>%
      tibble::tibble() %>%
      dplyr::slice(3) %>%
      unlist %>%
      unname()
    number <- page %>%
      rvest::html_node("#publicMeasure") %>%
      rvest::html_elements("li") %>%
      rvest::html_text() %>%
      tibble::tibble(sitetext = .) %>%
      dplyr::filter(stringr::str_detect(sitetext, "BILL") == TRUE |
               stringr::str_detect(sitetext, "RES") == TRUE) %>%
      dplyr::slice(1) %>%
      unlist() %>%
      unname()
    type <- page %>%
      rvest::html_elements("h3") %>%
      rvest::html_element("span") %>%
      rvest::html_text() %>%
      tibble::tibble() %>%
      tidyr::drop_na() %>%
      unlist() %>%
      unname()
    # extract table of actions
    actions <- page %>%
      rvest::html_node("table") %>%
      rvest::html_table(trim = TRUE) %>%
      dplyr::select(Date, Type, Description) %>%
      dplyr::mutate(Date = lubridate::mdy(Date)) %>%
      dplyr::mutate(vote_ayes_n = stringr::str_extract(Description, "(\\d) AYE", group = 1)) %>%
      dplyr::mutate(vote_no_n = stringr::str_extract(Description, "(\\d) NO", group = 1)) %>%
      dplyr::mutate(absent_n = stringr::str_extract(Description, "(\\d) ABSENT", group = 1)) %>%
      tidyr::separate(
        Description,
        into = c("Description", "AYES"),
        remove = F,
        sep = "\\d AYES:"
      ) %>%
      tidyr::separate(
        AYES,
        into = c("AYES", "NOES"),
        remove = F,
        sep = "\\d NOES:"
      ) %>%
      tidyr::separate(
        AYES,
        into = c("AYES", "NO"),
        remove = F,
        sep = "\\d NO:"
      ) %>%
      tidyr::separate(
        NOES,
        into = c("NOES", "ABSENT"),
        remove = F,
        sep = "\\d ABSENT:"
      ) %>%
      tidyr::separate(
        AYES,
        into = c("AYES", "ABSENT2"),
        remove = F,
        sep = "\\d ABSENT:"
      ) %>%
      tidyr::separate(
        NO,
        into = c("NO", "ABSENT3"),
        remove = F,
        sep = "\\d ABSENT:"
      ) %>%
      dplyr::mutate(ABSENT = dplyr::case_when(is.na(ABSENT) == T ~ ABSENT2, .default = ABSENT)) %>%
      dplyr::mutate(ABSENT = dplyr::case_when(is.na(ABSENT) == T ~ ABSENT3, .default = ABSENT)) %>%
      dplyr::mutate(NOES = dplyr::case_when(is.na(NOES) == T ~ NO, .default = NOES)) %>%
      dplyr::select(-ABSENT2, -NO, -ABSENT3) %>%
      dplyr::mutate(across(vote_ayes_n:absent_n, ~ as.numeric(.x))) %>%
      dplyr::mutate(across(vote_ayes_n:absent_n, ~ tidyr::replace_na(.x, 0))) %>%
      dplyr::mutate(
        type = type,
        name = name,
        introducer = introducer,
        number = number
      ) %>%
      dplyr::mutate(dplyr::across(c(Description:ABSENT, type:number), ~ stringr::str_squish(.x)))
    # save output
    measures_df <- rbind(measures_df, actions)
    measures_df_final <- measures_df %>%
      dplyr::select(number,name,type,introducer,Date,Type:absent_n) %>%
      dplyr::rename(event_type = Type,
                    event_description = Description,
                    measure_type = type,
                    measure_name = name,
                    measure_number = number,
                    measure_introducer = introducer,
                    event_date = Date) %>%
      dplyr::mutate(
        introduced_by_request = dplyr::case_when(stringr::str_detect(measure_introducer, "By Request") == TRUE ~ "YES", .default = "NO"),
        measure_introducer = stringr::str_remove(measure_introducer, " - By Request")
      ) %>%
      tidyr::separate_wider_delim(cols = measure_introducer, delim = ", ",
                           names = c("measure_introducer1","measure_introducer2"),
                           too_few = "align_start")
    # print(measure)
  }})
  return(measures_df_final)
}
