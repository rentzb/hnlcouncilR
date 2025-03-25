#' Create Network Graph of Cointroductions of Honolulu City Council Measures
#'
#' @param measures_df The measures tibble data frame created by `get_council_measures`
#' @param type The type of measure to selection. Options are "all", "bill", or "resolution".
#'
#' @returns igraph network plot
#' @export
#'
#' @details
#' This function creates a network plot using igraph that visualizes how often
#' Honolulu City Council members introduce measures together. Each line in the
#' graph indicates one measure that the members introduced together. Honolulu
#' City Council measures can only be introduced by a max of two members, though
#' many measures only have one introducer. Measures that were only introduced by
#' one member are not shown in the graph.
#'
#' @examples
#' measures_df <- get_council_measures(start_measure = 2796, end_measure = 2797)
#' cointro_network_graph(measures_df, type = "all")
cointro_network_graph <- function(measures_df, type = "all") {
  measures_df %>%
    dplyr::select(
      measure_number,
      measure_type,
      measure_introducer1,
      measure_introducer2,
      introduced_by_request
    ) %>%
    # keep last names only and if only 1 introducers copy to 2nd
    dplyr::mutate(
      measure_introducer1 = stringr::word(measure_introducer1, -1),
      measure_introducer2 = dplyr::case_when(
        is.na(measure_introducer2) == TRUE ~ measure_introducer1,
        .default = stringr::word(measure_introducer2, -1)),
        # correct Dos Santos-Tam
        dplyr::across(measure_introducer1:measure_introducer2, ~ stringr::str_replace(.x, "SANTOS-TAM","DOS SANTOS-TAM"))
      ) %>%
    # remove duplicates
    unique() %>% dplyr::filter(
      dplyr::case_when(
        type == "all" ~ measure_type == "resolution" |
          measure_type == "bill",
        .default = measure_type == type
      )
    ) %>%
    dplyr::select(measure_introducer1, measure_introducer2) %>%
    igraph::graph_from_data_frame(directed = FALSE) %>%
    igraph::simplify(remove.loops = TRUE, remove.multiple = FALSE) %>%
    plot(
      vertex.label.dist = 3.5,
      vertex.size = 10,
      vertex.label.cex = 0.5,
      main = glue::glue(
        "{ dplyr::case_when(
                         type == 'all' ~ paste0(stringr::str_to_title(type),' Measures'),
                         .default =  paste0(stringr::str_to_title(type),'s'))}"
      )
    )
}
