add_n_to_chapter_structure <- function(chapter_structure,
                                       data,
                                       variable_name = ".n") {
  target_grouping_variables <- c(".variable_name_dep", ".variable_name_indep")
  out <-
    chapter_structure |>
    dplyr::grouped_df(vars = target_grouping_variables) |>
    dplyr::group_map(.keep = TRUE, .f = ~ {
      target_grouping_variables_tmp <-
        lapply(target_grouping_variables, function(chapter_structure_col) {
          x <- as.character(unique(.x[[chapter_structure_col]]))
          if (length(x) == 0 || is.na(x)) {
            return(NULL)
          } else {
            return(chapter_structure_col)
          }
        }) |>
        unlist()

      if (!is.null(target_grouping_variables_tmp)) {
        .y[] <- lapply(.y, as.character)
        cols <- as.character(.y[, target_grouping_variables_tmp])
        .x[[variable_name]] <-
          dplyr::filter(
            data,
            dplyr::if_all(
              tidyselect::all_of(cols),
              ~ !is.na(.x)
            )
          ) |>
          nrow()
      } else {
        .x[[variable_name]] <- 0
      }
      .x
    })

  if (length(out) > 0) {
    out |>
      dplyr::bind_rows() |>
      dplyr::grouped_df(vars = dplyr::group_vars(chapter_structure))
  } else {
    attr(out, "ptype")
  }
}
