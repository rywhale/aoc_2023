input <- readr::read_table(
  "day6/input",
  col_names = FALSE
) |> 
  tidyr::pivot_longer(
    cols = X2:X5,
    names_to = "Race",
    values_to = "Value"
  ) |>
  tidyr::pivot_wider(
    names_from = X1,
    values_from = Value
  ) |>
  dplyr::mutate(
    Race = dplyr::row_number()
  )

names(input) <- c("Race", "Time", "Distance")

count_options <- function(time, record_distance) {
  all_options <- purrr::map_dbl(
    1:time,
    ~ {
      hold_length <- .x
      time_to_move <- time - hold_length
      distance_travel <- hold_length * time_to_move

      if (distance_travel > record_distance) {
        hold_length
      } else {
        NA
      }
    }
  )

  length(all_options[!is.na(all_options)])
}

final_pt1 <- input |>
  dplyr::rowwise() |>
  dplyr::mutate(
    winning_option_count = count_options(Time, Distance)
  )

message("Part 1 solution is: ", prod(final_pt1$winning_option_count))

final_pt2 <- tibble::tibble(
  Time = as.numeric(paste(input$Time, collapse = "")),
  Distance = as.numeric(paste(input$Distance, collapse = "")),
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    winning_option_count = count_options(Time, Distance)
  )

message("Part 2 solution is: ", prod(final_pt2$winning_option_count))