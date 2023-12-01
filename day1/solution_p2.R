library(dplyr)

input <- readr::read_lines(
  "day1/input"
)

number_strs <- c(
  "one" = "1",
  "two" = "2",
  "three" = "3",
  "four" = "4",
  "five" = "5",
  "six" = "6",
  "seven" = "7",
  "eight" = "8",
  "nine" = "9"
)

to_match <- c(
  names(number_strs),
  number_strs
)

# This is realllllly ugly
line_digits <- purrr::map(
  input,
  function(input_line){
    
    positions <- purrr::map_df(
      to_match,
      ~stringr::str_locate_all(
        input_line,
        .x
      ) |> 
        as.data.frame()
    ) |> 
      arrange(
        start
      )
    
    c(
      stringr::str_sub(
        input_line,
        positions$start[[1]],
        positions$end[[1]]
      ),
      stringr::str_sub(
        input_line,
        positions$start[nrow(positions)],
        positions$end[nrow(positions)]
      )
    )
  }
) |>
  purrr::map(
    stringr::str_replace_all,
    number_strs
  ) |>
  purrr::map_dbl(
    ~ as.numeric(
      paste0(.x[1], .x[length(.x)])
    )
  )

solution <- sum(
  line_digits
)

message("Answer is: ", solution)