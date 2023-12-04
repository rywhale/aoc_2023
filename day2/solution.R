input <- readr::read_lines(
  "day2/input"
)

# input <- c(
#   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
#   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
#   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
#   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
#   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
# )

parse_line_val <- function(input_line, calc_part1 = TRUE){
  
  game_id <- stringr::str_extract(
    input_line,
    "(?<=Game )[0-9]+"
  )
  
  col_vals <- purrr::map_vec(
    c("blue", "red", "green"),
    ~stringr::str_extract_all(
      input_line,
      paste0("[0-9]+(?= ", .x, ")")
    )
  )
  
  col_vals <- purrr::map(col_vals, as.numeric)
  
  if(calc_part1){
    if(any(col_vals[[1]] >  14, col_vals[[2]] > 12, col_vals[[3]] > 13)){
      solution <- 0
    }else{
      solution <- as.numeric(game_id)
    }
  }else{
    solution <- max(col_vals[[1]]) * max(col_vals[[2]]) * max(col_vals[[3]])
  }
  
  solution
  
}

part_1_solution <- purrr::map_dbl(
  input,
  parse_line_val
)

message("Part 1 answer is: ", sum(part_1_solution))

part2_solution <- purrr::map_dbl(
  input,
  parse_line_val,
  calc_part1 = FALSE
)

message("Part 2 answer is: ", sum(part2_solution))