input <- readr::read_lines(
  "day3/input"
)

# input <- c(
#   "467..114..",
#   "...*......",
#   "..35..633.",
#   "......#...",
#   "617*......",
#   ".....+.58.",
#   "..592.....",
#   "......755.",
#   "...$.*....",
#   ".664.598.."
# )

symbol_pos <- stringr::str_locate_all(
  input,
  '[^a-zA-Z0-9\\.]'
)

symbol_pos <- purrr::map_df(
  1:length(symbol_pos),
  ~{
    tibble::as_tibble(symbol_pos[[.x]]) |> 
      dplyr::mutate(
        input_line = .x
      )
  }
)

num_pos <- stringr::str_locate_all(
  input,
  "[0-9]+"
)

num_pos <- purrr::map_df(
  1:length(num_pos),
  ~{
    tibble::as_tibble(num_pos[[.x]]) |> 
      dplyr::mutate(
        input_line = .x
      )
  }
)

are_parts <- purrr::map_dbl(
  1:nrow(num_pos),
  ~{
    
    current_num <- num_pos[.x, ]
    char_start <- current_num$start - 1
    char_end <- current_num$end + 1
    
    current_line_sym <- symbol_pos |>
      dplyr::filter(
        input_line %in% c(
          current_num$input_line,
          current_num$input_line - 1,
          current_num$input_line + 1
        ),
        (start %in% char_start:char_end | end %in% char_start:char_end)
      )
    
    if(!nrow(current_line_sym)){
      return(NA)
    }
    
    substr(
      input[current_num$input_line],
      start = current_num$start,
      stop = current_num$end
    ) |> 
      as.numeric()
  }
)

message("Part 1 answer is: ", sum(are_parts, na.rm = TRUE))

gear_pos <- stringr::str_locate_all(
  input,
  "\\*"
)

gear_pos <- purrr::map_df(
  1:length(gear_pos),
  ~{
    tibble::as_tibble(gear_pos[[.x]]) |> 
      dplyr::mutate(
        input_line = .x
      )
  }
)

gear_ratios <- purrr::map_dbl(
  1:nrow(gear_pos),
  ~{
    
    current_gear <- gear_pos[.x, ]
    char_start <- current_gear$start - 1
    char_end <- current_gear$end + 1
    
    current_num <- num_pos |>
      dplyr::filter(
        input_line %in% c(
          current_gear$input_line,
          current_gear$input_line - 1,
          current_gear$input_line + 1
        ),
        (start %in% char_start:char_end | end %in% char_start:char_end)
      )
    
    if(nrow(current_num) != 2){
      return(NA)
    }
    
    first_num <- substr(
      input[current_num$input_line[[1]]],
      start = current_num$start[[1]],
      stop = current_num$end[[1]]
    ) |> 
      as.numeric()
    
    sec_num <- substr(
      input[current_num$input_line[[2]]],
      start = current_num$start[[2]],
      stop = current_num$end[[2]]
    ) |> 
      as.numeric()
    
    first_num * sec_num
  }
)

message("Part 2 answer is: ", sum(gear_ratios, na.rm = TRUE))
