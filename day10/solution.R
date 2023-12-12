input <- readr::read_lines(
  "day10/input"
)

input_matrix <- input |>
  stringr::str_split("", simplify = TRUE) |>
  rbind()

can_connect <- function(sp, ep, in_matrix) {
  start_symbol <- in_matrix[sp[[1]], sp[[2]]]
  end_symbol <- in_matrix[ep[[1]], ep[[2]]]

  if (start_symbol == "." | end_symbol == ".") {
    return(FALSE)
  }

  pos_diff <- sp - ep

  rel_pos <- dplyr::case_when(
    pos_diff[[1]] == -1 ~ "south",
    pos_diff[[1]] == 1 ~ "north",
    pos_diff[[2]] == -1 ~ "east",
    TRUE ~ "west"
  )

  sec_pos <- dplyr::case_when(
    pos_diff[[1]] == 1 ~ "south",
    pos_diff[[1]] == -1 ~ "north",
    pos_diff[[2]] == 1 ~ "east",
    TRUE ~ "west"
  )

  symbol_lookup <- list(
    "|" = c("north", "south"),
    "-" = c("east", "west"),
    "L" = c("north", "east"),
    "J" = c("north", "west"),
    "7" = c("south", "west"),
    "F" = c("south", "east")
  )

  rel_pos %in% symbol_lookup[[start_symbol]] & sec_pos %in% symbol_lookup[[end_symbol]]
}

get_possible_steps <- function(start_pos, max_pos) {
  tibble::tibble(
    step_x = c(
      start_pos[[1]] - 1,
      start_pos[[1]] + 1,
      start_pos[[1]],
      start_pos[[1]]
    ),
    step_y = c(
      start_pos[[2]],
      start_pos[[2]],
      start_pos[[2]] + 1,
      start_pos[[2]] - 1
    )
  ) |>
    dplyr::filter(
      step_x > 0,
      step_y > 0,
      step_x <= max_pos,
      step_y <= max_pos
    ) |>
    dplyr::mutate(
      step_key = paste0(step_x, ",", step_y)
    )
}

get_paths <- function(start_pos, start_symbol, input_matrix) {
  input_matrix[input_matrix == "S"] <- start_symbol

  paths_from_start <- get_possible_steps(
    start_pos = start_pos,
    max_pos = nrow(input_matrix)
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      valid_path = can_connect(
        sp = start_pos,
        ep = c(step_x, step_y),
        in_matrix = input_matrix
      )
    ) |>
    dplyr::filter(
      valid_path
    )

  paths_from_start <- paths_from_start[1, ] |>
    dplyr::add_row(
      step_x = start_pos[[1]],
      step_y = start_pos[[2]],
      step_key = paste(start_pos, collapse = ","),
      valid_path = TRUE,
      .before = 1
    )

  can_continue <- TRUE

  while(can_continue){
    start_x <- tail(paths_from_start$step_x, 1)
    start_y <- tail(paths_from_start$step_y, 1)

    next_step <- get_possible_steps(
      start_pos = c(start_x, start_y),
      max_pos = nrow(input_matrix)
    ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        valid_path = can_connect(
          sp = c(start_x, start_y),
          ep = c(step_x, step_y),
          in_matrix = input_matrix
        )
      ) |>
      dplyr::filter(
        valid_path,
        !step_key %in% paths_from_start$step_key
      )

    if(nrow(next_step) == 1){
      paths_from_start <- dplyr::bind_rows(
        paths_from_start,
        next_step
      )
      
    }else{
      can_continue <- FALSE
    }
  }

  paths_from_start
}

starting_pos <- which(input_matrix == "S", arr.ind = TRUE)

all_the_paths <- get_paths(starting_pos, "J", input_matrix)

message("Part 1 solution is: ", nrow(all_the_paths) / 2)
