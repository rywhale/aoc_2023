input <- readr::read_lines(
  "day11/input"
)

input_table <- stringr::str_split(
  input,
  "",
  simplify = TRUE
)

calc_lengths <- function(input_table, exp_factor = 1){
  cols_to_exp <- purrr::map_lgl(
    1:ncol(input_table),
    ~{
      all(input_table[, .x] == ".")
    }
  ) |> 
    which()
  
  rows_to_exp <- purrr::map_lgl(
    1:nrow(input_table),
    ~{
      all(input_table[.x, ] == ".")
    }
  ) |> 
    which()
  
  galaxy_pos <- which(
    input_table == "#",
    arr.ind = TRUE
  ) |> 
    tibble::as_tibble() |> 
    dplyr::arrange(
      row, col
    ) |>
    dplyr::mutate(
      ID = dplyr::row_number()
    ) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      row = row + sum(1:(row - 1) %in% rows_to_exp) * exp_factor,
      col = col + sum(1:(col - 1) %in% cols_to_exp) * exp_factor
    )
  
  galaxy_pairs <- combn(
    galaxy_pos$ID,
    m = 2,
    simplify = FALSE
  )
  
  all_lengths <- purrr::map_int(
    galaxy_pairs,
    ~{
      start_pos <- c(
        galaxy_pos$row[galaxy_pos$ID == .x[[1]]],
        galaxy_pos$col[galaxy_pos$ID == .x[[1]]]
      )
      
      end_pos <- c(
        galaxy_pos$row[galaxy_pos$ID == .x[[2]]],
        galaxy_pos$col[galaxy_pos$ID == .x[[2]]]
      )
      
      sum(abs(start_pos - end_pos))
    }
  )
  
  sum(all_lengths)
}

message("Part 1 solution is: ", calc_lengths(input_table, 1))
message("Part 2 solution is: ", calc_lengths(input_table, 999999))