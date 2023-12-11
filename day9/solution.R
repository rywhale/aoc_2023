input <- readr::read_lines(
  "day9/input"
)

solve_line <- function(line_nums){
  
  # Differences
  num_diffs <- diff(line_nums)
  diff_collect <- list(num_diffs)
  
  while(!all(num_diffs == 0)){
    num_diffs <- diff(num_diffs)
    diff_collect[[length(diff_collect) + 1]] <- num_diffs
  }
  
  diff_collect <- rev(diff_collect)
  
  purrr::walk(
    2:(length(diff_collect) + 1),
    ~{
      inc_val <- tail(diff_collect[[.x - 1]], 1)
      
      if(.x == length(diff_collect) + 1){
        line_nums[[length(line_nums) + 1]] <<- line_nums[[length(line_nums)]] + inc_val
      }else{
        diff_collect[[.x]] <<- diff_collect[[.x]] + inc_val
      }
    }
  )

  tail(line_nums, 1)
}

input_nums <- stringr::str_split(
  input,
  " "
) |>
  purrr::map(
    as.numeric
  )

test <- solve_line(input_nums[[2]])

pt1_solution <- purrr::map_int(
  input_nums,
  solve_line
)

message("Part 1 solution is: ", sum(pt1_solution))

pt2_solution <- purrr::map_int(
  input_nums,
  ~{
    solve_line(rev(.x))
  }
)

message("Part 2 solution is: ", sum(pt2_solution))