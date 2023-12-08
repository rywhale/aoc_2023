input <- readr::read_lines(
  "day8/input"
)

input_ins <- stringr::str_split_1(
  input[1],
  ""
)

input_nodes <- tibble::tibble(
  input[3:length(input)]
) |>
  tidyr::separate(
    1,
    into = c("node_start", "end"),
    sep = " = "
  ) |>
  tidyr::separate(
    end,
    into = c("L", "R"),
    sep = ", "
  ) |>
  dplyr::mutate_at(
    c("L", "R"),
    stringr::str_remove,
    pattern = "\\(|\\)"
  )

start <- "AAA"
ins_step <- 1

while(start != "ZZZ"){
  if(ins_step > length(input_ins)){
    input_ins <- rep(input_ins, 2)
  }

  position <- input_ins[ins_step]

  start <- dplyr::filter(
    input_nodes,
    node_start == start
  ) |>
    dplyr::pull(
      position
    )

  message(start)

  if(!start == "ZZZ"){
    ins_step <- ins_step + 1
  }
}

message("Part 1 solution: ", ins_step, " steps")

lookup_end <- function(start, position, nodes) {
  dplyr::filter(
    nodes,
    node_start == start
  ) |>
    dplyr::pull(
      position
    )
}

# Part 2
all_path_lens <- purrr::map_int(
  input_nodes$node_start[endsWith(input_nodes$node_start, "A")],
  ~ {
    end <- .x
    ins_step <- 1

    message("Calculating path for: ", .x)
    
    while (!endsWith(end, "Z")) {
      
      if (ins_step > length(input_ins)) {
        input_ins <- rep(input_ins, 2)
      }

      position <- input_ins[ins_step]
      end <- lookup_end(end, position, input_nodes)
      
      if(!endsWith(end, "Z")){
        ins_step <- ins_step + 1
      }
    }
    
    ins_step
  }
)

ans <- purrr::reduce(all_path_lens, pracma::Lcm)
message("Part 2 solution: ", ans, " steps")
