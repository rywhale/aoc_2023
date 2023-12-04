input <- readr::read_lines(
  "day4/input"
)

score_line <- function(input_line ,overlap_only = FALSE){
  
  card <- stringr::str_split_i(
    input_line,
    ":",
    2
  ) |>
    stringr::str_split_1(
      "\\|"
    )
  
  winners <- stringr::str_extract_all(card[1], "\\d+")
  my_nums <- stringr::str_extract_all(card[2], "\\d+")
  
  overlap <- sum(my_nums[[1]] %in% winners[[1]])
  
  if(overlap_only){
    return(overlap)
  }
  
  if(overlap == 0){
    return(0)
  }
  
  1 * (2 ** (overlap - 1))
}

all_card_scores <- purrr::map_int(
  input,
  score_line
)

message("Part 1 answer is: ", sum(all_card_scores))

all_card_overlaps <- purrr::map_int(
  input,
  score_line,
  overlap_only = TRUE
)

card_counts <- rep(
  1,
  length(all_card_overlaps)
)

purrr::walk(
  1:length(all_card_overlaps),
  ~{
    
    card_score <- all_card_overlaps[.x]
    
    first_card <- .x + 1
    last_card <- min(.x + card_score, length(card_counts))
    
    if(first_card <= last_card){
      message("Card score: ", card_score)
      message("Increasing cards ", first_card, " to ", last_card, " by ", (1 * card_counts[.x]))
      card_counts[first_card:last_card] <<- card_counts[first_card:last_card] + (1 * card_counts[.x])
    }
  }
)

message("Part 2 answer is: ", sum(card_counts))