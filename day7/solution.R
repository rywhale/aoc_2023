input <- readr::read_table(
  "day7/input",
  # "day7/input_test",
  # "day7/input_test_2",
  col_names = FALSE
)

names(input) <- c("hand", "bid")

card_lookup_pt1 <- letters[1:13]
names(card_lookup_pt1) <- c(
  2:9, "T", "J", "Q", "K", "A"
)

get_hand_type <- function(hand) {
  hand_elem <- stringr::str_split_1(hand, "")
  hand_tbl <- sort(table(hand_elem))
  unique_count <- length(unique(hand_elem))

  if (any(hand_tbl == 5)) {
    7
  } else if (any(hand_tbl == 4)) {
    6
  } else if (unique_count == 2) {
    5
  } else if (unique_count == 3 & 3 %in% hand_tbl) {
    4
  } else if (unique_count == 3 & sum(hand_tbl == 2) == 2) {
    3
  } else if (unique_count == 4 & 2 %in% hand_tbl) {
    2
  } else {
    1
  }
}

make_card_score_str <- function(hand, card_lookup) {
  
  hand_sp <- stringr::str_split_1(hand, "")

  card_scores <- purrr::map_chr(
    hand_sp,
    ~ card_lookup[[.x]]
  )

  paste(card_scores, collapse = "")
}

convert_card_score_str <- function(score_str){
  
  score_str_sp <- stringr::str_split_1(score_str, "")
  
  letter_score <- purrr::map_int(
    score_str_sp,
    ~{which(letters == .x)}
  )
  
  sum(letter_score)
}

jokerize <- function(hand, card_lookup) {
  
  if(!stringr::str_detect(hand, "J")){
    return(hand)
  }else{
    hand_elem <- stringr::str_split_1(hand, "")
    hand_tbl <- sort(table(hand_elem))
    
    joker_pos <- which(hand_elem == "J")
    joker_count <- length(joker_pos)
    possible_replacements <- names(card_lookup)[names(card_lookup) != "J"]
    
    replace_combs <- utils::combn(
      rep(possible_replacements, joker_count), 
      joker_count, 
      simplify = FALSE
    ) |>
      unique()
    
    replace_strs <- purrr::map_chr(
      replace_combs,
      ~{
        hand_elem[joker_pos] <- .x
        paste(hand_elem, collapse = "")
      }
    )
    
    loop_count <- 1
    best_score <- 0
    out <- ""
    while(loop_count <= length(replace_strs) & best_score != 7){
      # message("Loop: ", loop_count, " Max: ", length(replace_strs))
      str_score <- get_hand_type(replace_strs[loop_count])
      
      if(str_score > best_score){
        best_score <- str_score
        out <- replace_strs[loop_count]
      }
      
      loop_count <- loop_count + 1
    }
    
    replace_scores <- purrr::map_int(
      replace_strs,
      get_hand_type
    )
    
    out <- replace_strs[which.max(replace_scores)][[1]]
  }
  
  out
}

input_scored <- input |>
  dplyr::rowwise() |>
  dplyr::mutate(
    hand_type = get_hand_type(hand)
  ) |>
  dplyr::mutate(
    card_scores = make_card_score_str(
      hand,
      card_lookup_pt1
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(hand_type)
  ) |>
  dplyr::group_by(
    hand_type
  ) |>
  dplyr::arrange(
    card_scores,
    .by_group = TRUE
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    final_score = bid * dplyr::row_number()
  )

message("Part 1 solution is: ", sum(input_scored$final_score))

card_lookup_pt2 <- letters[1:13]
names(card_lookup_pt2) <- c(
  "J", 2:9, "T", "Q", "K", "A"
)

input_scored_pt2 <- input |>
  dplyr::rowwise() |>
  dplyr::mutate(
    hand_type = get_hand_type(hand),
    hand_jok = jokerize(hand, card_lookup_pt2),
    hand_type_jok = get_hand_type(hand_jok),
  ) |>
  dplyr::rowwise() |> 
  dplyr::mutate(
    card_scores = make_card_score_str(
      hand,
      card_lookup_pt2
    ),
    card_scores_jok = make_card_score_str(
      hand_jok,
      card_lookup_pt2
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(hand_type_jok)
  ) |>
  dplyr::group_by(
    hand_type_jok
  ) |>
  dplyr::arrange(
    card_scores,
    card_scores_jok,
    .by_group = TRUE
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    final_score = bid * dplyr::row_number()
  )
  
message("Part 2 solution is: ", sum(input_scored_pt2$final_score))
