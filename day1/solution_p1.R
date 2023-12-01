input <- readr::read_lines(
  "day1/input"
  )

first_digit <- stringr::str_extract(
  input,
  pattern = "[0-9]"
)

last_digit <- stringr::str_extract(
  stringi::stri_reverse(input),
  pattern = "[0-9]"
)

solution <- sum(
  as.numeric(paste0(first_digit, last_digit))
)

message("Answer is: ", solution)
