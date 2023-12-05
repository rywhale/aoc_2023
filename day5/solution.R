input <- readr::read_lines(
  # "day5/input_test"
  "day5/input"
)

parse_map <- function(map_text) {
  purrr::map_df(
    map_text,
    ~ {
      vals <- stringr::str_extract_all(.x, "\\d+")[[1]] |>
        as.numeric()


      tibble::tibble(
        source_start = vals[2],
        source_end = vals[2] + vals[3] - 1,
        dest_start = vals[1],
        dest_end = vals[1] + vals[3] - 1
      ) |>
        dplyr::arrange(source_start)
    }
  )
}

get_maps <- function(input_lines) {
  input_lines <- input_lines[input_lines != ""]

  header_locs <- stringr::str_which(
    input_lines,
    ":"
  )

  purrr::map(
    1:length(header_locs),
    ~ {
      header <- input_lines[header_locs[.x]]

      map_start <- header_locs[.x] + 1

      map_end <- ifelse(
        .x == length(header_locs),
        length(input_lines),
        header_locs[.x + 1] - 1
      )

      parse_map(input_lines[map_start:map_end])
    }
  )
}

lookup_soil_value <- function(location, maps) {
  current_val <- location

  purrr::walk(
    length(maps):1,
    ~ {
      current_map <- maps[[.x]] |>
        dplyr::filter(
          dest_start <= current_val,
          dest_end >= current_val
        )

      if (nrow(current_map)) {
        pos <- current_val - current_map$dest_start

        current_val <<- current_map$source_start + pos
      }
    }
  )

  current_val
}

lookup_location <- function(val, maps) {
  current_val <- val

  purrr::walk(
    1:length(maps),
    ~ {
      current_map <- maps[[.x]] |>
        dplyr::filter(
          source_start <= current_val,
          source_end >= current_val
        )

      if (nrow(current_map)) {
        pos <- current_val - current_map$source_start

        current_val <<- current_map$dest_start + pos
      }
    }
  )

  current_val
}

seeds <- stringr::str_extract_all(
  input[1],
  "\\d+"
)[[1]] |>
  as.numeric()

all_maps <- get_maps(input[2:length(input)])

all_pt1_locations <- purrr::map_dbl(
  seeds,
  lookup_location,
  maps = all_maps
)

message("Part 1 answer is: ", min(all_pt1_locations))

seeds_pt2 <- split(seeds, ceiling(seq_along(seeds) / 2)) |> 
  purrr::map(
    ~{
      .x[1]:(.x[1] + .x[2] - 1)
    }
  )

