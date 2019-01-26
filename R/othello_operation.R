#' @title generate the default board
#' @name generate_othello_base
#' @export
generate_othello_base <- function() {
   matrix(
     c(rep(0, 27), 1, -1, rep(0,6), -1, 1, rep(0, 27)),
     ncol = 8
   ) %>% tibble::as_tibble() %>%
  dplyr::mutate(
    row = seq(1, 8, 1)
  ) %>% tidyr::gather(col, val, -row) %>%
  dplyr::mutate(
    col = gsub('V', "", col) %>% as.numeric()
  )
}

#' @title  check directions
#' @name chk_one_direction
#' @param df board
#' @param row row of the piece
#' @param col col of the piece
#' @param piece_color color to check for continuity
#' @param search_color color to search
#' @param x  horizontal direction c(0, 1, -1)
#' @param y  veritcal direction c(0, 1, -1)
#' @param place T for new piece to play, F for pieces to flip
#' @export
chk_one_direction <- function(df, row, col, piece_color, search_color, x, y, place = T) {

   ###check closest opposite color piece of that direction
   pts <- tibble::tibble(
     row = !!row + y * seq(1, 8, 1),
     col = !!col + x * seq(1, 8, 1),
     dist = seq(1, 8, 1)
   ) %>%
   dplyr::inner_join(df, by = c("row", "col"))

   best_pt <- pts %>%
   dplyr::filter(
     val == search_color
   )

   if (nrow(best_pt) == 0) return(pts[0, ])

   best_pt <- best_pt %>% dplyr::filter(dist == min(dist))

   other_pts <- pts %>% dplyr::filter(dist < best_pt$dist)

   if (place) res <- best_pt else res <- other_pts

   if (nrow(best_pt) == 0) return(pts[0, ])
   if (nrow(best_pt) == 1 & nrow(other_pts) == 0) return(res)
   if ((other_pts %>% dplyr::pull(val) %>% sum()) != (nrow(other_pts) * (piece_color))) return(pts[0, ])
   return(res)
}

#' @title check valid move one direction
#' @name chk_valid_move_1d
#' @param df board state
#' @param row row of the piece
#' @param col col of the piece
#' @param x horizontal direction
#' @param y vertical direction
#' @export
chk_valid_move_1d <- function(df, row, col, x, y) {
  color <- df %>%
    dplyr::filter((row == !!row & col == !!col)) %>% pull(val)

   best_op_color <- chk_one_direction(df, row, col, piece_color = color, search_color = -1 * color, x, y)

   if (nrow(best_op_color) == 0) return(best_op_color[0, ])

   open_color <- chk_one_direction(df, row, col, piece_color = color, search_color = 0,
                                   -1 * x, -1 * y)
   if (nrow(open_color) == 1) return(open_color) else return(best_op_color[0, ])
}

#' @title check 8 directions around
#' @name chk_eight_ds
#' @param df board state
#' @param row row of the piece
#' @param col col of the piece
#' @export
chk_eight_ds <- function(df, row, col) {
  color <- df %>%
    dplyr::filter((row == !!row & col == !!col)) %>% pull(val)

  arround_df <- purrr::cross_df(list(row = row, col = col, x = c(-1, 0, 1), y = c(-1, 0, 1))) %>%
    dplyr::filter(x != 0| y != 0) %>%
    dplyr::mutate(row = row + y, col = col + x) %>%
    inner_join(df, by = c('row', 'col'))

  direction_df <- arround_df %>% filter(val == 0) %>%
    dplyr::mutate(x = x * -1, y = y * -1) %>%
    dplyr::select(x, y)

  if (nrow(direction_df) == 0) return(tibble::tibble(row = 0, col = 0, dist = 0, val = 0) %>% .[0, ])
  1:nrow(direction_df) %>% purrr::map_df(
    ~chk_valid_move_1d(df, row, col, direction_df$x[.], direction_df$y[.])
  )
}

#' @title check for legal moves
#' @name check_legal_move
#' @param color 1 or -1
#' @export
check_legal_move <- function(df, color) {
  oppo_pieces <- df %>% dplyr::filter(val == -1 * !!color)
  zero_pieces <- df %>% dplyr::filter(val == 0)

  if (nrow(oppo_pieces) == 0 | nrow(zero_pieces) == 0) return( df[0, ])

  1:nrow(oppo_pieces) %>%
    purrr::map_df(
      ~chk_eight_ds(df, oppo_pieces$row[.], oppo_pieces$col[.])
    ) %>% dplyr::distinct()
}

#' @title get piecies to flip
#' @name get_pieces_to_filp
#' @param df board
#' @param row row of the new piece
#' @param col col of the new piece
#' @param color color of the new piece
#' @export
get_pieces_to_flip <- function(df, row, col, color) {
  direction_df <-  purrr::cross_df(list(x = c(-1, 0, 1), y = c(-1, 0, 1))) %>%
    dplyr::filter(x != 0 | y != 0)

  1:nrow(direction_df) %>% purrr::map_df(
    ~chk_one_direction(df, row, col, piece_color = -1 * color, search_color = color
                       , direction_df$x[.], direction_df$y[.], F)
  )
}

#' @title make legal moves
#' @name make_legal_move
#' @param df board
#' @param legal_move_df legal move data frame
#' @param color of the legal move
#' @export
make_legal_move <- function(df, legal_move_df, color) {

  tmp_df <- dplyr::bind_rows(
    legal_move_df,
    get_pieces_to_flip(df, legal_move_df$row[1], legal_move_df$col[1], color)
  ) %>%
  dplyr::mutate(val = color)

  dplyr::bind_rows(
    tmp_df %>% dplyr::select(-dist),
    df %>% dplyr::anti_join(
      tmp_df, by = c('row', 'col')
    )
  ) %>%
  dplyr::arrange(col, row)
}

#' @title play randomly
#' @name play_randomly
#' @param df board
#' @param color color of the piece on turn
play_randomly <- function(df, color) {
   play_func <- function() {
      legal_move <- df %>% check_legal_move(color)
      if (nrow(legal_move) == 0) return(df %>% tidyr::spread(col, val))

      move_to_make <- legal_move[sample(1:nrow(legal_move), 1), ]
      df <<- make_legal_move(df, move_to_make, color)
      color <<- -1 * color
      return(df %>% tidyr::spread(col, val))
   }
}


