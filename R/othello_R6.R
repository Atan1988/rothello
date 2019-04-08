#' @title othello class
#' @name othelloR6
#' @description othello game R6 object
othelloR6 <- R6::R6Class("othelloR6", list(
  CurrPlayer = NULL,
  board_x = NULL,
  board_y = NULL,
  board = NULL,
  size = NULL,
  initialize = function(sz) {
    self$board <- generate_othello_base_M(sz, val = NULL)
    self$CurrPlayer <- 1
    self$board_x <- sz
    self$board_y <- sz
    self$size <- sz
  },
  get_valid = function(mv, board, zeros, CurrPlayer) {
    dir_mat <- dir_df[which(zeros$dir_mat[mv, ] == 1), ]

    lapply(1:nrow(dir_mat), function(i) {
      x <- dir_mat$x[i]; y <- dir_mat$y[i]
      val_locs <- mv + seq(1, self$board_x, 1) * self$board_y * x +  seq(1, self$board_y, 1) * 1 * y
      val_locs <- val_locs[val_locs >=1 & val_locs <= (self$board_y * self$board_x)]

      vals <- board[val_locs];
      first_oppo_loc <- which(vals == CurrPlayer)
      if (length(first_oppo_loc) > 0) {
        first_oppo_loc <- min(first_oppo_loc)
        if (sum(vals[1:(first_oppo_loc - 1)]) == ((first_oppo_loc - 1 )* CurrPlayer * -1 )) {
          return(val_locs[1:(first_oppo_loc - 1)])
        }
      }
    })
  },
  getValidMove = function(board, CurrPlayer) {
    zeros <- search_neighbor(board, CurrPlayer * -1)

    valid_flips <- sapply(zeros$mvs, self$get_valid, board = board,
                          zeros = zeros, CurrPlayer = CurrPlayer)
    names(valid_flips) <- zeros$mvs
    valid_flips[sapply(valid_flips, is.null)] <- NULL

    if (length(valid_flips) == 0) {
      moves <-  self$board_x * self$board_y + 1
    } else {
      moves <- names(valid_flips) %>% as.numeric()
    }
    return(moves)
  },
  getNextState = function(mv, board, CurrPlayer) {
    zeros <- search_neighbor(board, CurrPlayer * -1)
    if (mv == (self$board_x * self$board_y + 1)) {
      self$CurrPlayer <- self$CurrPlayer * -1
    } else {
      flips <- self$get_valid(mv, zeros)
      self$board[c(mv, unlist(flips))] <- self$CurrPlayer
      self$CurrPlayer <- self$CurrPlayer * -1
    }
  },
  getGameEnded = function() {
     self_pass <- self
  }
)
)
