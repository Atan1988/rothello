CanonicalForm <- function(x) UseMethod("CanonicalForm")

getValidMove <- function(x) UseMethod("getValidMove")

getNextState <- function(x, move) UseMethod("getNextState")

stringRepresentation <- function(x) UseMethod("stringRepresentation")

getGameEnded <- function(x) UseMethod("getGameEnded")

getActionSize <- function(x) UseMethod("getActionSize")

getBoardSize <- function(x) UseMethod("getBoardSize")

getSymmetries <- function(x) UseMethod("getSymmetries")
