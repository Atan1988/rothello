CanonicalForm <- function(x) UseMethod("CanonicalForm")

getValidMove <- function(x) UseMethod("getValidMove")

getNextState <- function(x, move) UseMethod("getNextState")

stringRepresentation <- function(x) UseMethod("stringRepresentation")

getGameEnded <- function(x) UseMethod("getGameEnded")
