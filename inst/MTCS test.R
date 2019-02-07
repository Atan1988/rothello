library(RSQLite)
library(DBI)

MTCS_value <- tibble::tibble(
  Id = NA, move = NA, parentId = NA, parentmove = NA, wins = NA, visits = NA
) %>% .[0, ]

parent_child <- tibble::tibble(
  Id = NA, move = NA, childId = NA, childmove = NA
) %>% .[0, ]

untried_moves <- tibble::tibble(
  Id = NA, move = NA, untried_move = NA
) %>% .[0, ]

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

DBI::dbWriteTable(con, 'MTCS_value', MTCS_value)
DBI::dbWriteTable(con, 'parent_child', parent_child)
DBI::dbWriteTable(con, 'untried_moves', untried_moves)

MT <- ini_MTCS()
