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


rootstate <- ini_othello(8)
rootstate1 <- mk_move(rootstate, move = rootstate$moves[1])

tree1 <- UCT(rootstate, itermax = 100)
tree2 <- UCT(rootstate1, itermax = 100)

tree1 %>% dplyr::tbl("MTCS_value") %>% dplyr::collect() -> df1
tree2 %>% dplyr::tbl("MTCS_value") %>% dplyr::collect() -> df2

View(df1); View(df2)



# Find out how many cores are being used
#getDoParWorkers()

library(foreach)
start <- Sys.time()
# Register cluster
library(doParallel)
registerDoParallel(6)
games <- foreach(i = 1:100, .packages = 'rothello') %dopar% UCT_playgame1(100, 100)
print(Sys.time() - start)
doParallel::stopImplicitCluster()

game_df <- 1:length(games) %>% purrr::map_df(function(x) {
  tibble::tibble(game_id = x,
                 move_id = seq(1, length(games[[x]]$moves), 1),
                 moves = games[[x]]$moves)
})

result_df <- 1:length(games) %>% purrr::map_df(function(x) {
  tibble::tibble(game_id = x, result = games[[x]]$result)
})

hist(games %>% purrr::map_dbl(~.[[2]]))
(games %>% purrr::map_dbl(~.[[2]]) %>% sort()) -> results
cat("player 2 win ", ifelse(results < 0, 1, 0) %>% sum(), "times\n")

con <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "games",
  host = "othelloai.ccz20a6chtws.us-east-2.rds.amazonaws.com",
  port = 3306,
  username = "atan",
  password = "gyjltzw3813"
)

exist_iter <- con %>% DBI::dbListTables() %>% .[grepl('game_history', .)] %>% gsub('game_history', "", .) %>%
  as.numeric() %>% max() %>% ifelse(is.infinite(.) | is.na(.), 0, .)
sim_iter <- exist_iter + 1
DBI::dbWriteTable(con, paste0('game_history', sim_iter), game_df, row.names = F)
DBI::dbWriteTable(con, paste0('game_result', sim_iter), result_df, row.names = F)

con %>% DBI::dbDisconnect()

start <- Sys.time()
games2 <- foreach(i = 1:100, .packages = 'rothello') %dopar% UCT_playgame(50, 100)
print(Sys.time() - start)

hist(games2 %>% purrr::map_dbl(~.[[2]]))

n <-  100
prog_bar <- dplyr::progress_estimated(n)
res <- 1:n %>%
  purrr::map(function(x) {
    res <- UCT_playgame(25, 100);
    prog_bar$tick()$print()
    return(res)
  })

hist(res %>% purrr::map_dbl(~.[[2]]))

start <- Sys.time()
game1 <- UCT_playgame(25, 100)
print(Sys.time() - start)
game1

microbenchmark(
  vector_data <- gdata::unmatrix(mat.pad[ind + 1, ind    ],byrow=T),
  vector_data <- as.vector(mat.pad[ind + 1, ind    ] %>% t()),
  vector_data <- as.vector(mat.pad[ind + 1, ind    ])
)

res <- UCT_playgame(25, 100)

start <- Sys.time()
res <- UCT_playgame1(100, 100)
print(Sys.time() - start)

val = c(rep(0, 3), rep(-1, 5), 0, rep(1, 2), rep(-1, 5), rep(1, 3),
        -1, 1, -1, 1, -1, rep(1, 7), -1, rep(1, 2), -1, rep(1, 2),
        -1, 1, -1, rep(1, 3), -1, 1, -1, 1, 0, rep(1, 4), -1, 1, -1, 1, 1, rep(-1, 7))

val = c(-1, 0, 1, 1, -1, -1, 0, 0,
        rep(-1, 6), 1, 0,
        1, -1, -1, rep(1, 4), -1,
        1, 1, rep(c(-1, 1), 3),
        1, 1, -1, -1, 1, 1, -1, -1,
        rep(-1, 5), 1, -1, -1,
        -1, -1, rep(1, 4), -1, -1,
        rep(-1, 8))

val = c(rep(1, 3), -1, rep(1, 4),
        1, 1, 0, -1, rep(1, 4),
        1, rep(-1, 3), 1, 1, -1, -1,
        rep(1, 8),
        1, -1, 1, rep(-1, 3), 1, 1,
        0, rep(-1, 3), 1, rep(-1, 3),
        0, rep(-1, 7),
        rep(-1, 6), 1, 1)

ini_othello(8, val = val, player = -1) -> rootstate
