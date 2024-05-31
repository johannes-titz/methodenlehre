# tbl <- read.csv2("data-raw/wissenschaftstheorie.csv", row.names = 1)
# tbl <- tbl %>%
#   #group_by(zuordnung) %>%
#   sample_n(6) %>%
#   pivot_wider(names_from = zuordnung, values_from = punkte) %>%
#   mutate_all(function(x) ifelse(is.na(x), 0, x))
