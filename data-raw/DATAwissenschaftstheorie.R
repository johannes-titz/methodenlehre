tbl <- read.csv2("data-raw/wissenschaftstheorie.csv", row.names = 1)
library(tidyr)
tbl <- tbl %>%
  mutate(rows_id = rownames(tbl), cols_id = abbreviate(zuordnung)) %>%
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  transmute(cols = as.factor(zuordnung), rows = Aussage, rows_id, cols_id,
            points = punkte)

wt <- tbl
usethis::use_data(wt, overwrite = T)
