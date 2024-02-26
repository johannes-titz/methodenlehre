d <- read.csv("data-raw/power.csv")
d2 <- tidyr::pivot_longer(d, Beta:Konfidenz, names_to = "rows",
                          values_to = "cols")
map_solution <- c("wird kleiner" = "wk", "bleibt gleich" = "bg",
                  "wird größer" = "wg")
d2$cols_id <- map_solution[d2$cols]
d2$rows_id <- d2$rows
d2 <- d2 %>%
  dplyr::mutate(cols = as.factor(cols), cols_id = as.factor(cols))
power2 <- d2

usethis::use_data(power2, overwrite = T)
