## code to prepare `DATASET` dataset goes here

skalenniveau <- read.csv2("data-raw/skalenniveau2.csv")
skalenniveau$cols <- factor(skalenniveau$cols,
                            c(levels = c("Nominal", "Ordinal", "Intervall",
                                         "Verhältnis", "Absolut")))
skalenniveau$cols_id <- forcats::fct_relabel(skalenniveau$cols, ~ substr(.x, 1, 1))
usethis::use_data(skalenniveau, overwrite = TRUE)
