## code to prepare `DATASET` dataset goes here

skalenniveau <- read.csv2("data-raw/skalenniveau2.csv")
skalenniveau$rows_id <- paste0("SK_", skalenniveau$rows_id)
skalenniveau$cols <- factor(skalenniveau$cols,
                            c(levels = c("Nominal", "Ordinal", "Intervall",
                                         "Verhältnis", "Absolut")))
skalenniveau$cols_id <- forcats::fct_relabel(skalenniveau$cols, ~ substr(.x, 1, 1))

librarian::shelf(simpleCache, dplyr)
simpleCache::setCacheDir("cache")
simpleCache("sk", qti::extract_results("data-raw/TESTSkalenniveaus_2024-02-09__09-42-24__010.zip"))

sk2 <- sk %>%
  group_by(file) %>%
  mutate(score = sum(score_candidate), sum_answer_given = sum(is_answer_given),
         rows_id = substr(id_question, 5, 10)) %>%
  select(-id_question) %>%
  filter(sum_answer_given >= 5) # only fully answered

sk3 <- sk2 %>%
  group_by(rows_id) %>%
  summarize(r_it = mycor(score_candidate, score-score_candidate), P = mean(score_candidate)/mean(score_max), dur = mean(duration), n = n())

skalenniveau <- merge(skalenniveau, sk3, by = "rows_id")

skalenniveau <- skalenniveau %>%
    mutate(item_nmbr = as.numeric(substr(rows_id, 5, 7))) %>%
    arrange(item_nmbr)

usethis::use_data(skalenniveau, overwrite = TRUE)


# lagemaße, kürzel
# qs <- d %>%
#   mutate(year = lubridate::year(date)) %>%
#   filter(title %in% c("lage2", "ls_statistiken"))
