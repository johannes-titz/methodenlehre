# Data for validity exercise
librarian::shelf(tidyr, dplyr)
d <- readODS::read_ods("~/mnt/tuccloud/validity.ods")
d$feedback <- stringr::str_replace_all(d$feedback, "\\n", "<br/>")
d$...8 <- NULL
# 16 scenarios if fully crossed, give numeric value for the same solution
# pattern DELETE LATER
d <- na.omit(d)

d$scenario <- apply(d[, c(3, 4, 6, 5)], 1, function(x) paste(substr(x, 1, 1), collapse = ""))
d <- d[substr(d$scenario, 4, 4) != "h",]
#d <- d[d$scenario %in% c(1:4, 13:16),]
#d[, 5] <- NULL
position_questions <- 3:6

ids <- c("intern", "externSample", "externSit", "between")

d2 <- tidyr::pivot_longer(d, cols = position_questions,
                          names_to = c("question"),
                          values_to = "solution")
# somewhat dangerous!
d2$q_id <- rep(ids, nrow(d))
# choices?

ct <- d2 %>%
  select(q_id, solution) %>%
  group_by(q_id) %>%
  unique() %>%
  mutate(id = vctrs::vec_group_id(solution),
         id = paste0("choice", id)) %>%
  pivot_wider(names_from = id, values_from = solution)

d_final <- left_join(d2, ct)
d_final$study <- stringi::stri_trans_general(d_final$study, "de-ASCII;
                                             Latin-ASCII")
validity_data <- d_final
validity_data <- validity_data %>%
  filter(study %in% c("haengebruecke", "anker", "rosenthal"),
         q_id != "externSit") %>%
  mutate(scenario = substr(scenario, 1, 3))
validity_data$choice3 <- NULL

usethis::use_data(validity_data, overwrite = T)
