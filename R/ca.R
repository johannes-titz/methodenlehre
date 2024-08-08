# devtools::install_github("johannes-titz/cofad")
# library(dplyr)
# data("furr_p4")
# d <- furr_p4
# n <- sample(5:25, 1)
# d2 <- d %>%
#   group_by(major) %>%
#   sample_n(n, replace = T)
#
# ca <- cofad::calc_contrast(dv = empathy, between = major,
#                            lambda_between = c("psychology" = 1, "education" = -1,
#                                               "business" = 0, "chemistry" = 0),
#                            data = d2)
# summary(ca)
# d2$wid <- seq(nrow(d2))
# ez::ezANOVA(d2, empathy, between = major, wid = wid, detailed = TRUE)
#
#
# within <- calc_contrast(dv = reading_test, within = music,
#                         lambda_within = c("without music" = 1.25,
#                                           "white noise" = 0.25,
#                                           "classic" = -0.75,
#                                           "jazz" = -0.75),
#                         id = participant, data = sedlmeier_p537)
# summary(within)
#
#
#
# glue::glue("In einem Artikel lesen sie folgende Angaben zu einer Kontrast-Analyse: F = {round(ca$sig[1], 3)}, r_effectsize = {round(ca$effects[1], 3)}. Insgesamt gab es {length(ca$lambda_between)} Gruppen und {nrow(d2)} Teilnehmer.")
#
# Wie groß sind die Freiheitsgrade zwischen für den F-Wert? 1
# Wie groß sind die Freiheitsgrade innerhalb für den F-Wert? ca$sig["df_inn"]
# Das hypothetisch erwartete Muster passt überzufällig
#
#
# library(cofad)
# d <- get_data("sedlmeier_p525")
# n <- sample(5:15, 1)
# d2 <- d %>%
#   group_by(between) %>%
#   sample_n(n, replace = T)
# d2 <- d2
# t16_2 <- calc_contrast(
#   dv = lsg,
#   between = between,
#   lambda_between = c("KT" = sample(-1:-10, 1), "JT" = sample(1:10, 1), "MT" = sample(1:10, 1)),
#   data = d2
# )
# s <- summary(t16_2)
#
# #
# # ftbl <- s$FTable
# # s
# #
# # #
# # # glue::glue("Wie groß ist {M(r['effectsize'])}?")
# # # glue::glue("Wie groß ist {M(r['contrast'])}?")
