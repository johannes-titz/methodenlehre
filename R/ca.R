#
# d <- cofad::furr_p4
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
#
# ftbl <- s$FTable
# s
# #
# # glue::glue("Wie groß ist {M(r['effectsize'])}?")
# # glue::glue("Wie groß ist {M(r['contrast'])}?")
