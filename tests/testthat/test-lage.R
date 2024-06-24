test_that("solution is correct", {
  vector <- c(11, 23, 24, 25, 26, 26, 27, 27, 27, 28, 29, 29)
  df <- lagemaße_qdf(vector)
  mad <- round(mean(abs(vector - 25.2)), 1)
  solution <- c(med = 26.5, q_25 = 24.5, q_75 = 27.5, IQA = 3,
                mw = 25.2, var = 21.3, s = 4.6, MAD = mad, R = 18)
  expect_equal(unlist(df$solution), solution)
})
