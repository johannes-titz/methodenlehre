#' Create question for exercise skalenniveau
#'
#' Draws a random sample of n exercises from the dataset skalenniveau from this
#' package. This dataset currently contains 56 questions. You can also just load
#' this dataset and select the questions yourself. Use df2sc2 to translate it to
#' a single choice table.
#'
#' @param n number of questions
#' @return OneInRowTable item object
#'
#' @export
skalenniveau <- function(n = 2) {
  sn_data <- get_data("skalenniveau")
  sn_data <- sn_data[sample(seq(nrow(sn_data)), n), ]
  df2sc2(sn_data, identifier = "skalenniveau", title = "Skalenniveau",
         story = "<p>Wählen Sie das höchstmögliche Skalenniveau für folgende Variablena us.</p>")
}

#' Create question for exercise skalenniveau for exam
#'
#' Difference to skalenniveau function is that we select only diagnostically
#' useful questions from the 56 available. Currently this amount to 12 questions
skalenniveau_klausur <- function(n = 2) {
  sn_data <- get_data("skalenniveau")
  sn_data <- sn_data %>%
      filter(rows_id %in% paste0("I", c(12, 25, 27, 30, 37, 40, 42, 46, 49, 51, 52, 53)))
  sn_data <- sn_data[sample(seq(nrow(sn_data)), n), ]
  df2sc2(sn_data, identifier = "skalenniveau", title = "Skalenniveau",
         story = "<p>Wählen Sie das höchstmögliche Skalenniveau für folgende Variablena us.</p>")
}
