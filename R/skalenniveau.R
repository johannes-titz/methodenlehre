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
  df2sctable2(sn_data, identifier = "skalenniveau", title = "Skalenniveau",
         story = "<p>Wählen Sie das höchstmögliche Skalenniveau für folgende Variablen aus.</p>")
}

#' Create question for exercise skalenniveau for exam
#'
#' Difference to skalenniveau function is that we select only diagnostically
#' useful questions from the 56 available. Currently this amount to 12 questions
#'
#' @export
skalenniveau_klausur <- function(n = 2) {
  sn_data <- get_data("skalenniveau")
  sn_data <- sn_data %>%
      filter(rows_id %in% paste0("I", c(12, 25, 27, 30, 37, 40, 42, 46, 49, 51, 52, 53)))
  sn_data <- sn_data[sample(seq(nrow(sn_data)), n), ]
  df2sctable2(sn_data, identifier = "skalenniveau", title = "Skalenniveau",
         story = "<p>Wählen Sie das höchstmögliche Skalenniveau für folgende Variablen aus.</p>")
}

#' Exercise Skalenniveau
#'
#' Create skalenniveau qti exercise for students.
#'
#' @details
#' One section with 5 elements randomly drawn.
#'
#' @param n number of items to draw for one run
#' @return test object AssessmentTestOpal
#' @export
skalenniveau_studis <- function(sample_n = 5) {
  d <- get_data("skalenniveau")
  sc_list <- df2sc2(d, "Was ist das höchste Skalenniveau für folgende Variable:",
                    shuffle = F)
  sec <- new("AssessmentSection", title = "Skalenniveaus",
             assessment_item = sc_list, shuffle = TRUE,
             visible = FALSE, identifier = "skalenniveaus",
             selection = sample_n)
  test <- new("AssessmentTestOpal", section = list(sec))
}
