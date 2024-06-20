#' stattests exercise
#'
#'
#' The exercise consists of selecting the appropriate test for a study. There
#' are 14 variants and 12 different tests.
#'
#' @param n_questions number of questions to create
#' @return list of exercises
#' @export
stattest <- function(n_questions = 1) {
  d <- get_data(stattests)
  rows <- d[sample(nrow(d), n_questions), ]
  l <- split(rows, seq(nrow(rows)))
  ex <- lapply(l, stattest_one, choices = unique(d$solution))
  if (length(ex) == 1) ex <- ex[[1]]
  ex
}

stattest_stud <- function(n_questions = 2) {
  d <- get_data(stattests)
  l <- split(d, seq(nrow(d)))
  ex <- lapply(l, stattest_one, choices = unique(d$solution))
  section <- new("AssessmentSection", identifier = "stattest",
                 assessment_item = ex, selection = n_questions)
  new("AssessmentTest", identifier = "stattest", section = list(section))
}

stattest_one <- function(row, choices) {
  between <- ifelse(row$n_between > 0, glue::glue(" Es gibt einen between-Faktor mit {row$n_between} Bedingungen."), "")
  within <- ifelse(row$n_within > 0, glue::glue(" Es gibt einen within-Faktor mit {row$n_within} Bedingungen."), "")
  mixed <- ifelse(row$n_between * row$n_within > 0, " Die Faktoren sind vollständig gekreuzt.", "")
  gegeben <- ifelse(row$parametric, "gegeben", "nicht gegeben")
  parametrisch <- glue::glue("Varianzhomogenität und Normalverteilung sind {gegeben}.")
  parametrisch <- ifelse(is.na(row$parametric), "", parametrisch)
  hypothesis <- ifelse(row$hypothesis == "unspecific", "Ihre Hypothese ist unspezifisch (Sie haben keine Vorstellung davon welche Mittelwerte in den jeweiligen Gruppen zu erwarten sind).", "Ihre Hypothese ist spezifisch (Sie erwarten bestimmte Mittelwerte in den jeweiligen Gruppen).")
  hypothesis <- ifelse(is.na(row$hypothesis), "", hypothesis)
  content <- glue::glue("<p>In Ihrer Abschlussarbeit führen Sie eine Untersuchung mit folgendem Design durch:{between}{within}{mixed} {parametrisch} Die AV besitzt ein {row$scalelevel}-Skalenniveau. {hypothesis} Welchen statistischen Test sollten Sie für die Auswertung benutzen? Bitte geben Sie den <b>einfachsten korrekten</b> Test für das <b>höchstmöglich passende</b> Skalenniveau an. Wenn z. B. ein t-Test und eine ANOVA möglich sind, wählen Sie den einfacheren Test, also t-Test.</p>")
  choices <- unique(choices)
  sc <- new("SingleChoice", identifier = paste0("stattests_", row$id),
            content = list(content), choices = choices,
            solution = which(choices == row$solution), title = "stattest")
  sc
}
