power_type <- function(seed = sample.int(1e4, 1)) {
  set.seed(seed)
  data <- get_data("power_type_data")
  data <- data %>%
    dplyr::mutate(question = glue::glue("Für Ihre Studie im EmpEx möchten Sie folgenden statistischen Test durchführen: <b>{test}</b>. Welche Größen müssen in diesem Fall (unter anderem) für eine <b>{question}</b>-Power-Analyse festgelegt werden? Wählen Sie alle notwendigen Größen aus. Für jede Teil-Frage bekommen Sie bei korrekter Antwort 0.25 Punkte, bei falscher Antwort -0.25 Punkte, jedoch nie weniger als 0 Punkte insgesamt."))
  row <- data %>%
    dplyr::sample_n(1)
  choices <- row %>%
    dplyr::select(Alpha:`Anzahl Gruppen`)
  choices <- unlist(as.vector(choices))
  cols_identifiers <- c("alpha", "power", "es", "N", "H1", "groups")
  wide <- tidyr::pivot_longer(row, cols = Alpha:`Anzahl Gruppen`,
                              names_to = "rows", values_to = "cols")
  wide$cols_id <- wide$cols
  wide$rows_id <- as.character(abbreviate(stringi::stri_trans_general(wide$rows,
                                              "de-ASCII;Latin-ASCII")))
  wide <- wide %>%
    dplyr::select(rows:rows_id) %>%
    dplyr::group_by(cols) %>%
    dplyr::sample_n(2)
  feedback3 <- "Beachten Sie, dass die Aufgabe zufällig erstellt wird. Das Feedback enthält somit auch Erklärungen für nicht gezeigte Auswahloptionen."
  fb <- new("ModalFeedback",
            content = list("<p>", row$feedback1, row$feedback2, feedback3,
                           "</p>"))
  mc <- multipleChoice(paste0("power_type", seed, row$id), content = list(row$question),
                 choices = wide$rows,
                 choice_identifiers = wide$rows_id,
                 points = wide$cols*0.5, feedback = list(fb))
  mc
}

power_type_stud <- function(seeds = 1:30) {
  ex <- parallel::mclapply(seeds, power_type)
  s <- section(content = ex, selection = 2, visible = F)
  test(s, "power_type_stud", "Power III")
}