#' Exercise regression_multiple
#'
#' Multiple regression setting with prestige data. 3 questions: what is the best
#' predictor? What is the standardized regression coefficient? Estimate of dv
#' for specific values of iv
#' @param seeds seeds for exercises, can be a single value or a vector for
#'   several exercises, defaults to randomly drawn 4-digits seeds.
#' @export
regression_multiple <- function(seeds = sample.int(1e4, 1)) {
  ex <- lapply(seeds, regression_multiple_one)
  if (length(ex) == 1) ex <- ex[[1]]
  ex
}

regression_multiple_one <- function(seed = sample.int(1e4, 1)) {
  set.seed(seed)
  prestige <- prepare_prestige(2)
  vars <- names(prestige$data)
  dv_unit <- c("education" = "Jahre", "women_percent" = "Prozent", "income" = "1000 USD pro Jahr", "prestige" = "z-Werte")[vars[1]]

  ivs <- paste(vars[-1], collapse = "+")
  model <- lm(as.formula(paste0(vars[1], "~", ivs)), data = prestige$data)
  output <- capture.output(print(summary(model)))
  output <- gsub("<", "&#60;", output)
  output <- gsub(">", "&#62;", output)

  vars_md <- paste0("<em>", vars, "</em>")
  model_sd <- lm.beta::lm.beta(model)

  story <- paste0(prestige$story, "<p>Sie berechnen eine Regression mit den Prädiktoren ",
                  paste0(vars_md[-1], collapse = ", "),
                  " und dem Kriterium ", vars_md[1], ". Der Regressionsoutput sieht wie folgt aus:</p>")
  parameters <- extract_parameters(model)

  coefs <- coef(model)
  s <- NULL
  s["x"] <- round(sd(prestige$data[,2]), 2)
  s["y"] <- round(sd(prestige$data[,1]), 2)
  b <- index0::as.index0(coefs)
  beta <- NULL
  beta <- mml_eq(beta[1L] <- b[1L]*dfrac(s["x"], s["y"]), T, round = 2)
  if (beta$res == 0) warning("Multiple regression exercise: beta is 0, you might want to use a different seed.")

  x <- prestige$data[sample(seq(nrow(prestige$data)), 1), 2:3]
  y_hat <- mml_eq(x[1L]*b[1L]+x[2L]*b[2L] + b[0L], T, round = 3)

  q3_text <- glue::glue("<p>Welche Schätzung ergibt sich im Modell für {vars_md[1]}, wenn {vars_md[2]} {x[1]} und {vars_md[3]} {x[2]} sind? Runden Sie auf 3 Dezimalstellen.")
  b_round <- round(b, 3)
  y_hat_round <- mml_eq(x[1L]*b_round[1L]+x[2L]*b_round[2L] + b_round[0L], T, round = 3)
  q3 <- numericGap(y_hat$res, "yhat", expected_length = nchar(y_hat$res),
                   placeholder = "", tolerance = abs(y_hat$res - y_hat_round$res),
                   tolerance_type = "absolute")

  fb3 <- glue::glue("<p>Es ergibt sich folgender Wert für die Schätzung von y: {y_hat$mml}</p>")

  fb1 <- "<p>Der unstandardisierte Regressionskoeffizient ist kein guter Indikator für die Vorhersagekraft. Wenn standardsierte Regressionskoeffizienten gegeben wären, könnten Sie diese nutzen. Alternativ bietet sich der p-Wert oder t-Wert an, da diese ebenfalls unabhängig von der Einheit der Variablen sind. Das heißt, Sie wählen einfach den größten t-Wert (unabhängig vom Vorzeichen) oder den kleinsten p-Wert.</p>"

  fb2 <- paste("<p>Der standardisierte Regressionskoeffizient kann auch nach einer unstandadisierten Regression über folgende Formel berechnet werden:", textutils::HTMLdecode(beta$mml),
               "</p>")


  tbl <- as.character(
  kableExtra::kable_styling(
    full_width=F, position = "left",
    kable(cbind(vars[2:1], as.numeric(s)), "html"))
  )
  feedback <- modalFeedback(list(fb1, fb2, fb3))
  e <- entry(
    paste0("regression_multiple", seed),
    content = list(story,
                   "<pre>",
                   paste(output, collapse = "<br/>"),
                   "</pre>",
                   "Ihnen liegen zudem noch folgende Standardabweichungen vor:",
                   tbl,
                   paste0("<p>Welcher Prädiktor hat die größte Vorhersagekraft für das Kriterium im Modell? Geben Sie die entsprechende Zahl in das Feld ein: [",
                          paste0(vars_md[-1], "=", seq(vars_md[-1]), collapse = " | "), "]"),
                   numericGap(as.numeric(which.max(abs(coef(model_sd)[-1]))), "iv_best", points = 0.5, expected_length = 1, placeholder = ""),
                   paste0("</p><p>Wie groß ist der standardisierte Regressionskoeffizient für ",
                          vars_md[2], "? Runden Sie das finale Ergebnis auf 2 Dezimalstellen."),
                   numericGap(beta$res, "beta", expected_length = nchar(beta$res)+2,
                              placeholder = "", tolerance = 1, tolerance_type = "relative"),
                   "</p>",
                   q3_text, q3, "</p>")
                   ,
    feedback = list(feedback)
  )
  e
}

regression_multiple_stud <- function(seeds = 1:20) {
  ex <- lapply(seeds, regression_multiple)
  s <- section(ex, selection = 2, visible = F)
  test4opal(s, "regression_multiple_stud",
            calculator = "scientific",
            files = get_supplement_paths())
}
