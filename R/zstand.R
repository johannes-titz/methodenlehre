#' Exercise zstand
#'
#' In this exercise the candidate has to calculate the raw value of a z-value and also find out the corresponding p-value for the z-value.
#' @param seed See for the exercise.
#' @return Entry rqti object
#' @examples
#' z <- zstand()
#' @export
zstand <- function(seed = sample.int(1e4, 1)) {
  set.seed(seed)
  z <- abs(round(rnorm(1), 2))
  i <- mml_eq(x <- 15L*z + 100L, T, round = 0)
  p <- mml_eq(p <- 1L - pnorm(z), T, flags = list(quote(round(2L))))
  text <- glue::glue("<p>In gängigen Intelligenz-Theorien gehen Forscher davon aus, dass der IQ normalverteilt ist. Ein Kollege von Ihnen hat bei einem Schüler einen Intelligenztest durchgeführt und nach dem Manual einen {M(z)}-Wert von {z} berechnet. Für ein Gutachten benötigen Sie jedoch den ursprünglichen IQ-Wert. Außerdem müssen Sie abschätzen ob der Schüler als hochbegabt klassifiziert werden kann. Beachten Sie, dass die IQ-Skala im Normalfall einen Mittelwert von 100 und eine Standardabweichung von 15 besitzt.</p>")

  gap1 <- numericGap(round(i$res), "iqvalue", tolerance = 0)
  fb1 <- glue::glue("<p>Der IQ ist üblicherweise so skaliert, dass der Mittelwert 100 und die Standardabweichung 15 beträgt. Man muss die {M(z)}-Standardisierungs-Formel umstellen und bekommt: {i$mml}.</p>")
  text2 <- "Wie häufig kommt ein solcher IQ-Wert oder ein höherer in der Population vor? Geben Sie das Ergebnis als Dezimalzahl an (z. B. 34% entspricht 0.34) und runden Sie auf 2 Dezimalstellen."
  gap2 <- numericGap(round(p$res, 2), "pvalue", tolerance = 1, tolerance_type = "relative")
  fb2 <- glue::glue("<p>Für die Berechnung der Wahrscheinlichkeit schauen wir in der {M(z)}-Tabelle nach dem zugehörigen {M(p)}-Wert: {mml_eq(pnorm(z))}. Dies ist allerdings genau die Gegenwahrscheinlichkeit, die uns angibt wie viele Personen einen niedrigeren IQ-Wert haben. Gefragt ist allerdings nach dem vorliegenden IQ-Wert oder einem höheren, wir rechnen also: {p$mml}.</p><p>Zusatzinformationen: Hochbegabung beginnt bei +2 SD, also einem z-Wert von 2, bzw. 130 IQ-Punkten. Der entsprechende p-Wert ist 0.023.</p>")
  feedback <- modalFeedback(list(fb1, fb2))

  entry <- entry("z", content = list(text, "<p>IQ (gerundet auf eine ganze Zahl):",
                                     gap1, "</p><p>", text2, gap2, "</p>"),
                 identifier = paste0("zstand", seed),
                 feedback = list(feedback))

  entry
}

zstand_stud <- function(seeds = 1:20) {
  x <- lapply(seeds, zstand)
  s <- section(x, selection = 1)
  test4opal(s, identifier = "zstand_stud", files = get_supplement_paths())
}
