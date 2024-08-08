eda_luecke <- function() {
  mygap <- function(solution, id, placeholder = "1 Wort", tolerance = 2) {
    gapTextOpal(solution, response_identifier = id, tolerance = tolerance,
                expected_length = 24, placeholder = placeholder, points = 0.5)
  }

  content <- list("<p>Bitte füllen Sie folgende Lücken. Hinweis: alle Lücken wurden auf eine Länge normiert, sodass die Lückenlänge kein Indikator für die Länge der korrekten Lösung ist.</p>",
                  "<p>Die",  mygap(c("Explorative Datenanalyse"), "eda", placeholder = "2 Wörter", tolerance = 4), " dient dazu durch optische Darstellung von Ergebnissen Muster und Zusammenhänge in Bezug auf die Forschungsfrage und darüber hinaus zu entdecken. Die Verfahren sind meist einfach anzuwenden und robust",
                  #mygap(c("robust", "stabil", "unanfällig"), "robust"),
                  " gegenüber einzelnen Ausreißern.  Ein gutes Beispiel dafür sind ", mygap(c("Boxplots", "Box-Plots"), "boxplots"), " die statt mit Mittelwert und Standardabweichung mit Median und Interquartilsabstand arbeiten. Extreme Werte haben besonders bei ", mygap(c("kleinen"), "kleinen"), " Stichproben einen überproportionalen Einfluss auf Mittelwert und Standardabweichung.</p>")
  entry("eda_luecke", content = content)
}
