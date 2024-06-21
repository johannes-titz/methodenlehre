#' Exercise forschungsprozess
#'
#' Simple ordering task for the scientific process.
#' @export
forschungsprozess <- function() {
  q <- "In welcher Reihenfolge läuft der wissenschaftliche Prozess ab? Ziehen Sie die Elemente von links nach rechts in die korrekte Position. (1=Anfang, 8=Ende). Sie bekommen nur auf die vollständig korrekte Reihenfolge einen Punkt, Teilordnungen können nicht berücksichtigt werden."

  c <- c("Theorie", "Forschungshypothese", "Präzisierung der Hypothese (statistische Hypothese)",
         "Design der Studie", "Durchführung der Studie", "Datenanalyse",
         "Interpretation der Daten", "Implikation für Theorie")
  ids <- c("theory", "hypo", "hypostat", "design", "doit", "analysis",
           "interpret", "theoryimpl")

  fb <- list("<p>Bitte beachten Sie, dass es nur auf die vollständig richtige Ordnung 1 Punkt gibt. Teilpunkte können nicht vergeben werden.</p>

<p> Die korrekte Reihenfolge ist:", "<ol>", paste0("<li>", c, "</li>"), "</ol>", "</p>")

  ordering(identifier = "forschungsprozess", choices = c,
           choices_identifiers = ids, content = list(q),
           points_per_answer = F, feedback = list(modalFeedback(content = fb)))
}
