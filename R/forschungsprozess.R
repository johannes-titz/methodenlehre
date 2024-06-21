#' Exercise forschungsprozess
#'
#' Simple ordering task for the scientific process.
#' @export
forschungsprozess <- function() {
  q <- "In welcher Reihenfolge läuft der wissenschaftliche Prozess ab? Ziehen Sie die Elemente von links nach rechts in die korrekte Position. (1=Anfang, 8=Ende)"

  c <- c("Theorie", "Forschungshypothese", "Präzisierung der Hypothese (statistische Hypothese)",
         "Design der Studie", "Durchführung der Studie", "Datenanalyse",
         "Interpretation der Daten", "Implikation für Theorie")
  ids <- c("theory", "hypo", "hypostat", "design", "doit", "analysis",
           "interpret", "theoryimpl")

  ordering(identifier = "forschungsprozess", choices = c,
           choices_identifiers = ids, content = list(q))
}
