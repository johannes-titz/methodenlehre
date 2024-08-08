qualitative_aussagen <- function() {
  p1 <- list("<p>In der qualitativen Forschung ist ", new("TextGapOpal", response_identifier = "kommunikativeVal", solution = "kommunikative Validierung", tolerance = 4, placeholder = "2 Wörter"), " ein Prozess, bei dem Forscher ihre Interpretationen und Ergebnisse den interviewten Personen vorlegen und diesen die Gelegenheit bieten, dazu Stellung zu nehmen. Dieser Ansatz stellt sicher, dass die Perspektiven der Teilnehmer korrekt widergegeben wurden.</p>")

  p2 <- list("<p>", new("TextGapOpal", response_identifier = "theoreticalSampling", solution = c("Theoretical Sampling",  "Theoretisches Sampling", "Theoretisches Stichprobenziehen"), tolerance = 4, placeholder = "2 Wörter"), "ist eine Methodik, bei der die Auswahl von Teilnehmern oder Fällen nicht zufällig erfolgt, sondern basierend auf aufkommenden theoretischen Konzepten. Forscher wählen gezielt Teilnehmer aus, die dazu beitragen können, bestehende Theorien zu vertiefen oder zu erweitern. Dieser iterative Prozess ermöglicht eine dynamische Anpassung der Stichprobe im Laufe der Studie.</p>")

  p3 <- list("<p>", new("TextGapOpal", response_identifier = "triangulation", solution = c("Triangulation"), tolerance = 3, placeholder = "1 Wort"), " bezieht sich auf die Anwendung verschiedener Methoden oder Datenquellen, um die Verlässlichkeit und Validität der Forschungsergebnisse zu stärken. Durch die Kombination von beispielsweise Interviews, Beobachtungen und Analysen von Dokumenten erhalten Forscher ein umfassenderes Bild des untersuchten Phänomens.</p>")

  p4 <- list("<p>", new("TextGapOpal", response_identifier = "codieren", solution = c("Codieren", "Kodieren", "Kodierung", "Codierung"), tolerance = 2, placeholder = "1 Wort"), " ist ein zentraler Schritt in der Analyse qualitativer Daten. Hier werden Muster, Themen und Kategorien aus den gesammelten Informationen abgeleitet. Durch diesen Prozess werden die Daten in interpretierbare Einheiten zerlegt, was es ermöglicht, Bedeutungen zu entdecken und Theorien zu entwickeln.</p>")

  list(p1, p2, p3, p4)
}

#' Exercise qualitative
#' @export
qualitative <- function(which_questions = sample(1:4, 1)) {
  qa <- qualitative_aussagen()
  qa2 <- unlist(qa[which_questions])
  story <- "<p>Ergänzen Sie die Lücken in folgendem Text zu qualitativen Methoden. </p>"
  content <- append(story, qa2)
  quali1 <- new("Entry", identifier = "qualitative", content = content)
  quali1
}
