set.seed(2)
stroop <- get_data("stroop")

# within
within <- stroop %>%
  filter(time == 1) %>%
  select(mode, id, vo2:percent_hrmax, reverse_stroop_neutral_test:stroop_interference)

iv_name <- "mode"
dv_names <- names(within)[!(names(within) %in% c("mode", "id"))]
dv_name <- sample(dv_names, 1)

anova_model <- function(iv_name, dv_name, data) {
  formula <- as.formula(glue::glue("{dv_name} ~ {iv_name} + Error(id/{iv_name})"))
  anova <- aov(formula, data = stroop)
  tbl <- summary(anova)[[2]][[1]]
  tbl <- round(tbl, 3)
  tbl_html <- html_table(kable(tbl, "html"))
  tbl_html <- gsub("&lt;", "&#60;", tbl_html)
  tbl_html <- gsub("&gt;", "&#62;", tbl_html)
  tbl_html <- gsub("NA", "", tbl_html)
  tbl_html <- as.character(tbl_html)
  tibble::lst(anova, tbl_html, tbl)
}

mdl <- anova_model(iv_name, dv_name, within)

story <- glue::glue("In der sportpsychologischen Forschung wird der Stroop-Test und seine Varianten häufig verwendet, um die Vorteile von sportlicher Betätigung auf die kognitive Funktion zu untersuchen. 48 junge Erwachsene nahmen an einem Within-Subjects-Experiment teil, bei dem die Bewegungsart variiert wurde (Gehen, Krafttraining, Badminton und Sitzen als Kontrolle). Nach jeder Intervention absolvierten die Teilnehmer Stroop-Tests in neutraler und inkongruenter Form sowie die reverse-Stroop-Tests in neutraler und inkongruenter Form. Erfasst wurde die Leistung bei jedem Test, sowie diverse Fitness-Maße. Die folgende ANOVA-Tabelle stellt die Ergebnisse dar, wobei <em>mode</em> die unabhängige Variable (Bewegungsart) ist. Die abhängige Variable ist <em>{dv_name}</em>")

takahashi_explain <- c('<details><summary>Erklärung der Variablen (optional)</summary><table>
    <tr>
        <th>Begriff</th>
        <th>Erklärung</th>
    </tr>
    <tr>
        <td><strong>VO2</strong></td>
        <td>Steht für die Sauerstoffaufnahme (Volumen pro Minute), die misst, wie viel Sauerstoff der Körper während maximaler körperlicher Anstrengung verbrauchen kann. Es ist ein Maß für die aerobe Kapazität.</td>
    </tr>
    <tr>
        <td><strong>percent_VO2peak</strong></td>
        <td>Gibt den Prozentsatz der maximal erreichten Sauerstoffaufnahme (VO2 Peak) während einer bestimmten Belastung an. Es zeigt, wie nahe eine Person während der Belastung an ihre maximale aerobe Kapazität herankommt.</td>
    </tr>
    <tr>
        <td><strong>HR</strong></td>
        <td>Abkürzung für Herzfrequenz, die Anzahl der Herzschläge pro Minute.</td>
    </tr>
    <tr>
        <td><strong>percent_HRmax</strong></td>
        <td>Gibt den Prozentsatz der maximalen Herzfrequenz an, der während einer Belastung erreicht wird. Es ist ein Maß dafür, wie stark das Herz während einer Übung arbeitet.</td>
    </tr>
    <tr>
        <td><strong>Reverse_Stroop_Neutral_Test</strong></td>
        <td>Ein Test aus der Stroop-Testfamilie, bei dem Teilnehmer schnell Farben oder Wörter identifizieren müssen, die neutral sind, also keine Interferenz durch gegenläufige Informationen (z.B. das Wort "Rot" in blauer Farbe) erfahren.</td>
    </tr>
    <tr>
        <td><strong>Reverse_Stroop_Incongruent_Test</strong></td>
        <td>Eine Variante des Reverse Stroop Tests, bei dem die Aufgabe erschwert ist durch inkongruente Informationen – zum Beispiel das Wort "Rot" in blauer Schrift.</td>
    </tr>
    <tr>
        <td><strong>Stroop_Neutral_Test</strong></td>
        <td>Ein psychologischer Test, bei dem Teilnehmer die Farbe von Wörtern benennen müssen, die neutral sind, also nicht den Namen einer Farbe darstellen (z.B. das Wort "Baum" in roter Farbe).</td>
    </tr>
    <tr>
        <td><strong>Stroop_Incongruent_Test</strong></td>
        <td>Eine Version des Stroop Tests, bei der die Wörter Farbnamen darstellen, die in einer nicht übereinstimmenden Farbe geschrieben sind (z.B. das Wort "Grün" in roter Farbe).</td>
    </tr>
    <tr>
        <td><strong>Reverse_Stroop_Interference</strong></td>
        <td>Misst die kognitive Beeinträchtigung im Reverse Stroop Test durch die Differenz in der Reaktionszeit zwischen neutralen und inkongruenten Bedingungen.</td>
    </tr>
    <tr>
        <td><strong>Stroop_Interference</strong></td>
        <td>Misst die kognitive Beeinträchtigung im Stroop Test durch die Differenz in der Reaktionszeit zwischen neutralen und inkongruenten Bedingungen.</td>
    </tr>
</table></details>')

anova_eta2p <- function(tbl) {
  QS <- tbl$`Sum Sq`
  names(QS) <- c("UV", "res")
  mml <- mml_eq(QS["UV"]/(QS["UV"]+QS["res"]), T)
  q1 <- glue::glue("Wie groß ist {M(eta['p']^2L)}? Geben Sie das Ergebnis als Dezimalzahl and und runden Sie auf 3 Dezimalstellen.")
  f1 <- paste("Laut Formelsammlung: ", mml$mml)
  list(q = list(q1, numericGap(mml$res, "eta2p")), fb = f1)
}

q1 <- anova_eta2p(mdl$tbl)$q

content = list("<p>", story, "</p>", "<p>",
               takahashi_explain, "</p>", mdl$tbl_html)
content <- append(content, q1)

render_qtijs(new("Entry", content = content))


#
# auch möglich:
#   - P-Wert aus Tabelle bestimmen
# - Vorherige Berechnung der mittleren Quadrate
# - Bestimmung der eta^2

## granovaGG has some nice data on anorexia

