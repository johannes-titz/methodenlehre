# mayb add
# - find p value from table
# - calculate mean squares
# - calculate eta²
#
# granovaGG has some nice data on anorexia

anova_model <- function(iv_name, dv_name, data, hide_f = FALSE) {
  formula <- as.formula(
    glue::glue("{dv_name} ~ {iv_name} + Error(id/{iv_name})")
  )
  anova <- aov(formula, data = data)
  tbl <- as.data.frame(summary(anova)[[2]][[1]])
  tbl[, 2:4] <- round(tbl[, 2:4], 3)
  tbl$`F value` <- ifelse(hide_f, NA, tbl$`F value`)
  p <- tbl$`Pr(>F)`[1]
  tbl$`Pr(>F)`[1] <- ifelse(round(p, 3) != 0, round(p, 3),
                            format(p, scientific = TRUE, digits = 3))

  tbl_html <- html_table(knitr::kable(tbl, "html"))
  tbl_html <- gsub("&lt;", "&#60;", tbl_html)
  tbl_html <- gsub("&gt;", "&#62;", tbl_html)
  tbl_html <- gsub("NA", "", tbl_html)
  tbl_html <- as.character(tbl_html)
  tibble::lst(anova, tbl_html, tbl)
}

anova_example <- function(seed = sample.int(1:1e5, 1)) {
  set.seed(seed)
  stroop <- get_data("stroop")

  # within
  within <- stroop %>%
    filter(time == 1) %>%
    select(mode, id, vo2:percent_hrmax,
           reverse_stroop_neutral_test:stroop_interference) %>%
    # vary df_UV
    filter(mode %in% sample(unique(mode), sample(2:4, 1)),
           id %in% sample(id, sample(38:48, 1)))

  # participants

  iv_name <- "mode"
  dv_names <- names(within)[!(names(within) %in% c("mode", "id"))]
  dv_name <- sample(dv_names, 1)

  mdl <- anova_model(iv_name, dv_name, within, hide_f = TRUE)

  dv <- unlist(within[dv_name])
  qs_ges <- sum((dv - mean(dv))^2)
  list(mdl = mdl, story = takahashi_story(dv_name), qs_ges = qs_ges)
}

takahashi_story <- function(dv_name) {
  glue::glue("In der sportpsychologischen Forschung wird der Stroop-Test und seine Varianten häufig verwendet, um die Vorteile von sportlicher Betätigung auf die kognitive Funktion zu untersuchen. Angelehnt an die Studie von Takahashi und Gove (2020) nahmen junge Erwachsene an einem Within-Subjects-Experiment teil, bei dem die Sportart variiert wurde. Nach jeder Sportart absolvierten die Teilnehmer Stroop-Tests in neutraler und inkongruenter Form sowie die Reverse-Stroop-Tests in neutraler und inkongruenter Form. Erfasst wurde die Leistung bei jedem Test, sowie diverse Fitness-Maße. Die folgende ANOVA-Tabelle stellt die Ergebnisse dar, wobei <em>mode</em> die unabhängige Variable (Sportart) ist. Die abhängige Variable ist <em>{dv_name}</em>.")
}

takahashi_explain <- function() {
  c('<details><summary>Erklärung der Variablen (optional)</summary><table>
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
}

hug_fb <- function(fb, summary) {
  list("<p><details><summary>", summary, "</summary>",
       fb, "</details></p>")
}

anova_eta2p <- function(tbl) {
  QS <- tbl$`Sum Sq`
  names(QS) <- c("UV", "res")
  mml <- mml_eq(dfrac(QS["UV"], (QS["UV"]) + QS["res"]), TRUE)
  q1 <- glue::glue("Wie groß ist {M(eta['p']^2L)} für die unabhängige Variable? Geben Sie das Ergebnis als Dezimalzahl an und runden Sie auf 3 Dezimalstellen.")
  fb <- paste("Laut Formelsammlung: ", mml$mml)
  list(q = list("<p>", q1, numericGap(mml$res, "eta2p"), "</p>"),
       fb = hug_fb(fb, q1),
       solution = mml$res)
}

anova_eta2_person <- function(tbl, qs_ges) {
  QS <- tbl$`Sum Sq`
  names(QS) <- c("UV", "res")
  QS["ges"] <- qs_ges
  mml <- mml_eq(QS["Person"] <- QS["ges"] - QS["UV"] - QS["res"], T)
  QS["Person"] <- mml$res
  q1 <- glue::glue("Wie viel Varianz wird durch die Personenvariable an der Gesamtvarianz der AV erklärt? Bitte geben Sie den Wert als Dezimalzahl an und NICHT in Prozent. Runden Sie auf 3 Dezimalstellen.")
  mml2 <- mml_eq(dfrac(QS["Person"], QS["ges"]), T)
  f1 <- glue::glue("{M(QS['Person'])} lässt sich berechnen über: {mml$mml}. {mml_eq(QS['ges'])} ist gegeben, sodass sich der Varianzanteil ergibt: {mml2$mml}")
  list(q = list("<p>", q1, numericGap(mml2$res, "eta2person"), "</p>"),
       fb = hug_fb(f1, q1),
       solution = mml2$res)
}

anova_n_groups <- function(tbl) {
  df <- tbl$Df
  names(df) <- c("UV", "res")
  mml <- mml_eq(k <- df["UV"] + 1L, T)
  q1 <- glue::glue("Wie viele Bedingungen gab es im Experiment?")
  f1 <- paste("Laut Formelsammlung: ", mml$mml)
  list(q = list("<p>", q1, numericGap(mml$res, "ngroups"), "</p>"),
       fb = hug_fb(f1, q1),
       solution = mml$res)
}

anova_n_participants <- function(tbl) {
  df <- tbl$Df
  names(df) <- c("UV", "res")
  k <- df["UV"] + 1
  mml <- mml_eq(n <- dfrac(df["res"], (k - 1L)) + 1L, TRUE)
  q1 <- glue::glue("Wie viele Personen haben am Experiment teilgenommen?")
  f1 <- glue::glue("Hierfür kann man die Formel für die Freiheitsgrade der Residuen nach der Anzahl der Messungen {M(n)} umstellen, wobei {M(k)} die Anzahl der Bedingungen ist:", mml$mml)
  list(q = list("<p>", q1, numericGap(mml$res, "nparticip"), "</p>"),
       fb = hug_fb(f1, q1),
       solution = mml$res)
}

anova_significant <- function(tbl) {
  p <- tbl$`Pr(>F)`[1]
  alpha <- sample(c(0.05, 0.01, 0.001), 1)
  q <- glue::glue("Unterscheiden sich die Mittelwerte zwischen den Bedingungen statistisch signifikant voneinander bei einem Alpha von {alpha}? [0=nicht signifkant, 1=signifkant]")
  f <- glue::glue("Wenn {M(p<=alpha)}, ist das Ergebnis statistisch signifikant.")
  list(q = list("<p>", q,
                numericGap(as.numeric(p <= alpha), "significant"),
                "</p>"),
       fb = hug_fb(f, q),
       solution = as.numeric(p <= alpha))
}

anova_f <- function(tbl) {
  sigma <- tbl$`Mean Sq`
  names(sigma) <- c("UV", "res")
  mml <- mml_eq(F <- dfrac(roof(sigma)["UV"], roof(sigma)["res"]), TRUE, round = 3)
  q1 <- glue::glue("Wie groß ist der F-Wert?")
  f1 <- paste("Die mittleren Quadratsummen entsprechen den Varianzschätzungen, deren Verhältnis wiederum den F-Wert bildet:", mml$mml)
  list(q = list("<p>", q1, numericGap(mml$res, "fvalue"), "</p>"),
       fb = hug_fb(f1, q1),
       solution = mml$res)
}

# besser bei between?
anova_varpop <- function(tbl) {
  sigma <- tbl$`Mean Sq`
  names(sigma) <- c("UV", "res")
  q <- glue::glue("Wenn die H0 falsch wäre, was wäre dann basierend auf dem ANOVA-Output die beste Schätzung für die Populationsvarianz der abhängigen Variable?")
  f <- paste("Wenn die H0 nicht stimmt, dann lässt sich die Varianz der Mittelwerte zwischen den Gruppen nicht für die Schätzung der Populationsvarianz verwenden. Die beste Schätzung wäre also die durchschnittliche Varianz innerhalb der Gruppen (Mean Sq, Residuals):", sigma["res"])
  list(q = list("<p>", q, numericGap(as.numeric(sigma["res"]), "varpop"),
                "</p>"),
       fb = hug_fb(f, q),
       solution = as.numeric(sigma["res"]))
}

anova_fcritical <- function(tbl)  {
  alpha <- sample(c(0.05, 0.01, 0.1), 1)
  fcrit <- round(qf(1 - alpha, tbl[1, 1], tbl[2, 1]), 3)
  q <- glue::glue("Was ist der kritische F-Wert für ein Alpha von {alpha}?")
  f <- glue::glue("Der kritische F-Wert muss aus einer entsprechenden Tabelle abgelesen werden für ein Alpha von {alpha} und für die Freiheitsgrade (Zähler und Nenner) von: {tbl[1,1]} und {tbl[2,1]}. Der berechnete Wert auf 3 Dezimalstellen gerundet beträgt: {fcrit}")
  list(q = list("<p>", q, numericGap(fcrit, tolerance = 1,
                                     tolerance_type = "absolute"),
                "</p>"),
       fb = hug_fb(f, q),
       solution = fcrit)
}

anova_questions <- function(tbl, qs_ges) {
  q1 <- anova_eta2p(tbl)
  q2 <- anova_n_groups(tbl)
  q3 <- anova_n_participants(tbl)
  q4 <- anova_significant(tbl)
  q5 <- anova_f(tbl)
  q6 <- anova_eta2_person(tbl, qs_ges)
  q7 <- anova_varpop(tbl)
  q8 <- anova_fcritical(tbl)
  q <- list(q1, q2, q3, q4, q5, q6, q7, q8)
  q
}

#' anova exercise
#'
#' A realistic within ANOVA output is presented based on a real study. 8
#' questions have to be answered. This is the raw version, producing an rqti
#' Entry object.
#' @param seed Seed for the exercise. Default is a random seed drawn from 1:1e5.
#' @return rqti Entry object
#' @example a <- anova()
#' @export
anova <- function(seed = sample.int(1e5, 1)) {
  a <- anova_example(seed)
  q <- anova_questions(a$mdl$tbl, a$qs_ges)
  questions <- sapply(q, function(x) x$q)
  feedback <- unlist(lapply(q, function(x) textutils::HTMLdecode(x$fb)))

  QS <- NULL
  QS["ges"] <- a$qs_ges

  content <- list("<p>", a$story, "</p>",
                  "<p>", takahashi_explain(), "</p>",
                  a$mdl$tbl_html,
                  glue::glue("<p>Gegeben ist außerdem noch die Quadratsumme gesamt: {mml_eq(QS['ges'])}</p>"))
  content <- append(content, questions)
  fb <- modalFeedback(list(feedback))
  new("Entry", identifier = paste0("anova", seed),
      content = content, feedback = list(fb))
}

#' anova test (student version)
#'
#' A realistic within ANOVA output is presented based on a real study. 8
#' questions have to be answered. This is the student version, containing 20
#' versions by default wrapped into a QTI test. One version is selected randomly
#' by default.
#'
#' @param seed Seed for the exercise, can also be a vector to produce several
#'   versions of the exercise. Default is a vector from 1:20.
#' @param selection How many exercises should be selected. Has to be smaller
#'   than length of seed.
#' @example a <- anova_stud()
#' @return rqti test object
#' @export
anova_stud <- function(seed = 1:20, selection = 1) {
  x <- lapply(seed, anova_one)
  s <- new("AssessmentSection", visible = FALSE,
           assessment_item = x, selection = selection)
  test <- new("AssessmentTestOpal", identifier = "anova",
              section = list(s), files = get_supplement_paths(),
              calculator = "scientific")
  test
}
