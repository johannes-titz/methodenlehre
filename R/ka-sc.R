ka_luecken <- function() {
  mygap <- function(solution, id) {
    gapTextOpal(solution, response_identifier = id, tolerance = 2, expected_length = 17, placeholder = "1 Wort", points = 0.5)
  }
  content <- list(
    "<p>Bitte füllen Sie folgende Lücken. Hinweis: alle Lücken wurden auf eine Länge normiert, sodass die Lückenlänge kein Indikator für die Länge der korrekten Lösung ist.</p>",
    "<p>Die ", mygap(c("Kontrastanalyse", "Kontrast-Analyse", "Kontrast Analyse"), "ka"), "ist eine Sonderform der Varianzanalyse, die zwei entscheidende Vorteile besitzt. Zum einen erlaubt sie den Test ", mygap(c("präziser", "präziserer", "genauer", "genauerer", "spezifischer", "spezifischerer"), "precise"), " Alternativhypothesen, während bei der Varianzanalyse immer nur eine so genannte ", mygap("Omnibus", "omnibus"), "-Alternativhypothese (irgendwelche Mittelwerte unterscheiden sich) getestet werden kann . Die Alternativhypothese wird anhand von ", mygap(c("Kontrastgewichten", "Lambdagewichten", "Gewichten", "Lambdas"), "weights"), " festgelegt und kann die Wahrscheinlichkeit einen tatsächlich vorhanden Effekt aufzudecken (diese Wahrscheinlichkeit wird auch als ",  mygap(c("Power", "Teststärke"), "power"), " bezeichnet) gegenüber einer Varianzanalyse deutlich erhöhen. Zum anderen ist es bei diesem Verfahren möglich ", mygap(c("Effektgrößen", "Effektstärken", "Effekte"), "es"), " zu berechnen, die eine Aussage darüber erlauben, wie sehr die Daten mit einer Hypothese übereinstimmen.</p>")
  entry("ka_luecken", content = content)
}


ca_sc <- function() {

}

ca_sc_questions <- function() {
  list(ca_advantages(), ca_res(), ca_lambda())
}

ca_advantages <- function(){
  choices <- c("lower_alpha" = "Reduzierung der Irrtumswahrscheinlichkeit",
               "precise" = "präzisere Hypothesen",
               "higher_power" = "höhere Teststärke",
               "es_interpretable" =  "besser interpretierbare Effektgrößen")

  singleChoice(identifier = "ca-advantages", choices = choices,
               choice_identifiers = names(choices),
               content = list("Was ist <i>kein</i> ausgewiesener Vorteil der Kontrastanalyse gegenüber der Varianzanalyse?"), points = 0.5
  )
}


ca_res <- function() {
  q <- glue::glue("{M(r['effect size'])} ist die Korrelation der Kontrastgewichte mit den ...")
  choices <- c("lambda_data" = "Daten.",
               "lambda_lambda" = "Lambdas.",
               "lambda_mean" = "Mittelwerten der Daten.",
               "mean_data" = "standardisierten Mittelwerten der Daten.")
  singleChoice(identifier = "ca-res", choices = choices,
               choice_identifiers = names(choices),
               content = list(q), points = 0.5)
}

ca_lambda <- function() {
  q <- "Welche Voraussetzung müssen Lambda-Gewichte <u>nicht</u> erfüllen?"

  choices = c("normaldist" = "Sie müssen normalverteilt sein, damit das mittlere L einem t-Test unterzogen werden kann.",
              "theoretical" = "Sie müssen vor der Analyse theoretisch begründet werden.",
              "meanzero" = "Sie müssen in Summe Null ergeben.",
              "relative" = "Sie müssen in ihren Verhältnissen den definierten Erwartungen entsprechen.")
  singleChoice(identifier = "ca-lambda", choices = choices,
               choice_identifiers = names(choices),
               content = list(q), points = 0.5)
}

# ca_purpose <- function() {
#   q <- "Wofür bieten sich Kontrastanalysen besonders an?"
#
#   Answerlist
# ----------
#   *  zur Analyse von Störeinflüssen durch Kontrastvariablen (KVs)
# *  als non-parametrisches Äquivalent zur Varianzanalyse
# *  zur Zusammenfassung von Variablen zu Kontrasten (Lambdas)
# *  zur Überprüfung einer spezifischen Hypothese
