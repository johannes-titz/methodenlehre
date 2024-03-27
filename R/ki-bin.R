# Wie groß ist der Standardfehler für die Stichprobenverteilung des gefundenen Anteils? Runden Sie auf 4 Dezimalstellen. <<3>> -->

# Wie groß sollte man N wählen, damit unabhängig vom gefundenen Anteil das 95%-Konfidenzintervall auf 1% genau bestimmt werden kann? -->

# Das exakte Intervall ohne Approximation ist: $[`r res_exact[1]`, `r res_exact[2]`]$

#' Exercise ki_bin
#' 
#' In this exercise you have to calculate a confidence interval for a proportion.
#' 
#' @return: Entry rqti object
#' @export
ki_bin <- function() {

  data <- get_data("ki_bin_data")
  N <- round(sample(c(c(50:150), 500:600, 1400:1500), 1))
  data <- data %>%
      dplyr::mutate(p = as.numeric(substr(Gesamt, 1, 4)) / 100) %>%
      dplyr::filter(N*p*(1-p) > 9) 
  row <- data %>%
      dplyr::sample_n(1)
  
  p <- row$p #sample(seq(0.4, 0.6, 0.01), 1)
  interval <- sample(c(90, 95, 99) / 100, 1)
  z <- round(qnorm(1L - (1L - interval) / 2L), 2)
  alpha <- 1 - interval
  z_q <- quote(qnorm(dfrac(alpha, 2L)))
  hook(se, sigma[roof(p)]) ;
  res <- quote(roof(p) %+-% {z * sqrt(dfrac(roof(p) * (1L - roof(p)), N))})
  eval_res <- round(eval(res), 4)
  
  var <- quote(dfrac(p*(1 - p), N))
  
  story <- glue::glue("<p>Laut Jacobi et al. (2016) ist die 12-Monats-Prävalenz in % (mit 95%-KI) für die ICD-10 Störung {row$Störung}: {row$Gesamt}. Die Studie basierte auf einer Fallzahl von 5303 Personen.</p>
  <p>Sie planen eine ähnliche Studie, diesmal jedoch mit einer Stichprobe von {N} Personen. Ihr Konfidenzniveau wählen Sie bei {interval*100}%. Wenn Sie als beste Schätzung für den wahren Anteil das Ergebnis von Jacobi et al. (2016) benutzen, also {p*100}%, welche Grenzen für das Konfidenzintervall würden sich dann ergeben? Wenn nötig, runden Sie Zwischenergebnisse auf 2 Dezimalstellen in % (z. B. 0.302484 entspricht 30.25%). Bitte geben Sie das Ergebnis auf 2 Dezimalstellen genau in % an. Z. B. 12.11 (für 12.11%), nicht jedoch 0.1211. Nutzen Sie die Approximation über die Normalverteilung.</p>")

  res_exact <- PropCIs::exactci(p*N, N, interval)$conf.int
  tolerance <- ceiling(abs(eval(res)-res_exact)*1e3)/10

  gap1 <- new("NumericGap", response_identifier = "ki-bin_ll",
    solution = round(eval(res)[1]*100, 2), 
    tolerance = tolerance[1], 
    expected_length = 4)
  gap2 <- new("NumericGap", response_identifier = "ki-bin_ul",
    solution = round(eval(res)[2]*100, 2),
    tolerance = tolerance[2], expected_length = 4)
  fb_text <- ki_bin_fb(res, interval, alpha, z_q, p, z, N)
  fb <- new("ModalFeedback", content = list(fb_text))
  new("Entry", content = list(story, "<p>Oberes Limit", gap1, "</p><p>Unteres Limit", gap2, "</p>" ))
}

ki_bin_fb <- function(res, interval, alpha, z_q, p, z, N) {
  glue::glue("Man sollte hier die Approximation über die Normalverteilung nutzen: {mathml(res)}. {mathml('p')} und {mathml('N')} sind gegeben. Die Konfidenz beträgt {mathml(interval)}, womit Alpha {mathml(alpha)} ist und {mathml('z=')} {mathml(z_q)} $=$ {mathml(eval(z_q))}

  <p>Das Ergebnis ist folgendes Intervall in %: {round(eval(res)*100, 2)} </p>")
}


