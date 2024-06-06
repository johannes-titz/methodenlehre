# Wie groß ist der Standardfehler für die Stichprobenverteilung des gefundenen Anteils? Runden Sie auf 4 Dezimalstellen. <<3>> -->

# Wie groß sollte man N wählen, damit unabhängig vom gefundenen Anteil das 95%-Konfidenzintervall auf 1% genau bestimmt werden kann? -->

# Das exakte Intervall ohne Approximation ist: $[`r res_exact[1]`, `r res_exact[2]`]$

#' Exercise ki_bin
#'
#' In this exercise you have to calculate a confidence interval for a proportion.
#'
#' @return: Entry rqti object
#' @export
ki_bin <- function(seed = sample.int(1e4, 1)) {
  ex <- lapply(seed, ki_bin_one)
  if (length(ex) == 1) ex <- ex[[1]]
  ex
}

ki_bin_one <- function(seed = sample.int(1e4, 1)) {
  set.seed(seed)
  data_orig <- get_data("ki_bin_data")
  data_orig <- data_orig %>%
    dplyr::mutate(p = as.numeric(substr(Gesamt, 1, 4)) / 100)
  # repeat, until variance condition is satisfied
  data <- data.frame()
  while (nrow(data) == 0) {
    N <- round(sample(c(c(75:175), 500:600, 1400:1500), 1))
    data <- data_orig %>%
        dplyr::filter(N*p*(1-p) > 9)
  }

  row <- data %>%
      dplyr::sample_n(1)

  p <- row$p #sample(seq(0.4, 0.6, 0.01), 1)
  interval <- sample(c(90, 95, 99) / 100, 1)
  alpha <- 1 - interval
  mml_z_q <- mml_eq(z <- qnorm(dfrac(alpha, 2L)), T)
  z <- round(mml_z_q[[2]], 2)
  hook(se, sigma[roof(p)]) ;
  res <- quote(roof(p) %+-% {z * sqrt(dfrac(roof(p) * (1L - roof(p)), N))})
  eval_res <- round(eval(res), 4)[2:1]

  var <- quote(dfrac(p*(1 - p), N))

  story <- glue::glue("<p>Laut Jacobi et al. (2016) ist die 12-Monats-Prävalenz in % (mit 95%-KI) für die ICD-10 Störung {row$Störung}: {row$Gesamt}. Die Studie basierte auf einer Fallzahl von 5303 Personen.</p>
  <p>Sie planen eine ähnliche Studie, diesmal jedoch mit einer Stichprobe von {N} Personen. Ihr Konfidenzniveau wählen Sie bei {interval*100}%. Wenn Sie als beste Schätzung für den Populations-Anteil das Ergebnis von Jacobi et al. (2016) benutzen, also {p*100}%, welche Grenzen für das Konfidenzintervall würden sich dann ergeben? Wenn nötig, runden Sie Zwischenergebnisse auf 2 Dezimalstellen in % (z. B. 0.302484 entspricht 30.25%). Bitte geben Sie das Ergebnis auf 2 Dezimalstellen genau in % an. Z. B. 12.11 (für 12.11%), nicht jedoch 0.1211. Nutzen Sie die Approximation über die Normalverteilung.</p>
  <p>Weitere Hinweise (optional):
  <details><summary>Was ist eine 12-Monats-Prävalenz?</summary>
   <p>Die <i>12-Monats-Prävalenz</i> bezieht sich auf die Anzahl der Personen in einer bestimmten Population, die innerhalb der letzten 12 Monate vor der Erhebung an einer bestimmten Krankheit oder einem bestimmten Gesundheitszustand gelitten haben. Diese Prävalenzrate wird häufig in epidemiologischen Studien verwendet, um das Ausmaß einer Krankheit oder eines Gesundheitszustands in einer Bevölkerung zu quantifizieren.</p></details>
   <details><summary>Was ist das ICD-10?</summary>
   <p>ICD-10 steht für <i>Internationale statistische Klassifikation der Krankheiten und verwandter Gesundheitsprobleme - 10. Revision</i>. Es handelt sich um ein diagnostisches Klassifikationssystem, das von der Weltgesundheitsorganisation (WHO) entwickelt wurde und weltweit zur Klassifizierung und Codierung von Krankheiten, Gesundheitszuständen und damit zusammenhängenden Problemen verwendet wird.</p></details>
  </p>")

  res_exact <- PropCIs::exactci(p*N, N, interval)$conf.int
  tolerance <- ceiling(abs(eval_res-res_exact)*1e3)/10

  gap1 <- new("NumericGap", response_identifier = "ki_bin_ll",
    solution = round(eval_res[1]*100, 2),
    tolerance = tolerance[1],
    expected_length = 4)
  gap2 <- new("NumericGap", response_identifier = "ki_bin_ul",
    solution = round(eval_res[2]*100, 2),
    tolerance = tolerance[2], expected_length = 4)
  fb_text <- ki_bin_fb(res, eval_res, interval, alpha, mml_z_q, p, z, N)
  fb <- new("ModalFeedback", content = list(fb_text))
  new("Entry", identifier = paste0("ki_bin", seed),
      content = list(story, "<p>Unteres Limit (in %)", gap1,
                     "</p><p>Oberes Limit (in %)", gap2, "</p>" ),
      feedback = list(fb))
}

ki_bin_fb <- function(res, eval_res, interval, alpha, mml_z_q, p, z, N) {
  p. <- mathml(quote(roof(p)))
  N. <- mathml(quote(N))
  alpha. <- mathml(quote(alpha))
  fb <- glue::glue("<p>Man sollte hier die Approximation über die Normalverteilung nutzen: {mathml(res)}</p><p>{p.} und {N.} sind gegeben. Das Dach auf dem {p.} bedeutet einfach nur, dass es eine Schätzung für den Anteil in der Population ist. Wir haben ja nur den Wert aus der Stichprobe und wissen nicht was der genaue Wert in der Population ist. Die Konfidenz beträgt {mathml(interval)} womit Alpha {mathml(alpha)} ist und {mml_z_q[1]}. Diesen Wert kann man auch in einer üblichen z-Tabelle nachschlagen. Hierbei schaut man dann einfach bei {mml_eq(1L-alpha/2L)} da bei z-Tabellen üblicherweise große Flächenanteile angegeben sind. Halbieren muss man {alpha.} da das Konfidenzintervall in der Mitte liegt, man also links und rechts {mml_eq(alpha/2L)} abschneidet.</p>

  <p>Das Ergebnis ist folgendes Intervall in %: <math><mo>[</mo><mn>{eval_res[1]*100}</mn><mo>;</mo><mi>{eval_res[2]*100}</mi><mo>]</mo> </math></p>")
  fb <- textutils::HTMLdecode(fb)
  fb
}

ki_bin_stud <- function(seeds = 1:20) {
  ex <- lapply(seeds, ki_bin)
  s <- new("AssessmentSection", assessment_item = ex, selection = 1)
  new("AssessmentTestOpal", identifier = "ki_bin_stud",
      title = "Konfidenzintervall",
      section = list(s), calculator = "scientific-calculator",
      files = get_supplement_paths())
}

ki_bin_fb2 <- function(res, eval_res, interval, alpha, mml_z_q, p, z, N) {
  p_area <- katex_mathml(glue::glue("1-\\alpha/2={1-alpha/2}"), F)
  formula_ci <- katex_mathml(r"[\hat{p} \pm \sigma_{\mathrm{Anteil}}\cdot z_{\mathrm{Konfidenz}}]")
  sigma <- katex_mathml(r"[\sigma_{\mathrm{Anteil}}=\sqrt{\frac{\hat{p} (1-\hat{p})}{n}}]")
  z_q <- katex_mathml(glue::glue(r"[z=\phi^{-1}(\alpha/2)=<<round(as.numeric(mml_z_q[2]), 2)>>]", .open = "<<", .close = ">>"))
  pm <- katex_mathml('\\hat{p}', F)
  Nm <- katex_mathml('n', F)
  alpham <- katex_mathml('\\alpha', F)
  result <- katex_mathml(glue::glue(r"[[{eval_res[1]};{eval_res[2]}]]"), F)
  glue::glue("<p>Man sollte hier die Approximation über die Normalverteilung nutzen: {formula_ci} wobei {sigma} {pm} und {Nm} sind gegeben. Das Dach auf dem {pm} bedeutet einfach nur, dass es eine Schätzung für den Anteil in der Population ist. Wir haben ja nur den Wert aus der Stichprobe und wissen nicht was der genaue Wert in der Population ist. Die Konfidenz beträgt {interval}, womit Alpha {alpha} ist und {z_q}. Diesen Wert kann man in einer üblichen z-Tabelle nachschlagen. Hierbei schaut man dann einfach bei {p_area}, da bei z-Tabellen üblicherweise große Flächenanteile angegeben sind. Halbieren muss man {alpham}, da das Konfidenzintervall in der Mitte liegt, man also links und rechts {alpha/2} abschneidet.</p>
  <p>Das Ergebnis ist folgendes Intervall in %: {result*100}</p>")
}
