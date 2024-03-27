## todos
#
#- select randomly effect size from sedlmeier study, rs and Ns vary
#- make a test with a random seed, upload to opal, let hiwi check if everything is correct, save test somehow
#- formelsammlung checken, alles da?
#- split up the sections, random part should be separated for easier testing
#- maybe add more calculation examples?

ma_gap <- function(solution) {
    gap_numeric(solution, tolerance = 1, tolerance_type = "relative", 
                expected_length = 2)
}

ma_n <- function() {
      round(EnvStats::rnorm(1, 150, 25))
  }

random_r <- function() {
  round(EnvStats::rnormTrunc(1, 0.15, 0.3, 0.1, 0.7), 2)
}

study_d <- function() {
  # es d
  study_d <- tibble::lst(stat = round(EnvStats::rnormTrunc(1, 0.3, 1, 0.1, 1.5), 2), n = ma_n())
  study_d$n <- ifelse(study_d$n %% 2 == 0, study_d$n, study_d$n - 1)
  study_d$r_espkg <- d_to_r(study_d$stat)
  d <- study_d$stat
  n <- study_d$n
  mml <- mml_eq(r <- dfrac(d, sqrt(d^2L + 4L)), return_result = T)
  study_d$question <- glue::glue("Studie mit {mml_eq(d)} und {mml_eq(n)}, sodass {mathml('r==')}")
  study_d$fb <- mml$mml
  study_d$solution <- mml$res
  study_d$title <- "Effektgröße d"
  study_d
}

study_F <- function() {
  # es from F
  df <- vector()
  df["inn"] <- ma_n()
  r <- random_r()
  F <- round(r^2*df["inn"] / (1-r^2), 2) # check
  study_F <- tibble::lst(df_inn = df["inn"], 
                  stat = F, r = r)
  mml_r <- mml_eq(r <- dfrac(F, F + df["inn"]), T)
  mml_n <- mml_eq(n <- df["inn"] + 2L, T)
  study_F$n <- mml_n$res
  study_F$fb <- paste(paste0("<p>", c(mml_r$mml, mml_n$mml), "</p>"), collapse = "")
  study_F$solution <- mml_r$res
  study_F$question <- glue::glue("Studie mit {mml_eq(F)} und {mml_eq(df['inn'])}")
  study_F$title <- "Effektgröße F"
  study_F
}

study_t <- function() {
  df <- ma_n()
  r <- random_r()
  t <- round(sqrt(r^2*df/(1-r^2)), 2)
  study <- tibble::lst(df, r)
  study$r_espkg <- effectsize::t_to_r(t, df)$r
  mml_r <- mml_eq(r <- sqrt(dfrac(t^2L, t^2L+df)), T)
  mml_n <- mml_eq(n <- df + 2L, T)
  study$n <- mml_n$res
  study$fb <- paste(paste0("<p>", c(mml_r$mml, mml_n$mml), "</p>"), collapse = "")
  study$solution <- mml_r$res
  study$question <- glue::glue("Studie mit {mml_eq(t)} und {mml_eq(df)}")
  study$title <- "Effektgröße t"
  study
}

study_eta2 <- function() {
  # es form eta^2
  r <- round(random_r(), 2)
  eta <- r
  mml_r <- mml_eq(r <- sqrt(eta^2L), T)
  question <- glue::glue("Studie mit {mml_eq(eta^2L)} und {mml_eq(df)}")
  study <- tibble::lst(n = ma_n(), fb = mml_r$mml, 
                       solution = mml_r$res, question, r)
  study$title <- paste("Effektgröße", mathml(quote(eta^2L)))
  study
}

metaanalyse <- function() {
  #librarian::shelf(EnvStats, effectsize)

  l <- tibble::lst(study_eta2(), study_t(), study_F(), study_d())
  lnames <- names(l)
  d <- dplyr::bind_rows(l)

  d <- d %>%
      dplyr::mutate(id = lnames, solution = round(solution, 2),
      tolerance = 1, tolerance_type = "relative")
      
  sedlmeier <- 0.27
  sedlmeier_n <- 855
  
  rs <- c(sedlmeier, d$solution)
  Ns <- c(sedlmeier_n, d$n)
  mean_effect <- round(weighted.mean(rs, Ns), 2)
  questions <- unlist(df2gap(d), use.names = F)
  story <- ma_story(sedlmeier, sedlmeier_n)
  final_question <- list("Berechnen Sie nun die an der Stichprobe gewichtete mittlere Effektgröße, inklusive der Original-Studie von Sedlmeier et al. (2012).", ma_gap(mean_effect))
  final_fb <- ma_fb_final(rs, Ns)

  fb <- paste0("<details><summary>", c(d$title, "mittlere, gewichtete ES"), "</summary>", c(d$fb, final_fb), 
  "</details>")
  content <- list(story, final_question)
  content <- append(questions, story, 1)
  new("Entry", identifier = "metaanalyse", content = content)#, feedback = list(new("ModalFeedback", content = list(fb))))
  fb
}

ma_story <- function(r, n) {
  glue::glue("Sie lesen die Meta-Analyse von Sedlmeier et. al (2012) zur Wirkung von Meditation. Im Ergebnisteil wird ein Durchschnittseffekt von {mml_eq(r)} für die abhängige Variable Stressempfinden ({mml_eq(n)} berichtet. Sie kennen einige neuere Studien zur Wirkung von Meditation auf Stressempfinden und wollen einen aktualisierten Durchschnittseffekt berechnen. Da Sie keine Rohdaten haben, müssen Sie die Effektgrößen aus den berichteten Statistiken der Studien ausrechnen.

Folgende Statistiken stehen für die zusätzlichen Studien zur Verfügung. Gehen Sie davon aus, dass stets die Kontrollgruppe mit der Experimentalgruppe verglichen wurde. Berechnen Sie zunächst die Effektgröße {mathml('r')} für diese Studien. Runden Sie auf 2 Dezimalstellen!")
}


#
#Mittlere gewichtete Effektgröße: `r gap_helper(mean_effect) `
#
## feedback
#
#
#<!-- <details > -->
#<!--   <summary>Statistik $\eta^2$</summary> -->
#<!--   $\eta^2$ ist wie $r^2$ zu behandeln, man zieht also einfach die Wurzel und bekommt:  -->
#
#<!--   $$r=\sqrt{\eta^2} = \sqrt{  `r study_eta2$stat`  } = `r study_eta2$r`$$ -->
#<!-- </details> -->
#
#<details >
# <summary>Statistik $t$</summary>
# In der Formelsammlung ist die Umrechnung angegeben:
#
# $$r = \sqrt{\frac{t^2}{t^2+\mathrm{df}}} = \sqrt{\frac{  `r study_t$stat`  ^2}{`r study_t$stat`^2+\mathrm{`r study_t$df`}}} = `r study_t$r`$$
#
# </details>
#
#<details >
# <summary>Statistik $F$</summary>
#
# Bei 1 Freiheitsgrad (between), ist der $F$-Wert einfach nur $t^2$sodass ähnlich zum $t$-Beispiel gilt: 
# 
# $$r = \sqrt{\frac{F}{F+\mathrm{df_\mathrm{inn}}}} = \sqrt{\frac{ `r study_F$stat` }{`r study_F$stat` + `r study_F$df`}} = `r study_F$r`$$
#
#</details>
#
#
#
#```{r, include=F}
#rs_string <- paste(rs, collapse = "+")
#Ns_string <- paste(Ns, collapse = "+")
#rsNs_string <- paste(paste(rs, Ns, sep = r"(\cdot)"),
#                     collapse = "+")
#```
#
#<details>
# <summary>Mittlere Effektgröße</summary>

ma_fb_final <- function(r, n) {
  i <- 1:length(r)
  mml <- mml_eq(r["mean"] <- dfrac(sum_over(r[i]*n[i], i==1L, I), sum_over(n[i], i==1L, I)), T)

  glue::glue("<p>Die Effektgrößen sollten laut Aufgabenstellung mit der Stichprobengröße gewichtet werden. Bei {mathml('t')} und {mathml('F')} sind nur die Freiheitsgrade angegeben, man müsste also noch {mathml('+2')} rechnen (für 2 Gruppen). Bei den großen Fallzahlen und der Mittelwertsberechnung macht dies jedoch keinen merklichen Unterschied.</p>
<p>
Nicht zu vergessen ist natürlich der Originaleffekt, der durch das große {mathml('N')} den neuen Gesamteffekt immer noch dominieren wird.</p>
{mml$mml}")
}

#$$\bar{r} = \frac{ \sum_{i=1}^n r_i N_i }{\sum_{i=1}^n N_i }$$
#
# $$= \frac{`r rsNs_string` }{`r Ns_string` } = `r mean_effect` $$
#</details>
#
