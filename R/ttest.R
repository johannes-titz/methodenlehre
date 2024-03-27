#todo: tabellen updaten, sodass 0.001 auch geht
ttest_data <- function() {
  data(ipip2)
  data(ipipneo_items)

  countries <- sort(sample(unique(ipip2$country), 2))
  item <- ipipneo_items %>%
    dplyr::sample_n(1)

  data <- ipip2[ipip2$country %in% countries, c(item$item_char, "country")]
  data <- na.omit(data)
  data <- data[order(data$country),]
  names(data)[1] <- c("dv")

  alpha <- sample(c(0.05, 0.01), 1)
  hypothesis <- c("two.sided" = mathml(quote(mu["A"] != mu["B"])), 
                  "less" = mathml(quote(mu["A"] < mu["B"])),
                  "greater" = mathml(quote(mu["A"] > mu["B"])))
  actual_alphas <- c(alpha / 2, alpha, 1 - alpha) 
  df <- nrow(data) - 2
  critical_t <- lapply(actual_alphas, function(alpha) mml_eq(qt(alpha, df), T))
  critical_t <- plyr::ldply(critical_t, data.frame)
  df_results <- data.frame(hypothesis = names(hypothesis), mml = hypothesis,
                           alpha = actual_alphas, critical_t = round(critical_t$res, 3),
    mml_t = critical_t$mml)
  
  result <- df_results %>%
    dplyr::sample_n(1)

  labels <- ifelse(!item$reverse_coded, '1="very inaccurate" bis 5="very accurate"',
                   '1="very accurate" bis 5="very inaccurate"')

  ttest <- t.test(dv ~ country, data = data, var.equal = T, 
                  alternative = names(hypothesis), alpha = alpha)

  ttest_output <- capture.output(print(ttest))
  ttest_output <- paste("<pre>", paste(ttest_output, collapse = "<br/>"), "</pre>", collapse = "")

  #es <- effectsize::cohens_d(ttest)
  n <- as.numeric(table(data$country))
  t <- ttest$statistic

  tibble::lst(n, t, ttest_output, labels, countries, result, df, alpha)
}

ttest <- function() {
  ttest_data <- ttest_data()
  t <- ttest_data$t
  n <- ttest_data$n
  df <- ttest_data$df
  mml_d <- mml_eq(d <- t * dfrac((n[1] + n[2]), (sqrt(df * n[1] * n[2]))), T)
  mml_n <- mml_eq(n[2] <- df + 2 - n[1], T)

  #solution <- c(ttest_data$result$critical_t)
  #              as.numeric(ttest$p.value < alpha),
  #              mround(mml_d$res, 3),
ml#_n$res,

  #  fb2 <- 

  #fb <- c(part1, mml_d$mml, "Hier müssen wir nur schauen ob der p-Wert kleiner oder gleich Alpha ist.", mml_n$mml, result$mml_t)
  #extol <- c(abs(round(solutions[1], 3)*0.01), 0, 0, 0.041)
  #tolerance <- c(0.01, 0, 0, 0.041)
  #qdf <- data.frame(question, solution, tolerance, tolerance_type = "absolute")
}

ttest_story <- function(item_name, hypothesis, labels) {
  glue::glue("
<p>Johnson (2014) entwickelte einen offenen Big-Five-Fragebogen anhand eines großen Internet-Samples. Sie re-analysieren die Daten und schauen sich Unterschiede zwischen verschiedenen Ländern an. In einer Analyse vergleichen Sie das Item {item_name} (<i>I {item_content}</i>) mit den Ausprägungen von {labels} zwischen Teilnehmern aus {countries[1]} (n = {n[1]} und {countries[2]} (n nicht angegeben). Hierfür rechnen Sie einen t-Test mit der Hypothese H0: {hypothesis}
</p>
<p>
{ttest_output}
</p>
<p>
Beantworten Sie folgende Fragen. Runden Sie, wenn nötig, auf 3 Dezimalstellen.
</p>")
}

#ttest_questions <- function() {
#  * Wie groß ist die Effektgröße $d$? (1 Punkt) `r gap_numeric(solutions[1], tolerance = 0.01)`
#  * Ist das Ergebnis bei einem Alpha von `r alpha` signifikant? (0 = nicht signifikant, 1 = signifikant) (0.5 Punkte) `r gap_numeric(solutions[2])`
#  * Wie groß ist die Gesamt-Stichprobe? (1 Punkt) `r gap_numeric(solutions[3])` <!-- sollte raus, da statistiken schlecht-->
#  * Was ist der kritische $t$-Wert bei einem Alpha von `r alpha`. Wenn sie mehrere t-Werte angeben möchten, geben Sie nur den kleineren an. (1 Punkt) `r gap_numeric(solutions[4], tolerance = 0.041)`
#}

#feedback <- c("Die Effektgröße für unabhängige Stichproben kann man direkt aus dem t-Wert approximieren: $d = t\\cdot\\frac{n_A+n_B}{\\sqrt{\\mathrm{df}\\cdot n_A\\cdot n_B}}$. Eine Stichprobengröße steht schon da, die andere lässt sich aus den Freiheitsgraden berechnen (siehe Frage 3). Bitte beachten Sie, dass diese Effektgröße nur eine Approximation ist. Um den exakten Wert auszurechnen, brauchen Sie die Rohdaten.",
#"Das Ergebnis ist signifikant wenn $p\\leq\\alpha$.",
#"Die Gesamtstichprobe kann aus den Freiheitsgraden berechnet werden. Für unabhängige Stichproben gilt bei Varianzhomogenität: $\\mathrm{df} = n_A-1+n_B-1 = N-2$. Die Formel nach N umstellen und schon haben wir die Freiheitsgrade.",
#aussage)

fb_critical_t <- function(hypothesis, hypothesis_mml, critical_t, alpha, df, mml_t) {
  df120 <- ifelse(df > 120, glue::glue("Da die Tabelle bei {mathml(quote(df==120L))} endet, gehen wir approximativ von unendlich großen Freiheitsgraden aus."), "")
  part1 <- glue::glue("<p>Um den kritischen t-Wert zu ermitteln brauchen wir die Freiheitsgrade, die im Output angegeben sind: {mml_eq(df)}. {df120} </p> <p>Der kritische t-Wert hängt von der Alternativhypothese ab. In diesem Fall ist die Hypothese: {hypothesis_mml}.")

  aussage1 <- glue::glue("{part1} Stellen wir uns die t-Verteilung vor, erwarten wir also ein extremes Ergebnis auf der linken oder rechten Seite der Verteilung. Alpha muss in diesem Fall halbiert werden und es gibt zwei kritische t-Werte. Einen für die Fläche von {alpha/2} und einen für die Fläche von {1-alpha/2}. Da Tabellen üblicherweise die Integrale für große Flächenanteile angeben, schauen wir bei {1-alpha/2} und finden einen t-Wert von {-critical_t}. Der andere t-Wert ist exakt der gleiche, nur negiert: {critical_t}. Die Aufgabenstellung fordert uns auf diesen kleineren Wert anzugeben.</p>")

  aussage2 <- glue::glue("{part1} Stellen wir uns die t-Verteilung vor, erwarten wir also ein extremes Ergebnis auf der linken Seite der Verteilung. Alpha muss in diesem Fall nicht halbiert werden und es gibt nur einen kritischen t-Wert für die Fläche von {alpha}. Da Tabellen üblicherweise die Integrale für große Flächenanteile angeben können wir den t-Wert nicht direkt ablesen. Die t-Verteilung ist allerdings symmetrisch also schauen wir einfach bei {1-alpha} und finden einen t-Wert von {-critical_t}, den wir nur noch negieren müssen: {critical_t}.</p>") 

  aussage3 <- glue::glue("{part1} Stellen wir uns die t-Verteilung vor, erwarten wir also ein extremes Ergebnis auf der rechten Seite der Verteilung. Alpha muss in diesem Fall nicht halbiert werden und es gibt nur einen kritischen t-Wert für die Fläche {1 - alpha}. Da Tabellen üblicherweise die Integrale für große Flächenanteile angeben, können wir den t-Wert direkt bei {1-alpha} ablesen und finden einen t-Wert von {critical_t}.") 

  aussagen <- c(two.sided = aussage1, less = aussage2, greater = aussage3)
  aussage <- aussagen[hypothesis]
}
