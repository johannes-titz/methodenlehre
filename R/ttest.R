#todo: tabellen updaten, sodass 0.001 auch geht
#todo: ggf. rundung auf 3 dezimalstellen um tabellen, oder rechner zulassen
ttest_data <- function() {
  ipip2 <- get_data(ipip2)
  ipipneo_items <- get_data(ipipneo_items)

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
  hypothesis <- html_to_xml(hypothesis)
  actual_alphas <- c(alpha / 2, alpha, 1 - alpha)
  df <- nrow(data) - 2
  critical_t <- lapply(actual_alphas, function(alpha) mml_eq(qt(alpha, df), T))
  critical_t <- plyr::ldply(critical_t, data.frame)
  df_results <- data.frame(hypothesis = names(hypothesis),
                           mml = hypothesis,
                           alpha = actual_alphas,
                           critical_t = round(critical_t$res, 3),
                           mml_t = critical_t$mml)

  result <- df_results %>%
    dplyr::sample_n(1)

  labels <- ifelse(!item$reverse_coded, '1="very inaccurate" bis 5="very accurate"',
                   '1="very accurate" bis 5="very inaccurate"')

  ttest <- t.test(dv ~ country, data = data, var.equal = T,
                  alternative = result$hypothesis, alpha = alpha)

  ttest_output <- capture.output(print(ttest))
  ttest_output <- gsub("<", "&#60;", ttest_output)
  ttest_output <- paste("<pre>", paste(ttest_output, collapse = "<br/>"),
                        "</pre>", collapse = "")

  #es <- effectsize::cohens_d(ttest)
  n <- as.numeric(table(data$country))
  t <- ttest$statistic
  p <- ttest$p.value
  tibble::lst(n, t, ttest_output, labels, countries, result, df, alpha, p,
              item)
}

ttest_d <- function(ttest_data) {
  t <- ttest_data$t
  n <- ttest_data$n
  df <- ttest_data$df
  mml_d <- mml_eq(d <- t * dfrac(n[1L] + n[2L], sqrt(df * n[1L] * n[2L])), T)
  solution <- mml_d$res
  q <- "Wie groß ist die Effektgröße d?"
  fb <- glue::glue("Die Effektgröße für unabhängige Stichproben kann man direkt aus dem t-Wert approximieren: {mml_d$mml}. Eine Stichprobengröße steht schon da, die andere lässt sich aus den Freiheitsgraden berechnen (siehe Frage 3). Bitte beachten Sie, dass diese Effektgröße nur eine Approximation ist. Um den exakten Wert auszurechnen, brauchen Sie die Rohdaten.")
  list(q = list("<p>", q, numericGap(solution, "d", tolerance = 1,
                                    tolerance_type = "relative"), "</p>"),
       fb = hug_fb(fb, q), solution = solution)
}

ttest_sig <- function(ttest_data) {
  alpha <- ttest_data$alpha
  q <- glue::glue("Ist das Ergebnis bei einem Alpha von {alpha} signfikant? (0 = nicht signfikant, 1 = signifikant)")
  p <- ttest_data$p
  solution <- as.numeric(p <= alpha)
  fb <- glue::glue("Das Ergebnis ist signifikant wenn {M(p<=alpha)}. Die korrekte Angabe wäre hier also {solution}.")
  list(q = list("<p>", q, numericGap(solution, "sig", points = 0.5), "</p>"),
       fb = hug_fb(fb, q), solution = solution)
}

ttest_krit <- function(ttest_data) {
  alpha <- ttest_data$alpha
  q <- glue::glue("Was ist der kritische t-Wert bei einem Alpha von {alpha}. Wenn sie mehrere t-Werte angeben möchten, geben Sie nur den kleineren an.")
  df <- ttest_data$df
  fb <- fb_critical_t(ttest_data$result$hypothesis, ttest_data$result$mml,
                      ttest_data$result$critical_t,
                      ttest_data$alpha, df, ttest_data$result$mml_t)
  solution <- ttest_data$result$critical_t
  list(q = list("<p>", q, numericGap(solution, "crit", tolerance = 0.041,
                                    tolerance_type = "absolute"), "</p>"),
       fb = hug_fb(fb, q), solution = solution)
}

ttest_n <- function(ttest_data) {
  n <- ttest_data$n
  df <- ttest_data$df
  mml_n <- mml_eq(n[2L] <- df + 2L - n[1L], T)
  q <- "Wie groß ist die Gesamtstichprobe?"
  fb <- glue::glue("Die Gesamtstichprobe kann aus den Freiheitsgraden berechnet werden. Für unabhängige Stichproben gilt bei Varianzhomogenität: {mml_n$mml}.")
  solution <- mml_n$res
  list(q = list("<p>", q, numericGap(solution, "n"), "</p>"),
       fb = hug_fb(fb, q),
       solution = solution)
}

ttest_one <- function(seed = sample.int(1e4, 1)) {
  set.seed(seed)
  ttest_data <- ttest_data()
  ttest_story <- ttest_story(ttest_data$item$item_char,
                             ttest_data$item$item_content,
                             ttest_data$result$mml,
                             ttest_data$labels, ttest_data$n,
                             ttest_data$countries,
                             ttest_data$ttest_output)
  q1 <- ttest_d(ttest_data)
  q2 <- ttest_sig(ttest_data)
  q3 <- ttest_n(ttest_data)
  q4 <- ttest_krit(ttest_data)
  q <- list(q1, q2, q3, q4)
  questions <- sapply(q, function(x) x$q)
  feedback <- unlist(lapply(q, function(x) textutils::HTMLdecode(x$fb, T, F, F)))

  content <- list(ttest_story)
  content <- append(content, questions)
  fb <- modalFeedback(list(feedback))
  new("Entry", identifier = paste0("ttest", seed),
      content = unlist(content), feedback = list(fb))
}

ttest_story <- function(item_name, item_content, hypothesis, labels, n,
                        countries, ttest_output) {
  glue::glue("
<p>Johnson (2014) entwickelte einen offenen Big-Five-Fragebogen anhand eines großen Internet-Samples. Sie reanalysieren die Daten und schauen sich Unterschiede zwischen verschiedenen Ländern an. In einer Analyse vergleichen Sie das Item {item_name} (<i>I {item_content}</i>) mit den Ausprägungen von {labels} zwischen Teilnehmern aus Gruppe A: {countries[1]} (n = {n[1]}) und Gruppe B: {countries[2]} (n nicht angegeben). Hierfür rechnen Sie einen t-Test mit der Hypothese H1: {hypothesis}
</p>
<p>
{ttest_output}
</p>
<p>
Beantworten Sie folgende Fragen. Runden Sie, wenn nötig, auf 3 Dezimalstellen.
</p>")
}

fb_critical_t <- function(hypothesis, hypothesis_mml, critical_t, alpha, df, mml_t) {
  df120 <- ifelse(df > 120, glue::glue("Da die Tabelle bei {mathml(quote(df==120L))} endet, gehen wir approximativ von unendlich großen Freiheitsgraden aus."), "")
  part1 <- glue::glue("<p>Um den kritischen t-Wert zu ermitteln brauchen wir die Freiheitsgrade, die im Output angegeben sind: {mml_eq(df)}. {df120} </p> <p>Der kritische t-Wert hängt von der Alternativhypothese ab. In diesem Fall ist die Hypothese: {hypothesis_mml}.")

  aussage1 <- glue::glue("{part1} Stellen wir uns die t-Verteilung vor, erwarten wir also ein extremes Ergebnis auf der linken oder rechten Seite der Verteilung. Alpha muss in diesem Fall halbiert werden und es gibt zwei kritische t-Werte. Einen für die Fläche von {alpha/2} und einen für die Fläche von {1-alpha/2}. Da Tabellen üblicherweise die Integrale für große Flächenanteile angeben, schauen wir bei {1-alpha/2} und finden einen t-Wert von {-critical_t}. Der andere t-Wert ist exakt der gleiche, nur negiert: {critical_t}. Die Aufgabenstellung fordert uns auf diesen kleineren Wert anzugeben.</p>")

  aussage2 <- glue::glue("{part1} Stellen wir uns die t-Verteilung vor, erwarten wir also ein extremes Ergebnis auf der linken Seite der Verteilung. Alpha muss in diesem Fall nicht halbiert werden und es gibt nur einen kritischen t-Wert für die Fläche von {alpha}. Da Tabellen üblicherweise die Integrale für große Flächenanteile angeben können wir den t-Wert nicht direkt ablesen. Die t-Verteilung ist allerdings symmetrisch also schauen wir einfach bei {1-alpha} und finden einen t-Wert von {-critical_t}, den wir nur noch negieren müssen: {critical_t}.</p>")

  aussage3 <- glue::glue("{part1} Stellen wir uns die t-Verteilung vor, erwarten wir also ein extremes Ergebnis auf der rechten Seite der Verteilung. Alpha muss in diesem Fall nicht halbiert werden und es gibt nur einen kritischen t-Wert für die Fläche {1 - alpha}. Da Tabellen üblicherweise die Integrale für große Flächenanteile angeben, können wir den t-Wert direkt bei {1-alpha} ablesen und finden einen t-Wert von {critical_t}.</p>")

  aussagen <- c(two.sided = aussage1, less = aussage2, greater = aussage3)
  aussage <- aussagen[hypothesis]
}
