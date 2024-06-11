power_data <- function() {
  df <- c(seq(16, 100, 2), seq(1016, 1100, 2))
  es_d <- seq(0.2, 1.5, 0.1)

  grid <- expand.grid(df = df, es_d= es_d, alpha = seq(0.01, 0.1, 0.01))
  grid <- grid %>%
    dplyr::mutate(t = round(es_d*sqrt(df) / 2, 2),
           criterion = round(qt(1-alpha, df), 2),
           beta = round(pt(criterion - t, df), 2)) %>%
    dplyr::filter(beta >= 0.01, beta != alpha) %>%
    dplyr::sample_n(1)

  df <- grid$df
  t <- grid$t
  es_d <- grid$es_d
  criterion <- grid$criterion
  beta <- grid$beta
  alpha <- grid$alpha
  mml_n <- mml_eq(n <- df + 2L, T)
  mml_d <- mml_eq(d <- dfrac(2L*t, sqrt(df)), T)

  x <- seq(-10, 10, 0.001)
  density_null <- dt(x, df)
  df_null <- data.frame(x = x, y = density_null, hypothesis = "null")
  density_alternative <- dt(x, df, ncp = t)
  df_alternative <- data.frame(x = x, y = density_alternative, hypothesis = "alternative")

  d <- rbind(df_null, df_alternative)
  # to have better zoom for plots
  d <- d[d$y >= 0.001, ]

  # this should be improved
  d$below_criterion <- ifelse(d$x < criterion, "Below_criterion", "Above_criterion")
  d$region <- as.factor(paste0(d$hypothesis, d$below_criterion))
  d$case <- ifelse(d$region %in% c("nullBelow_criterion", "alternativeAbove_criterion"), 0, 1)
  tibble::lst(alpha, beta, criterion, d, t, df, es_d, mml_n, mml_d)
}

#' @import ggplot2
power_plot <- function(d, criterion, alpha, beta) {
  p <- ggplot(d, aes(x, y, fill = region)) +
    geom_line() +
    geom_vline(xintercept = criterion, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic(base_size = 14) +
  #  annotate(geom = "text", x=c(0, t), y = -0.01, label = c(0, t)) +
    #theme(legend.position = "none") +
    annotate(geom = "text", x = criterion*1, y = max(d$y)*1.07,
             label = "Kriterium", family = "Noto Sans") +
    scale_x_continuous("t") +
    scale_y_continuous("Dichte") +
    theme(legend.position = c(0.12, 0.9))#,
          #legend.title = element_text(size = 12),
          #legend.text = element_text(size = 12, family = "Noto Sans"))
          #text = element_text(size = 12, family = "Latin Modern Roman"))

    # a bit tricky because there are 4 cases
    #scale_fill_manual(values=c("#d8b365", "#5ab4ac", "#d8b365", "#5ab4ac"))

  col1 <- "#FFEDA0" # "#d8b365"
  col2 <- "#FEB24C" # "#5ab4ac"

  p1 <- p  +
    geom_ribbon(aes(ymin=0, ymax=y*case), alpha = 0.9) +
  #  annotate(geom = "text", x=criterion*c(0.8, 1.2), y = 0.01, label = c(beta, alpha)) +
    scale_fill_manual("Anteil Fläche", breaks = c("alternativeBelow_criterion", "nullAbove_criterion"),
                      values = rep(c(col1, col2), 2), #c("#d8b365", "#5ab4ac", "#d8b365", "#5ab4ac"),
    labels = as.character(c(beta, alpha)))

  p2 <- p +
    geom_ribbon(aes(ymin=0, ymax=y*!case), alpha = 0.7) +
  #  annotate(geom = "text", x=criterion*c(0.8, 1.2), y = 0.01, label = c(1-alpha, 1-beta)) +
    scale_fill_manual(name = "Anteil Fläche", breaks = c("alternativeAbove_criterion", "nullBelow_criterion"),
                      values = rep(c(col1, col2), 2), # c("#d8b365", "#5ab4ac", "#d8b365", "#5ab4ac"),
    labels = c(as.character(c(1-beta, 1-alpha))))

  #s <- svglite::svgstring(standalone = F)
  final_plot <- sample(list(p1, p2), 1)[[1]]
  #print(final_plot)
  #s()
  ggsave("power.png", final_plot, width = 10*0.7, height = 8*0.7, dpi = "print", device=ragg::agg_png)
  paste("<img width='650' src='", base64enc::dataURI(file = "power.png", mime = "image/png"), "'/>'")
}

power_story <- function(t, df) {
  study <- sample(c("in der Lesefähigkeit zwischen Jungen und Mädchen untersuchen. Ihre Hypothese ist, dass Mädchen besser lesen können als Jungen.", "zwischen Verhaltenstherapie und psychodynamischer Therapie untersuche. Ihre Hypothese ist, dass Verhaltenstherapie besser wirkt als psychodynamische Therapie.", "zwischen digitaler Lehre und Präsenzlehre untersuchen. Ihre Hypothese ist, dass Präsenzlehre besser ist als digitale Lehre."), 1)

  glue::glue("Sie planen eine Studie, bei der Sie den <i>Unterschied</i> {study} Vorab möchten Sie eine Power-Analyse durchführen und bitten einen Kollegen um Hilfe. Ihr Kollege visualisiert ein mögliches Szenario und sendet Ihnen folgende Hinweise:

<p>Dargestellt sind zwei t-Verteilungen für die Freiheitsgrade von {df}. Die Nullhypothese geht von einem Unterschied von 0 aus, somit auch einem t-Wert von 0. Die Alternativhypothese wurde nach Berücksichtigung eines realistisch zu erwartenden Effekts um einen t-Wert von {t} konstruiert. Die mit Farbe gefüllten Flächen sind mit den entsprechenden Wahrscheinlichkeiten dargestellt. Die zwei Untersuchungs-Gruppen sind gleich groß.</p>

<p>Sie sind sich nicht ganz sicher, ob Ihr Kollege eine sinnvolle Power-Analyse berechnet hat und stellen sich nun die folgenden Fragen für dieses Szenario.

Hinweis: Bitte geben Sie Wahrscheinlichkeitsangaben nur als Dezimalzahlen und NICHT in Prozent an. Z. B. 0.05, jedoch NICHT 5 (%).
</p>")
}

#```{r, echo = F, fig.cap="", dpi = 110, figh.width = 4*0.9, fig.height = 5}
#, fig.cap="Abbildung 1", fig.width = 400, fig.height = 300}

power_qdf <- function(alpha, beta, d, n) {
   df <- as.data.frame(rbind(
     c("Wie groß ist die Wahrscheinlichkeit die Nullhypothese abzulehnen, wenn sie stimmt?", alpha,  "alpha", 0),
     c("Wie groß ist die Wahrscheinlichkeit die Nullhypothese anzunehmen, wenn sie stimmt?", 1-alpha,  "one_minus_alpha", 0),
     c("Wie groß ist ist die Wahrscheinlichkeit die Alternativhypothese abzulehnen, wenn sie stimmt?", beta,  "beta", 0),
     c("Wie groß ist die die Wahrscheinlichkeit die Alternativhypothese anzunehmen, wenn sie stimmt?", 1-beta, "power", 0),
     c("Wie groß ist die angenommene Effektgröße d in der Population? Hinweis: Runden Sie auf zwei Dezimalstellen!", round(d, 2),  "d", 1),
     c("Wie groß ist die Gesamt-Stichprobe?", n,  "N_from_df", 0)))
  names(df) <- c("question", "solution", "id", "tolerance")
  df$tolerance_type <- "relative"
  df$expected_length <- 2
  df2gap(df)
}

#```{r echo=F, eval=F}
#librarian::shelf(easystats)
#t_to_d(t, df, alternative = "greater")$d
#t
#N
#```

power_fb <- function(mml_d, mml_n) {
  glue::glue(
  "<p>2 Wahrscheinlichkeiten sind immer gegeben, die anderen 2 ergeben sich auch der Gegenwahrscheinlichkeit. In den ersten vier Fragen geht es nur um die richtige Zuordnung der Wahrscheinlichkeiten zu den Parametern.</p>
<p>
1. Alpha ist die Fläche über dem Kriterium der Nullhypothesen-Verteilung (über der linken Verteilung).
</p><p>
2. 1-Alpha ist die Fläche unter dem Kriterium der Nullhypothesen-Verteilung (unter der linken Verteilung).
</p><p>
3. Beta ist die Fläche unter dem Kriterium der Alternativhypothesen-Verteilung (unter der rechten Verteilung).
</p><p>
4. Die Power ist die Fläche über dem Kriterium der Alternativhypothesen-Verteilung (über der rechten Verteilung).
</p><p>
Fragen 5 und 6 lassen sich über die Formelsammlung lösen:
</p><p>
5. Bei gleich großen Stichproben gilt approximativ: {mml_d}
</p><p>
6. Pro Mittelwert reduzieren sich die Freiheitsgrade um 1. Da es hier zwei Gruppen gibt gilt also {mml_n}")
}

#' Exercise power
#'
#' Visual exercise for significance test.
#'
#' @return Entry object
#' @export
power <- function(seeds = sample.int(1e4, 1)) {
  ex <- lapply(seeds, power_one)
  if (length(ex) == 1) ex <- ex[[1]]
  ex
}

power_one <- function(seed = sample.int(1e4, 1)) {
  set.seed(seed)
  d <- power_data()
  plot_string <- power_plot(d$d, d$criterion, d$alpha, d$beta)
  story <- (power_story(d$t, d$df))
  fb <- power_fb(d$mml_d$mml, d$mml_n$mml)
  q_list <- power_qdf(d$alpha, d$beta, d$mml_d$res, d$mml_n$res)
  content <- list(story, plot_string)
  content <- append(content, q_list, 3)
  entry <- new("Entry", identifier = paste0("power", seed), content = content)
}

power_stud <- function(seeds = 1:20) {
  ex <- power(seeds)
  s <- section(ex, selection = 1)
  test4opal(s, identifier = "power_stud",
            files = get_supplement_paths(),
            calculator = "scientific")
}
