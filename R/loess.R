runter_rauf <- function() {
  x <- -49:0
  y <- -x^2 + rnorm(length(x), sd = 200)
  y <- y/(1000)+12
  x <- x+50
  data.frame(x, y)
}

loess_sport <- function() {
  d <- runter_rauf()
  plot(d)
  y_pred <- predict(loess(y~x, data = d, span = 0.75))
  lines(y_pred, x = d$x)

  p <- ggplot(d, aes(x, y)) + geom_point() + geom_smooth(method = "loess") +
    theme_classic() + ggtitle("Abbildung 1")
  file <- tempfile(fileext = ".png")

  p2 <- ggplot(d, aes(x, y)) + geom_point() +
    geom_smooth(method = "loess", span = 0.3) +
    theme_classic() + ggtitle("Abbildung 2")
  #p3 <- gridExtra::grid.arrange(p, p2, nrow = 1)
  p3 <- cowplot::plot_grid(p, p2, nrow = 1)

  file2 <- tempfile(fileext = ".png")
  ggplot2::ggsave(file2, plot = p3, width = 10, height = 4)

  imgtag2 <- imgfile2tag(file2)

  content <- list(imgtag2, glue::glue("<p>In beiden Abbildungen (1 und 2) sind die gleichen Daten dargestellt, nur die eingezeichnete Kurve unterscheidet sich. Zu sehen ist die Laufgeschwindigkeit einer Person (y in km/h) in Abhängigkeit der Trainingssession (x in Wochen) über 50 Wochen.</p><p> Sie wollen diesen Zusammenhang modellieren. Wie könnten die Variablen transformiert werden, sodass der Zusammenhang linear wird? </p><p>Der Exponent {M(lambda)} in {M(x^lambda)} muss "),
  inlineChoice(c("kleiner 1 sein", "größer 1 sein"), response_identifier = "x",
               points = 0.5),
  glue::glue("oder der Exponent {M(lambda)} in {M(y^lambda)} muss"),
  inlineChoice(c("größer 1 sein", "kleiner 1 sein"), response_identifier = "y",
               points = 0.5),
  "</p>",
  "<p>In welcher Abbildung wurden mehr benachbarte Punkte in der Loess-Prozedur einbezogen?",
  inlineChoice(c("Abbildung 1", "Abbildung 2"), response_identifier = "img",
               points = 0.5),
  "</p>"
  )
  entry("loess_sport", content = content)
}
