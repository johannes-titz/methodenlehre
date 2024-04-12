#' Example data for Bayes Revision
#'
#' Creates a random fourfould table for bayes exercise.
#'
#' @param seed Random seed
#' @return list with elements solution, solution_perc, solution_tree which are
#'   all vectors containg p_hit, p_miss, p_false_alarm, p_correct_rejection,
#'   p_revision, solution has decimal points, solution_perc percentages,
#'   solution_tree for a tree with 10k elements. In addition the list contains
#'   as numerics base_rate, sensitivity and specificity in decimals.
bayes_example <- function(seed = sample.int(1e3)) {
  set.seed(seed)
  ok <- FALSE
  while (!ok) {
    base_rate <- round(runif(1, 0.15, 0.30), digits = 2)
    sensitivity <- round(runif(1, 0.6, 0.8), digits = 2)
    specificity <- round(runif(1, 0.6, 0.8), digits = 2)

    p_hit <- base_rate * sensitivity
    p_miss <- base_rate * (1 - sensitivity)
    p_false_alarm <- (1 - base_rate) * (1 - specificity)
    p_correct_rejection <- (1 - base_rate) * specificity

    p <- unlist(tibble::lst(p_hit, p_miss, p_false_alarm, p_correct_rejection))
    ok <- sum(p) == 1 & all(p > 0) & all(p < 1) &
      abs(sensitivity - specificity) > 0.05
  }
  solution <- p
  solution["p_revision"] <- p_hit / (p_hit + p_false_alarm)
  solution_perc <- round(solution * 100)
  solution_tree <- round(solution * 10e3)
  return(tibble::lst(solution, solution_perc, solution_tree, base_rate,
                     sensitivity, specificity))
}

#' generate questions for bayes
#'
#' From bayes_example list, a question data frame is created that can be fed
#' to df2gap
#'
#' @param example bayes_example list
#' @return data frame for df2gap
bayes_qdf <- function(solution) {
  label <- c(
    hit = "tatsächlich geeignet UND vom Assessment-Center als geeignet eingeschätzt",
    miss = "tatsächlich geeignet UND vom Assessment-Center als ungeeignet eingeschätzt",
    false_alarm = "tatsächlich ungeeignet UND vom Assessment-Center als geeignet eingeschätzt",
    correct_rejection = "tatsächlich ungeeignet UND vom Assessment-Center als ungeeignet eingeschätzt"
  )
  label <- paste(
    "Wahrscheinlichkeit für die Konjunktion der zwei Ereignisse: ",
    label, " (in %)"
  )
  label <- c(
    label,
    "Nun wollen Sie abschließend für Ihren Bericht abschätzen, wie wahrscheinlich es ist, dass eine als geeignet eingeschätzte Führungskraft, tatsächlich geeignet ist (in %)")
  df <- data.frame(solution = solution, tolerance = 1,
                   tolerance_type = "absolute", question = label,
                   expected_length = 2)
  row.names(df) <- names(label)
  df
}

#' create plot for bayes exercise
#'
#' @param example list from bayes_example
#' @return currently only the captured print since svg does not work in opal?
#'
#' @importFrom data.tree Node SetEdgeStyle
#' @importFrom DiagrammeR  export_graph
bayes_plot <- function(example) {
  base <- 10e3
  base_rate <- example$base_rate
  freq <- example$solution * base

  root <- Node$new(base)
  suited <- root$AddChild(base_rate * base, geeignet = "ja")
  hit <- suited$AddChild(freq["p_hit"], geeignet = "ja", AC_geeignet = "ja")
  miss <- suited$AddChild(freq["p_miss"], geeignet = "ja", AC_geeignet = "nein")
  unsuited <- root$AddChild(base * (1 - base_rate), geeignet = "nein")
  false_alarm <- unsuited$AddChild(freq["p_false_alarm"], geeignet = "nein",
                                   AC_geeignet = "ja")
  correct_rejection <- unsuited$AddChild(freq["p_correct_rejection"],
                                         geeignet = "nein",
                                         AC_geeignet = "nein")

  SetEdgeStyle(suited, label = "geeignet")
  SetEdgeStyle(unsuited,label = "ungeeignet")
  SetEdgeStyle(hit, label = "als geeig. eingestuft")
  SetEdgeStyle(miss,label = "als ungeeig. eingestuft")
  SetEdgeStyle(correct_rejection, label = "als ungeeig. eingestuft")
  SetEdgeStyle(false_alarm,label = "als geeig. eingestuft")
  export_graph(data.tree::ToDiagrammeRGraph(root), "bayes.png")
  tf1 <- "bayes.png"
  txt <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")  # Convert the graphic image to a base 64 encoded string.
  image <- htmltools::HTML(paste0('<img src="data:image/png;base64,', txt, '"/>'))
  # Save the image as a markdown-friendly html object.

  output <- capture.output(print(root, "geeignet", "AC_geeignet"))
  output <- substring(output, 2)
  output <- gsub("levelName", "#Personen", output)
  # alternative, text based
  output <- paste("<pre>", paste(output, collapse = "<br/>"), "</pre>", collapse = "")
  image
}

#' generate story for bayes exercise
#'
#' @param example list form bayes_example
#' @return currently the story plus the questions
bayes_story <- function(example) {
  part1 <- glue::glue_data(example, "<p>Sie werden als externer Berater beauftragt, die Qualität eines Führungskräfte-Assessment-Centers einer großen Firma zu bewerten. Aufgrund theoretischer Überlegungen leiten Sie ab, dass ungefähr {base_rate*100} % aller Bewerber tatsächlich geeignet sind. Auf Basis der Daten vieler Bewerber und späterer Evaluationen ihrer Eignung, können Sie außerdem zwei weitere Wahrscheinlichkeiten abschätzen:</p>

<p>Sie wissen, dass von den Personen, die tatsächlich geeignet sind, {sensitivity*100} % vom Assessment-Center als geeignet eingeschätzt werden.</p>

<p>Von denjenigen, die tatsächlich ungeeignet sind, werden
{specificity*100} % vom Assessment-Center als ungeeignet eingeschätzt.</p>

<p>Zunächst wollen Sie die Konjunktions-Wahrscheinlichkeiten berechnen um anschließend bedingte Wahrscheinlichkeiten für falsche Entscheidungen abzuschätzen.</p>

<p>Geben Sie die Lösungen in PROZENT an und runden Sie auf ein ganze Zahl! (Zum Beispiel: Wenn Sie denken die Antwort ist 56.70%, geben Sie 57 in das Antwortfeld ein, NICHT jedoch 0.57.)</p>")

  questions_objects <- df2gap(bayes_qdf(example$solution_perc))
  story <- c(part1, unlist(questions_objects, use.names = F))
  story
}

#' generate feedback for bayes exercise
#'
#' @param example list from bayes_example
#' @return content for feedback
bayes_feedback <- function(example) {

  part1 <- paste("<p>Sie können auf die Formeln zurückgreifen, jedoch ist eine Visualisierung über ein Baumdiagramm die einfachste Möglichkeit diese Aufgabe zu lösen:</p>")

  part2 <- paste("<p>Hier wird insgesamt von 10000 Personen ausgegangen. Rechnet man die Häufigkeiten am Endes des Baums durch 10000, kommt man auf die Prozent-Werte für alle 4 Fälle. Diese sollen laut Aufgabenstellung auf ganze %-Werte gerundet werden, also, ",
  paste(example$solution_perc[1:4], "% ", collapse = " "),
  "</p>",
  "Die finale Wahrscheinlichkeit ergibt sich dann aus der Anzahl der Geeigneten unter allen als geeignet eingeschätzten (mit 100 multipliziert für %):",
  glue::glue_data(example$solution_perc,
                  "{p_hit} / ({p_hit} + {p_false_alarm}) * 100 = {p_revision} %"),

   "Beachten Sie, dass bei der letzten Berechnung die Zwischenergebnisse bereits auf ganze Prozente gerundet wurden. Der genauere Wert ist:",
  glue::glue_data(example$solution_tree,
                  "{p_hit} / ({p_hit} + {p_false_alarm}) * 100 = {p_revision/100} %."),
  "Die Toleranz bei der Aufgabe beträgt 1 Prozentpunkt."
  )

  plot <- bayes_plot(example)
  paste(part1, plot, part2)
}

#' Bayes exercise
#'
#' Creates one bayes exercise in qti format
#'
#' @param seed random seed
#' @return qti object of type Entry
bayes_one <- function(seed = sample.int(1e3, 1)) {
  ex <- bayes_example(seed = seed)
  story <- bayes_story(ex)
  questions <- bayes_qdf(ex)
  feedback <- bayes_feedback(ex)
  new("Entry", identifier = paste0("bayesS", seed), title = "Bayes",
      content = c(story),
      feedback = list(new("ModalFeedback", content = list(feedback))))
}

#' Bayes exercise
#'
#' Creates one bayes exercise in qti format
#'
#' @param seed random seed
#' @return qti object of type Entry
#' @export
bayes <- function(seed = sample.int(1e3, 1)) {
  b <- lapply(seed, bayes_one)
  if (length(b) == 1) b <- b[[1]]
  b
}

#
# bayes_studis <- function(seeds = 1:20) {
#   exercises <- lapply(seeds, bayes)
#   section <- new("AssessmentSection", identifier = "bayes",
#                  assessment_item = exercises, selection = 1)
#   test <- new("AssessmentTest", section = list(section))
#   test
# }
