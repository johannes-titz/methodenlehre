#' Create questions for exercise lagemaße
#'
#' @param x This is the variable to create questions for.
#' @return data frame containing all information for questions for qti
#'
#' @export
lagemaße_qdf <- function(x) {
    # todo: generate feedback
    x <- sort(x)
    n <- length(x)

    # median
    hook(tiefe_med, Tiefe["Median"])
    tiefe_med_q <- quote(tiefe_med <- dfrac(n + 1L, 2L))
    eval(tiefe_med_q)
    med <- mean(x[c(ceiling(tiefe_med), floor(tiefe_med))])

    # quartiles
    hook(tiefe_q75, Tiefe["Q75"])
    hook(tiefe_q25, Tiefe["Q25"])
    tiefe_q25_q <- quote(tiefe_q25 <- dfrac(floor(tiefe_med) + 1L, 2L))
    tiefe_q75_q <- quote(tiefe_q75 <- n + 1L - dfrac(floor(tiefe_med) + 1L , 2L))
    eval(tiefe_q75_q)
    eval(tiefe_q25_q)

    # shortcut, if there are two values; if not, mean of the same value is
    # still the same value
    q_25 <- mean(x[c(ceiling(tiefe_q25), floor(tiefe_q25))])
    q_75 <- mean(x[c(ceiling(tiefe_q75), floor(tiefe_q75))])
    hook(q_25, Q["25"])
    hook(q_75, Q["75"])

    # IQA
    iqa_q <- quote(IQA <- q_75 - q_25)
    eval(iqa_q)

    # mean
    hook(mw, mean(x)) # shortcut, only for notation, is this for prolog only?
    mw_q <- quote(mw <- round(dfrac(sum(x), n), 2))
    eval(mw_q)

    # var
    hook(var, s^2L)
    var_q <- quote(var <- dfrac(sum((x - mw)^2L), n))
    eval(var_q)

    #sd
    sd_q <- quote(s <- sqrt(dfrac(sum((x - mw)^2L), n)))
    eval(sd_q)

    # mad
    mad_q <- quote(MAD <- dfrac(sum(abs(x - mw)), n))
    eval(mad_q)

    range_q <- quote(R <- max(x) - min(x))
    eval(range_q)

    solution <- tibble::lst(med, q_25, q_75, IQA,
                            mw, var, s, MAD, R)
    solution <- lapply(solution, round, 1)

    question <- c("Median", "Unteres Quartil", "Oberes Quartil",
                 "Interquartilsabstand", "Mittelwert", "Varianz",
                 "Standardabweichung", "Mittlerer absoluter Abstand",
                 "Range")
    feedback <- tibble::lst(tiefe_med_q, tiefe_q25_q, tiefe_q75_q,
                            iqa_q, mw_q, var_q, sd_q, mad_q, range_q)
    feedback <- sapply(feedback, mathml::mathml)
    solution_fb <- append(solution[-c(1:3)],
                          tibble::lst(tiefe_med, tiefe_q25, tiefe_q75), 0)
    feedback <- Map(function(x, y) paste(x, mathml(paste("=", y))),
                    feedback,
                    solution_fb)

    tolerance <- c(0, 0, 0, 0, 1, 1, 1, 1, 0)
    df <- as.data.frame(cbind(solution, question, tolerance, feedback))
    df
}

#' Create davis data set for lagemaße exercise
#'
#' @details
#'
#' This handles reasonable filtering. Note that it makes sense to use an even
#' n because this increases difficulty. At least for the formulas we use (Tiefe_Median = (n+1)/2 and Tiefe_Quartil = (|_Tiefe_Median_|+1)/2).
#'
#' Further note that the dv (Körpergröße and Körpergewicht) is actually the difference between reported and actual weight and height. I find it more interesting to see this difference as it reveals biases in human perception.
#'
#' @param dv "Körpergröße" or "Körpergewicht", which is actually the difference in height and weight, one randomly selected if dv = NULL (default).
#' @param n sample size, defaults to random selection of 8, 10, or 12, when n = NULL
#' @param seed seed to reproduce data set
#'
#' @return list with vector and story
#' @export
lagemaße_davis <- function(dv = NULL,
                           n = NULL,
                           seed = sample.int(1e3, 1)) {
    # der erste part ließe sich auslagern in eine funktion vector_davis
    set.seed(seed)
    if (is.null(dv)) dv <- sample(c("Körpergröße", "Körpergewicht"), 1)
    n <- ifelse(is.null(n), sample(c(8, 10, 12), 1), n)

    Davis <- carData::Davis
    Davis <- Davis %>%
        na.omit() %>%
        mutate(Körpergröße = weight - repwt,
               Körpergewicht = height - repht) %>%
        dplyr::filter(abs(Körpergewicht) <= 10, abs(Körpergröße) <= 10)
    vector <- Davis %>%
        sample_n(n) %>%
        pull(dv)
    story <- paste0("<p>Davis (1990) verglich die Körperwahrnehmung von Personen, die keinen oder viel Sport trieben (N=200). Unter anderem erfasste Davis die berichtete Körpergröße und die tatsächliche Körpergröße, sowie das berichtete Körpergewicht und das tatsächliche Körpergewicht. Aus dem Datensatz wurden zufällig ", length(vector), " Personen gezogen. Sie schauen sich die Variable ", names(dv), " an und bestimmen die Differenz zwischen tatsächlichem und berichtetem Wert.</p><p>Differenzen: ", paste0(vector, collapse = " "), "</p><p> Nun wollen Sie einige typische deskriptive Statistiken berechnen. Runden Sie, wenn nötig, auf 1 Dezimalstelle.</p>")
    tibble::lst(vector, story, seed, name = "Davis")
}

#'
#' Create qti item for lagemaße
#'
#' @param dv dependent variable, numeric vector
#' @param story story before questions
#' @param question which descriptives should be tested?
#' @param identifier identifier for assessment item
#'
#' @return Entry object of qti class
#'
#' @details
#' Note that identifiers for specific questions are automatically created.
#'
lagemaße2 <- function(dv, story,
                      question = c(
                          "Median", "Unteres Quartil", "Oberes Quartil",
                          "Interquartilsabstand", "Mittelwert", "Varianz",
                          "Standardabweichung", "Mittlerer absoluter Abstand",
                          "Range"),
                      identifier = "lagemaße") {
    # seed einbeziehen?
    # questions by question_df berücksichtigen?
    # generate qti questions from dv
  qdf <- lagemaße_qdf(dv)
  qdf <- qdf %>%
      dplyr::filter(question %in% !!question)
  rows <- split(qdf, 1:nrow(qdf))


  res <- unlist(lapply(rows, df2gap), use.names = F)

  res <- append(res, story, 0)

  feedback1 <- paste0("Zunächst sollte die Variable sortiert und die Tiefe notiert werden:",
                      kableExtra::kable_styling(
                          full_width = F,
                          position = "left",
                          knitr::kable(t(cbind("Wert" = sort(dv),
                                               "Tiefe" = seq(dv))), "html")
                      ),
                      "Nun lassen sich die Statistiken einfach berechnen. Beachten Sie, dass bei dem Median und den Quartilen die Tiefe berechnet wird. Die finale Lösung müssen Sie dann bei der Tiefe der sortierten Variable ablesen.")
  feedback <- paste("<summary>", qdf$question, "</summary>", "<p>",
                    qdf$feedback, "</p>")
  # use <details open> to open it
  feedback <- paste("<details>", feedback, "</details>", collapse = "")
  feedback <- paste(feedback1, feedback, collapse = "")
  feedback <- new("ModalFeedback", content = list(feedback))

  exercise <- new("Entry", content = res,
                  identifier = identifier,
                  feedback = list(feedback))
  exercise
}

#'
#' question vector that can be used as input for lagemaße functions
lagemaße_question <- function() {unlist(lagemaße_qdf(1:10)$question)}

#'
#'
#' lagemaße creates a qti Entry exercise for descriptive statistics
#'
#' @study list with elements vector, story and seed
#' @question question data frame
#'
#' @return Entry object of qti class
#'
#' @details
#' Check out the defaults, which are just functions to generate vector, story
#' and the question data frame.
#' @export
lagemaße <- function(study = lagemaße_davis(), question = lagemaße_question()) {
    lagemaße2(study$vector, study$story,
              identifier = paste0("lagemaße", study$name, "S", study$seed),
              question = question)
}

#' returns list of seed lagemaße exercises
#'
#' @details
#' Note that variance is computing extensive, so you might not want to use that
#' for an exam.
#'
#' @export
lagemaße_klausur <- function(seed, question = lagemaße_question()[c(1:3, sample(4:5, 1))]) {
    lapply(seed, function(x) lagemaße(lagemaße_davis(seed = x), question))
}

#'
#' generate first section exercises, focusing on simple measures
#' @return qti object of class Entry with 3 gaps
s1 <- function(seed) {
    lagemaße(lagemaße_davis("Körpergewicht", seed = seed),
             question = sample(c("Median", "Unteres Quartil", "Oberes Quartil", "Interquartilsabstand", "Range"), 3))
}

#'
#' generate second section exercises, focusing on more complex measures.
#' @return qti object of class entry with 2 gaps
s2 <- function(seed) {
    lagemaße(lagemaße_davis("Körpergröße", seed = seed),
             question = sample(c("Mittelwert", "Varianz", "Standardabweichung",
                                 "Mittlerer absoluter Abstand"), 2))
}

# für studis: vektor-länge, auswahl der statistiken
# gerade zahl, ungerade zahl, var/sd trennen?

#'
#' lagemaße default exercise for students with two sections, each containing
#' 20 different variants, the first focusing on simple calculations, the second
#' on more complex ones. One of each section is selected randomly. Stat tables and formulas are provided, as well as
#' the scientific calculator. This is OPAL-specific, not tested on other qti
#' platforms.
lagemaße_studis <- function() {
    s1_list <- lapply(1:20, s1)
    s2_list <- lapply(21:40, s2)
    s1 <- new("AssessmentSection", assessment_item = s1_list, selection = 1,
              identifier = "section1")
    s2 <- new("AssessmentSection", assessment_item = s2_list, selection = 1,
              identifier = "section2")
    test <- new("AssessmentTestOpal", section = list(s1, s2),
                identifier = "lagemaße",
                files = get_supplement_paths(),
                calculator = "scientific-calculator")
    test
}
