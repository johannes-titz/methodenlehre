
#' helper function to extract parameters for regression exercise from lm model
#' @param model lm model object
#' @return list of important parameters, rounded to 3 decimal points
extract_parameters <- function(model) {
  data <- model$model
  fit <- lm.beta::lm.beta(model, complete.standardization = T)
  summary <- summary(fit)
  coef <- summary$coefficients

  b <- coef[2, "Estimate"]
  b_p <- coef[2, "Pr(>|t|)"] # problem
  b_se <- coef[2, "Std. Error"]
  b0 <- coef["(Intercept)", "Estimate"]
  b0_p <- coef["(Intercept)", "Pr(>|t|)"]
  beta <- coef[2, "Standardized"]

  x <- data[, 2]
  x_pos <- sample(which(x != 0), 1)
  x_random <- x[x_pos]
  y_actual <- data[x_pos, 1]
  # round before calculating!
  y_fitted <- round(b0, 3) + round(b, 3) * round(x_random, 3)
  resid_example <- round(y_actual, 3) - round(y_fitted, 3)

  sem <- summary(fit)$sigma

  # auslagern, da abweichend
  mypoly <- summary(lm(data[, 1] ~ poly(data[, 2], 4)))

  parameters <- tibble::lst(b, b_p, b_se, b0, b0_p, beta, x_random, y_fitted, y_actual, resid_example, sem, poly_r_squared = mypoly$r.squared)
  # round them all off; note that for calculations (y_fitted, resid_example) the rounding already happens for the involved terms!
  parameters <- lapply(parameters, round, 3)
  parameters
}

#' Prepare data for study Tovee
#'
#' @return list with two elements, data and story
prepare_tovee <- function() {
  d <- get_data(tovee)
  d[, 1] <- round(d[, 1] * 100)
  which_rows <- sample(1:nrow(d), size = sample(35:45, 1),
                       replace = T)
  data <- d[which_rows, 2:1]
  story <- paste0("In einer bekannten Studie untersuchten Tovée et al. (1999) wie weibliche Studierende die Attraktivität von Männern mit unterschiedlichen Körpermaßen bewerten. In der Studie kam heraus, dass der Waist-to-Chest-Ratio <em>wcr</em>, der beste Prädiktor für die Attraktivitätsbewertung <em>attract</em> ist. Der Waist-to-Chest-Ratio ist das Verhältnis zwischen Hüfte und Schultern (hier ausgedrückt in %). Sie finden einen Teil des Datensatzes im Internet, bestehend aus ", nrow(data), " bewerteten Männern.")
  tibble::lst(data, story)
}

prepare_prestige <- function() {
  Prestige <- carData::Prestige
  Prestige$women <- round(Prestige$women)
  names(Prestige)[3] <- "women%"
  Prestige$income <- Prestige$income / 1000
  which_rows <- sample(1:nrow(Prestige), size = sample(70:95, 1),
                       replace = FALSE)
  data <- Prestige[which_rows, sample(1:4, 2)]
  story <- paste0("Der <em>Prestige</em> Datensatz besteht aus 102 Berufen/Berufsgruppen eines Zensus in Kanada von 1971. Die Variablen sind unter anderem der durchschnittliche Bildungsgrad in Jahren (<em>education</em>), das durchschnittliche Einkommen in der Einheit 1000 USD pro Jahr (<em>income</em>), der Anteil an Frauen in % (<em>women%</em>) sowie der Pineo-Porter Prestige Score (<em>prestige</em>), der das Ansehen eines Berufs erfasst. Die Variablen beziehen sich alle auf die Berufsgruppe. ",
                 "Sie finden den Datensatz im Internet, jedoch ist dieser unvollständig mit nur ", nrow(data), " Berufen.\n")
  tibble::lst(data, story)
}

prepare_teaching_ratings <- function() {
  TeachingsRatings <- get_data("TeachingRatings")

  N <- nrow(TeachingRatings)
  which_rows <- sample(1:nrow(TeachingRatings), size = sample(round(N*0.9):N, 1),
                       replace = FALSE)
  data <- TeachingRatings[which_rows, sample(c("age", "beauty", "eval"), 2)]

  story <- paste0("Der `TeachingRatings` Datensatz besteht aus Bewertungen von ", N, " Dozenten von 2000-2002 an der University of Texas in Austin. Die Variablen sind unter anderem das Alter des Dozenten (`age`), die eingeschätzte Attraktivität des Dozenten aus einer vorherigen Studie (`beauty`) und die Bewertung der Lehrkompetenz (`eval`). ",
                 "In einer Replikationsstudie wurde die gleiche Untersuchung nochmals durchgeführt, diesmal mit ", nrow(data), " Dozenten.\n")
  tibble::lst(data, story)
}

prepare_davis <- function() {
  Davis <- carData::Davis
  Davis <- Davis[Davis$height >= 100, ]
  Davis <- na.omit(Davis)
  N <- nrow(Davis)
  which_rows <- sample(1:N, size = sample(round(N*0.15):N, 1),
                       replace = FALSE)
  data <- Davis[which_rows, sample(2:5, 2)]

  story <- paste0("Davis (1990) führte eine Studie zur Körperwahrnehmung und Gewicht durch (N=200). Er verglich Personen die Sport trieben, mit denen die keinen Sport trieben. Unter anderem erfasste Davis das Körpergewicht (<em>weight</em>) und die Körpergröße (<em>height</em>), sowie das wahrgenommene Körpergewicht (<em>repwt</em>) und die wahrgenommene Körpergröße (<em>repht</em>). ",
                 "In einer Replikationsstudie wurde die gleiche Untersuchung nochmals durchgeführt, diesmal mit ", nrow(data), " Probanden.")
  tibble::lst(data, story)
}

prepare_sleepstudy <- function() {
  sleepstudy <- lme4::sleepstudy
  which <- sample(sleepstudy$Subject, sample(10:17, 1))
  sleepstudy$Days <- sleepstudy$Days - 2
  data <- sleepstudy[sleepstudy$Subject %in% which & sleepstudy$Days >= 0, c("Reaction", "Days")]
  data$Reaction <- round(data$Reaction)
  story <- paste0("Belenky et al. (2003) führten eine Studie zu Schlafdeprivation durch und erfassten für 18 Probanden die Reaktionszeit in ms (<em>Reaction</em>) über mehrere Tage (<em>Days</em>). Tag 0 zählt hierbei als Baseline (es gab keine Schlafdeprivation). Sie schauen sich einen Teil des Datensatzes an mit insgesamt ", nrow(data), " Messungen von ", length(which), " Probanden.")
  tibble::lst(data, story)
}

prep_functions <- function() {
  c("prepare_tovee", "prepare_sleepstudy", "prepare_davis", "prepare_prestige")
  #, "prepare_teaching_ratings")
}

#' takes prepare function and produces a list with all variables needed
#' to create regression exercise
regression_example <- function(prepare_function, seed) {
    f <- get(prepare_function)
    example <- f()
    data <- example$data
    story <- example$story

    variables <- names(data)
    variables_md <- paste0("<em>", names(data), "</em>")
    model <- lm(data)
    model_sd <- lm.beta::lm.beta(model)

    story <- paste0(story, "<p>Sie berechnen eine Regression zwischen ", variables_md[2], " und ", variables_md[1], " und erstellen eine Abbildung:</p>")
    parameters <- extract_parameters(model)
    return(tibble::lst(model, model_sd, variables, variables_md, story, data, parameters, seed, prepare_function))
}

#' Create a html table for lm model
#'
#' writes out sjPlot regression table to table.html
#'
#' @param sem standard error of measurement, which is not available in model and
#'   not generated by tab_model
get_html_table <- function(model, sem) {
  filepath <- tempfile()
  col_order <- unlist(sample(list(c("est", "se"), c("std.est", "std.se"),

                                  "stat", "std.stat", "p", "std.p",
                                  "df.error", "response.level")))

  # use print method, otherwise print.sjTable ist not called
  print(sjPlot::tab_model(
    model,
    show.se = T,
    show.stat = T,
    show.p = T,
    show.fstat = T, show.df = F, show.r2 = F, digits.p = 3,
    show.std = T, string.pred = "Predictor", string.est = "b",
    string.se = "SE (b)", string.std = "beta",
    string.std_se = "SE (beta)", string.stat = "t-Wert",
    emph.p = F,
    show.aic = sample(c(T, F), 1),
    digits = 3,
    show.dev = sample(c(T, F), 1),
    show.loglik = sample(c(T, F), 1),
    title  = paste0("Residual Standard Error (Standardschätzfehler) = ", sem),
    col.order = col_order,
    file = filepath
  ))
  lines <- paste0(readLines(filepath), collapse = "")
  lines <- gsub("&lt;", "&#60;", lines)
  lines <- gsub("&gt;", "&#62;", lines)
  textutils::HTMLdecode(lines, hex = F, decimal = F)
}

prepare_html <- function(obj) {
    data <- obj$data
    parameters <- obj$parameters
    model <- obj$model
    variables <- obj$variables

    reg_plot <- qplot(data[, 2], data[, 1], alpha=I(0.5),
                      ylab = variables[1], xlab = variables[2]
    ) +
        theme_bw() +
        theme_classic()
    html_plot <- encodeGraphic(reg_plot)
    tab <- get_html_table(obj$model, obj$parameters$sem)
    story_part2 <- "Beantworten Sie folgende Fragen. Wenn nötig, runden Sie bei numerischer Eingabe auf <b>3 Dezimalstellen</b>. Beachten Sie, dass die Reihenfolge der Spalten in der Tabelle variiert."
    c(obj$story, html_plot, tab, story_part2)
}

#' encode ggplot graphic as png and return as html string <img> in base64
#' @param g ggplot2 graphic
encodeGraphic <- function(g) {
  tf1 <- tempfile(fileext = ".png")
  png(tf1, width = 600, height = 500, res = 125,
      type = "cairo")  # Get an unused filename in the session's temporary directory, and open that file for .png structured output.
  print(g)  # Output a graphic to the file
  dev.off()  # Close the file.
  txt <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")  # Convert the graphic image to a base 64 encoded string.
  myImage <- htmltools::HTML(paste0('<img src="data:image/png;base64,', txt, '"/>'))
  # Save the image as a markdown-friendly html object.
  return(myImage)
}

## combination table------
# grid <- expand.grid(prep_functions, 1:100, stringsAsFactors = F)
# res <- Map(regression_example, grid$Var1, grid$Var2)
#
# tbl <- lapply(res, function(x) data.frame(story = substr(x$story, 1, 10), seed = x$seed, c(x$parameters), x$prepare_function))
# tbl <- bind_rows(tbl)
# tbl
#
# tbl2 <- tbl %>%
#     mutate(linear_rel = (poly_r_squared - beta^2) <= 0.05,
#            no_nulleffect = beta^2 >= 0.01)
#
# tbl3 <- tbl2 %>%
#     filter(no_nulleffect, linear_rel)
#
# min <- min(table(tbl3$story))
#
#
# tbl3 %>%
#     group_by(story) %>%
#     summarize(mean(b_p > 0))

#' helper to create questions for regression exercise
#' q = question, s = solution, p = points, t = tolerance
get_questions <- function(parameters, variables_md) {
  row1 <- list(q = "Die Korrelation zwischen den beiden Variablen beträgt: ",
               s = parameters$beta,
               p = 0.5,
               t = 0,
               id = "cor")

  row2 <- list(q = paste0("Wenn ", variables_md[2], " um eine Einheit steigt, um wie viele Einheiten verändert sich dann ", variables_md[1], " im Modell?"),
               s = parameters$b,
               p = 1,
               t = 0,
               id = "b")

  row3 <- list(q = paste0("Wenn ", variables_md[2], " um eine Standardabweichung steigt, um wie viele Standardabweichungen verändert sich dann ", variables_md[1], " im Modell?"),
               s = parameters$beta,
               p = 1,
               t = 0,
               id = "beta")

  row4 <- list(q = paste0("Wenn ", variables_md[2], " 0 ist, welchen Wert nimmt ", variables_md[1], " im Modell an?"),
               s = parameters$b0,
               p = 1,
               t = 0,
               id = "intercept")

  alpha <- sample(c(0.001, 0.01, 0.05), 1)
  sig <- as.numeric(parameters$b_p <= alpha)
  row5 <- list(q = paste("Ist der Anstieg bei einem α von ", alpha, " statistisch signifikant? (1 = signifikant, 0 = nicht signifikant)"),
               s = sig,
               p = 1,
               t = 0,
               id = "sig")

  row6 <- list(q = paste0("Wenn man über das Modell die Variable ", variables_md[1], " vorhersagt, um wie viel weicht diese Vorhersage im Schnitt vom tatsächlichen Wert ab?"),
               s = parameters$sem,
               p = 1,
               t = 0,
               id = "sem")

  row7 <- list(q = "Was ist approximativ das UNTERE Limit des 68% Konfidenzintervalls für den unstandardisierten Anstieg? (Eine Berechnung aus den gegebenen Werten der Tabelle ist ausreichend.)",
               s = round(parameters$b - parameters$b_se, 3),
               p = 0.5,
               t = 1,
               id = "ci_ll")

  row8 = list(q = "Was ist approximativ das OBERE Limit des 68% Konfidenzintervalls für den unstandardisierten Anstieg? (Eine Berechnung aus den gegebenen Werten der Tabelle ist ausreichend.)",
              s = round(parameters$b + parameters$b_se, 3),
              p = 0.5,
              t = 1,
              id = "ci_ul")

  row9 = list(q = paste0("Welchen Wert nimmt ", variables_md[1], " im Regressionsmodell an, wenn ", variables_md[2], " ", parameters$x_random, " ist?"),
              s = parameters$y_fitted,
              p = 1,
              t = 1,
              id = "yhat")

  row10 = list(q = paste0("Wenn ", variables_md[2], " ", parameters$x_random, " ist, ist ein tatsächlich vorkommender Wert für ", variables_md[1], " ", parameters$y_actual, ". Wie groß ist in diesem Fall das Residuum?"),
               s = parameters$resid_example,
               p = 1,
               t = 2,
               id = "resid")

  row11 = list(q = "Wie groß ist R²?",
               s = round(parameters$beta**2, 3),
               p = 0.5,
               t = 1,
               id = "r2")

  df <- data.frame(dplyr::bind_rows(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11))

  ablesen <- "Dieser Wert lässt sich in der Tabelle einfach ablesen unter: "
  # prepare some useful hooks
  b <- c(parameters$b0, parameters$b)
  x <- parameters$x_random
  b0 <- parameters$b0
  b1 <- parameters$b
  SE_b1 <- parameters$b_se
  hook(b0, b[0L])
  hook(b1, b[1L])
  hook(SE_b1, SE(b[1L]))
  hook(e, roof(epsilon))
  y <- c(parameters$y_actual, parameters$y_fitted)
  y <- parameters$y_actual
  y_hat <- parameters$y_fitted
  hook(y_hat, roof(y))
  SE <- c(NA, parameters$b_se)

  # generate text
  answers <- c(paste0(ablesen, "beta (", variables_md[2], "). Der standardisierte Regressionskoeffizient entspricht Beta in der einfachen linearen Regression."),
               paste0(ablesen, "b (", variables_md[2], ")"),
               paste0(ablesen, "beta (", variables_md[2], ")"),
               paste0(ablesen, "b (Intercept)"),
               paste("Wenn,", mathml(quote(p <= alpha)), " ist das Ergebnis signifikant, ansonsten ist es nicht signifikant."),
               paste0(ablesen, "Residual Standard Error"),
               paste0("Hierfür muss man wissen, dass bei der Standardnormalverteilung zwischen -1SD und +1SD 68% der Werte liegen. Entsprechend lässt sich der gefragte Wert einfach berechnen über, ", mml_eq(b1-SE_b1), ". Der Wert ist allerdings nur eine Schätzung, da b t-verteilt ist und nicht normalverteilt. Das fällt nur bei sehr kleinen N (unter 20-30) ins Gewicht. In diesem Fall wäre das Konfidenzintervall kleiner als 68% (die Konfidenz wäre also geringer als wir annehmen)."),
               paste0("Siehe vorherige Erklärung. Der gefragte Wert lässt sich einfach berechnen über ", mml_eq(b1+SE_b1), "."),
               paste0("Hierfür muss man die Regressiongerade aufstellen und für: ", mml_eq(x), " lösen. Also: ", mml_eq(y_hat <- b0+b1*x)),
               paste0("Das Residuum ergibt sich aus ", mml_eq(e <- y-y_hat), ". Das Vorzeichen des Ergebnisses muss beachtet werden, denn es gilt: ", mathml(quote(e!=y_hat-y))),
               paste0("Hierfür muss man nur die Korrelation (das Beta des Anstieges) quadrieren."))

  df$feedback <- answers
  names(df) <- c("question", "solution", "points", "tolerance", "id", "feedback")
  df$tolerance_type <- "relative"
  df
}

#' Exercise regression
#'
#' In this exercise the student gets a regression table and graphic and must
#' answer 11 questions.
#'
#' @param study Study scenario, by default a random one is drawn. Check out the function `pref_functions()` for all studies.
#' @param seed seed for randomization
#' @param which_questions integer vector for question selection
#' @return rqti Entry object
#' @export
regression <- function(study = sample(prep_functions(), 1), seed = sample.int(1e6, 1),
                       which_questions = 1:11) {
  set.seed(seed)
  ex <- regression_example(study, seed)
  content <- prepare_html(ex)
  q <- get_questions(ex$parameters, ex$variables_md)[which_questions, ]
  questions <- df2gap(q)
  feedback <- new("ModalFeedback", title = "Feedback",
                  content = list(paste("<p><details><summary>", q$question, "</summary>",
                                       textutils::HTMLdecode(q$feedback), "</details></p>")))
  cont <- append(content, questions)
  new("Entry", identifier = paste0("regression", seed), content = cont,
      feedback = list(feedback))
}

#' Exercise regression (student versions)
#'
#' Creates several versions of `regression`, puts them into a section to select
#' one at random and puts the section into a test.
#'
#' @param seeds numeric vector of seeds
#' @return rqti Test object
#' @export
regression_stud <- function(seeds = 1:20) {
  ex <- lapply(seeds, function(x) regression(seed = x))
  s <- new("AssessmentSection", assessment_item = ex, selection = 1)
  test <- new("AssessmentTestOpal", identifier = "regression",
              section = list(s), files = get_supplement_paths(),
              calculator = "scientific-calculator")
  test
}
