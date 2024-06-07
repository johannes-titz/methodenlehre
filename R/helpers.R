#'
#' Gets all files in inst/supplements of methodenlehre package
#'
#' @importFrom fs path_package
#' @export
get_supplement_paths <- function() {
    path <- fs::path_package("supplements", package = "methodenlehre")
    dir(path, full.names = T)
}

#' df2gap
#'
#' Takes dataframe of questions and transforms it to NumericGap qti.
#'
#' @param data frame that has the same names as NumericGap class, except for
#' response_identifier, which is taken from the rownames.
#'
#' @return list of NumericGap objects wrapped in html paragraphs.
#' @import rqti
#' @export
df2gap <- function(df) {
  helper <- function(row) {
    list("<p>", paste0(row$question),
         new("NumericGap",
             solution = as.numeric(row$solution),
             response_identifier = row$id,
             expected_length = ifelse(is.null(row$expected_length), nchar(row$solution),
                                      row$expected_length),
             tolerance = as.numeric(row$tolerance),
             tolerance_type = row$tolerance_type),
         "</p>")
  }
  # this is somewhat more robust
  #df$id <- row.names(df)
  rows <- purrr::transpose(df)
  res <- unlist(lapply(rows, helper))
  res
}

#' create sc table for df that has different values in cols for each row
#'
df2sc <- function(file) {

}

#' Create sc table for df that has the same values in cols for each row
#'
#' @param df with values rows, cols, rows_id and cols_id
#' @return OneInRowTable item object
#' @details rows is what will be shown in the rows sc table; if you want a
#' specific order cols should be a factor
#'
#' Important: To have cols as pure distractors (never correct), cols and cols_id
#' should be a factor with additional levels!
#'
df2sctable <- function(df, identifier, title = identifier, shuffle = T,
                       story, feedback = "", points = nrow(df) / 2) {
 df$cols <- as.factor(df$cols)
 df$cols_id <- as.factor(df$cols_id)
 mt <- new("OneInRowTable",
           content = list(story),
           feedback = list(new("ModalFeedback", content = list(feedback))),
           rows = df$rows,
           rows_identifiers = df$rows_id,
           cols = levels(df$cols), # important to use levels!
           cols_identifiers = levels(df$cols_id), # same
           answers_identifiers = paste(df$rows_id, df$cols_id),
           points = points,
           title = title,
           identifier = identifier,
           shuffle = shuffle
 )
 mt
}

combine_expression <- function(g, h) {
    substitute(g*h, list(g=g, h=h))
}


equal_to <- function(g, h) {
    substitute(y == x, list(y=h, x=g))
}

#' @importFrom rstudioapi viewer
#' @importFrom rmarkdown render
render_html <- function(x){
    dir <- tempfile()
    dir.create(dir)
    htmlFile <- file.path(dir, "index.html")
    writeLines(x, con = htmlFile)
    # (code to write some content to the file)
    rmarkdown::render(htmlFile, "html_document")
    rstudioapi::viewer(htmlFile)
}

#'
#' @importFrom utils data
get_data <- function(...)
{
    e <- new.env()
    name <- data(..., envir = e)[1]
    e[[name]]
}

#' @importFrom purrr transpose
#'
df2sc2 <- function(d, question, choices = levels(d$cols),
                   shuffle = T) {
    sc <- function(d) {
        new("SingleChoice",
            content = list(question, d$rows),
            shuffle = shuffle,
            solution = d$cols,
            identifier = d$rows_id,
            choices = choices,
            points = 0.5,
            orientation = "horizontal"
        )
    }
    l <- purrr::transpose(d) # make data frame rows
    sc_list <- lapply(l, sc)
    sc_list
}

mlehre <- function(which_exam = c("mlehreI", "mlehreII"), sections) {
    new("AssessmentTestOpal",
        identifier = paste(which_exam[1], substr(Sys.Date()), 1, 4),
        section = sections,
        academic_grading = TRUE,
        grade_label = "VORLÄUFIGE Note: ",
        calculator = "scientific-calculator",
        files = get_supplement_paths())
}

mlehreI <- function(sections) {
    mlehre("mlehreI", sections)
}

mlehreII <- function(sections) {
    mlehre("mlehreII", sections)
}

#' take screenshot of opal website with capture-website-cli
#' requires capture-website-cli on system!
webshot <- function(url, file, width = 800, height = 600, quality = 1,
                    fullpage = T, selector, trim = T) {
  p <- processx::process$new("npx",
                        c("capture-website-cli",
                          url,
                          paste0("--element=", selector),
                          #"--delay 2",
                          paste0("--output=", file),
                          paste0("--width=", width),
                          paste0("--quality=", quality),
                          paste0("--height=", height),
                          "--overwrite",
                          ifelse(fullpage, "--full-page", "")),
  )
  p$wait(10000)
  p$kill()
  # remove whitespace
  if (trim) system(paste("mogrify -trim ", file))
  #system(paste0("convert -resize ", width, "x", height, " ", file, " ", file))
}

webshot_qtijs <- function(url, file) {
  webshot(url, file, selector = "body", trim = F)
}
webshot_opal <- function(url, file) {
  webshot(url, file, selector = ".content-container-inner",
          fullpage = T)
}

#' @export
mycor <- function(x, y) {
  x_ok <- sd(x) == 0
  y_ok <- sd(y) == 0
  x_ok <- ifelse(is.na(x_ok), F, T)
  y_ok <- ifelse(is.na(y_ok), F, T)
  if (!x_ok) return(NA)
  if (!y_ok) return(NA)
  cor(x, y)
}

#' transform r expression to mml with solution
mml_eq <- function(orig_expr, return_result = F, flags = list(quote(round(3L))),
                   round = 3) {
  expr <- rlang::enexpr(orig_expr)
  # required, otherwise it is not evaluted in the environment where it was created
  res <- rlang::eval_tidy(rlang::enquo(orig_expr))[[1]]
  res <- round2(res, round)
  res <- ifelse(as.integer(res) == res, as.integer(res), res)
  if (return_result) {
      return(list(mml = mathml(rlang::expr(!!expr == !!res), flags = flags), res = res))
  } else {
      return(mathml(rlang::expr(!!expr == !!res), flags = flags))
  }
}

M <- function(x){
  mathml(rlang::enexpr(x))
}

latexmath <- function(string, embrace = "$$") {
  file <- tempfile()
  file_out <- tempfile()
  writeLines(glue::glue("{embrace}{string}{embrace}"), file)
  rmarkdown::pandoc_convert(file, to = "html", from = "latex",
                                        options = "--mathml", output = file_out)
  readLines(file_out)
}

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

metadata_jt <- function(version = "") {
  qti_metadata(qti_contributor("Johannes Titz"),
               rights = "CC-BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/",
               version = version)
}

html_table <- function(kable) {
  kableExtra::kable_styling(
    full_width = F, position = "left",
    kable_input = kable
  )
}

#' put fb into a paragraph with details and summary
hug_fb <- function(fb, summary) {
  list("<p><details><summary>", summary, "</summary>",
       fb, "</details></p>")
}

#' Prepares data for subsequent analysis. This is for independent exercises, not
#' exams. Summarizes overall score for the exercise, which allows to calculate
#' r_it on item level.
prepare_items <- function(d, percent_complete = 0.5) {
  d2 <- d %>%
    # transform to numerics
    mutate(score_candidate = as.numeric(score_candidate),
           score_max = as.numeric(score_max)) %>%
    group_by(file) %>%
    # calculate overall score of candidate and how many answers were given
    mutate(score = sum(score_candidate),
           sum_answer_given = sum(as.logical(candidate_response != "")),
           N = n()) %>%
           #select(-id_question) %>% # always the same?
    filter(sum_answer_given/N >= percent_complete) # fully answered only
  d2
}

#' d has to be prepared accordingly so that it contains the overall score of the
#' candidate (score_candidate"). See `prepare_items`.
#'
#' @return tibble with id_answer, r_it, P, and n (number of answers)
analysis_items_default <- function(d, groups = c("id_answer")) {
  qs <- d %>%
    prepare_items() %>%
    group_by_at(groups) %>%
    summarize(r_it = mycor(score_candidate, score - score_candidate),
              P = mean(score_candidate) / mean(score_max),
              n = n()) %>%
    filter(!is.na(r_it))
  qs
}

#' exercise level, treat each exercise as unit, e.g. can be used for
#' skalenniveaus or for exams?

prepare_data_exercises <- function(d) {
  sk2 <- d %>%
    group_by(file) %>%
    mutate(score = sum(score_candidate),
           sum_answer_given = sum(is_answer_given),
           n = n()) %>%
    filter(sum_answer_given == max(n)) # only fully answered
  sk2
}

#' default analysis on exercises level, return r_it, difficulty (P), duration
#' (dur), number of items
analysis_exercises_default <- function(d) {
  sk3 <- d %>%
    prepare_data_exercises() %>%
    group_by(id_question) %>%
    summarize(r_it = cor(score_candidate, sum_score-score_candidate),
              P = mean(score_candidate) / mean(score_max),
              dur = mean(duration), n = n())
  sk3
}

copy_opal_archive <- function() {
  file.copy("/home/jt/mnt/opal/home/private/archive/methoden.guru", "data-raw/",
            recursive = TRUE)
}

screenshot_chromote <- function(url, height = 1080) {
  b <- ChromoteSession$new(height = height)
  b$Page$navigate(url)
  b$Page$loadEventFired()
  b$Page$navigate("https://bildungsportal.sachsen.de/onyx/test?")
  b$Page$loadEventFired()
  b$screenshot(selector = "#main-content", scale = 1.5)
}

html_to_xml <- function(input) {
  i2 <- gsub("&lt;", "&#60;", input)
  output <- gsub("&gt;", "&#62;", i2)
  textutils::HTMLdecode(output, T, F, F)
}
