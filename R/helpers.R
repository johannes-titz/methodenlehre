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
                       story, feedback = "") {
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
           points = nrow(df) / 2,
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
  if (sd(x) == 0) return(NA)
  if (sd(y) == 0) return(NA)
  cor(x, y)
}

#' transform r expression to mml with solution
mml_eq <- function(orig_expr, return_result = F, flags = list(quote(round(3L)))) {
  expr <- rlang::enexpr(orig_expr)
  # required, otherwise it is not evaluted in the environment where it was created
  res <- rlang::eval_tidy(rlang::enquo(orig_expr))[[1]]
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
