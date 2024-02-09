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
#' @export
df2gap <- function(df) {
    list("<p>", paste0(df$question, ": "),
         new("NumericGap", solution = as.numeric(df$solution),
             response_identifier = rownames(df), expected_length = 2,
             tolerance = as.numeric(df$tolerance), tolerance_type = "relative"),
         "</p>")
}

#' create sc table for df that has different values in cols for each row
#'
df2sc <- function(file) {

}

#' Create sc table for df that has the same values in cols for each row
#'
#' @param df with values rows, cols, rows_id and cols_id
#' @return OneInRowTable item object
#' @details
#' rows is what will be shown in the rows sc table; if you want a specific order
#' cols should be a factor
#'
df2sctable2 <- function(df, identifier, title = identifier, shuffle = T,
                   story) {
 df$cols <- as.factor(df$cols)
 df$cols_id <- as.factor(df$cols_id)
 mt <- new("OneInRowTable",
           content = list(story),
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
webshot <- function(url, file, width = 800, height = 2000) {
  system(paste0("npx capture-website-cli ", url,
                " --element='.content-container-inner'",
                " --output=", file,
                " --width=", width,
                " --height=", height,
                " --overwrite",
                " --full-page")
  )
}
