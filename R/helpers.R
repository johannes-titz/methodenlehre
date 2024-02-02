
get_supplement_paths <- function() {
    path <- fs::path_package("supplements", package = "methodenlehre")
    dir(path, full.names = T)
}


render_qtijs <- function(question) {
    test <- new("AssessmentTest", section = list(section), identifier = "qtijs")
    createQtiTest(test, dir = "qtijs.zip")
    render_zip("qtijs.zip")
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

df2sc <- function(file) {

}

#'
#' Create sc table for df that has the same values in cols for each row
#'
#' @param df with values rows, cols, rows_id and cols_id
#'
#' @details
#' rows is what will be shown in the rows sc table; if you want a specific order
#' cols should be a factor
#'
df2sc2 <- function(df, identifier, title = identifier, shuffle = T) {
 df$cols <- as.factor(df$cols)
 df$cols_id <- as.factor(df$cols_id)
 mt <- new("OneInRowTable",
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

render_html <- function(x){
    dir <- tempfile()
    dir.create(dir)
    htmlFile <- file.path(dir, "index.html")
    writeLines(x, con = htmlFile)
    # (code to write some content to the file)
    rmarkdown::render(htmlFile, "html_document")
    rstudioapi::viewer(htmlFile)
}
