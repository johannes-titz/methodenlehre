#' Exercise wissenschaftstheorie
#'
#' 5 statements are randomly drawn from 13, student has to assign the correct
#' philosophy of sicence.
#'
#' @param seed seed for exercise
#' @export
wissenschaftstheorie <- function(seed = sample.int(1e3, 1)) {
  set.seed(seed)
  wt <- get_data(wt)
  df <- wt %>%
    #group_by(cols) %>%
    sample_n(5)
  df2sctable(df, story = "", identifier = "wissenschaftstheorie")
}
