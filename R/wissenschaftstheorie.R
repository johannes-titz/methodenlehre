#' Exercise wissenschaftstheorie
#'
#' 5 statements are randomly drawn from 13, student has to assign the correct
#' philosophy of sicence.
#'
#' @param seed seed for exercise
#' @export
wissenschaftstheorie <- function(seed = sample.int(1e3, 1)) {
  story <- "<p>Ordnen Sie bitte folgende Aussagen/Namen/Begriffe den Wissenschaftstheorien zu. Manche Begriffe spielen bei verschiedenen Wissenschaftstheorien eine Rolle, sind jedoch trotzdem immer einer ganz bestimmten Theorie zuortenbar, da sie dort besonders wichtig sind.</p>
  <p>
  <i>Hinweis</i>: Falls Aussagenlogiken (Formeln) vorkommen, steht B für Beobachtung und H für Hypothese.</p>"

  set.seed(seed)
  wt <- get_data(wt)
  df <- wt %>%
    #group_by(cols) %>%
    sample_n(5)
  df2sctable(df, story = story,
             identifier = "wissenschaftstheorie")
}
