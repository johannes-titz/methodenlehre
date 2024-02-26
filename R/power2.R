#' Power2 exercise
#'
#' In this single-choice table exercise, power2, you are presented with six
#' different scenarios related to a significance test setup. Your task is to
#' identify the impact on specific parameters when one of them is altered.
#'
#' @param case which case to construct, see get_data(power2), column id, default
#'   is one random case
#'
#' @return OneInRowTable
#'
#' @export
power2 <- function(case = sample(power2_cases(), 1)) {
  d <- get_data("power2")
  selection <- d %>%
    dplyr::filter(id == case)

  intro <- c("<p>Eine Studentin führt eine Power-Analyse durch und variiert dabei verschiedene Parameter, um die Auswirkungen auf andere Parameter zu untersuchen. Wählen Sie in der Tabelle die zutreffenden Veränderungen aus.</p><p>Hinweis: Konfidenz bezieht sich auf die Wahrscheinlichkeit die H0 anzunehmen, wenn sie stimmt.</p>")

  intro <- c(intro, paste0("<p>Was passiert wenn man <b>", selection$story[1],
                           "</b>?</p>"))
  selection <- na.omit(selection)
  df2sctable(selection, "power2", shuffle = F, story = intro,
             feedback = d$feedback[1])
}

#' Show all cases of power2
#'
#' A helper that returns ids of power2 to ease selection
#'
#' @export
power2_cases <- function() {
  unique(get_data(power2)$id)
}
