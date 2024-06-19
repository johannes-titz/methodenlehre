# authors: Annika Sternkopf, Johannes Titz
# todo, clean up, produce output for students and for actual exam
# students: 4 exercises, maybe from 4 different studies?
# exam: 4 versions, with same scenario?
# maybe simplify and comment code, it is hard to read but it is
# also very general

#' r is the record with all information
#'
#' choices must start with choice
#' @import dplyr
choice_helper <- function(r) {
  choices <- r %>%
    dplyr::select(starts_with("choice")) %>% unlist()
  solution_num <- which(choices == r$solution)
  order <- (1:length(choices))[-solution_num]
  order <- c(solution_num, order)
  c("<p>",
    r$question,
    new("InlineChoice", response_identifier = r$q_id,
        solution_index = solution_num, points = 0.5,
        choices = choices),
    "</p>"
  )
}

# [s]election is a row (list) from the read in data frame
# the other stuff is taken from the global environment as the
# questions, choices and ids do not change
# only solutions need to be read from the selection
#
question_helper <- function(tbl) {
  records <- split(tbl, tbl$q_id)
  q_objs <- lapply(records, choice_helper)

  # avoid nested list for object creation!
  q_objs <- unlist(q_objs, use.names = F)
  feedback <- modalFeedback(list(tbl$feedback[1]))
  entry_dropdown <- new(
    "Entry",
    content = c(as.character(htmltools::p(tbl$story[1])), "<p>", q_objs, "</p>"),
    identifier = paste0("validity_", tbl$study[1], "_", tbl$scenario[1]),
    feedback = list(feedback)
  )
}

# now we can split the read in data frame and apply the helpers
# the result will be a list with all stories as objects

section_helper <- function(tbl, selection = 1) {
  d_split <- split(tbl, list(tbl$scenario, tbl$study))
  res <- lapply(d_split, question_helper)

  # wrap it up in a section
  new("AssessmentSection", assessment_item = res,
      selection = selection, identifier = paste0(tbl$study[1]),
      visible = T)
}

#' @export
validity_stud <- function(which_study = "haengebruecke", selection = 2) {
  d <- get_data("validity_data")
  d <- d %>%
    filter(study %in% which_study)
  # studenten
  d_studies <- split(d, d$study)
  sections <- lapply(d_studies, section_helper, selection = selection)

  top_section <- new("AssessmentSection", assessment_item = sections,
                     selection = length(which_study), identifier = "top")
  test <- new("AssessmentTestOpal", section = list(top_section),
              identifier = "validity_stud", title = "Validität")
  test
}

#' Exercise validity
#'
#' Currently only the Dutton & Aron 1974 study is included with different
#' scenarios. 3 questions regarding validity and design must be answered.
#'
#' @param scenario The scenario to choose from, see `validity_scenarios` for
#'   options
#' @export
validity <- function(scenario = sample(validity_scenarios(), 1)) {
  d <- get_data("validity_data")
  d <- d[d$scenario == scenario, ]
  question_helper(d)
}

#' @export
validity_scenarios <- function() {
  d <- get_data("validity_data")
  unique(d$scenario)
}
