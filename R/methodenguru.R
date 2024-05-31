#' @export
methodenguru_resource_id <- function() {
  return("1671679683120887006")
}

#' @export
methodenguru_course_id <- function() {
  "38156107780"
}

#' @export
methodenguru_node_id <- function(shortName) {
  d <- get_course_elements(methodenguru_resource_id())
  d$nodeId[d$shortName == shortName]
}

#' Data from opal methodenguru course
#'
#' @param shortName Short name of exercise.
#' @param path where to download to
#' @export
methodenguru_data <- function(shortName, path = "data-raw/results/") {
  node_id <- methodenguru_node_id(shortName)
  get_course_results(methodenguru_resource_id(), node_id, path = path)
}

#' @export
methodenguru_elements <- function() {
  get_course_elements(methodenguru_resource_id())
}

#' @export
methodenguru_url <- function(shortName) {
  node_id <- methodenguru_node_id(shortName)
  course_id <- methodenguru_course_id()
  glue::glue("https://bildungsportal.sachsen.de/opal/auth/RepositoryEntry/{course_id}/CourseNode/{node_id}")
}
