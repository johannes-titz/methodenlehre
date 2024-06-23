#' @export
methodenlehreI_resource_id <- function() {
  return("102403180398258")
}

#' @export
methodenlehreI_course_id <- function() {
  "26279182337"
}

#' @export
methodenlehreI_node_id <- function(shortName) {
  d <- get_course_elements(methodenlehreI_resource_id())
  d$nodeId[d$shortName == shortName]
}

#' Data from opal methodenlehreI course
#'
#' @param shortName Short name of exercise.
#' @param path where to download to
#' @export
methodenlehreI_data <- function(shortName, path = "data-raw/results/") {
  node_id <- methodenlehreI_node_id(shortName)
  get_course_results(methodenlehreI_resource_id(), node_id, path = path)
}

#' @export
methodenlehreI_elements <- function() {
  get_course_elements(methodenlehreI_resource_id())
}

#' @export
methodenlehreI_url <- function(shortName) {
  node_id <- methodenlehreI_node_id(shortName)
  course_id <- methodenlehreI_course_id()
  glue::glue("https://bildungsportal.sachsen.de/opal/auth/RepositoryEntry/{course_id}/CourseNode/{node_id}")
}
