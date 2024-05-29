

# createQtiTest(a, test_path("xmls"), verification = T)

library(selenider)
library(testthat)
library(methodenlehre)
#library(mathml)

test_that("anova with seed 1 gives 8 points", {
  # prepare
  a <- anova_stud(1)
  upload2opal(a, "pkg_anova", open_in_browser = F)
  ex <- anova_example(1)
  qs <- anova_questions(ex$mdl$tbl, ex$qs_ges)
  solutions <- sapply(qs, function(x) x$solution)

  # start selenider session
  session <- selenider::selenider_session()
  open_url("https://bildungsportal.sachsen.de/opal/auth/RepositoryEntry/44879740928")
  open_url("https://bildungsportal.sachsen.de/onyx/test?")
  #take_screenshot(view=T)
  inputs <- ss("input")
  #elem_set_value(inputs[[3]], "32")
  Map(function(x, y) elem_set_value(x, y), as.list(inputs)[3:10], solutions)
  #elem_set_value(inputs[[10]], solutions[8])
  #take_screenshot(view = T)

  # submit
  button <- s(".finish")
  elem_click(button)
  #take_screenshot(view = T)

  # check results
  s(".box-light") |>
    elem_expect(has_text("8 / 8"))
})

# test_that("anova is a valid xml file", {
#
# })

# this is useless because many tags are not allowed, such as details, math;
# also style cannot be used
# rqti:::verify_qti(read_xml(test_path("xmls/anova1.xml")))
