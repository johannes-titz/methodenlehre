# mlehre I Wiederholung 2024 SS
s <- 20242

sk <- skalenniveau_klausur(3, seed = s) #1.5
reg <- regression(seeds = s, which_questions = sample(1:11, 6)) # 6
z <- zstand(s) # 2
b <- bayes(s) # 5
t <- ttest(s) # 3.5
p <- power2()
l <- lage_klausur(s) # 4
w <- wissenschaftstheorie(s)
q <- qualitative(which_questions = 3:4)
# metaanalyse?
fp <- forschungsprozess()

ex <- list(sk, fp, w, l, z, b, t, reg, p, q)

sum(sapply(ex, function(x) x@points))
length(ex)

secs <- section(ex)

exam <- mlehreI(list(secs))

#
# rqti:::create_section_test(exam@section[[1]], ".", T)
# undebug(rqti:::create_section_test)
# debug(rqti:::buildAssessmentSection)
r(exam)

createQtiTest(exam, dir = "EXAMS")
#system("cp EXAMS/mlehreI2024.zip ~/mnt/opal/home/private/")

nodeId <- methodenlehreI_node_id("Prüfungsentwurf")
res <- get_resources()
resource_id <- res %>%
  filter(displayname == "MLI2024") %>%
  pull(key)
upload2opal(exam, "MLI2024", open_in_browser =  F)

update_course_test(methodenlehreI_resource_id(), nodeId, resource_id)
