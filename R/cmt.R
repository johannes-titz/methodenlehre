
cmt_matrix <- function(seed) {
  set.seed(seed)
  v <- sort(round(rnorm(9, 100, 15)))
  m <- matrix(v[c(6, 2, 1, 9, 8, 5, 7, 4, 3)], byrow=T, nrow=3,
              dimnames = list(c("a", "b", "c"), c("x", "y", "z")))
  tbl <-  kableExtra::kable_styling(knitr::kable(m, format = "html", booktabs=T),
                                    full_width = F, position = "left")

  cmt_matrix <- entry("cmt_matrix", content = list(
    as.character(tbl),
    "<p>Für die gegebene Conjoint-Measurement-Matrix ist, unter Vernachlässigung von Messfehlern, die einfache Aufhebung (single cancellation)",
    inlineChoice(c("erfüllt", "nicht erfüllt"), points = 0.5, response_identifier = "singleCancellation"),
    " und die doppelte Aufhebung (double cancellation)",
    inlineChoice(c("nicht erfüllt", "erfüllt"), points = 0.5, response_identifier = "doubleCancellation"),
    ".</p>")
  )
  cmt_matrix
}

# Die Ausprägungen der AV müssen geordnet werden können
# die Funktion in P = f(A, X) muss zwingend additiv sein (P=A+X).

cmt_series <- function(seed) {
  set.seed(seed)
  v <- sort(round(rnorm(9, 100, 15)))
  while (length(unique(v)) < 8) v <- sort(round(rnorm(9, 100, 15)))
  m <- data.frame(c1 = c(v[1], v[3], v[5], "?"), c2 = c(v[c(2,4,6)], "Q=?"),
                  c3 = c("?", v[c(5,7,8)]))
  row.names(m) <- c(0, "T=?", 4, 5)
  tbl <-  kableExtra::kable_styling(
    knitr::kable(
      m, format = "html", booktabs=T,
      col.names = c(0, 1, 2)),
    full_width = F, position = "left"
  )
tbl

cmt_matrix <- entry("cmt_series", content = list(
  as.character(tbl),
  "<p>Die Tabelle zeigt die Mittelwerte der abhängigen Variable (AV) für eine Conjoint-Measurement-Matrix. Es wurde versucht, eine Standardfolge zu erstellen, wobei einige Werte noch nicht bestimmt wurden (fehlende Werte sind mit `?` gekennzeichnet). Die erste Zeile und die erste Spalte enthalten die Werte der Standardfolge, d. h., den Ausprägungen der unabhängigen Variablen (UVs) wurden konsistente Werte auf einer neuen Standardskala zugeordnet (0, 1, 2, 4, 5). Dabei wurde angenommen, dass die AV (P) sich additiv aus den UVs (A und X) ergibt, also \\(f_1(P)=f_2(A)+f_3(X)\\). Es fehlen mehrere Werte, doch relevant sind hier nur die Werte für T und Q. Damit die Standardfolge mit den bereits zugewiesenen bzw. ermittelten Werten konsistent ist, muss T=",
  gapNumeric(2, points = 0.5, expected_length = 3),
  " sein und Q=",
  gapNumeric(107, points = 0.5),
  " sein.</p>",
  "<p>Hinweis: Messfehler können vernachlässigt werden. Es ist daher davon auszugehen, dass die gegebenen Werte exakt bestimmt und korrekt sind.</p>")
)

  cmt_matrix
}
#render_qtijs(cmt_series(2025))

#
# ipip <- methodenlehre::ipip2
# de <- ipip %>%
#   filter(grepl("Germany", country))
# librarian::shelf(MPsychoR)
#
# data("YouthDep")
#
# enames <- grep("^CDI", names(You), value = T)
# de2 <- de[, enames]
# fit1 <- grm(YouthDep[, -27], constrained = F)
# plot(fit1, type = "OCCu", category = NULL) # "OCCUu", "OCCl"
# fsc <- factor.scores.grm(fit1, resp.patterns = de2)

