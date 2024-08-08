formular_einsicht <- function(n) {
  q <- lapply(seq(n), function(x) essay(title = paste0("Frage", x)))
  test(section(q), "einsicht")
}
