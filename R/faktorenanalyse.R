# Parallel-Analyse ist nicht offensichtlich, man müsste es ergänzen
# ("rote Linie zeigt die Grenze aus der Parallelanalyse" oder so was)
# - Varianzaufklärung in Prozent? also z. B. 70% oder 0.70? Beispiel
# vorgeben

# Another classic data set is the 9 variable Thurstone problem which is discussed in detail by R. P. McDonald (1985, 1999) and and is used as example in the sem package as well as in the PROC CALIS manual for SAS. These nine tests were grouped by Thurstone and Thurstone, 1941 (based on other data) into three factors: Verbal Comprehension, Word Fluency, and Reasoning. The original data came from Thurstone and Thurstone (1941) but were reanalyzed by Bechthold (1961) who broke the data set into two. McDonald, in turn, selected these nine variables from the larger set of 17 found in Bechtoldt.2. The sample size is 213.

# The simFA function is rather complex and does not create reasonable loadings with default settings. It is easier to create your own h2 (communalities). partitions is useful to create different partitions of items per factor. With MonteCarlo we can actually get the raw data, which is more useful.
fa_data <- function(n_factors_model = sample(2:6, 1),
                       n_items = sample(9:12, 1)) {
  n_items_per_fac <- partitions::parts(n_items)
  n_items_per_fac <- n_items_per_fac[, sample(1:ncol(n_items_per_fac), 1)]
  communalities <- runif(n_items, 0.5, 0.8)
  out <-  fungible::simFA(Model = list(NFac = n_items,
                                       NItemPerFac=n_items_per_fac),
              MonteCarlo = list(NSamples = 1, Raw = TRUE),
              Loadings = list(h2 = communalities),
              CrossLoadings = list(ProbCrossLoad = 1E-12,
                   CrossLoadRange = c(0,0),
                   CrudFactor = .1))
  out$Monte$MCData[[1]]
}

fa_example <- function(n_factors_model = sample(2:6, 1)) {
  data <- fa_data(n_factors_model)
  # png("fa1.png", width = 800, height = 600)
  # psych::cor.plot(data, numbers=TRUE, xlas = 2)
  # dev.off()
  fa_parallel <- EFAtools::PARALLEL(data, eigen_type = "EFA", method = "ML",
                                    n_datasets = 100, n_factors = n_factors_model)
  # png("fa2.png", width = 800, height = 600)
  # dev.off()
  # img1 <- as.character(htmltools::img(src = base64enc::dataURI(file = "fa1.png",
  #                                                 mime = "image/png")))
  # img2 <- as.character(htmltools::img(src = base64enc::dataURI(file = "fa2.png",
  #                                                 mime = "image/png")))

  fa <- EFAtools::EFA(data, n_factors_model, method = "ML", rotation = "varimax")
  h2 <- rowSums(round(fa$rot_loadings, 2)^2)
  kaiser_num <- length(which(fa_parallel$eigenvalues_EFA[,1] > 1))
  explained_var <- fa_parallel$eigenvalues_EFA[, 1] / ncol(data)
  n_items <- ncol(data)
  tibble::lst(fa_parallel, fa, h2, n_factors_model, data, kaiser_num, n_items,
              explained_var)
}

fa_table_parallel <- function(fa_parallel) {
  # n_extract_factors <- sample(1:sum(fa_parallel$pc.values > 1), 1)
  # lim95 <- apply(fa_parallel$values, 2,function(x) quantile(x, .95))
  # tbl <- data.frame(cbind(eigenvalue = fa_parallel$pc.values, pa95limit = lim95))
  tbl <- fa_parallel$eigenvalues_EFA
  kableExtra::kable_styling(
    full_width=F, position = "left",
    kable_input = kable(round(tbl, 2), booktabs = T,
                        caption = "Faktoren-Lösung",
                        col.names = c("Eigenwert", "Mittelwert Eigenwert PA",
                                      "95 % Perzentil Eigenwert PA"))
  )
}

fa_table2 <- function(myfa) {
  output <- round(cbind(`Item Nummer` = 1:length(myfa$h2),
                        myfa$rot_loadings), 3)
  row.names(output) <- NULL
  tbl <-tbl <- kableExtra::kable_styling(
    kable_input = kable(output, booktabs = T, caption = "Faktoren-Lösung"),
    full_width = F,
    position = "left")
  tbl
}

faktorenanalyse <- function() {
  fa <- fa_example()
  p1 <- glue::glue("<p>Der berühmte Psychologe Thurstone hat im Jahr 1941 eine Studie zu Intelligenz durchgeführt, die Sie versuchen zu replizieren. Sie erheben Daten zu verschiedenen Intelligenz-Aufgaben bei 250 Probanden und möchten diese mit Hilfe einer Faktorenanalyse auswerten.</p><p>

Die folgende Tabelle zeigt die Eigenwerte und das 95%-Quantil der zufällig erzeugten Eigenwerte einer Parallelanalyse:</p>

{fa_table_parallel(fa$fa_parallel)}

<p>Wie viele Faktoren sollten nach dem Kaiserkriterium extrahiert werden?")
  q1 <- new("NumericGap", response_identifier = "kaiser", solution = fa$kaiser_num)
  p2 <- "</p><p>Wie viele Faktoren sollten nach der Parallelanalyse extrahiert werden?"
  q2 <- new("NumericGap", response_identifier = "pa", solution = fa$fa_parallel$n_fac_EFA)
  which_fac <- sample(seq(fa$n_items), 1)
  p3 <- glue::glue("Wie viel Varianz wird durch Faktor {which_fac} erklärt? Geben Sie den Wert als Dezimalzahl an (z. B. 0.43 für 43%) und runden Sie auf 2 Dezimalstellen.")
  q3 <- new("NumericGap", response_identifier = "var",
            solution = as.numeric(fa$fa_parallel$eigenvalues_EFA[which_fac, 1] / fa$n_items))
  eigenvalues <- kableExtra::kable_styling(
    kable_input = kable((fa$fa$vars_accounted[1,]), booktabs = T, caption = "Faktoren-Lösung"), full_width = F,
    position = "left")
  which_item <- sample(seq(fa$n_items), 1)
  p4 <- glue::glue("</p><p>Sie entschließen sich dazu, {fa$n_factors_model} Faktor(en) zu extrahieren und eine Varimax-Rotation durchzuführen. Die Ergebnisse sind in folgender Tabelle dargestellt.</p> {fa_table2(fa$fa)}

                   <p>Die Eigenwerte für diese Lösung sind: {eigenvalues} </p><p>Welche Kommunalität hat Item {which_item}?")

  q4 <- new("NumericGap", response_identifier = "h2", solution = as.numeric(fa$h2[which_item]))
  new("Entry", identifier = "fa",
      content = list(p1, q1, p2, q2, p3, q3, p4, q4
                     , "</p>"))
}
