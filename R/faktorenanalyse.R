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
              MonteCarlo = list(NSamples = 1, Raw = TRUE,
                                SampleSize = sample(250:1000, 1)),
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
                                    n_datasets = 100, n_factors = 1)
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
                        caption = "Tabelle Parallel-Analyse",
                        col.names = c("Eigenwert", "Mittelwert Eigenwert PA",
                                      "95 % Perzentil Eigenwert PA"))
  )
}

fa_table2 <- function(myfa) {
  rot_loadings <- myfa$rot_loadings
  rot_loadings <- rot_loadings#[, order(colnames(rot_loadings))]
  output <- round(cbind(`Item Nummer` = 1:length(myfa$h2),
                        rot_loadings), 3)
  row.names(output) <- NULL
  tbl <-tbl <- kableExtra::kable_styling(
    kable_input = kable(output, digits = 2, booktabs = T,
                        caption = "Tabelle Faktoren-Lösung",
                        col.names = c("Item Nummer", paste0("F", seq(ncol(output)-1)))),

    full_width = F,
    position = "left")
  tbl
}

faktorenanalyse <- function(seed = sample.int(1e5, 1)) {
  set.seed(seed)
  fa <- fa_example()
  eigenvalues <- fa$fa$vars_accounted_rot[1,]
  names(eigenvalues) <- paste0("F", seq(eigenvalues))
  eigenvalues_table <- kableExtra::kable_styling(
    kable_input = kable(eigenvalues, booktabs = T,
                        caption = "Tabelle Eigenwerte",
                        digits = 2,
                        col.names = c("Faktor", "Eigenwert")), full_width = F,
    position = "left")

  p1 <- glue::glue("<p>Der berühmte Psychologe Thurstone hat im Jahr 1941 eine Studie zu Intelligenz durchgeführt, die Sie versuchen zu replizieren. Sie erheben Daten zu verschiedenen Intelligenz-Aufgaben bei {nrow(fa$data)} Probanden und möchten diese zunächst mit Hilfe einer explorativen Faktorenanalyse auswerten.</p><p>

Die folgende Tabelle zeigt die Eigenwerte und das 95%-Quantil der zufällig erzeugten Eigenwerte einer Parallel-Analyse (PA):</p>

{fa_table_parallel(fa$fa_parallel)}

<p>Wie viele Faktoren sollten nach dem Kaiserkriterium extrahiert werden?")

  q1 <- new("NumericGap", response_identifier = "kaiser", solution = fa$kaiser_num)
  p2 <- "</p><p>Wie viele Faktoren sollten nach der Parallelanalyse extrahiert werden?"
  q2 <- new("NumericGap", response_identifier = "pa", solution = fa$fa_parallel$n_fac_EFA)
  which_fac <- sample(seq(fa$n_factors_model), 1)
  p3 <- glue::glue("</p><p>Wie viel Varianz wird durch Faktor {which_fac} erklärt? Geben Sie den Wert als Dezimalzahl an (z. B. <b>0.43</b> für 43%) und runden Sie auf 2 Dezimalstellen.")

  eigenvalue <- round(eigenvalues[which_fac], 2)
  var_explained <- as.numeric(round(eigenvalue / fa$n_items, 2))
  q3 <- new("NumericGap", response_identifier = "var",
            solution = var_explained, tolerance = 0.01)

  which_item <- sample(seq(fa$n_items), 1)
  p4 <- glue::glue("</p><p>Sie entschließen sich aufgrund theoretischer Überlegungen dazu, {fa$n_factors_model} Faktor(en) zu extrahieren und eine Varimax-Rotation durchzuführen. Die Ergebnisse sind in folgender Tabelle dargestellt.</p> {fa_table2(fa$fa)}

                   <p>Die Eigenwerte für diese Lösung sind: {eigenvalues_table} </p>

                   <p>Welche Kommunalität hat Item {which_item}? Runden Sie auf 2 Dezimalstellen.")

  q4 <- new("NumericGap", response_identifier = "h2", solution = as.numeric(round(fa$h2[which_item], 2)), tolerance = 0.01)

  feedback <- fa_feedback(which_item, fa$h2, fa$n_factors_model, fa$n_items, eigenvalue)
  new("Entry", identifier = paste0("fa", seed),
      content = list(p1, q1, p2, q2, p4, q4, p3, q3
                     , "</p>"), feedback = list(feedback))
}

fa_feedback <- function(h2_item, h2, n_factors_model, n_items, eigenvalue) {
  content <- c("Hier schaut man einfach bei der Tabelle zur Parallel-Analyse wie viele Eigenwerte größer oder gleich 1 sind. Die Idee ist nur Faktoren zu extrahieren, die mehr Varianz erklären als ein einzelnes Item.",
               "Hier schaut man bei der Tabelle zur Parallel-Analyse wie viele Eigenwerte größer oder gleich dem 95% Perzentil der Eigenwerte aus der Parallel-Analyse entsprechen. Die Idee ist nur Faktoren zu extrahieren, die überzufällig große Eigenwerte haben. Das 95%-Perzentil ist in dieser Hinsicht willkürlich gewählt und kann natürlich in eigenen Studien angepasst werden.",
               paste("Hierfür muss man bei der Tabelle <i>Faktoren-Lösung</i> beim entsprechendem Item die Faktorladungen quadrieren und aufsummieren: ",
               latexmath(glue::glue("h_<<h2_item>>^2 = \\sum_{f=1}^<<n_factors_model>> \\lambda_{<<h2_item>>,f}^2 = <<h2[h2_item]>>", .open="<<", .close = ">>"))),
               paste0(glue::glue("Die Gesamtvarianz entspricht der Anzahl der Items, da bei der Faktoren-Analyse die Items standardisiert werden. Die Gesamtvarianz ist also {n_items}. Der Eigenwert eines Faktors entspricht gleichzeitig der Varianz dieses Faktors. Man teilt also einfach den Eigenwert durch die Gesamtvarianz: "),
                      latexmath(glue::glue("\\frac{\\lambda_k}{M}=\\frac{<<eigenvalue>>}{<<n_items>>}=<<round(eigenvalue/n_items, 2)>>", .open="<<", .close =">>"))))

  summary_html <- paste0("<summary>", "Frage ", seq(content), "</summary>")
  content <- paste0("<details>", summary_html, content, "</details>")
  new("ModalFeedback", content = list(content))
}

faktorenanalyse_stud <- function(seeds = 1:20) {
  exercises <- parallel::mclapply(seeds, faktorenanalyse)
  section <- new("AssessmentSection", assessment_item = exercises, selection = 1)
  test <- new("AssessmentTestOpal", identifier = "faktorenanalyse",
              section = list(section), calculator = "scientific-calculator",
              files = get_supplement_paths())
}
