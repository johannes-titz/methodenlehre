## code to prepare `neo_pi_r_eugene`
#setwd("..")
# load libs
librarian::shelf(qgraph) # for big5 variable names

# ipip-neo-120

library(haven)
#ipipneo <- read_spss("data-raw/IPIP120.por")
ipipneo <- foreign::read.spss("data-raw/IPIP120.por", to.data.frame = T)
ipipneo_cp <- ipipneo

# only items
ipipneo <- ipipneo[, c(paste("I", 1:120, sep = ""))]

# missing values are 0, see description
ipipneo[ipipneo == 0] <- NA

# 6 facets, 4 items
grid <- expand.grid(c("N", "E", "O", "A", "C"), 1:6, 1:4)
item_number <- paste(grid$Var1, 1:120, sep = "")

colnames(ipipneo) <- item_number

countries <- table(ipipneo_cp$COUNTRY)
countries <- names(countries[countries > 1e3 & countries < 10e3])

ipip_new <- cbind(ipipneo, sex = ipipneo_cp$SEX, country = ipipneo_cp$COUNTRY)
ipip2 <- ipip_new[ipipneo_cp$COUNTRY %in% countries, ]
ipip2$country <- gsub("South Afr", "South Africa", ipip2$country)
ipip2$country <- gsub("New Zeala", "New Zealand", ipip2$country)
ipip2$country <- gsub("Netherlan", "Netherlands", ipip2$country)
ipip2$country <- gsub("Philippin", "Philippins", ipip2$country)
ipip2$country <- gsub("South Kor", "South Korea", ipip2$country)
ipip2$country <- gsub("Afghanist", "Afghanistan", ipip2$country)

usethis::use_data(ipip2, overwrite = TRUE)

# ipineo items
items <- read.csv("data-raw/ipip_neo_120_items.csv",
                  header = F, col.names = c("item_number", "item_content"))
reverse_coded <- rep(F, 120)
reverse_coded[c(96, 101, 106, 51, 81, 11, 116, 62, 92, 67, 97, 102, 107,
                68, 98, 73, 103, 48, 78, 108, 53, 83, 113, 88, 118,
                94, 9, 39, 69, 99, 74, 104, 19, 49, 79, 109, 24, 54, 84, 114,
                89, 119, 40, 70, 100, 75, 105, 80, 110, 85, 115, 30, 60, 90, 120)] <- T
items <- cbind(items, reverse_coded = reverse_coded)
items$item_content <- paste(items$item_content, ifelse(items$reverse_coded, " (R)", ""), sep ="")
items$item_char <- paste0(rep(c("N", "E", "O", "A", "C"), 24), items$item_number)
ipipneo_items <- items

usethis::use_data(ipipneo_items, overwrite = TRUE)

