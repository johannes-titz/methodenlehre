librarian::shelf(dplyr)
p3 <- readODS::read_ods("data-raw/power_type.ods")

power_type_data <- p3
usethis::use_data(power_type_data, overwrite = T)

