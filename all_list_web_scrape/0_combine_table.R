library(tidyverse)
library(readxl)

combined_table <- list.files("data/table_list", pattern = "\\.csv$",
                             full.names = TRUE) %>%
  lapply(read.csv) %>%
  bind_rows() %>%
  mutate(id = row.names(.)) %>%
  select(id, 2:14)

save(combined_table, file = "data/table_list_combined/combined_table.rda")
write.csv(combined_table, file = "data/table_list_combined/combined_table.csv")

