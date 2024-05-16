# ============================================================================ #
# data cleansing - vhis premium summary ======================================
# ============================================================================ #


# configuration ----
library(readxl)
library(tidyverse)

# female premium ----
premium_female_xlsx <- "data/vhis/Standard_Plan_Premium_Summary_Female.xlsx"

dtt0 <- read_excel(premium_female_xlsx, skip=2, col_names = FALSE )
colnames(dtt0) <- paste0("x", (1:ncol(dtt0)))
dtt0[c(1:2, 5:nrow(dtt0)),2] = dtt0[c(1:2, 5:nrow(dtt0)),1] 
dtt0$x1 <- NULL

dim_row_age <- dtt0 %>%
  rownames_to_column() %>% 
  gather(colname, attained_age, -rowname) %>% 
  filter(colname == "x2" & !rowname %in% c("1", "2", "3", "4", "106")) %>%
  select(-colname)

dim_col_other <- dtt0 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  filter(!colname == "x2") %>%
  filter(rowname %in% c("1", "2", "3", "4")) %>%
  arrange(rowname) %>%
  fill(value) %>% 
  spread(key = rowname, value = value) %>% 
  rename(
    provider = `1`,
    policy_type = `2`,
    nb_age_range = `3`,
    renew_age_range = `4` 
  )

premium_female <- dtt0 %>% 
  rownames_to_column() %>% 
  gather(colname, premium, -rowname) %>% 
  filter(!colname == "x2") %>%
  filter(!rowname %in% c("1", "2", "3", "4", "106")) %>%
  left_join(dim_row_age, by = "rowname") %>%
  left_join(dim_col_other, by = "colname") %>%
  mutate(gender = "female")
  

# male premium ----
premium_male_xlsx <- "data/vhis/Standard_Plan_Premium_Summary_Male.xlsx"

dtt0 <- read_excel(premium_male_xlsx, skip=2, col_names = FALSE )
colnames(dtt0) <- paste0("x", (1:ncol(dtt0)))
dtt0[c(1:2, 5:nrow(dtt0)),2] = dtt0[c(1:2, 5:nrow(dtt0)),1] 
dtt0$x1 <- NULL

dim_row_age <- dtt0 %>%
  rownames_to_column() %>% 
  gather(colname, attained_age, -rowname) %>% 
  filter(colname == "x2" & !rowname %in% c("1", "2", "3", "4", "106")) %>%
  select(-colname)

dim_col_other <- dtt0 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  filter(!colname == "x2") %>%
  filter(rowname %in% c("1", "2", "3", "4")) %>%
  arrange(rowname) %>%
  fill(value) %>% 
  spread(key = rowname, value = value) %>% 
  rename(
    provider = `1`,
    policy_type = `2`,
    nb_age_range = `3`,
    renew_age_range = `4` 
  )

premium_male <- dtt0 %>% 
  rownames_to_column() %>% 
  gather(colname, premium, -rowname) %>% 
  filter(!colname == "x2") %>%
  filter(!rowname %in% c("1", "2", "3", "4", "106")) %>%
  left_join(dim_row_age, by = "rowname") %>%
  left_join(dim_col_other, by = "colname") %>%
  mutate(gender = "male")

  
# combine all ----
vhis_standard_premium <- rbind(premium_female, premium_male) %>%
  mutate(
    provider = gsub("\r\n", "", provider),
    provider = str_replace(provider, "（", "("),
    provider = str_replace(provider, "）", ")"),
    provider = gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", provider),
    provider = gsub(" ?\\( ?\\) ?", "", provider),
    provider = gsub("^\\(1976\\)", "", provider),
    
    policy_type = gsub("\r\n", "", policy_type),
    policy_type = gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", policy_type),
    policy_type = gsub("^ \\/", "", policy_type),
    
    attained_age = as.numeric(attained_age),
    premium = as.numeric(premium),
  ) %>%
  select(provider, policy_type, gender,
         nb_age_range, renew_age_range, attained_age,
         premium)

unique(vhis_standard_premium$policy_type)
unique(vhis_standard_premium$provider)


# save data ----
saveRDS(vhis_standard_premium, "data/vhis/vhis_standard_premium.rds")


# load to database ----
library(dsar)
con_ds_write <- psql_con("WRITE")

psql_write_table(
  con_ds_write, 
  "external.vhis_standard_premium",
  vhis_standard_premium, 
  overwrite = FALSE
)

# tmp ----
psql_write_table(
  con_ds_write, 
  "external.vhis_standard",
  standard_df, 
  overwrite = FALSE
)

psql_write_table(
  con_ds_write, 
  "external.vhis_flexi",
  flexi_df, 
  overwrite = FALSE
)



# END ==========================================================================
  
  