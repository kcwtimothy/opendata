# ============================================================================ #
# HK Open Data ===============================================================
# explore the feasiblility of data consolidation standard ====================
# ============================================================================ #


# configuration ----------------------------------------------------------------
source("utils.R")
library(dplyr)


# data consolidation -----------------------------------------------------------
df1.1_s <- fetch_tool(combined_table, 18448) %>%
  filter(Age !=  "All") %>%
  select(-14) %>%
  gather("year", "stat_value", -c("Age", "Item")) %>%
  mutate(
    ct_id = "18448",
    stat_name = case_when(
      Item == "Number" ~ "casualities_count",
      Item == "Number per 1000 population" ~ "casualities_ratio"
      ),
    dimdt_name = "year",
    dimdt_factor = year,
    dimot_name_1 = "class_of_road_user",
    dimot_factor_1 = "All",
    dimot_name_2 = "age",
    dimot_factor_2 = Age
  ) %>%
  select(ct_id, stat_name, stat_value, 
         dimdt_name, dimdt_factor, starts_with("dimot"))


df1.11_s <- fetch_tool(combined_table, 18508) %>%
  filter(!grepl("All", Age)) %>%
  select(-contains("All")) %>%
  gather("stat_name", "stat_value", -"Age") %>%
  mutate(
    ct_id = "18508",
    dimdt_name = "year",
    dimdt_factor = "2018",
    dimot_name_1 = "age",
    dimot_factor_1 = Age,
    dimot_name_2 = "gender",
    dimot_factor_2 = case_when(
      grepl("Female", stat_name) ~ "female",
      grepl("Male", stat_name) ~ "male",
      grepl("Unknown sex", stat_name) ~ "unknown sex"
      )
  ) %>%
  mutate(
    stat_name = gsub("Female|Male|Unknown sex", "", stat_name)
  ) %>%
  select(ct_id, stat_name, stat_value, 
         dimdt_name, dimdt_factor, starts_with("dimot"))


# combine all consolidated data ------------------------------------------------
df <- rbind(df1.1_s, df1.11_s)


# data digest for well consolidated data ---------------------------------------

df <- hk_insurers
# 0 - get an idea of the statistics 
table(df$stat_name)
table(df$ct_id)

ids <- unlist(strsplit(unique(df$ct_id), "_"))
# premiumns / unearned_premiums
# commission_payable / commission_payable_ratio
# net_claims_incurred_ratio / paid_claims
# retention_ratio
# unexpired_risk_provision / outstanding_claims_provision 
# technical_reserves 

# 1 -  get an idea of all the dimension and its factors of each ct_id
tbl_no <- unique(df$ct_id)
n <- names(df)[grepl("name|dim.*factor", names(df))]

# lapply(tmp[n], unique)

for (i in tbl_no){
  print(paste0("=== ct_id:", i, " ==="))
  tmp <- df %>% filter(ct_id == i)
  for (x in n){
    print(paste0("--- ", x, " ---"))
    u <- unique(tmp[[x]])
    print(u)
  }
}

# 2 - get an idea of the most frequent used dimensions  
dims <- df %>%
  select(ct_id, dimdt_name, contains("dimot_name")) %>%
  distinct()

dns <- names(dims)[grepl("dimot_name", names(dims))]

all_dims <- NULL
for (i in dns){
  tmp <- dims[[i]]
  all_dims <- c(all_dims,tmp)
}

dims_freq <- data.frame(dimot_names = all_dims) %>%
  group_by(dimot_names) %>%
  summarise(freq = n())



# ****** end ****** ============================================================



  