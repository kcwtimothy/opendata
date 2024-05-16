# ============================================================================ #
# IA data exploration ========================================================
# ============================================================================ #

# 0.Configuration ==============================================================
source("utils.R")


# scrape the list for ia in advance, make it adjust to "combined_table" structure
names(list_df)
combined_table <- list_df %>%
  transmute(
    id = row_number(),
    data_dataset = data.dataset.title.en,
    data_resource = data.resource.title.en,
    data_format = data.resource.format,
    download_links = data.resource.url
  ) 
  

# 1. Annual Statistics for Long Term Business ====
# take the report year = 2017 as a study case 
combined_table %>% filter(data_dataset == "Annual Statistics for Long Term Business 2017") %>% view()
# ids <- c(7663:7720)
ids <- c(857:914)
combined_table[ids,c("data_dataset", "data_resource")]

ia_longterm_raw <- list()

for (id in ids){
  x <- paste0(as.character(id), ": ", combined_table[id,"data_resource"])
  
  ia_longterm_raw[[x]] <- fetch_tool(combined_table, id) %>%
    mutate(
      ct_id = id,
      resource_name = combined_table[id,"data_resource"]
    ) %>%
    select(ct_id, resource_name, everything())
}

sapply(ia_longterm_raw, names)

eng <- names(ia_longterm_raw)[grep("English", names(ia_longterm_raw))]

eng[-grep("Insurers' Statistics", eng)]
eng[grep("Insurers' Statistics", eng)]


# 2. Quarterly Release of Provisional Statistics for Long Term Business ====
combined_table %>% 
  filter(data_dataset == "Quarterly Release of Provisional Statistics for Long Term Business 2019  (January to September)") %>% view()

# take 2019Jantosep as study case 
ids <- c(2674:2735)

ia_provisional_longterm_raw <- list()

for (id in ids){
  x <- paste0(as.character(id), ": ", combined_table[id,"data_resource"])
  
  ia_provisional_longterm_raw[[x]] <- fetch_tool(combined_table, id) %>%
    mutate(
      ct_id = id,
      resource_name = combined_table[id,"data_resource"]
    ) %>%
    select(ct_id, resource_name, everything())
}

sapply(ia_provisional_longterm_raw, names)
eng <- names(ia_provisional_longterm_raw)[grep("English", names(ia_provisional_longterm_raw))]

eng[-grep("Insurers' Statistics", eng)]
eng[grep("Insurers' Statistics", eng)]


# 3. Annual Statistics for General Business ====


# 4. Quarterly Release of Provisional Statistics for General Business ====



















# ****** end ****** ============================================================
