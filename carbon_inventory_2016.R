library(tidyverse)
library(tidyr)
library(here)
library(janitor)
library(plotly)
library(kableExtra)
library(effsize)
library(broom)
library(formattable)
library(purrr)

###############################################################
# Natural lands inventory
###############################################################

# read in data. file descriptions above each file.

# carbon value table from CARB
carbon_vals <- read.csv(here::here("files", "luts", "lut_lf_carb.csv"), encoding = "UTF-8")

# existing vegetation cover (LANDFIRE) look up table
evc_lut <- read.csv(here::here("files", "luts", "lut_evc.csv"), encoding = "UTF-8") %>%
  clean_names() %>%
  rename(classnames_evc = x_u_feff_classnames)

# existing vegetation height (LANDFIRE) look up table
evh_lut <- read.csv(here::here("files", "luts", "lut_evh.csv"), encoding = "UTF-8") %>% 
  clean_names() %>% 
  rename(classnames_evh = x_u_feff_classnames)

# existing vegetation type (LANDFIRE) look up table
evt_lut <- read.csv(here::here("files", "luts", "lut_evt.csv"), encoding = "UTF-8") %>% 
  clean_names() %>% 
  rename(classnames_evt = x_u_feff_evt_name)

# nitrogen look up table
lut_n <-  read.csv(here::here("files", "luts", "lut_n.csv"), encoding = "UTF-8") %>% 
  clean_names() %>% 
  rename(nitrogen_cat = x_u_feff_nitrogen)

# 2016 evc/evh/evt for 2016 (our pre-processed data)
lf_evc_16 <- read_csv(here::here("files", "natlands", "LF_2016_EVC.csv"))

lf_evh_16 <- read_csv(here::here("files",  "natlands", "LF_2016_EVH.csv"))

lf_evt_16 <- read_csv(here::here("files",  "natlands", "LF_2016_EVT.csv"))

# clean datasets, do appropriate merges
carbon_vals <- carbon_vals %>% 
  clean_names("snake") %>% 
  filter(!is.na(w_total_mt_cha)) %>% 
  rename(total_mt = w_total_mt_cha) %>% 
  rename(grouped = x_u_feff_lf_key)

precombin_df <- merge(lf_evc_16, lf_evh_16, by = "OBJECTID")

# combine all three (EVC, EVH, EVT) landfire dataframes for 2016, and clean

lf_reclass_n <- read.csv(here::here("files", "luts", "lf_reclass_nitrogen.csv"), encoding = "UTF-8") %>% 
  clean_names() %>% 
  rename(classnames_evt = x_u_feff_evt) %>% 
  rename(lf_n_category = n_category)

combined_lf_df <- merge(precombin_df, lf_evt_16, by = "OBJECTID") %>% 
  dplyr::select(OBJECTID, pointid, CLASSNAMES.x, CLASSNAMES.y, EVT_NAME,  Reclass_16) %>% 
  clean_names("snake") %>% 
  rename(classnames_evc = classnames_x) %>% 
  rename(classnames_evh = classnames_y) %>%
  rename(classnames_evt = evt_name) %>% 
  left_join(evh_lut, by = "classnames_evh") %>% 
  left_join(evc_lut, by = "classnames_evc") %>% 
  left_join(evt_lut, by = "classnames_evt") %>% 
  mutate(grouped = paste(evt_group, evh_group, evc_group, sep = "")) %>% 
  left_join(lf_reclass_n, by = "classnames_evt") %>% 
  dplyr::select(-reclass_16) %>% 
  rename(reclass_16 = reclass_category)

# remove NAs - code no longer needed, updated csv's reflect that we updated groups that showed up as NA

# is_na <- combined_lf_df %>% 
#   filter(is.na(evt_group)) %>% 
#   select(classnames_evt)

# find missing values

# is_na_unique <- unique(is_na$classnames_evt)

# make dataframe of missing values - COME BACK TO THIS

# is_na_unique <- data.frame(is_na_unique)
# write_csv(is_na_unique, here::here("files", "natlands", "missing_classnames_evt.csv"))

# missing_names_evt <- read.csv(here::here("files", "natlands", "missing_classnames_evt.csv"))

###############################################################
# Add agricultural lands
###############################################################

# read in ag data

ag_2016_raw <- read.csv(here::here("files", "ag", "ag_2016.csv"), encoding = "UTF-8", na.strings=c(""," ", "NoData", "NA"))

ag_2016 <- ag_2016_raw %>% 
  dplyr::select(!c(organic, crop_list)) %>% 
  rename(nitrogen = nitrogren_) %>% 
  clean_names("snake") %>% 
  rename(pointid = objectid) %>% 
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(nitrogen = as.character(nitrogen)) %>% 
  mutate(nitrogen = ifelse(ag_class == "Barren / Fallow" | ag_class == "Greenhouse", 0, nitrogen)) %>% 
  mutate(ag_class = ifelse(ag_class == "Irrigated Pasture",  "Fodder", as.character(ag_class))) %>% 
  mutate(ag_class = ifelse(ag_class == "Barren / Fallow",  "Fallow", as.character(ag_class))) %>% 
  mutate(source = ifelse(is.na(ag_class), "landfire", "calag"))

# merge natural lands with ag, replace grouped name with ag classification where appropriate

combined_ag_natland <- merge(combined_lf_df, ag_2016, by = "pointid") %>%
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(lf_n_category = as.character(lf_n_category)) %>% 
  mutate(grouped = ifelse(is.na(ag_class), grouped, ag_class)) %>% 
  mutate(reclass_cat = ifelse(is.na(ag_class), as.character(reclass_16), as.character(ag_class))) %>%
  mutate(reclass_cat = as.character(reclass_cat)) %>% 
  mutate(reclass_cat = ifelse(reclass_cat == "Barren / Fallow",  "Fallow", as.character(reclass_cat))) %>% 
  dplyr::select(evt_group, pointid, reclass_cat, grouped, nitrogen, lf_n_category, source) %>% 
  rename(nitrogen_cat = nitrogen) %>% 
  mutate(reclass_cat = ifelse(reclass_cat == "Wetland",  "Riparian/Wetland", as.character(reclass_cat))) %>% 
  mutate(reclass_cat = ifelse(reclass_cat == "Irrigated Pasture",  "Fodder", as.character(reclass_cat))) %>% 
  mutate(nitrogen_cat = ifelse(lf_n_category != "", as.character(lf_n_category), as.character(nitrogen_cat)))

# simplified file to use in GIS
reclass_map_file <- combined_ag_natland %>% 
  dplyr::select(pointid, reclass_cat)

# calculate stored carbon and nitrous oxide emissions for each pixel (900 sq m)

ag_natland_carbon_n_16 <- combined_ag_natland %>% 
  left_join(carbon_vals, by = "grouped") %>%
  mutate(mt_900 = (total_mt*.09)) %>% # MT carbon per hectare multiplied by .09 to get metric tons of carbon per pixel (900 sq m)
  left_join(lut_n, by = "nitrogen_cat") %>% 
  mutate(lbs_n_pixel = (n_rate_lbs_acre*.222395)) %>% # nitrogen application rate (pounds per acre) multiplied by .222395 to get pounds of N applied per per pixel
  mutate(emit_n_lbs_pix = (lbs_n_pixel * .0175)) %>% # 1.75% of nitrogen escapes at N2O emissions
  dplyr::select(!c(n_rate_lbs_acre, lbs_n_pixel)) %>% 
  mutate(stock_abvgc_mtco2e_pixel = (mt_900*1)) %>%  # multiply metric tons of carbon by 3.67 to get MT of CO2 equivalent # Decided to report as MT instead, replace 3.67 value w 1 to not break rest of code
  mutate(emit_no_mtco2e_pix = emit_n_lbs_pix*298*0.000453592) # multiply pounds to N2O emissions by 298 to convert to pounds CO2e, then by 0.000453592 to get metric tonnes

# read in soil data - unit = gC / m^2

soil <- read_csv(here::here("files", "soil", "ssurgo.csv")) %>% 
  dplyr::select(pointid, soc0_30)
         
all_c_n_soil <- merge(ag_natland_carbon_n_16, soil, by = "pointid") %>% 
  mutate(soil900 = (soc0_30*900)) %>% #per m^2 to per 900 m^2
  mutate(soilMT = (soil900/1000000)) %>%  # grams to metric tons of organic carbon
  mutate(stock_soilc_mtco2e_pix = soilMT*1) # convert to CO2e # Decided to report as MT instead, replace 3.67 value w 1 to not break rest of code
  
# okay...let's see if we can make a nice table somehow

all_clean_16_no_tree <- all_c_n_soil %>% 
  dplyr::select(pointid, reclass_cat, stock_abvgc_mtco2e_pixel, stock_soilc_mtco2e_pix, emit_no_mtco2e_pix, source) %>% 
  mutate(emit_no_mtco2e_pix = replace_na(emit_no_mtco2e_pix, 0))

# total_abvg_c_16 <- comma(sum(all_clean_16_no_tree$stock_abvgc_mtco2e_pixel, na.rm = TRUE))
# 
# total_soilc_16 <- comma(sum(all_clean_16_no_tree$stock_soilc_mtco2e_pix, na.rm = TRUE))
# 
# total_no_16 <- comma(sum(all_clean_16_no_tree$emit_no_mtco2e_pix, na.rm = TRUE))
# 
# total_net_16 <- (comma(total_abvg_c_16 + total_soilc_16 - total_no_16))
# 
# ci_summary_nocat_16 <- data.frame(total_abvg_c_16, total_soilc_16, total_no_16, total_net_16)

# this one below is really not necessary - can remove later after checking I didn't use it for something else

all_acreages_16 <- all_clean_16_no_tree %>% 
  group_by(reclass_cat) %>% 
  summarize(pixels = n()) %>% 
  mutate(sqmeter = pixels*900) %>% 
  mutate(acreage = sqmeter/4047) %>% 
  dplyr::select(! c(pixels, sqmeter)) %>% 
  adorn_totals()

#create a summary (this is the inventory! yay!)
ci_summary_cat_16 <- all_clean_16_no_tree %>%
  dplyr::select(!source) %>% 
  group_by(reclass_cat) %>%
  summarise_all(.funs = c(sum="sum"), na.rm = TRUE) %>%
  mutate(net = (stock_soilc_mtco2e_pix_sum + stock_abvgc_mtco2e_pixel_sum)) %>%
  merge(all_acreages_16, by = "reclass_cat") %>% 
  dplyr::select(!pointid_sum)

#ag acreages only
# ag_acres_calc_16 <- all_clean_16_no_tree %>% 
  # filter(source == "calag") %>% 
  # dplyr::select(!source) %>% 
  # group_by(reclass_cat) %>% 
  # summarize(pixels = n()) %>% 
  # mutate(sqmeter = pixels*900) %>% 
  # mutate(acreage = sqmeter/4047) %>% 
  # dplyr::select(! c(pixels, sqmeter)) %>% 
  # adorn_totals()

# another summary without landfire ag points to use for baseline
# summary_merge <- all_clean_16_no_tree %>%
  # filter(source == "calag") %>% 
  # dplyr::select(!source) %>% 
  # group_by(reclass_cat) %>%
  # summarise_all(.funs = c(sum="sum"), na.rm = TRUE) %>%
  # mutate(net = (stock_soilc_mtco2e_pix_sum + stock_abvgc_mtco2e_pixel_sum - emit_no_mtco2e_pix_sum)) %>%
  # merge(ag_acres_calc_16, by = "reclass_cat") %>% 
  # dplyr::select(!pointid_sum)

####################################################################################
# Now let's project agricultural land acreage and carbon to 2030

# calculate same but filter for just ag to use for baseline projection (I know this is clunky, it's bc of how adorn_total works)
# *NOTE* - do we want the LANDFIRE "Agriculture" category in here? In 2012 and 2019? I think we decided to not use it for baseline?
# ag_acreage_16 <- summary_merge %>%
  # filter(reclass_cat %in% c("Fallow", "Fodder", "Orchard", "Row Crop", "Vineyard", "Greenhouse", "Pastureland")) %>% 
  # adorn_totals() %>% 
  # mutate(year = '2016')

# add in urban forestry aboveground #s and create row to add to dfs

# CO2e stored/urban tree canopy (metric tons/acre)

tree_num <- 114.8730627/3.67

# This may need to be hard coded - need to double check this cell reference is correct 
urban_acres <- all_acreages_16[2, 2]
 
tree_row <-data.frame("Urban Forestry (Aboveground Only)", tree_num*urban_acres, 0, 0, tree_num*urban_acres, 0)
names(tree_row)<-c("reclass_cat", "stock_abvgc_mtco2e_pixel_sum", "stock_soilc_mtco2e_pix_sum", "emit_no_mtco2e_pix_sum", "net", "acreage")

ci_summary_cat_16 <- rbind(ci_summary_cat_16, tree_row) %>% 
  mutate(reclass_cat = ifelse(reclass_cat == "Urban Forestry (Aboveground Only)", "Developed", reclass_cat)) %>% 
  group_by(reclass_cat) %>% 
  summarise_all(sum) %>% 
  adorn_totals()

# easier to rename columns after merges are done!

colnames(ci_summary_cat_16) = c("Landcover Classification", "Total Aboveground Carbon (MT C)", "Total Soil Carbon (MT C)", "Total NO Emissions (MT CO2e)", "Total Stocks (MT C)", "Acres")
colnames(ag_acreage_16) = c("Landcover Classification", "Total Aboveground Carbon (MT C)", "Total Soil Carbon (MT C)", "Total NO Emissions (MT CO2e)", "Total Stocks (MT C)", "Acres", "Year")

# read in ag 2012 and 2019

ag_2019_raw <- read.csv(here::here("files", "ag", "ag_2019.csv"), encoding = "UTF-8", na.strings=c(""," ", "NoData", "NA"))

ag_2019 <- ag_2019_raw %>% 
  dplyr::select(!c(organic, crop_list)) %>% 
  rename(nitrogen = nitrogren_) %>% 
  clean_names("snake") %>% 
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(nitrogen = as.character(nitrogen)) %>% 
  mutate(nitrogen = ifelse(ag_class == "Barren / Fallow" | ag_class == "Greenhouse", 0, nitrogen)) %>% 
  mutate(ag_class = ifelse(ag_class == "Irrigated Pasture",  "Fodder", as.character(ag_class))) %>% 
  mutate(ag_class = ifelse(ag_class == "Barren / Fallow",  "Fallow", as.character(ag_class))) %>% 
  rename(pointid = objectid) %>% 
  mutate(source = ifelse(is.na(ag_class), "landfire", "calag"))

ag_2012_raw <- read.csv(here::here("files", "ag", "ag_2012.csv"), encoding = "UTF-8", na.strings=c(""," ", "NoData", "NA"))

ag_2012 <- ag_2012_raw %>% 
  dplyr::select(!c(organic, crop_list)) %>% 
  rename(nitrogen = nitrogren_) %>% 
  clean_names("snake") %>% 
  rename(pointid = objectid) %>% 
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(nitrogen = as.character(nitrogen)) %>% 
  mutate(nitrogen = ifelse(ag_class == "Barren / Fallow" | ag_class == "Greenhouse", 0, nitrogen)) %>% 
  mutate(ag_class = ifelse(ag_class == "Irrigated Pasture",  "Fodder", as.character(ag_class))) %>% 
  mutate(ag_class = ifelse(ag_class == "Barren / Fallow",  "Fallow", as.character(ag_class))) %>% 
  mutate(nitrogen = ifelse(ag_class == "Pastureland", "Field Crops", nitrogen)) %>% 
  mutate(source = ifelse(is.na(ag_class), "landfire", "calag"))

ag_2016_update <- ag_2016_raw %>% 
  dplyr::select(!c(organic, crop_list)) %>% 
  rename(nitrogen = nitrogren_) %>% 
  clean_names("snake") %>% 
  rename(pointid = objectid) %>% 
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(nitrogen = as.character(nitrogen)) %>% 
  mutate(nitrogen = ifelse(ag_class == "Barren / Fallow" | ag_class == "Greenhouse", 0, nitrogen)) %>% 
  mutate(ag_class = ifelse(ag_class == "Irrigated Pasture",  "Fodder", as.character(ag_class))) %>% 
  mutate(ag_class = ifelse(ag_class == "Barren / Fallow",  "Fallow", as.character(ag_class))) %>% 
  mutate(nitrogen = ifelse(ag_class == "Pastureland", "Field Crops", nitrogen)) %>% 
  mutate(source = ifelse(is.na(ag_class), "landfire", "calag"))

# create tables for 2012 and 2019

ag_files_list <- list(ag_2012, ag_2016_update, ag_2019)

# first, merge with LANDFIRE in case we want to keep LANDFIRE ag? It's an extra step but right now this is easier for me to code so I can just copy and paste
fx_merge <- function(ag) {
  
  merge(combined_lf_df, ag, by = "pointid") %>%
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(grouped = ifelse(is.na(ag_class), grouped, ag_class)) %>% 
  mutate(reclass_cat = ifelse(is.na(ag_class), reclass_16, ag_class)) %>% 
  dplyr::select(evt_group, pointid, reclass_cat, grouped, nitrogen, source) %>% 
  rename(nitrogen_cat = nitrogen) %>% 
  filter(source == "calag")
  
}

results <- lapply(ag_files_list, fx_merge) %>% 
  setNames(c(2012, 2016, 2019))

# make into data frames

fx_df <- function(result){
  df_name <- data.frame(result)
}

dfs <- lapply(results, fx_df) %>% 
  setNames(c(2012, 2016, 2019))

# calculate stored carbon and nitrous oxide emissions for each pixel (900 sq m)

fx_ghg_calc <- function(dfs) {

dfs %>% 
  left_join(carbon_vals, by = "grouped") %>%
  mutate(mt_900 = (total_mt*.09)) %>% # MT carbon per hectare multiplied by .09 to get metric tons of carbon per pixel (900 sq m)
  left_join(lut_n, by = "nitrogen_cat") %>% 
  mutate(lbs_n_pixel = (n_rate_lbs_acre*.222395)) %>% # nitrogen application rate (pounds per acre) multiplied by .222395 to get pounds of N applied per per pixel
  mutate(emit_n_lbs_pix = (lbs_n_pixel * .0175)) %>% # 1% of nitrogen escapes at NO emissions
  dplyr::select(!c(n_rate_lbs_acre, lbs_n_pixel)) %>% 
  mutate(stock_abvgc_mtco2e_pixel = (mt_900*1)) %>%  # multiply metric tons of carbon by 3.67 to get MT of CO2 equivalent # Decided to report as MT instead, replace 3.67 value w 1 to not break rest of code
  mutate(emit_no_mtco2e_pix = emit_n_lbs_pix*298*0.000453592) # multiply pounds to NO emissions by 298 to convert to pounds CO2e, then by 0.000453592 to get metric tonnes 

}

ghgs <- lapply(dfs, fx_ghg_calc) %>% 
  setNames(c(2012, 2016, 2019))

ghg_dfs <- lapply(ghgs, fx_df) %>% 
  setNames(c(2012, 2016, 2019))

# add soil

fx_soil <- function(ghg_dfs) {
  
ghg_dfs %>%  
  merge(soil, by = "pointid") %>% 
  mutate(soil900 = (soc0_30*900)) %>% #per m^2 to per 900 m^2
  mutate(soilMT = (soil900/1000000)) %>%  # grams to metric tons of organic carbon
  mutate(stock_soilc_mtco2e_pix = soilMT*1) # convert to CO2e #Decided to report as MT instead, replace 3.67 value w 1 to not break rest of code
}

soil_results <- lapply(ghg_dfs, fx_soil) %>% 
  setNames(c(2012, 2016, 2019))

soil_results_dfs <- lapply(soil_results, fx_df) %>% 
  setNames(c(2012, 2016, 2019))

# name these just to have them
combined_2012_df <- soil_results_dfs[[1]]
combined_2016_df <- soil_results_dfs[[2]]
combined_2019_df <- soil_results_dfs[[3]]


fx_all_clean <- function(df) {
df %>% 
  dplyr::select(reclass_cat, stock_abvgc_mtco2e_pixel, stock_soilc_mtco2e_pix, emit_no_mtco2e_pix) %>% 
  group_by(reclass_cat) %>% 
    summarise_all(.funs = c(sum="sum"), na.rm = TRUE) %>% 
    adorn_totals()
}

summaries <- lapply(soil_results_dfs, fx_all_clean) %>% 
  setNames(c(2012, 2016, 2019))

summaries_dfs <- lapply(summaries, fx_df) %>% 
  setNames(c(2012, 2016, 2019))

names <- names(summaries_dfs)

res_list <- vector("list", length = length(names)) %>% 
  setNames(names)

# lastly, make some  tables

for(i in names){

loop_total_table <- soil_results_dfs[[i]] %>% 
  group_by(reclass_cat) %>% 
  summarize(pixels = n()) %>% 
  mutate(sqmeter = pixels*900) %>% 
  mutate(acreage = sqmeter/4047) %>% 
  merge(summaries_dfs[[i]], by = "reclass_cat") %>%
  filter(reclass_cat %in% c("Fallow", "Fodder", "Orchard", "Row Crop", "Pastureland", "Vineyard", "Greenhouse")) %>% 
  mutate(net = (stock_soilc_mtco2e_pix_sum + stock_abvgc_mtco2e_pixel_sum)) %>% 
  adorn_totals() %>% 
  mutate(year = i)
  
res_list[[i]] <- loop_total_table
  
  }

total_tables <- lapply(res_list, fx_df) %>% 
  setNames(c(2012, 2016, 2019))

# and name them as objects

ag_acreage_12 <- total_tables[[1]]
ag_acreage_16 <- total_tables[[2]]
ag_acreage_19 <- total_tables[[3]]

# For ease of use, here are the most important outputs:

ci_summary_cat_16 #this is the full carbon inventory for 2016: contains acreage, soil carbon, aboveground carbon, and NO for all categories

# save the inventory
write_csv(ci_summary_cat_16, here::here("results", "inventory_16.csv"))

# these are only calag data - no landfire - will use in next step (baseline calculation)
ag_acreage_12
ag_acreage_16
ag_acreage_19

# write ag info to csvs to use for next phase of project

write_csv(ag_acreage_12, here::here("results", "ag_final_12.csv"))
write_csv(ag_acreage_16, here::here("results", "ag_final_16.csv"))
write_csv(ag_acreage_19, here::here("results", "ag_final_19.csv"))

# write GIS file

write_csv(reclass_map_file, here::here("results", "reclass_map_file.csv"))

# for mapping in R
write_csv(all_clean_16_no_tree, here("results", "all_points_values.csv"))
