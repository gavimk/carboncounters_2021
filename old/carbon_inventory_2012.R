library(tidyverse)
library(tidyr)
library(here)
library(janitor)
library(plotly)
library(kableExtra)
library(effsize)
library(broom)
library(formattable)

###############################################################
# Natural lands inventory
###############################################################

# read in data. file decriptions above each file.

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

# 2012 evc/evh/evt for 2012 (our pre-processed data)
lf_evc_12 <- read_csv(here::here("files", "natlands", "LF_2016_EVC.csv"))

lf_evh_12 <- read_csv(here::here("files",  "natlands", "LF_2016_EVH.csv"))

lf_evt_12 <- read_csv(here::here("files",  "natlands", "LF_2016_EVT.csv"))

# clean datasets, do appropriate merges
carbon_vals <- carbon_vals %>% 
  clean_names("snake") %>% 
  filter(!is.na(w_total_mt_cha)) %>% 
  rename(total_mt = w_total_mt_cha) %>% 
  rename(grouped = x_u_feff_lf_key)

precombin_df <- merge(lf_evc_12, lf_evh_12, by = "OBJECTID")

# combine all three (EVC, EVH, EVT) landfire dataframes for 2012, and clean

combined_lf_df <- merge(precombin_df, lf_evt_12, by = "OBJECTID") %>% 
  select(OBJECTID, pointid, CLASSNAMES.x, CLASSNAMES.y, EVT_NAME,  Reclass_16) %>% 
  clean_names("snake") %>% 
  rename(classnames_evc = classnames_x) %>% 
  rename(classnames_evh = classnames_y) %>%
  rename(classnames_evt = evt_name) %>% 
  left_join(evh_lut, by = "classnames_evh") %>% 
  left_join(evc_lut, by = "classnames_evc") %>% 
  left_join(evt_lut, by = "classnames_evt") %>% 
  mutate(grouped = paste(evt_group, evh_group, evc_group, sep = ""))

# remove NAs

is_na <- combined_lf_df %>% 
  filter(is.na(evt_group)) %>% 
  select(classnames_evt)

# find missing values

is_na_unique <- unique(is_na$classnames_evt)

# make dataframe of missing values - COME BACK TO THIS

is_na_unique <- data.frame(is_na_unique)
# write_csv(is_na_unique, here::here("files", "natlands", "missing_classnames_evt.csv"))

# missing_names_evt <- read.csv(here::here("files", "natlands", "missing_classnames_evt.csv"))

###############################################################
# Add agricultural lands
###############################################################

# read in ag data

ag_2012_raw <- read.csv(here::here("files", "ag", "ag_2012.csv"), encoding = "UTF-8", na.strings=c(""," ", "NoData", "NA"))

ag_2012 <- ag_2012_raw %>% 
  select(!c(organic, crop_list)) %>% 
  rename(nitrogen = nitrogren_) %>% 
  clean_names("snake") %>% 
  rename(pointid = objectid) %>% 
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(nitrogen = as.character(nitrogen)) %>% 
  mutate(nitrogen = ifelse(ag_class == "Barren / Fallow" | ag_class == "Greenhouse", 0, nitrogen)) %>% 
  mutate(ag_class = ifelse(ag_class == "Irrigated Pasture",  "Fodder", as.character(ag_class)))

# merge natural lands with ag, replace grouped name with ag classification where appropriate

combined_ag_natland <- merge(combined_lf_df, ag_2012, by = "pointid") %>%
  mutate(ag_class = as.character(ag_class)) %>% 
  mutate(grouped = ifelse(is.na(ag_class), grouped, ag_class)) %>% 
  mutate(reclass_cat = ifelse(is.na(ag_class), reclass_16, ag_class)) %>% 
  select(evt_group, pointid, reclass_cat, grouped, nitrogen) %>% 
  rename(nitrogen_cat = nitrogen)

#calculate acreage per category - these are wrong, we should investigate!
acreage_calc_12 <- combined_ag_natland %>% 
  group_by(reclass_cat) %>% 
  summarize(pixels = n()) %>% 
  mutate(sqmeter = pixels*900) %>% 
  mutate(acreage = sqmeter/4047) %>% 
  adorn_totals()

ag_acreage_12 <- acreage_calc_12 %>% 
  filter(reclass_cat %in% c("Agriculture", "Fodder", "Orchard", "Row Crop", "Vineyard", "Greenhouse"))

# calculate stored carbon and nitrous oxide emissions for each pixel (900 sq m)

ag_natland_carbon_n_12 <- combined_ag_natland %>% 
  left_join(carbon_vals, by = "grouped") %>%
  mutate(mt_900 = (total_mt*.09)) %>% # MT carbon per hectare multiplied by .09 to get metric tons of carbon per pixel (900 sq m)
  left_join(lut_n, by = "nitrogen_cat") %>% 
  mutate(lbs_n_pixel = (n_rate_lbs_acre*.222395)) %>% # nitrogen application rate (pounds per acre) multiplied by .222395 to get pounds of N applied per per pixel
  mutate(emit_n_lbs_pix = (lbs_n_pixel * .01)) %>% # 1% of nitrogen escapes at NO emissions
  select(!c(n_rate_lbs_acre, lbs_n_pixel)) %>% 
  mutate(stock_c_mtco2e_pixel = (mt_900*3.67)) %>%  # multiply metric tons of carbon by 3.67 to get MT of CO2 equivalent
  mutate(emit_no_mtco2e_pix = emit_n_lbs_pix*298*0.000453592) # multiply pounds to NO emissions by 298 to convert to pounds CO2e, then by 0.000453592 to get metric tonnes

# take sum of metric tonnes per 900 square meters - will need to be updated later

# prelim_total_c_12 <- sum(ag_natland_carbon_n$mt_900, na.rm = TRUE)
# print(comma(prelim_total_c_12)) # MT carbon
# 
# prelim_total_co2e_carbon <- sum(ag_natland_carbon_n$mtco2e_c, na.rm = TRUE)
# print(comma(prelim_total_co2e_carbon)) # MT co2e (from total stored carbon)
# 
# prelim_co2e_no_12 <- sum(ag_natland_carbon_n$mtco2e_no_emit, na.rm = TRUE)
# print(comma(prelim_co2e_no_12)) # metric tonnes co2e (from calculated NO emissions)
# 
# prelim_vec <- c(prelim_total_c_12, prelim_total_co2e_carbon, prelim_co2e_no_12)
# print(comma(prelim_vec))

# at the end of this script, we need to make sums of carbon per landcover class. the following classifications will be used:
## forest
## barren
## shrubland
## developed
## wetland
## grassland
## vineyard
## orchard
## row crop
## fodder
## pasture

# code for this (NOT RUN) to use later:
# df <- df_mt %>% 
# pivot_longer(! )
# groupby(reclass_cat, ) %>% 
# summarize

