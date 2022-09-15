# WORKING DIRECTORY SHOULD BE 'data' FOLDER

##### Load Pacakges #####
library(httr)
library(jsonlite)
library(plotly)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

##### Get Census ACS (DP05) Data from API #####
# # Access ACS 2018-5Yr API
# acs_url <-
#   "https://api.census.gov/data/2018/acs/acs5/profile?get=group(DP05)&for=state:*"
# acs_request <- GET(acs_url)
# acs_response <- content(acs_request,
#                         as = "text",
#                         encoding = "UTF-8")
# acs_raw <- fromJSON(acs_response, flatten = TRUE)
# 
# # Get ACS variable codes
# acs_variables <-
#   (
#     GET("https://api.census.gov/data/2018/acs/acs5/profile/groups/DP05") %>%
#     content(as = "text") %>%
#     fromJSON())$variables
# 
# # Get real column names
# acs_colnames <- acs_raw[1,]                                 # Get vector of col names
# for (i in 1:length(acs_colnames))
# {
#   this_col <- acs_colnames[i]
#   if (!(this_col %in% c("GEO_ID", "NAME", "state")))        # If not different name
#   {
#     this_col_name <- acs_variables[[this_col]]$label
#     acs_colnames[i] <- this_col_name
#   }
# }
# colnames(acs_raw) <- acs_colnames                           # Add real colnames
# 
# # Convert ACS data from matrix to data frame
# acs_raw2 <-
#   acs_raw[2:nrow(acs_raw),] %>%                             # Delete row 1 (var codes)
#   as_tibble() %>%                                           # make into tibble
#   select(GEO_ID, state, NAME, everything()) %>%             # reorder columns
#   mutate_at(vars(everything(), -GEO_ID, -state, -NAME),
#             as.numeric)                                     # Make variables numerics
# 
# write_csv(acs_raw2, "usracebars/data/2018_acs_dp05.csv")




##### Data Cleaning #####
acs_raw2 <- read_csv("2018_acs_dp05.csv")



# Clean ACS Dataset to isolate race/ethnicity variables
hispanic_var_name <- "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)"
all_of_race_var_stem <- "Estimate!!Race alone or in combination with one or more other races!!Total population"
NH_var_stem <- "Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino"

acs_race_vars <-
  acs_raw2 %>% 
  select(GEO_ID, state, NAME,
         Population = all_of_race_var_stem,
         Hispanic = hispanic_var_name,
         White = paste0(all_of_race_var_stem, "!!White"),
         Black = paste0(all_of_race_var_stem, "!!Black or African American"),
         AIAN = paste0(all_of_race_var_stem, 
                       "!!American Indian and Alaska Native"),
         Asian = paste0(all_of_race_var_stem, "!!Asian"),
         NHPI = paste0(all_of_race_var_stem, 
                       "!!Native Hawaiian and Other Pacific Islander"),
         # Other = paste0(all_of_race_var_stem, 
         #               "!!Some other race"),
         White_NH = paste0(NH_var_stem, "!!White",
                           " alone"),
         Black_NH = paste0(NH_var_stem, "!!Black or African American",
                           " alone"),
         AIAN_NH = paste0(NH_var_stem, 
                       "!!American Indian and Alaska Native",
                       " alone"),
         Asian_NH = paste0(NH_var_stem, "!!Asian",
                           " alone"),
         NHPI_NH = paste0(NH_var_stem, 
                       "!!Native Hawaiian and Other Pacific Islander",
                       " alone")#,
         # Other_NH = paste0(NH_var_stem,
         #               "!!Some other race", " alone")
         ) %>%            # Rename race vars
  mutate(AsianNHPI = Asian + NHPI,
         AsianNHPI_NH = Asian_NH + NHPI_NH) %>% 
  filter(NAME != "Puerto Rico")                             # Remove Puerto Rico
  
# Calcuate Percent of total Population
vars_to_not_change <- vars(-GEO_ID, -state, -NAME, -Population)

# ACS Percent Demographics with Deomgraphic Populations
acs_percents_total <- 
  acs_race_vars %>% 
  mutate_at(vars_to_not_change, 
            ~ . / Population * 100) %>%                     # Calculate percents
  rename_at(vars_to_not_change,
            ~ paste0(., "_perc")) %>%                       # Rename percent vars
  inner_join(acs_race_vars) %>%                             # Add total populations
  select((order(colnames(.)))) %>%                          # Alphabetize columns
  select(GEO_ID, state, NAME, Population, everything())     # Reorder vars

# ACS Percent Demographics only
acs_percents <-
  acs_percents_total %>% 
  select(GEO_ID, state, NAME, Population,
         ends_with("perc"))

acs_map_population_percents <-
  acs_percents %>% 
  select(everything(), -ends_with("NH_perc")) %>%           # Get rid of NH (for now)
  rename_at(vars_to_not_change, 
            ~ substr(., 1, str_length(.) - 5)) %>%          # Remove "_perc"
  select(everything(), -"AsianNHPI") %>%                    # Remove AsianNHPI
  mutate_at(vars_to_not_change,
            ~ round(., digits = 1)) %>%                     # Round to two decimals
  gather(key = "Category", value = "Percent",
         -GEO_ID, -state, -NAME, -Population) %>%           # Convert to long data
  mutate(abb = case_when(
    NAME == "District of Columbia" ~ "DC",
    TRUE ~ state.abb[match(NAME, state.name)]
  )) %>%                                                    # Add state abbr.
  arrange(NAME) %>% 
  mutate(
    Category = case_when(
      Category == "AIAN" ~ "American Indian or Alaska Native",
      Category == "NHPI" ~ "Native Hawaiian or Pacific Islander",
      TRUE ~ Category
      )
    ) %>% 
  mutate(
    flavortext = paste0(NAME, '<br>',
                  Category, ': ', Percent, '<br>')
  )                                                         # Add hovertext


##### Get Dataset of Ratios #####

acs_map_data_for_ratios <-
  acs_percents %>% 
  select(everything(), -ends_with("NH_perc")) %>%           # Get rid of NH (for now)
  rename_at(vars_to_not_change, 
            ~ substr(., 1, str_length(.) - 5)) %>%          # Remove "_perc"
  select(everything(), -"AsianNHPI") %>%                    # Remove AsianNHPI
  gather(key = "Category", value = "Percent",
         -GEO_ID, -state, -NAME, -Population) %>%           # Convert to long data
  mutate(abb = case_when(
    NAME == "District of Columbia" ~ "DC",
    TRUE ~ state.abb[match(NAME, state.name)]
  )) %>%                                                    # Add state abbr.
  mutate(
    Category = case_when(
      Category == "AIAN" ~ "American Indian or Alaska Native",
      Category == "NHPI" ~ "Native Hawaiian or Pacific Islander",
      TRUE ~ Category
      )
    ) %>% 
  arrange(NAME)

# Import COVID19 Racial Tracker Data
covidcases_raw <- read_csv("RacialTrackerCases20200502.csv",
                           col_types = c(StateAbbr = 'c'))
coviddeaths_raw <- read_csv("RacialTrackerDeaths20200502.csv",
                            col_types = c(StateAbbr = 'c'))

# Get vector of regions not 50 states nor DC
allregionsvec <- unique(covidcases_raw$StateAbbr)           # Get all states
not50statesnordc <- 
  allregionsvec[!(allregionsvec %in% c(state.abb, "DC"))]   # Regions not DC nor 50 states

# Clean covid case data
covidcases <-
  covidcases_raw %>% 
  select(StateAbbr, Count = "Daily Count", "Known White", 
         "Known Black", "Known Asian", "Known AIAN", 
         "Known NHPI", "Unknown Race", "Known Hispanic",
         "Unknown Ethnicity") %>%                           # Isolate necessary vars
  rename_at(vars(-StateAbbr, -Count, 
                 -"Unknown Race", -"Unknown Ethnicity"),
            ~substr(., 7, str_length(.))) %>%               # Remove "known" from names
  filter(!(StateAbbr %in% not50statesnordc)) %>%            # 50 states and DC only
  mutate_at(vars(-StateAbbr, -Count),
            ~(./Count * 100)) %>%                           # Calculate percents
  select(everything(), -Count) %>% 
  gather(key = "Category", value = "CasePercent",
         -StateAbbr) %>% 
  mutate(
    Category = case_when(
      Category == "AIAN" ~ "American Indian or Alaska Native",
      Category == "NHPI" ~ "Native Hawaiian or Pacific Islander",
      TRUE ~ Category
      )
  )

# Clean covid death data
coviddeaths <-
  coviddeaths_raw %>% 
  select(StateAbbr, Count = "Daily Count", "Known White", 
         "Known Black", "Known Asian", "Known AIAN", 
         "Known NHPI", "Unknown Race", "Known Hispanic",
         "Unknown Ethnicity") %>%                           # Isolate necessary vars
  rename_at(vars(-StateAbbr, -Count, 
                 -"Unknown Race", -"Unknown Ethnicity"),
            ~substr(., 7, str_length(.))) %>%               # Remove "known" from names
  filter(!(StateAbbr %in% not50statesnordc)) %>%            # 50 states and DC only
  mutate_at(vars(-StateAbbr, -Count),
            ~(./Count * 100)) %>%                           # Calculate percents
  select(everything(), -Count) %>% 
  gather(key = "Category", value = "DeathPercent",
         -StateAbbr) %>% 
  mutate(
    Category = case_when(
      Category == "AIAN" ~ "American Indian or Alaska Native",
      Category == "NHPI" ~ "Native Hawaiian or Pacific Islander",
      TRUE ~ Category
      )
    )


# Make dataset containing percent cases/deaths over percent pop
covid_acs <-
  inner_join(acs_map_data_for_ratios, covidcases,
             by = c("abb" = "StateAbbr", 
                    "Category" = "Category")) %>% 
  inner_join(coviddeaths,
             by = c("abb" = "StateAbbr", 
                    "Category" = "Category")) %>%           # Join datasets
  rename(PopulationPercent = "Percent") %>%                 # Rename percent cases
  mutate(CasesRatio = CasePercent/PopulationPercent) %>%    # Percent cases / percent pop
  mutate(DeathsRatio = DeathPercent/PopulationPercent) %>%  # Percent deaths / percent pop
  mutate_at(vars(PopulationPercent,
                 CasesRatio, DeathsRatio,
                 CasePercent, DeathPercent),
            ~ round(., digits = 2)) %>%                     # Round to two decimals
  filter(Category != "Other") %>%                           # Remove others
  mutate(flavortext = 
           paste0('<b>', NAME, '</b><br>',
                  'Race/Ethnicity: ', Category, '<br><br>',
                  'Percent ', Category, ': ', PopulationPercent, 
                  '<br>',
                  'Percent Cases: ', CasePercent, '<br>',
                  'Percent Deaths: ', DeathPercent, '<br>'
                  )
         )



##### Write CSV #####
write_csv(covid_acs, "covid_acs.csv")
write_csv(acs_map_data_for_ratios, "acs_map_data_for_ratios.csv")
write_csv(acs_map_population_percents, "acs_map_population_percents.csv")
