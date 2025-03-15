# CONFIGURATION ----------------------------------------------------------------

library(countrycode)
library(vdemdata)
library(modelsummary)
library(sandwich)
library(tinytable)
library(countrycode)
library(tidyverse)

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

# NB! REMEMBER TO LOAD THESE LIBERARIS WHEN GATHERING DATA!

#library(WDI)
library(readsdmx)

## Data grafting check ---------------------------------------------------------

# fbic_unique <- unique(infl_by_china$iso3c)
# vdem_unique <- unique(freexprs$iso3c)

# Values in vector fbic_unique that does not occur in vector vdem_unique. The 
# FBIC-data includes many countries not featured in the V-Dem dataset.

# setdiff(fbic_unique, vdem_unique)

# Values in vector vdem_unique that does not occur in vector fbic_unique. China 
# is the obvious one out as it is expressly not in the FBIC-dataset, then the 
# West Bank, Gaza, Somaliland, and Zanzibar also does not appear in the 
# FBIC-dataset.

# setdiff(vdem_unique, fbic_unique)

# Using the V-Dem dataset as the foundation, I join the V-Dem and FBIC data to
# create the base dataset everything else will build upon.

# FUNCTIONS --------------------------------------------------------------------

Density <- function(x) ''

# DEFINITIONS ------------------------------------------------------------------

## Restricted ------------------------------------------------------------------

# Creates a definition of the West that is relatively restricted: my criteria are
# that the country have a long history of democracy and is well established 
# within the Western sphere of influence. Countries that democratised relatively
# late like Poland and South Korea are not included.

west_1 <- c('Andorra', 'Australia', 'Austria', 'Belgium', 'Canada', 'Denmark', 
            'Finland', 'France', 'Germany', 'Greece', 'Iceland', 'Ireland', 
            'Israel', 'Italy', 'Japan', 'Liechtenstein', 'Luxembourg', 'Malta', 
            'Monaco', 'Netherlands', 'New Zealand', 'Norway', 'Portugal', 
            'San Marino', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom', 
            'United States')

## Expanded --------------------------------------------------------------------

# The more expansive definition of the West is less restricted: my criteria are 
# that the country belong to the EU27 or has a reputation of strong democracy
# tracing back to the 1990s.

west_2 <- c('Andorra', 'Australia', 'Austria', 'Belgium', 'Bulgaria', 'Canada', 
            'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 
            'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 
            'Ireland', 'Israel', 'Italy', 'Japan', 'Latvia', 'Liechtenstein', 
            'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'New Zealand', 
            'Norway', 'Poland', 'Portugal', 'Romania', 'San Marino', 'Slovakia', 
            'Slovenia', 'South Korea', 'Spain', 'Sweden', 'Switzerland', 
            'Taiwan', 'United Kingdom', 'United States')

# V-DEM DATA -------------------------------------------------------------------

## 1994 subset -----------------------------------------------------------------

# Loading V-Dem dataset, and sub-setting years after 1994 and adding in COW_codes
# and ISO3c-codes for countries missing this/are ambiguous.  

base <- vdem %>% 
  filter(year >= 1994) %>% 
  mutate(COWcode = case_when(country_name == 'Palestine/West Bank' ~ 667,
                             country_name == 'Palestine/Gaza'      ~ 668,
                             country_name == 'Somaliland'          ~ 521,
                             country_name == 'Hong Kong'           ~ 715,
                             country_name == 'Zanzibar'            ~ 511,
                             .default = as.numeric(COWcode)),
         iso3c = countrycode(COWcode, origin = 'cown', destination = 'iso3c'),
         iso3c = case_when(country_name == 'Kosovo'              ~ 'UNK',
                           country_name == 'Palestine/West Bank' ~ 'PWB',
                           country_name == 'Palestine/Gaza'      ~ 'PGZ',
                           country_name == 'Somaliland'          ~ 'SOL',
                           country_name == 'Hong Kong'           ~ 'HKG',
                           country_name == 'Serbia'              ~ 'SRB',
                           country_name == 'Zanzibar'            ~ 'EAZ',
                           .default = as.character(iso3c)))%>% 
  select(c(country_name,
           iso3c,
           year, 
           v2x_freexp_altinf, # Freedom of Expression and Alternative Information index (V-Dem Codebook, pp. 50-51)
           v2mecenefi,        # Internet censorship effort (V-Dem Codebook, pp. 207-208)
           v2x_regime)) %>%   # Regimes of the world ordinal scale (V-Dem Codebook, pp. 292-293)
  rename(freedom  = v2x_freexp_altinf,
         internet = v2mecenefi,
         regime   = v2x_regime)

# FBIC INDEX DATA --------------------------------------------------------------

## Restrict and save smaller version -------------------------------------------

if(!file.exists('data/fbic_short.RData')){
  fbic <- read.csv('data/fbic.csv') %>% 
    filter(year >= 1994 & countrya %in% c('China', west_2))
  
  save(fbic, file = 'data/fbic_short.RData')
}

## Load fbic index -------------------------------------------------------------

load('data/fbic_short.RData')

# The main FBIC-variable is measured from 0 to 1, where 0 indicates no 
# influence from Country A on Country B, and 1 indicates the most influence ever 
# measured between two countries from 1960 through 2018. Moyer et al., Codebook,
# p. 24.

# This variable measures how much influence another country has on China,
# Spoiler alert! in 2023 it was Russia.

# infl_on_china <- fbic %>% 
#   filter(countryb == 'China' & year >= 1994)

## China Influence Data --------------------------------------------------------

# Filter all rows where China is the influencer (China == countryb), for all
# years from 1994 (this threshold comes from Lührmann and Lindberg p. 1102).

infl_by_china <- fbic %>% 
  filter(countrya == 'China' & year >= 1994) %>% 
  select(c(countryb, year, iso3b, fbic)) %>% 
  rename(iso3c = iso3b) %>% 
  group_by(iso3c)

## West_1 influence data -------------------------------------------------------

# Create data set with a restricted definition of the 'West'

west_1_data <- fbic %>% 
  filter(countrya %in% west_1 & year >= 1994) %>% 
  select(c(countryb, year, iso3b, fbic)) %>% 
  rename(iso3c = iso3b) %>% 
  group_by(iso3c, year)

# Summarise the total influence from restricted 'West'

infl_by_west_1 <- west_1_data %>% 
  summarise(sum(fbic)) %>% 
  rename(west_1 = 'sum(fbic)')

## West_2 influence data -------------------------------------------------------

# Create data set with an expanded definition of the 'West'

west_2_data <- fbic %>% 
  filter(countrya %in% west_2 & year >= 1994) %>% 
  select(c(countryb, year, iso3b, fbic)) %>% 
  rename(iso3c = iso3b) %>% 
  group_by(iso3c, year)

# Summarise the total influence from expanded 'West'

infl_by_west_2 <- west_2_data %>% 
  summarise(sum(fbic)) %>% 
  rename(west_2 = 'sum(fbic)')










# CONTROL VARIABLE DATA --------------------------------------------------------

## WDI data --------------------------------------------------------------------

# Here I load data from the World Bank for GDP per capita and natural resource 
# rents. This data is subset for the period 1994-2023.

# GDP PER CAPITA
# The GDP per capita variable has many missing variables, so it will be combined
# with data from the IMF and the UN. The average of these data will be used as 
# the GDP per capita variable when modeling.

# NATURAL RESOURCE RENTS
# I will use the World Bank data on total natural resources rent (% of GDP) 
# [NY.GDP.TOTL.RT.ZS] this has data from 1970 to 2021.
# URL: https://databank.worldbank.org/source/adjusted-net-savings/Series/NY.GDP.TOTL.RT.ZS
#
# Note: using this variable will remove some observations.

## WDI data load ---------------------------------------------------------------

if(!file.exists('data/wdi.Rdata')){
  wdi <- WDI(indicator = c('NY.GDP.PCAP.CD', 
                           'NY.GDP.TOTL.RT.ZS', 
                           'NY.GDP.PETR.RT.ZS', 
                           'NY.GDP.NGAS.RT.ZS'), 
             start     = 1994) %>% 
    filter(!(iso3c %in% c('AFE', 'AFW', 'ARB', 'CSS', 'CEB', 'EAR', 'EAS', 'EAP', 
                          'TEA', 'EMU', 'ECS', 'ECA', 'TEC', 'EUU', 'FCS', 'HPC', 
                          'IBD', 'IBT', 'IDB', 'IDX', 'LTE', 'LCN', 'LAC', 'TLA', 
                          'LDC', 'LMY', 'MEA', 'MNA', 'TMN', 'MIC', 'NAC', 'OED', 
                          'OSS', 'PSS', 'PST', 'PRE', 'SST', 'SAS', 'TSA', 'SSF', 
                          'SSA', 'TSS', 'WLD', 'IDA'))) %>% 
    filter(!(iso2c %in% c('XD', 'XM', 'XN', 'XY', 'XT'))) %>% 
    mutate(iso3c = case_when(iso3c == 'XKX' ~ 'UNK',
                             .default = as.character(iso3c))) %>% 
    rename(gdppc = NY.GDP.PCAP.CD,
           rents = NY.GDP.TOTL.RT.ZS,
           oil = NY.GDP.PETR.RT.ZS,
           gas = NY.GDP.NGAS.RT.ZS) %>% 
    select(!(iso2c))

# Saves WDI data locally to not keep asking WDI for data

save(wdi, file = 'data/wdi.Rdata')
}

# Loads WDI data

load('data/wdi.Rdata')

wdi <- wdi %>% 
  rename(gdppc_wdi = gdppc)


# The only country that is in the vector base_unique, that does not 
# appear in the wdi_unique vector is Taiwan.  

# wdi_unique <- unique(wdi$iso3c)
# base_unique <- unique(base$iso3c)
# 
# setdiff(base_unique, wdi_unique)

## IMF data --------------------------------------------------------------------

# Loads the IMF data on GDP per capita. I did not get the API to work as I could
# not find the flowRef. The data was in a wide format, so I had to change it to 
# a long format.

imf <- read.csv('data/gdppc_imf.csv', sep = ';') %>% 
  rename(iso3c = ISO, country = Country) %>% 
  select(country, iso3c, dplyr::starts_with('X')) %>% 
  pivot_longer(dplyr::starts_with('X'),
                 names_to = 'year',
                 values_to = 'gdppc_imf') %>% 
  mutate(year     = as.numeric(str_remove(year, 'X')),
         iso3c    = case_when(iso3c == 'UVK' ~ 'UNK',
                                .default = as.character(iso3c)),
         gdppc_imf = ifelse(gdppc_imf == 'n/a', NA, gdppc_imf),
         gdppc_imf = str_remove(gdppc_imf, ','),
         gdppc_imf = as.numeric(gdppc_imf)) %>% 
  filter(country != '') %>% 
  select(iso3c, year, gdppc_imf)

# Makes a data frame for the two missing countries to be added to the World Bank
# data. This is Taiwan and Venezuela. 

# imf_supplement <- imf %>% 
#   filter(iso3c %in% c('TWN', 'VEN')) %>% 
#   mutate(country = str_replace(country, 'Taiwan Province of China', 'Taiwan')) %>% 
#   rename(gdppc = gdppc_imf)
# 
# # Comparison data for WDI and IMF data.
# 
# gdp_compare <- wdi %>% 
#   full_join(imf, by = c('iso3c', 'year'))

## UN data ---------------------------------------------------------------------

# URL: http://data.un.org/Data.aspx?q=gdp+per+capita&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a1

un <- read.csv('data/gdppc_un.csv') %>% 
  rename(country = Country.or.Area,
         year = Year,
         gdppc_un = Value) %>% 
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c'),
         iso3c = ifelse(country == 'Kosovo', 'UNK', as.character(iso3c)),
         gdppc_un = as.numeric(gdppc_un),
         year = as.numeric(year)) %>% 
  filter(year >= 1994 & country != 'United Republic of Tanzania: Zanzibar') %>% 
  select(iso3c, year, gdppc_un)

# The UN variable for Tanzania is split up into Tanzania (mainland) and Tanzania
# (Zanzibar) this is a problem when combining and using the data. Since the
# population in Zanzibar is a magnitude smaller than for the mainland, I have
# decided to simply remove Zanzibar from the data.
#
# Population:
# Tanzania (mainland) = 67.4 million
# Tanzania (mainland) = 1.9 million 

## OECD data -------------------------------------------------------------------

## ODA current prices (OECD)

# if(!file.exists('data/oda.csv')){
#   sdmx <- read_sdmx('https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC2@DF_DAC2A,1.2/ALLD.BIH+HRV+CYP+GIB+XKV+MLT+MDA+MNE+MKD+SRB+SVN+TUR+UKR+DZA+EGY+LBY+MAR+TUN+F4_X+BDI+COM+DJI+ERI+ETH+KEN+MDG+MWI+MUS+MYT+MOZ+RWA+SYC+SOM+SSD+SDN+TZA+UGA+ZMB+ZWE+AGO+CMR+CAF+TCD+COG+COD+GNQ+GAB+STP+BWA+SWZ+LSO+NAM+ZAF+BEN+BFA+CPV+CIV+GMB+GHA+GIN+GNB+LBR+MLI+MRT+NER+NGA+SHN+SEN+SLE+TGO+AIA+ATG+ABW+BHS+BRB+VGB+CYM+CUB+DMA+DOM+GRD+HTI+JAM+MSR+SXM+KNA+LCA+VCT+TTO+TCA+BLZ+CRI+SLV+GTM+HND+MEX+NIC+PAN+ARG+BOL+BRA+CHL+COL+ECU+GUY+PRY+PER+SUR+URY+VEN+BMU+BRN+KHM+CHN+PRK+HKG+IDN+KOR+LAO+MAC+MYS+MNG+PHL+SGP+TWN+THA+TLS+VNM+AFG+ARM+AZE+BGD+BTN+GEO+IND+KAZ+KGZ+MDV+MMR+NPL+PAK+LKA+TJK+TKM+UZB+BHR+IRN+IRQ+ISR+JOR+KWT+LBN+OMN+QAT+SAU+SYR+ARE+PSE+YEM+FJI+NCL+PNG+SLB+VUT+KIR+MHL+FSM+NRU+MNP+PLW+COK+PYF+NIU+WSM+TKL+TON+TUV+WLF+BLR+ALB.206.USD.V?startPeriod=1994&dimensionAtObservation=AllDimensions')
#   write.csv(sdmx, 'data/oda.csv')
# }
# 
# oda_oecd <- read.csv('data/oda.csv') %>% 
#   rename(iso3c = RECIPIENT,
#          year  = TIME_PERIOD,
#          oda   = ObsValue) %>% 
#   select(iso3c, year, oda)
# 
# # Sets attribute
# attr(oda_oecd$iso3c, 'label') <- 'ISO3C code'
# attr(oda_oecd$year,  'label') <- 'Year'
# attr(oda_oecd$oda,   'label') <- 'Official Development Assistance [ODA] (current US$ in millions)'

## GDP per capita ---------------------------------------------------------------

# GDP per capita based on averaged data from the World Bank, International 
# Monetary Found, and United Nations. Includes a logged variable of GDP per capita.

## Gdppc full dataset -----------------------------------------------------------

gdppc <- wdi %>% 
  select(country, iso3c, year, gdppc_wdi) %>% 
  full_join(imf, by = c('iso3c', 'year')) %>% 
  full_join(un, by = c('iso3c', 'year')) %>% 
  mutate(gdppc     = rowMeans(across(starts_with('gdppc')), na.rm = T),
         gdppc     = ifelse(gdppc == 'NaN', NA, gdppc),
         gdppc_log = log(gdppc))

# Sets attributes to the dataframe

attr(gdppc$country,   'label') <- 'Country name'
attr(gdppc$iso3c,     'label') <- 'ISO3C code'
attr(gdppc$year,      'label') <- 'Year'
attr(gdppc$gdppc_wdi, 'label') <- 'GDP per capita from WDI (current US$)'
attr(gdppc$gdppc_imf, 'label') <- 'GDP per capita from IMF (current US$)'
attr(gdppc$gdppc_un,  'label') <- 'GDP per capita from UN (current US$)'
attr(gdppc$gdppc,     'label') <- 'Averaged GDP per capita (current US$, WDI, IMF, UN)'
attr(gdppc$gdppc_log, 'label') <- 'Averaged GDP per capita logged (current US$, WDI, IMF, UN)'

## Gdppc subset data ------------------------------------------------------------

# Subset used to make combining data easier

gdppc_subset <- gdppc %>% 
  select(iso3c, year, gdppc, gdppc_log)

## Natural resource rents ------------------------------------------------------

### Dataset combining total, oil, and gas rents --------------------------------

rents <- wdi %>%
  select(iso3c, year, rents, oil, gas)

## ODA % of GNI (OECD) ---------------------------------------------------------

if(!file.exists('data/odagni.csv')){
  sdmx <- read_sdmx('https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC2@DF_DAC2A,1.2/ALLD.BIH+HRV+CYP+GIB+XKV+MLT+MDA+MNE+MKD+SRB+SVN+TUR+UKR+DZA+EGY+LBY+MAR+TUN+F4_X+BDI+COM+DJI+ERI+ETH+KEN+MDG+MWI+MUS+MYT+MOZ+RWA+SYC+SOM+SSD+SDN+TZA+UGA+ZMB+ZWE+AGO+CMR+CAF+TCD+COG+COD+GNQ+GAB+STP+BWA+SWZ+LSO+NAM+ZAF+BEN+BFA+CPV+CIV+GMB+GHA+GIN+GNB+LBR+MLI+MRT+NER+NGA+SHN+SEN+SLE+TGO+AIA+ATG+ABW+BHS+BRB+VGB+CYM+CUB+DMA+DOM+GRD+HTI+JAM+MSR+SXM+KNA+LCA+VCT+TTO+TCA+BLZ+CRI+SLV+GTM+HND+MEX+NIC+PAN+ARG+BOL+BRA+CHL+COL+ECU+GUY+PRY+PER+SUR+URY+VEN+BMU+BRN+KHM+CHN+PRK+HKG+IDN+KOR+LAO+MAC+MYS+MNG+PHL+SGP+TWN+THA+TLS+VNM+AFG+ARM+AZE+BGD+BTN+GEO+IND+KAZ+KGZ+MDV+MMR+NPL+PAK+LKA+TJK+TKM+UZB+BHR+IRN+IRQ+ISR+JOR+KWT+LBN+OMN+QAT+SAU+SYR+ARE+PSE+YEM+FJI+NCL+PNG+SLB+VUT+KIR+MHL+FSM+NRU+MNP+PLW+COK+PYF+NIU+WSM+TKL+TON+TUV+WLF+BLR+ALB.286.USD.V?startPeriod=1994&dimensionAtObservation=AllDimensions')
  write.csv(sdmx, 'data/odagni.csv')
}

oda_gni <- read.csv('data/odagni.csv') %>% 
  rename(iso3c = RECIPIENT,
         year = TIME_PERIOD,
         oda = ObsValue) %>% 
  select(iso3c, year, oda) %>% 
  mutate(iso3c = ifelse(iso3c == 'XKV', 'UNK', iso3c),
         oda   = ifelse(oda < 0, 0, oda)) %>% # Sets country-years where ODA is negative to 0
  data.frame()

# The oda_gni change is to put 0 on each country that does not receive ODA, but
# is not 'poor', e.g., they belong in the dataset as donors or no-receivers, not
# as missing data. 

# Sets attributes to dataframe

attr(oda_gni$iso3c, 'label') <- 'ISO3C code'
attr(oda_gni$year,  'label') <- 'Year'

## Combined OECD data ---------------------------------------------------------

# oecd_oda <- oda_oecd %>% 
#   full_join(oda_gni, by = join_by(iso3c, year))

# Just use the oda_gni dataset, its better. 

## Consolidated Democracies ----------------------------------------------------

## Dataset ---------------------------------------------------------------------

# I Define a consolidated democracy as having an Electoral Democracy Index score
# of >= 0.8 for at least 15 consecutive years. 

consolidation <- vdem %>% 
  mutate(COWcode = case_when(country_name == 'Palestine/West Bank' ~ 667,
                             country_name == 'Palestine/Gaza'      ~ 668,
                             country_name == 'Somaliland'          ~ 521,
                             country_name == 'Hong Kong'           ~ 715,
                             country_name == 'Zanzibar'            ~ 511,
                             .default = as.numeric(COWcode)),
         iso3c = countrycode(COWcode, origin = 'cown', destination = 'iso3c'),
         iso3c = case_when(country_name == 'Kosovo'              ~ 'UNK',
                           country_name == 'Palestine/West Bank' ~ 'PWB',
                           country_name == 'Palestine/Gaza'      ~ 'PGZ',
                           country_name == 'Somaliland'          ~ 'SOL',
                           country_name == 'Hong Kong'           ~ 'HKG',
                           country_name == 'Serbia'              ~ 'SRB',
                           country_name == 'Zanzibar'            ~ 'EAZ',
                           .default = as.character(iso3c))) %>% 
  select(country_name, year, iso3c, v2x_polyarchy, v2x_libdem) %>% 
  drop_na() %>% 
  group_by(iso3c) %>% 
  mutate(condition      = v2x_polyarchy >= .8, # Condition saying EDI >= 0.8
         cumulative_sum = cumsum(condition),   # Cumulative sum for condition variable
         difference     = diff(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, cumulative_sum), 15), # I do not understand this 
         consolidated   = difference == 15,    # Checks whether there are 10 consecutive years of high-level democracy 
         cons_numeric   = ifelse(consolidated == T, 1, 0)) %>% 
  filter(year >= 1994)

# Subset 

consolidation_subset <- consolidation %>% 
  select(iso3c, year, cons_numeric) %>% 
  rename(consolidated_democracy = cons_numeric)



# BASE DATA --------------------------------------------------------------------

# base DATA -------------------------------------------------------------

base <- base %>% 
  left_join(infl_by_china, by = c('iso3c', 'year')) %>% 
  filter(!(iso3c %in% c('PWB', 'PGZ', 'SOL', 'EAZ', 'CHN'))) %>% 
  select(!(countryb)) %>%
  filter(!is.na(fbic)) %>% 
  rename(country  = country_name) %>% 
  ungroup()

# Included variables:
#
# country                = English country name               (country_name from V-Dem)
# iso3c                  = ISO3c code                         (used to join data [V-Dem is base dataset])
# year                   = Year                               (used to join data)
# freedom                = Dependent variable                 (v2x_freexp_altinf [V-Dem])
# internet               = Internet censorship                (v2mecenefi [V-dem])
# fbic                   = FBIC-score                         (fbic [FBIC])
# regime                 = Regimes of the world               (v2x_regime [V-Dem])
# west_1                 = Restricted West                    (See fbic.R)
# west_2                 = Expanded West                      (See fbic.R)
# gdppc_log              = GDP per capita (log)               ([WDI, IMF, UN])
# rents                  = Natural resource rents (% of GDP)  (NY.GDP.TOTL.RT.ZS [WDI])
# oil                    = Oil rents                          (NY.GDP.PETR.RT.ZS [WDI])
# gas                    = Gas rents                          (NY.GDP.NGAS.RT.ZS [WDI])
# oda                    = Aid (% of GNI)                     (OECD.DCD.FSD,DSD_DAC2@DF_DAC2A,1.2 [OECD])
# consolidated_democracy = EDI >= .8 for 15 consecutive years (Own calculation [V-Dem])

base <- base %>% 
  left_join(infl_by_west_1,       by = join_by(iso3c, year)) %>% 
  left_join(infl_by_west_2,       by = join_by(iso3c, year)) %>% 
  left_join(gdppc_subset,         by = join_by(iso3c, year)) %>% 
  left_join(rents,                by = join_by(iso3c, year)) %>% 
  left_join(oda_gni,              by = join_by(iso3c, year)) %>% 
  left_join(consolidation_subset, by = join_by(iso3c, year)) %>% 
  mutate(oda = case_when(iso3c == 'ARE' & year > 1995 ~ 0,
                         iso3c == 'AUS' ~ 0,
                         iso3c == 'AUT' ~ 0,
                         iso3c == 'BEL' ~ 0,
                         iso3c == 'BGR' ~ 0,
                         iso3c == 'BHR' & year > 2004 ~ 0,
                         iso3c == 'BLR' & year < 2005 ~ 0,
                         iso3c == 'BRB' & year > 2010 ~ 0,
                         iso3c == 'CAN' ~ 0,
                         iso3c == 'CHE' ~ 0,
                         iso3c == 'CHL' & year > 2018 ~ 0,
                         iso3c == 'CYP' & year > 1996 ~ 0,
                         iso3c == 'CZE' ~ 0,
                         iso3c == 'DEU' ~ 0,
                         iso3c == 'DNK' ~ 0,
                         iso3c == 'ESP' ~ 0,
                         iso3c == 'EST' ~ 0,
                         iso3c == 'FIN' ~ 0,
                         iso3c == 'FRA' ~ 0,
                         iso3c == 'GBR' ~ 0,
                         iso3c == 'GRC' ~ 0,
                         iso3c == 'HKG' ~ 0,
                         iso3c == 'HUN' ~ 0,
                         iso3c == 'IRL' ~ 0,
                         iso3c == 'ISL' ~ 0,
                         iso3c == 'ISR' & year > 1996 ~ 0,
                         iso3c == 'ITA' ~ 0,
                         iso3c == 'JPN' ~ 0,
                         iso3c == 'KOR' & year > 1999 ~ 0,
                         iso3c == 'KWT' & year > 1995 ~ 0,
                         iso3c == 'LTU' ~ 0,
                         iso3c == 'LUX' ~ 0,
                         iso3c == 'LVA' ~ 0,
                         iso3c == 'MLT' ~ 0,
                         iso3c == 'NLD' ~ 0,
                         iso3c == 'NOR' ~ 0,
                         iso3c == 'NZL' ~ 0,
                         iso3c == 'OMN' & year > 2010 ~ 0,
                         iso3c == 'POL' ~ 0,
                         iso3c == 'PRT' ~ 0,
                         iso3c == 'QAT' & year > 1995 ~ 0,
                         iso3c == 'ROU' ~ 0,
                         iso3c == 'RUS' ~ 0,
                         iso3c == 'SAU' & year > 2007 ~ 0,
                         iso3c == 'SGP' & year > 1995 ~ 0,
                         iso3c == 'SVK' ~ 0,
                         iso3c == 'SVN' & year > 2002 ~ 0,
                         iso3c == 'SWE' ~ 0,
                         iso3c == 'SYC' & year > 2017 ~ 0,
                         iso3c == 'TTO' & year > 2010 ~ 0,
                         iso3c == 'TWN' ~ 0,
                         iso3c == 'UKR' & year < 2005 ~ 0,
                         iso3c == 'URY' & year > 2017 ~ 0,
                         iso3c == 'USA' ~ 0,
                         .default = as.numeric(oda)))

# Sets attributes to dataframe

attr(base$country,  'label') <- 'Country name'
attr(base$iso3c,    'label') <- 'ISO3C code'
attr(base$year,     'label') <- 'Year'
attr(base$freedom,  'label') <- 'Freedom of expression (v2x_freexp_altinf)'
attr(base$internet, 'label') <- 'Internet censorship (v2mecenefi)'
attr(base$fbic,     'label') <- 'FBIC index score from China (fbic)'
attr(base$regime,   'label') <- 'Ordinal regime variable 0-3 (v2x_regime)'
attr(base$west_1,                 'label') <- 'FBIC index total score from the \'West\' (restricted)'
attr(base$west_2,                 'label') <- 'FBIC index total score from the \'West\' (expanded)'
attr(base$oda,                    'label') <- 'Official Development Assistance [ODA] (% of GNI)'
attr(base$consolidated_democracy, 'label') <- 'EDI-score of >= 0.8 for 15 consecutive year'

# Removes datasets to keep the environment tidy

rm(fbic, infl_by_china, west_1_data, west_2_data, infl_by_west_1, infl_by_west_2, gdppc, gdppc_subset, rents, 
   oda_gni, consolidation, consolidation_subset, imf, un, wdi)

# VARIABLE CHECK ---------------------------------------------------------------

## Summary ---------------------------------------------------------------------

datasummary(freedom + fbic + regime + west_2 + gdppc + rents + oda ~ 
              N + Mean + Median + SD + Min + Max + Density, 
            data = base) %>% 
  plot_tt(j = 8,
          fun = 'density',
          data = list(base$freedom, base$fbic,
                      base$regime, base$west_2, 
                      base$gdppc, base$rents, 
                      base$oda),
          color = '#ff9214')






