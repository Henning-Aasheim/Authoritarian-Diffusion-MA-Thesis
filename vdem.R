# CONFIGURATION ----------------------------------------------------------------

library(vdemdata)
library(ggtext)
library(maps)
library(countrycode)
library(gt)
library(scales)
library(tidyverse)

## Palettes --------------------------------------------------------------------

### Palette defining five levels ------------------------------------------------

palette_7_free <- setNames(c('#ff9214', '#ffb168', '#ffd1ac', '#f1f1f1', 
                             '#a4b1bd', '#5b758b', '#003f5c'),  
                           c('Very strong positive change',
                             'Strong positive change', 
                             'Positive change', 
                             'No or small change', 
                             'Negative Change', 
                             'Strong negative change',
                             'Very strong negative change'))

breaks_7_free <- c('Very strong positive change', 
                   'Strong positive change', 
                   'Positive change', 
                   'No or small change', 
                   'Negative Change', 
                   'Strong negative change',
                   'Very strong negative change')

# FUNCTIONS --------------------------------------------------------------------

full_summary <- function(x){
  summary <- list(tibble(n = length(x[!is.na(x)]), # Calculates number of observations
                         mean = mean(x, na.rm=T),  # Calculates mean
                         median = median(x),       # Calculates median
                         sd = sd(x, na.rm=T),      # Calculates standard deviation
                         min = min(x),             # Calculates minimum value
                         max = max(x)))            # Calculates maximum value
  return(summary)
}

# V-DEM DATA -------------------------------------------------------------------

# Loading V-Dem dataset, and sub-setting years after 1994 and adding in COW_codes
# and ISO3c-codes for countries missing this/are ambiguous.  

vdem_1994 <- vdem %>% 
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
                           .default = as.character(iso3c)))


# Digital Society Survey -------------------------------------------------------

## Coordinated Information Operations ------------------------------------------

infop <- vdem_1994 %>% 
  filter(year >= 2000) %>% 
  select(c(country_name, 
           country_text_id,
           country_id,
           year, 
           v2smgovdom,  # Government dissemination of false information domestic
           v2smgovab,   # Government dissemination of false information abroad
           v2smpardom,  # Party dissemination of false information domestic
           v2smparab,   # Party dissemination of false information abroad
           v2smfordom,  # Foreign governments dissemination of false information
           v2smforads)) # Foreign governments ads

## Digital Media Freedom -------------------------------------------------------

digifree <- vdem_1994 %>% 
  filter(year >= 2000) %>% 
  select(c(country_name, 
           country_text_id,
           country_id,
           year, 
           v2smgovfilcap,   # Government Internet filtering capacity
           v2smgovfilprc,   # Government Internet filtering in practice
           v2smgovshutcap,  # Government Internet shut down capacity
           v2smgovshut,     # Government Internet shut down in practice
           v2smgovsm,       # Government social media shut down in practice
           v2smgovsmalt,    # Government social media alternatives
           v2smgovsmmon,    # Government social media monitoring
           v2smgovsmcenprc, # Government social media censorship in practice
           v2smgovcapsec,   # Government cyber security capacity
           v2smpolcap))     # Political parties cyber security capacity

## State Internet Regulation Capacity and Approach -----------------------------

intreg <- vdem %>% 
  filter(year >= 2000) %>% 
  select(c(country_name, 
           country_text_id,
           country_id,
           year, 
           v2smregcon,  # Internet legal regulation content
           v2smprivex,  # Privacy protection by law exists
           v2smprivcon, # Privacy protection by law content
           v2smregcap,  # Government capacity to regulate online content
           v2smregapp,  # Government online content regulation approach
           v2smlawpr,   # Defamation protection
           v2smdefabu)) # Abuse of defamation and copyright law by elites

## Online Media Polarisation ---------------------------------------------------

onlimed <- vdem_1994 %>% 
  filter(year >= 2000) %>% 
  select(c(country_name,
           country_text_id,
           country_id,
           year, 
           v2smonex,   # Online media existence
           v2smonper,  # Online media perspectives
           v2smmefra)) # Online media fractionalisation

## Social Cleavages ------------------------------------------------------------

# I have excluded the following variables from the socclev dataset:
#
# v2smhargr       (Online harassment groups)
# v2smhargrtxt    (Other online harassment groups)
# v2smorgtypes    (Types of organization through social media)
# v2smorgtypestxt (Other types of organization through social media)
#
# The reason for this is that their structure is different from the other
# variables I use in the dataset.

socclev <- vdem_1994 %>% 
  filter(year >= 2000) %>% 
  select(c(country_name,
           country_text_id,
           country_id,
           year, 
           v2smorgviol,    # Use of social media to organise offline violence
           v2smorgavgact,  # Average people’s use of social media to organise offline action
           v2smorgelitact, # Elites’ use of social media to organise offline action
           v2smcamp,       # Party/candidate use of social media in campaigns
           v2smarrest,     # Arrests for political content
           v2smpolsoc,     # Polarization of society
           v2smpolhate))   # Political parties hate speech

# WORLD MAP DATA ---------------------------------------------------------------

world <- map_data('world') %>% 
  rename(country = region) %>% 
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c', warn = F),
         iso3c = case_when(country == 'Barbuda' ~ 'ATG',
                           country == 'Canary Islands' ~ 'ESP',
                           country == 'Micronesia' ~ 'FSM',
                           country == 'Heard Island' ~ 'AUS',
                           country == 'Chagos Archipelago' ~ 'GBR',
                           country == 'Kosovo' ~ 'UNK',
                           country == 'Saint Martin' ~ 'FRA',
                           country == 'Bonaire' ~ 'NLD',
                           country == 'Sint Eustatius' ~ 'NLD',
                           country == 'Saba' ~ 'NLD',
                           country == 'Madeira Islands' ~ 'PRT',
                           country == 'Azores' ~ 'PRT',
                           country == 'Ascension Island' ~ 'GBR',
                           country == 'Grenadines' ~ 'VCT',
                           country == 'Virgin Islands' & subregion == ' British' ~ 'GBR',
                           country == 'Virgin Islands' & subregion == ' US' ~ 'USA',
                           country == 'Greenland' ~ 'DNK',
                           country == 'French Guiana' ~ 'FRA',
                           .default = as.character(iso3c)))

# FREEDOM OF EXPRESSION DATA ---------------------------------------------------

## Base dataset ----------------------------------------------------------------

freexprs <- vdem_1994 %>% 
  select(c(country_name,
         iso3c,
         year, 
         v2x_freexp_altinf, # Freedom of Expression and Alternative Information index (V-Dem Codebook, pp. 50-51)
         v2mecenefi,        # Internet censorship effort (V-Dem Codebook, pp. 207-208)
         v2x_regime)) %>%   # Regimes of the world ordinal scale (V-Dem Codebook, pp. 292-293)
  rename(freedom  = v2x_freexp_altinf,
         internet = v2mecenefi,
         regime   = v2x_regime)

## Dataset including coordinates -----------------------------------------------

freedom_map <- freexprs %>% 
  filter(year == 2023) %>% 
  right_join(world, by = 'iso3c', relationship = 'many-to-many')

## Datasets of change in freedom of expression [1994-2023] ---------------------

freedom_change_29_map <- freexprs %>% 
  filter(year %in% c(1994, 2023)) %>% 
  group_by(iso3c) %>% 
  summarise(change = diff(freedom)) %>% 
  mutate(change_nom = case_when(change >= .4                     ~ 'Very strong positive change',
                                change <  .4   & change >=  .2   ~ 'Strong positive change',
                                change <  .3   & change >=  .025 ~ 'Positive change',
                                change <  .025 & change >= -.025 ~ 'No or small change',
                                change < -.025 & change >= -.2   ~ 'Negative Change',
                                change < -.2   & change >= -.4   ~ 'Strong negative change',
                                change <= -.4                    ~ 'Very strong negative change')) %>%  
  right_join(world, by = 'iso3c', relationship = 'many-to-many')

# Alternative division of nominal categories

# mutate(change_nom = case_when(change >= .5                   ~ 'Very strong positive change',
#                               change <  .5  & change >=  .3  ~ 'Strong positive change',
#                               change <  .3  & change >=  .05 ~ 'Positive change',
#                               change <  .05 & change >= -.05 ~ 'No or small change',
#                               change < -.05 & change >= -.3  ~ 'Negative Change',
#                               change < -.3  & change >= -.5  ~ 'Strong negative change',
#                               change <= -.5                  ~ 'Very strong negative change')) %>%  

## Datasets of change in freedom of expression [2013-2023] ---------------------

freedom_change_10_map <- freexprs %>% 
  filter(year %in% c(2013, 2023)) %>% 
  group_by(iso3c) %>% 
  summarise(change = diff(freedom)) %>% 
  mutate(change_nom = case_when(change >= .4                     ~ 'Very strong positive change',
                                change <  .4   & change >=  .2   ~ 'Strong positive change',
                                change <  .3   & change >=  .025 ~ 'Positive change',
                                change <  .025 & change >= -.025 ~ 'No or small change',
                                change < -.025 & change >= -.2   ~ 'Negative Change',
                                change < -.2   & change >= -.4   ~ 'Strong negative change',
                                change <= -.4                    ~ 'Very strong negative change')) %>%  
  right_join(world, by = 'iso3c', relationship = 'many-to-many')

# MAPS -------------------------------------------------------------------------

## Map of freedom of expression [2023] (Continuous) ----------------------------

freedom_map %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = freedom), colour = 'black') +
  scale_fill_continuous(high     = '#ff9214',
                        low      = '#003f5c',
                        na.value = 'grey80') +
  labs(fill = 'Chinese Influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

## Map of change in freedom of expression [1994-2023] (Continuous) -------------

freedom_change_29_map %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = change), colour = 'black') +
  scale_fill_continuous(high     = '#ff9214',
                        low      = '#003f5c',
                        na.value = 'grey80') +
  labs(fill = 'Chinese Influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

## Map of change in freedom of expression [1994-2023] (Nominal) ----------------

freedom_change_29_map %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = change_nom), colour = 'black') +
  scale_fill_manual(values = palette_7_free, breaks = breaks_7_free,
                    na.value = 'grey30') +
  labs(fill = 'Chinese influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

## Map of change in freedom of expression [2013-2023] (Nominal) ----------------

freedom_change_10_map %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = change_nom), colour = 'black') +
  scale_fill_manual(values = palette_7_free, breaks = breaks_7_free,
                    na.value = 'grey30') +
  labs(fill = 'Chinese influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

# INDICATORS -------------------------------------------------------------------

## Declining indicators 2013-2023 ----------------------------------------------

# Top 20 declining indicator (V_DEM: Nord et al. 2024)

declining_indicators_3 <- data.frame(
  indicator = c('Government censorship effort (Media)', 
                'Freedom of academic and cultural expression',
                'Harassment of journalists',
                'Elections free and fair',
                'CSO repression',
                'Range of consultation',
                'Freedom of discussion for women',
                'Freedom of discussion for men',
                'CSO entry and exit',
                'Reasoned justification',
                'Print/boadcast media perspectives',
                'Freedom of foreign movement',
                'Transparent laws with predictable enforcement',
                'Rigorous and impartial public administration',
                'Media bias',
                'Print/broadcast media critical',
                'Engaged society',
                'Respect counterarguments',
                'EMB autonomy',
                'Executive oversight'),
  type = c('Freedom of Expression',  'Freedom of Expression', 
           'Freedom of Expression',  'Clean Elections', 
           'Freedom of Association', 'Deliberative Component',
           'Freedom of Expression',  'Freedom of Expression', 
           'Freedom of Association', 'Deliberative Component', 
           'Freedom of Expression',  'Liberal Component',
           'Liberal Component',      'Liberal Component', 
           'Freedom of Expression',  'Freedom of Expression',  
           'Deliberative Component', 'Deliberative Component',
           'Clean Elections',        'Liberal Component'),
  decline = c(45, 39, 36, 35, 35, 34, 33, 31, 30, 29, 29, 27, 26, 26, 26, 25, 25, 24, 24, 23)
)

declining_indicators_3 %>% 
  ggplot(aes(y = reorder(indicator, decline))) +
  geom_bar(aes(x = decline, fill = type),
           stat = 'identity', 
           width = 0.7,
           position = position_dodge(width = .75)) +
  scale_x_continuous(expand = expansion(mult = 0),
                     breaks = seq(0, 46, 4)) +
  scale_fill_manual(values = c('#ffd1ac', '#ff9214', 
                               '#5b758b', '#003f5c', 'grey80')) +
  labs(x    = 'Number of declining indicators',
       y    = NULL,
       fill = 'Type:') +
  guides(fill = guide_legend(nrow = 2)) +
  theme_classic(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.justification = c(0, 1),
        legend.margin = margin(l = -100, unit = 'mm'),
        axis.title = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(linetype = 'dashed'))

## Declining indicators 2013-2023 ----------------------------------------------

# Top 20 declining indicator (V_DEM: Nord et al. 2024)

declining_indicators_4 <- data.frame(
  indicator = c('Government censorship effort (Media)', 
                'Freedom of academic and cultural expression',
                'CSO repression',
                'Harassment of journalists',
                'Engaged society',
                'Elections free and fair',
                'Transparent laws with predictable enforcement',
                'Range of consultation',
                'Media self-censorship',
                'Media bias',
                'Print/broadcast media critical',
                'Freedom of discussion for men',
                'Legislature investigates in practice',
                'CSO consultation',
                'Freedom of discussion for women',
                'CSO entry and exit',
                'Reasoned justification',
                'Print/boadcast media perspectives',
                'Opposition parties autonomy',
                'Freedom from political killings'),
  type = c('Freedom of Expression',  'Freedom of Expression', 
           'Freedom of Association', 'Freedom of Expression', 
           'Deliberative Component', 'Clean Elections',
           'Liberal Component',      'Deliberative Component', 
           'Freedom of Expression',  'Freedom of Expression', 
           'Freedom of Expression',  'Freedom of Expression',
           'Liberal Component',      'Participatory Component', 
           'Freedom of Expression',  'Freedom of Association',  
           'Deliberative Component', 'Freedom of Expression',
           'Freedom of Association', 'Liberal Component'),
  decline = c(44, 41, 41, 35, 33, 33, 32, 32, 32, 31, 30, 30, 29, 29, 28, 28, 27, 27, 27, 27)
)

declining_indicators_4 %>% 
  ggplot(aes(y = reorder(indicator, decline))) +
  geom_bar(aes(x = decline, fill = type),
           stat = 'identity', 
           width = 0.7,
           position = position_dodge(width = .75)) +
  scale_x_continuous(expand = expansion(mult = 0),
                     breaks = seq(0, 46, 4)) +
  scale_fill_manual(values = c('#444e86', '#ff6e54', '#ff9214', '#003f5c', '#dd5182', '#955196')) +
  labs(x    = 'Number of declining indicators',
       y    = NULL,
       fill = 'Type:') +
  guides(fill = guide_legend(nrow = 2)) +
  theme_classic(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.justification = c(0, 1),
        legend.margin = margin(l = -100, unit = 'mm'),
        axis.title = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(linetype = 'dashed'))
  
