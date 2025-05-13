# CONFIGURATION ---------------------------------------------------------------

library(maps) # Used to download map data and make maps
library(countrycode) # Used to make country names compatible
library(gt) # Used to make tables
library(scales) # Used to create bins and breaks for the tables (col_bin())
library(tidyverse) # Used for data wrangling

## Palettes --------------------------------------------------------------------

### Palette defining five levels ------------------------------------------------

palette_chn <- setNames(c('#ff9214', '#ffb168', '#ffd1ac', '#f1f1f1', 
                        '#a4b1bd', '#5b758b', '#003f5c'), 
                      c('Very strong positive change', 'Strong positive change', 
                        'Positive change', 'No or small change', 
                        'Negative change', 'Strong negative change', 'Very strong negative change'))

### Palette defining seven levels ----------------------------------------------

palette_chn_us <- setNames(c('#ff9214', '#ffb168', '#ffd1ac', '#f1f1f1', 
                        '#a4b1bd', '#5b758b', '#003f5c'), 
                   c('Strongly China influenced', 'China influenced', 
                     'Weakly China influenced', 'Similar', 'Weakly US influenced', 
                     'US influenced', 'Strongly US influenced'))

## Breaks ----------------------------------------------------------------------

### Five-level break -----------------------------------------------------------

breaks_chn <- c('Very strong positive change', 'Strong positive change', 
              'Positive change', 'No or small change', 
              'Negative change', 'Strong negative change', 'Very strong negative change')

### Seven-level break ----------------------------------------------------------

breaks_chn_us <- c('Strongly China influenced', 'China influenced', 
              'Weakly China influenced', 'Similar', 'Weakly US influenced', 
              'US influenced', 'Strongly US influenced')

## Bins ------------------------------------------------------------------------

### Seven-level bin ------------------------------------------------------------

bins_7 <- col_bin(bins = c(-1, -.3, -.1, -.01, .01, .1, .3, 1),
                  palette = c('#ff9214', '#ffb168', '#ffd1ac', '#f1f1f1', 
                              '#a4b1bd', '#5b758b', '#003f5c'))

### Seven-level bin reversed ---------------------------------------------------

bins_7_rev <- col_bin(bins = c(-1, -.2, -.08, -.03, .03, .08, .2, 1),
                  palette = c('#003f5c', '#5b758b', '#a4b1bd', '#f1f1f1', 
                              '#ffd1ac', '#ffb168', '#ff9214'))


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

# FBIC INDEX DATA --------------------------------------------------------------

## Restrict and save smaller version -------------------------------------------

if(!file.exists('data/fbic.RData')){
  fbic <- read.csv('data/fbic.csv') %>% 
    filter(year >= 1994 & countrya %in% c('China', west_2)) %>% 
    select(countrya, countryb, year, iso3a, iso3b, fbic, bandwidth, 
           politicalbandwidth, economicbandwidth, securitybandwidth, dependence, 
           economicdependence, securitydependence)
  
  save(fbic, file = 'data/fbic.RData')
}

## Load fbic index -------------------------------------------------------------

load('data/fbic.RData')

# The main FBIC-variable is measured from 0 to 1, where 0 indicates no 
# influence from Country A on Country B, and 1 indicates the most influence ever 
# measured between two countries from 1960 through 2018. Moyer et al., Codebook,
# p. 24.

# This variable measures how much influence another country has on China,
# Spoiler alert! in 2023 it was Russia.

# infl_on_china <- fbic %>% 
#   filter(countryb == 'China' & year >= 1994)

# DATA WRANGELING --------------------------------------------------------------

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


# Doing the same as above for the US.

infl_by_us <- fbic %>% 
  filter(countrya == 'United States' & year >= 1994) %>% 
  select(c(countryb, year, iso3b, fbic)) %>% 
  rename(iso3c = iso3b) %>% 
  group_by(iso3c)


# The below code calculates the difference between US and Chinese influence,
# where positive values indicates that US influence is greater than Chinese
# influence, and vice versa. The interesting thing is why it works? It would
# seem more logical if it were the other way around, because I haven't told it
# that the US is supposed to be the 'positive' value.
# The fbic_nom variable is a nominal variable to make it easier to see the
# strength of the relationships. 

infl_chn_v_us <- fbic %>% 
  filter(countrya %in% c('China', 'United States') & year == 2023) %>% 
  select(c(countryb, year, iso3b, fbic)) %>% 
  rename(iso3c = iso3b) %>% 
  group_by(iso3c) %>% 
  summarise(fbic = diff(fbic)) %>% 
  mutate(fbic_nom = case_when(fbic <= .02 & fbic >= -.02 ~ 'Similar',
                              fbic  > .02 & fbic <=  .1  ~ 'Weakly US influenced',
                              fbic  > .1  & fbic <=  .3  ~ 'US influenced',
                              fbic  > .3                 ~ 'Strongly US influenced',
                              fbic  < -.02 & fbic >= -.1 ~ 'Weakly China influenced',
                              fbic  < -.1  & fbic >= -.3 ~ 'China influenced',
                              fbic  < -.3                ~ 'Strongly China influenced'))

## Change in Chinese influence data [1994-2023] --------------------------------

# Change in China-emanating influence 1994-2023

infl_change <- fbic %>% 
  filter(countrya == 'China' & year %in% c(1994, 2023)) %>% 
  select(c(countryb, year, iso3b, fbic)) %>%
  rename(iso3c = iso3b) %>% 
  group_by(iso3c) %>% 
  rows_append(data.frame(countryb = 'Serbia',
                         year     = 1994,
                         iso3c    = 'SRB',
                         fbic     = as.numeric(0.003460911))) %>% 
  rows_append(data.frame(countryb = 'Kosovo',
                       year     = 1994,
                       iso3c    = 'UNK',
                       fbic     = as.numeric(0.003460911))) %>% 
  rows_append(data.frame(countryb = 'Montenegro',
                         year     = 1994,
                         iso3c    = 'MNE',
                         fbic     = as.numeric(0.003460911))) %>% 
  rows_append(data.frame(countryb = 'South Sudan',
                         year     = 1994,
                         iso3c    = 'SSD',
                         fbic     = as.numeric(0.09932989))) %>% 
  summarise(fbic = diff(fbic)) %>% 
  mutate(fbic_nom = case_when(fbic >=  .2                 ~ 'Very strong positive change',
                              fbic  <  .2  & fbic >=  .08 ~ 'Strong positive change',
                              fbic  <  .08 & fbic >=  .03 ~ 'Positive change',
                              fbic  <  .03 & fbic >= -.03 ~ 'No or small change',
                              fbic  < -.03 & fbic >= -.08 ~ 'Negative change',
                              fbic  < -.08  & fbic >= -.2  ~ 'Strong negative change',
                              fbic <= -.2                 ~ 'Very strong negative change'))

## Change in Western influence data (Restricted) -------------------------------

# Calculates change in influence from 1994 for restricted definition of the 'West'

infl_change_west_1 <- infl_by_west_1 %>% 
  filter(year %in% c(1994, 2023)) %>% 
  group_by(iso3c) %>% 
  summarise(west_1 = diff(west_1)) %>% 
  mutate(fbic_nom = case_when(west_1 >=  .25                   ~ 'Very strong positive change',
                              west_1  <  .25  & west_1 >= .15   ~ 'Strong positive change',
                              west_1  <  .15  & west_1 >= .05  ~ 'Positive change',
                              west_1  < .05  & west_1 >= -.05 ~ 'No or small change',
                              west_1  < -.05 & west_1 >= -.15  ~ 'Negative change',
                              west_1  < -.15  & west_1 >= -.25  ~ 'Strong negative change',
                              west_1 <= -.25                   ~ 'Very strong negative change'))

## Change in Western influence data (Expanded) ---------------------------------

# Calculates change in influence from 1994 for expanded definition of the 'West'

infl_change_west_2 <- infl_by_west_2 %>% 
  filter(year %in% c(1994, 2023)) %>% 
  group_by(iso3c) %>% 
  summarise(west_2 = diff(west_2)) %>% 
  mutate(fbic_nom = case_when(west_2 >=  .25                   ~ 'Very strong positive change',
                              west_2  <  .25  & west_2 >= .15   ~ 'Strong positive change',
                              west_2  <  .15  & west_2 >= .05  ~ 'Positive change',
                              west_2  < .05  & west_2 >= -.05 ~ 'No or small change',
                              west_2  < -.05 & west_2 >= -.15  ~ 'Negative change',
                              west_2  < -.15  & west_2 >= -.25  ~ 'Strong negative change',
                              west_2 <= -.25                   ~ 'Very strong negative change'))

## World map data --------------------------------------------------------------

# I then do the same for the map information as I did above, also using the
# country code package. Remember I have coloured Greenland by using Denmark and
# French Guiana as France. Several smaller territories have not received this
# treatment, except the ones below. 

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

## Datasets with map data ------------------------------------------------------

# Merging the different data sets to the world map data

map_base <- infl_by_china %>% 
  filter(year == 2023) %>% 
  right_join(world, by = 'iso3c', relationship = 'many-to-many')

map_base_us <- infl_by_us %>% 
  filter(year == 2023) %>% 
  right_join(world, by = 'iso3c', relationship = 'many-to-many')

map_base_diff <- infl_chn_v_us %>% 
  right_join(world, by = 'iso3c', relationship = 'many-to-many')

map_base_change <- world %>% 
  left_join(infl_change, by = 'iso3c', relationship = 'many-to-many')

map_base_change_west_1 <- world %>% 
  left_join(infl_change_west_1, by = 'iso3c', relationship = 'many-to-many')

map_base_change_west_2 <- world %>% 
  left_join(infl_change_west_2, by = 'iso3c', relationship = 'many-to-many')

# MAPS -------------------------------------------------------------------------

## Map of Chinese influence (Continuous) ---------------------------------------

map_base %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic), colour = 'black') +
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

## Map of US influence (Continuous) --------------------------------------------

map_base_us %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic), colour = 'black') +
  scale_fill_continuous(high     = '#ff9214',
                        low      = '#003f5c',
                        na.value = 'grey80') +
  labs(fill = 'US Influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

# The US has stronger 'top' ties than China, so the two maps must be adjusted 
# for this, or gathered in a map showing which countries favour whom.

## Map of difference in China-US influence (Continuous) ------------------------

# Map showing difference between US and Chinese influence in the world with a
# continuous fbic-variable

map_base_diff %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic), colour = 'black') +
  scale_fill_gradient2(high     = '#ff9214',
                       mid      = 'white',
                       low      = '#003f5c',
                       na.value = 'grey40',
                       midpoint = 0,
                       limits   = c(-.64, .64)) +
  labs(fill = 'US Influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

## Map of difference in China-US influence (Nominal) ---------------------------

# Map showing difference between US and Chinese influence in the world with a
# nominal fbic-variable

map_base_diff %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic_nom), colour = 'black') +
  scale_fill_manual(values = palette_chn_us, breaks = breaks_chn_us) +
  labs(fill = 'Influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))


## Map of change in influence from China [1994-2023] (Continuous) --------------

map_base_change %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic), colour = 'black') +
  scale_fill_continuous(high     = '#ff9214',
                        low      = '#003f5c',
                        na.value = 'grey80') +
  labs(fill = 'Change in Chinese influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

## Map of change in influence from China [1994-2023] (Nominal) -----------------

map_base_change %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic_nom), colour = 'black') +
  scale_fill_manual(values = palette_chn, breaks = breaks_chn, na.value = 'grey40') +
  labs(fill = 'Chinese influence:') +
  guides(fill = guide_legend(nrow = 3,
                             title.position = 'top')) +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15,
                                    hjust = .5),
        legend.key.size = unit(8, 'mm'),
        legend.text = element_text(size = 13))

## Map of change in Western influence [Restricted] (Nominal) -------------------

map_base_change_west_1 %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic_nom), colour = 'black') +
  scale_fill_manual(values = palette_chn, breaks = breaks_chn) +
  labs(fill = 'Western influence:') +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

## Map of change in Western influence [Expanded] (Nominal) ---------------------

map_base_change_west_2 %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic_nom), colour = 'black') +
  scale_fill_manual(values = palette_chn, breaks = breaks_chn) +
  labs(fill = 'Western influence:') +
  guides(fill = guide_legend(nrow = 3,
                             title.position = 'top')) +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15,
                                    hjust = .5),
        legend.key.size = unit(8, 'mm'),
        legend.text = element_text(size = 13))

# TABLES -----------------------------------------------------------------------

## Table of difference in China-US influence ----------------------------------- 

# Table with the values colour-coded for readability

infl_chn_v_us %>% 
  mutate(country = countrycode(iso3c, destination = 'country.name', 
                               origin = 'iso3c', warn = F),
         country = case_when(iso3c == 'UNK' ~ 'Kosovo',
                             .default = as.character(country))) %>%
  ungroup() %>% 
  select(c(country, fbic, fbic_nom)) %>%
  arrange(-fbic) %>% 
  gt() %>% 
  tab_header('FBIC score difference') %>% 
  data_color(columns = fbic,
             fn = bins_7)

## Table of change in Chinese influence ----------------------------------------

infl_change %>% 
  mutate(country = countrycode(iso3c, destination = 'country.name',
                               origin = 'iso3c', warn = F),
         country = case_when(iso3c == 'UNK' ~ 'Kosovo',
                             .default = as.character(country))) %>%
  ungroup() %>% 
  select(c(country, fbic, fbic_nom)) %>%
  arrange(-fbic) %>% 
  ## slice_max(n = 10, order_by = fbic) %>% 
  gt() %>% 
  tab_header('FBIC score difference') %>% 
  data_color(columns = fbic,
             fn = bins_7_rev)


