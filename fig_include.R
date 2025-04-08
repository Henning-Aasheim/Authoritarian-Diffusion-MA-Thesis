# CONFIG -----------------------------------------------------------------------

library(maps) # Used to download map data and make maps
library(countrycode) # Used to make country names compatible
library(vdemdata) # to load v-dem data
library(ggtext) # Used for labels on the waves plot
library(tidyverse) # Used for data wrangling

## Load base data --------------------------------------------------------------

load('data/base.RData')

## Load vdem data --------------------------------------------------------------

vdem <- vdem

## Load fbic index -------------------------------------------------------------

load('data/fbic.RData')

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

# DECLINING INDICATORS 2014-2024 -----------------------------------------------

## data ------------------------------------------------------------------------

# Top 20 declining indicator (V_DEM: Nord et al. 2025)

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

## Plot ------------------------------------------------------------------------

declining_indicators_4 %>% 
  ggplot(aes(y = reorder(indicator, decline))) +
  geom_bar(aes(x = decline, fill = type),
           stat = 'identity', 
           width = 0.7,
           position = position_dodge(width = .75)) +
  scale_x_continuous(expand = expansion(mult = 0),
                     breaks = seq(0, 46, 4)) +
  scale_fill_manual(values = c('#444e86', '#ff6e54', '#ff9214', '#003f5c', '#dd5182', '#955196')) +
  labs(x    = 'Number of declining countries',
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
        panel.grid.minor.x = element_line(linetype = 'dashed'),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = 'pt'))

ggsave('illustrations/declining_indicators.jpeg', units = 'px', width = 2700, height = 2000, dpi = 300)

# WAVES ------------------------------------------------------------------------

## Main data -------------------------------------------------------------------

diffs <- vdem %>% 
  select(v2x_polyarchy, year, country_name, country_id) %>% 
  mutate(diffs = v2x_polyarchy - lag(v2x_polyarchy),
         dems = ifelse(diffs > 0.1, 1, 0),
         dems = ifelse(diffs < -0.1, 2, dems),
         autdem = ifelse(v2x_polyarchy >= 0.3, 1, 0)) %>% 
  group_by(year, autdem) %>% 
  count(autdem) %>% 
  drop_na() %>% 
  group_by(year) %>% 
  mutate(total = sum(n),
         prct = n/total)

j <- c(1922, 1942, 1962, 1975, 2010)

first <- data.frame(a = 1932,    b = .95, label = paste('1<sup>st</sup> wave'))
second <- data.frame(a = 1968.5, b = .95, label = paste('2<sup>nd</sup> wave'))
third <- data.frame(a = 2018,    b = .95, label = paste('3<sup>rd</sup> wave'))


diffs %>% 
  filter(year >= 1900) %>% 
  ggplot(aes(x = year, y = prct, colour = as.factor(autdem), group = autdem)) +
  annotate('rect', fill = 'black', alpha = .08, xmin = 1922, xmax = 1942,
           ymin = -Inf, ymax = Inf) +
  annotate('rect', fill = 'black', alpha = .08, xmin = 1962, xmax = 1975,
           ymin = -Inf, ymax = Inf) +
  annotate('rect', fill = 'black', alpha = .08, xmin = 2010, xmax = Inf,
           ymin = -Inf, ymax = Inf) +
  geom_vline(xintercept = j, linetype = '22') +
  geom_richtext(data = first, aes(x = a, y = b, label = label), 
                family = 'serif', 
                fontface = 'bold', 
                label.r = unit(0, 'pt'), 
                inherit.aes = F) +
  geom_richtext(data = second, aes(x = a, y = b, label = label), 
                family = 'serif', 
                fontface = 'bold', 
                label.r = unit(0, 'pt'), 
                inherit.aes = F) +
  geom_richtext(data = third, aes(x = a, y = b, label = label), 
                family = 'serif', 
                fontface = 'bold', 
                label.r = unit(0, 'pt'), 
                inherit.aes = F) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1780, 2030, 10)) +
  scale_y_continuous(breaks = seq(0, 1, .1), labels = function(x) paste0(x*100, '%')) +
  scale_colour_manual(labels = c('Less', 'Greater'), values = c('#003f5c','#ff9214')) +
  labs(x = 'Year',
       y = 'Per cent of countries',
       colour = 'EDI less or greater than 0.3:') +
  guides(colour = guide_legend(nrow = 1)) +
  theme_classic(base_family = 'serif') +
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = 'pt'))

ggsave('illustrations/waves.jpeg', dpi = 300, units = 'px', width = 2700, height = 2000)

# CHANGE IN CHINESE LINKAGES ---------------------------------------------------

# Change in China-emanating influence 1994-2023

## Main data -------------------------------------------------------------------

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
                              fbic  < -.08  & fbic >= -.2 ~ 'Strong negative change',
                              fbic <= -.2                 ~ 'Very strong negative change'))

## Data merge ------------------------------------------------------------------

map_base_change <- world %>% 
  left_join(infl_change, by = 'iso3c', relationship = 'many-to-many')

## Map -------------------------------------------------------------------------

map_base_change %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic_nom), colour = 'black', linewidth = .2) +
  scale_fill_manual(values = palette_chn, breaks = breaks_chn, na.value = 'grey40') +
  labs(fill = 'Chinese influence:') +
  guides(fill = guide_legend(nrow = 3)) +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

ggsave('illustrations/chinese_influence.jpeg', units = 'px', width = 2700, height = 2000, dpi = 300)

# CHANGE IN WESTERN LINKAGES ---------------------------------------------------

# Calculates change in influence from 1994 for expanded definition of the 'West'

## Main data -------------------------------------------------------------------

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

## Data merge ------------------------------------------------------------------

map_base_change_west_2 <- world %>% 
  left_join(infl_change_west_2, by = 'iso3c', relationship = 'many-to-many')

## Map -------------------------------------------------------------------------

map_base_change_west_2 %>% 
  filter(iso3c != 'ATA') %>% 
  ggplot(aes(long, lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = fbic_nom), colour = 'black', linewidth = .2) +
  scale_fill_manual(values = palette_chn, breaks = breaks_chn) +
  labs(fill = 'Western influence:') +
  guides(fill = guide_legend(nrow = 3)) +
  theme_void(base_family = 'serif') +
  theme(legend.position = 'bottom',
        legend.title = element_text(margin = margin(b = 18, r = 10, unit = 'pt'),
                                    face   = 'bold',
                                    size   = 15),
        legend.key.size = unit(8, 'mm'))

ggsave('illustrations/western_influence.jpeg', units = 'px', width = 2700, height = 2000, dpi = 300)

# SINGLE COUNTRY STUDIES -------------------------------------------------------

# Facet plot -------------------------------------------------------------------

base %>% 
  filter(country %in% c('Timor-Leste', 'Nicaragua', 'Cambodia','North Korea')) %>% 
  select(country, year, freedom, fbic) %>% 
  pivot_longer(cols = c('freedom', 'fbic'),
               names_to = 'type',
               values_to = 'score') %>% 
  ggplot(aes(x = year, y = score, colour = type)) +
  facet_wrap(~factor(country, levels = c('Timor-Leste', 'Nicaragua', 'Cambodia','North Korea')), scales = 'free') +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = c('#ff9214', '#003f5c'), labels = c('Linkages to China', 'Freedom')) +
  scale_x_continuous(breaks = seq(1994, 2024, 5)) +
  scale_y_continuous(name = 'Freedom of expression and FBIC score',
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  labs(x = 'Year',
       colour = 'Type:') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        strip.background = element_blank(),
        strip.text = element_text(size = 15, face = 'bold'),
        panel.spacing.x = unit(10, 'mm'),
        legend.position = 'bottom',
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.key.size = unit(10, 'mm'),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = 'pt'),
        axis.title.x = element_text(margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(margin = margin(r = 10, unit = 'pt')))

ggsave('illustrations/single_country_plots.jpeg', units = 'px', width = 2700, height = 2700, dpi = 300)