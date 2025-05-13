# CONFIG -----------------------------------------------------------------------

library(vdemdata)
library(ggtext)
library(modelsummary)
library(kableExtra)
library(tidyverse)

load('data/base.RData')

vdem <- vdem

load('data/fbic.Rdata')

# WAVES ------------------------------------------------------------------------

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

# first <- paste("1^{st}*' wave'")
# second <- paste("2^{nd}*' wave'")
# third <- paste("3^{rd}*' wave'")

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

# FIRST TEST WAVES -------------------------------------------------------------

diffs %>% 
  ggplot(aes(x = year, y = prct, colour = autdem, group = autdem)) +
  geom_line(linewidth = 1.2) +
  geom_vline(aes(xintercept = 1922)) +
  geom_vline(aes(xintercept = 1942)) +
  geom_vline(aes(xintercept = 1962)) +
  geom_vline(aes(xintercept = 1975)) +
  geom_vline(aes(xintercept = 2010)) +
  scale_x_continuous(breaks = seq(1780, 2030, 20)) +
  scale_y_continuous(breaks = seq(0, 1, .1)) +
  theme_classic()

summary(vdem$v2x_polyarchy)

# iNDICATOR TEST ---------------------------------------------------------------

indicators_13_23 <- vdem %>% 
  filter(year %in% c(2013, 2023)) %>% 
  select(!starts_with(c('v2x', 'v3'))) %>% 
  group_by(country_name)

# Change in freedom score  -----------------------------------------------------

free_count <- base %>% 
  mutate(diff = freedom - lag(freedom, n = 3)) %>% 
  select(iso3c, year, diff, internet) %>% 
  mutate(neg = case_when(diff < 0 ~ 'neg',
                         diff > 0 ~ 'pos',
                         .default = 'zero')) %>% 
  group_by(year, neg) %>% 
  count() %>% 
  filter(year >= 1997)

free_count %>% 
  ggplot(aes(year, n, colour = neg)) +
  geom_line(linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1997, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 120, 10)) +
  scale_colour_manual(values = c('#003f5c','#ff9214', 'grey'),
                      labels = c('Negative', 'Positive', 'No change')) +
  labs(x      = 'Year',
       y      = 'Number of countries',
       colour = 'Three year difference in freedom variable:') +
  theme_classic(base_family = 'serif') +
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13))

# DEMOCRATIC DEVELOPMENT -------------------------------------------------------

democratic_development <- vdem %>% 
  filter(year >= 1960) %>% 
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
           v2x_freexp_altinf,   # Freedom of Expression and Alternative Information index (V-Dem Codebook, pp. 50-51)
           v2mecenefi,          # Internet censorship effort (V-Dem Codebook, pp. 207-208)
           v2x_regime,          # Regimes of the world ordinal scale (V-Dem Codebook, pp. 292-293)
           e_regionpol_7C)) %>% # Region variable with 7 categories (V-Dem Codebook p. 395) 
  rename(freedom  = v2x_freexp_altinf,
         internet = v2mecenefi,
         regime   = v2x_regime,
         region   = e_regionpol_7C)

fbic_development <- read.csv('C:/Users/ny bruker/OneDrive/UiO/Master/master/data/fbic.csv') %>% 
  filter(countrya == 'China') %>% 
  select(countrya, countryb, year, iso3a, iso3b, fbic, bandwidth, 
         politicalbandwidth, economicbandwidth, securitybandwidth, dependence, 
         economicdependence, securitydependence)

# REGIME TYPE INFORMATION ------------------------------------------------------

regime <- base %>% 
  group_by(regime) %>% 
  summarise(mean_freedom = mean(freedom),
            mean_fbic    = mean(fbic, na.rm = T),
            sd_freedom   = sd(freedom),
            sd_fbic      = sd(fbic, na.rm = T),
            min_freedom  = min(freedom),
            min_fbic     = min(fbic, na.rm = T),
            max_freedom  = max(freedom),
            max_fbic     = max(fbic, na.rm = T)) %>% 
  t() %>%
  data.frame() %>% 
  rename('Closed Autocracy' = X1, 
         'Electoral Autocracy' = X2, 
         'Electoral Democracy' = X3, 
         'Liberal Democracy' = X4) %>% 
  slice_tail(n = -1)
  # pivot_longer(!regime, names_to = 'type', values_to = 'values') %>% 
  # group_by(regime) %>% 
  # mutate(freedom = ifelse(str_detect(type, 'freedom'), 1, 0))

kbl(regime)


# It's a mess, but it somehow works...

regime2 <- base %>% 
  group_by(regime) %>% 
  summarise(mean_freedom = mean(freedom),
            mean_fbic    = mean(fbic, na.rm = T),
            sd_freedom   = sd(freedom),
            sd_fbic      = sd(fbic, na.rm = T),
            min_freedom  = min(freedom),
            min_fbic     = min(fbic, na.rm = T),
            max_freedom  = max(freedom),
            max_fbic     = max(fbic, na.rm = T)) %>% 
  pivot_longer(cols = ends_with('freedom'),names_to = 'freedom', values_to = 'freedom_s') %>% 
  pivot_longer(cols = ends_with('fbic'),names_to = 'fbic', values_to = 'fbic_s') %>% 
  pivot_wider(names_from = 'regime', values_from = ends_with('_s')) %>% 
  mutate(freedom = str_replace(freedom, '_freedom', ''),
         fbic = str_replace(fbic, '_fbic', '')) %>% 
  filter(freedom == fbic) %>% 
  rename(a = freedom_s_0,
         b = fbic_s_0,
         c = freedom_s_1,
         d = fbic_s_1,
         e = freedom_s_2,
         f = fbic_s_2,
         g = freedom_s_3,
         h = fbic_s_3,
         ' ' = freedom) %>% 
  select(!fbic)

kbl(regime2, booktabs = T, digits = 3, format = 'latex',
    col.names = linebreak(c(' ', 'Freedom', 'Fbic', 'Freedom', 'Fbic', 'Freedom', 'Fbic', 'Freedom', 'Fbic'))) %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, 'Closed Autocracy' = 2, 'Electoral Autocracy' = 2, 
                   'Electoral Democracy' = 2, 'Liberal Democracy' = 2))

## Freedom ---------------------------------------------------------------------

regime_freedom <- base %>% 
  mutate(regime = factor(regime)) %>% 
  group_by(regime) %>% 
  summarise(mean = mean(freedom),
            sd   = sd(freedom),
            min  = min(freedom),
            max  = max(freedom),
            q1   = quantile(freedom, .25),
            q2   = quantile(freedom, .5),
            q3   = quantile(freedom, .75))

regime_freedom %>% 
  ggplot(aes(regime, fill = regime)) +
  geom_boxplot(stat = 'identity',
               aes(lower = q1,
                   middle = q2,
                   upper = q3,
                   ymin = min,
                   ymax = max))

base %>% 
  mutate(regime = factor(regime)) %>% 
  ggplot(aes(regime, freedom, fill = regime)) +
  geom_violin() +
  scale_x_discrete(labels = c('Closed\nautocracy', 'Electoral\nautocracy', 
                              'Electoral\nDemocracy', 'Liberal\ndemocracy')) +
  scale_y_continuous(breaks = seq(0, 1, .1)) +
  scale_fill_manual(values = c('#444e86', '#dd5182', '#ff9214', '#003f5c')) +
  labs(x = 'Regime Type',
       y = 'Freedom',) +
  guides(fill = 'none') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = 'pt'),
        axis.title.x = element_text(margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(margin = margin(r = 10, unit = 'pt')))

ggsave('illustrations/variation_freedom.jpeg', dpi = 300, units = 'px', width = 2700, height = 2000)

## Fbic ------------------------------------------------------------------------

regime_fbic <- base %>% 
  filter(!is.na(fbic)) %>% 
  mutate(regime = factor(regime)) %>% 
  group_by(regime) %>% 
  summarise(mean = mean(fbic),
            sd   = sd(fbic),
            min  = min(fbic),
            max  = max(fbic),
            q1   = quantile(fbic, .25),
            q2   = quantile(fbic, .5),
            q3   = quantile(fbic, .75))

base %>% 
  mutate(regime = factor(regime)) %>% 
  ggplot(aes(regime, fbic, fill = regime)) +
  geom_violin() +
  scale_x_discrete(labels = c('Closed\nautocracy', 'Electoral\nautocracy', 
                              'Electoral\nDemocracy', 'Liberal\ndemocracy')) +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  scale_fill_manual(values = c('#444e86', '#dd5182', '#ff9214', '#003f5c')) +
  labs(x = 'Regime Type',
       y = 'Linkages to China',) +
  guides(fill = 'none') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = 'pt'),
        axis.title.x = element_text(margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(margin = margin(r = 10, unit = 'pt')))

ggsave('illustrations/variation_fbic.jpeg', dpi = 300, units = 'px', width = 2700, height = 2000)
