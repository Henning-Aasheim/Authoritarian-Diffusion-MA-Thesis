# CONFIG -----------------------------------------------------------------------

library(tidyverse)
library(vdemdata)
library(ggtext)

load('data/base.RData')

vdem <- vdem

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
