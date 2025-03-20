# CONFIG -----------------------------------------------------------------------

library(tidyverse)
library(gt)

# DATA -------------------------------------------------------------------------

# Loads the base dataset
load('data/base.Rdata')

## Country ranking --------------------------------------------------------------

base %>% 
  select(c(country, year, freedom)) %>%
  filter(year %in% c(1994, 2024)) %>% 
  group_by(country) %>% 
  summarise(freedom = diff(freedom)) %>% 
  mutate(colour = '') %>% 
  arrange(-freedom) %>% 
  ungroup() %>% 
  ## slice_max(n = 10, order_by = fbic) %>% 
  gt() %>% 
  tab_style(locations = cells_body(
    columns = colour,
    rows = freedom < 0), style = list(cell_fill(color = '#003f5c'))
  ) %>% 
  tab_style(locations = cells_body(
    columns = colour,
    rows = freedom > 0), style = list(cell_fill(color = '#ff9214'))
  ) %>% 
  cols_label(
    country = md('**Country**'),
    freedom = md('**Freedom**'),
    colour  = ''
  ) %>% 
  opt_table_font('serif')

# Cambodia ---------------------------------------------------------------------

# Highest increase in linkages to China

base %>% 
  filter(country == 'Cambodia') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2023, 2)) +
  scale_y_continuous(name = 'Chinese influence', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Freedom of expression', 
                                         breaks = seq(0, 1, .05)),
                     breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(colour = '#ff9214'),
        axis.text.y = element_text(colour = '#ff9214'),
        axis.line.y = element_line(colour = '#ff9214'),
        axis.ticks.y = element_line(colour = '#ff9214'),
        axis.title.y.right = element_text(colour = '#003f5c'),
        axis.text.y.right = element_text(colour = '#003f5c'),
        axis.line.y.right = element_line(colour = '#003f5c'),
        axis.ticks.y.right = element_line(colour = '#003f5c'))

# Nicaragua --------------------------------------------------------------------

# Largest fall in freedom of expression

base %>% 
  filter(country == 'Nicaragua') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2023, 2)) +
  scale_y_continuous(name = 'Chinese influence', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Freedom of expression', 
                                         breaks = seq(0, 1, .05)),
                     breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(colour = '#ff9214'),
        axis.text.y = element_text(colour = '#ff9214'),
        axis.line.y = element_line(colour = '#ff9214'),
        axis.ticks.y = element_line(colour = '#ff9214'),
        axis.title.y.right = element_text(colour = '#003f5c'),
        axis.text.y.right = element_text(colour = '#003f5c'),
        axis.line.y.right = element_line(colour = '#003f5c'),
        axis.ticks.y.right = element_line(colour = '#003f5c'))

# Timor-Leste ------------------------------------------------------------------

# Highest increase in freedom of expression

base %>% 
  filter(country == 'Timor-Leste') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2023, 2)) +
  scale_y_continuous(name = 'Chinese influence', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Freedom of expression', 
                                         breaks = seq(0, 1, .05)),
                     breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(colour = '#ff9214'),
        axis.text.y = element_text(colour = '#ff9214'),
        axis.line.y = element_line(colour = '#ff9214'),
        axis.ticks.y = element_line(colour = '#ff9214'),
        axis.title.y.right = element_text(colour = '#003f5c'),
        axis.text.y.right = element_text(colour = '#003f5c'),
        axis.line.y.right = element_line(colour = '#003f5c'),
        axis.ticks.y.right = element_line(colour = '#003f5c'))

# Maldives ------------------------------------------------------------------

# 2nd highest increase in freedom of expression

base %>% 
  filter(country == 'Maldives') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2023, 2)) +
  scale_y_continuous(name = 'Chinese influence', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Freedom of expression', 
                                         breaks = seq(0, 1, .05)),
                     breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(colour = '#ff9214'),
        axis.text.y = element_text(colour = '#ff9214'),
        axis.line.y = element_line(colour = '#ff9214'),
        axis.ticks.y = element_line(colour = '#ff9214'),
        axis.title.y.right = element_text(colour = '#003f5c'),
        axis.text.y.right = element_text(colour = '#003f5c'),
        axis.line.y.right = element_line(colour = '#003f5c'),
        axis.ticks.y.right = element_line(colour = '#003f5c'))

# Largest decrease in linkages to China

# Gambia ------------------------------------------------------------------

# Largest decrease in linkages to China after North Korea

base %>% 
  filter(country == 'Iran') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2023, 2)) +
  scale_y_continuous(name = 'Chinese influence', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Freedom of expression', 
                                         breaks = seq(0, 1, .05)),
                     breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(colour = '#ff9214'),
        axis.text.y = element_text(colour = '#ff9214'),
        axis.line.y = element_line(colour = '#ff9214'),
        axis.ticks.y = element_line(colour = '#ff9214'),
        axis.title.y.right = element_text(colour = '#003f5c'),
        axis.text.y.right = element_text(colour = '#003f5c'),
        axis.line.y.right = element_line(colour = '#003f5c'),
        axis.ticks.y.right = element_line(colour = '#003f5c'))


# Luxembourg ------------------------------------------------------------------

# No change in freedom score

base %>% 
  filter(country == 'Luxembourg') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2023, 2)) +
  scale_y_continuous(name = 'Chinese influence', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Freedom of expression', 
                                         breaks = seq(0, 1, .05)),
                     breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y = element_text(colour = '#ff9214'),
        axis.text.y = element_text(colour = '#ff9214'),
        axis.line.y = element_line(colour = '#ff9214'),
        axis.ticks.y = element_line(colour = '#ff9214'),
        axis.title.y.right = element_text(colour = '#003f5c'),
        axis.text.y.right = element_text(colour = '#003f5c'),
        axis.line.y.right = element_line(colour = '#003f5c'),
        axis.ticks.y.right = element_line(colour = '#003f5c'))

