# CONFIG -----------------------------------------------------------------------

library(tidyverse)
library(gt)
library(knitr)
library(kableExtra)

# DATA -------------------------------------------------------------------------

# Loads the base dataset
load('data/base.Rdata')

## Country rankings ------------------------------------------------------------

### Freedom of expression ------------------------------------------------------

base %>% 
  select(c(country, year, freedom)) %>% # Selects the variables I need.
  filter(year %in% c(1994, 2024)) %>% # Filter the year between which I measure difference.
  group_by(country) %>% # Group by country so that I measure within-country difference.
  summarise(freedom = diff(freedom)) %>% # Calculates difference.
  mutate(colour = '') %>%  # Creates an empty variable that will work as a place-holder for colour.
  arrange(freedom) %>% # Arrange by freedom score.
  ungroup() %>% # Ungroups so that it does not divide the rows into separate ones for country name and freedom score.
  group_by(freedom < 0) %>% # Groups the freedom variable into two groups, one above zero and one zero or below. This is done as to so that the next step works.
  top_n(5, abs(freedom)) %>% # Here I select the top/highest absolute values of the two groups I made above.
  ungroup() %>% # I ungroup to avoid grouping problems.
  arrange(-freedom) %>% # I then arrange the table by the reverse freedom score.
  select(!`freedom < 0`) %>% # I then remove the grouping variable from step 8. 
  gt() %>% # I then make a gt table.
  tab_style(locations = cells_body(
    columns = colour,
    rows = freedom < 0), style = list(cell_fill(color = '#003f5c'))
  ) %>%  # I Here locate the cell body of every row in the colour-column where freedom score is less than zero and make the cell colour blue. 
  tab_style(locations = cells_body(
    columns = colour,
    rows = freedom > 0), style = list(cell_fill(color = '#ff9214'))
  ) %>% # I Here locate the cell body of every row in the colour-column where freedom score is less than zero and make the cell colour orange. 
  cols_label(
    country = md('**Country**'),
    freedom = md('**Freedom**'),
    colour  = ''
  ) %>% # Here I set column names and make them bold, I also remove the name of the colour-column. 
  opt_table_font('serif') %>% # I make the font serif.
  fmt_number(decimals = 3) # I round the numbers to 3 decimals. 




### Linkages -------------------------------------------------------------------

# For steps on how to make the table, see above. 

base %>% 
  select(c(country, year, fbic)) %>%
  filter(year %in% c(1994, 2023)) %>% 
  group_by(country) %>% 
  summarise(fbic = diff(fbic)) %>% 
  mutate(colour = '') %>% 
  arrange(fbic) %>% 
  ungroup() %>% 
  group_by(fbic > .02) %>% 
  top_n(5, abs(fbic))%>% 
  ungroup() %>% 
  arrange(-fbic) %>% 
  select(!`fbic > 0.02`) %>% 
  gt() %>% 
  tab_style(locations = cells_body(
    columns = colour,
    rows = fbic < 0), style = list(cell_fill(color = '#003f5c'))
  ) %>% 
  tab_style(locations = cells_body(
    columns = colour,
    rows = fbic > 0), style = list(cell_fill(color = '#ff9214'))
  ) %>% 
  cols_label(
    country = md('**Country**'),
    fbic    = md('**Freedom**'),
    colour  = ''
  ) %>% 
  opt_table_font('serif') %>% 
  fmt_number(decimals = 3)

# Cambodia ---------------------------------------------------------------------

# Highest increase in linkages to China

base %>% 
  filter(country == 'Cambodia') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2024, 2)) +
  scale_y_continuous(name = 'Freedom of expression', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Linkages to China', 
                                         breaks = seq(0, 1, .1)),
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y.right = element_text(colour = '#ff9214'),
        axis.text.y.right = element_text(colour = '#ff9214'),
        axis.line.y.right = element_line(colour = '#ff9214'),
        axis.ticks.y.right = element_line(colour = '#ff9214'),
        axis.title.y = element_text(colour = '#003f5c'),
        axis.text.y = element_text(colour = '#003f5c'),
        axis.line.y = element_line(colour = '#003f5c'),
        axis.ticks.y = element_line(colour = '#003f5c'))

# Nicaragua --------------------------------------------------------------------

# Largest fall in freedom of expression

base %>% 
  filter(country == 'Nicaragua') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2024, 2)) +
  scale_y_continuous(name = 'Freedom of expression', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Linkages to China', 
                                         breaks = seq(0, 1, .1)),
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y.right = element_text(colour = '#ff9214'),
        axis.text.y.right = element_text(colour = '#ff9214'),
        axis.line.y.right = element_line(colour = '#ff9214'),
        axis.ticks.y.right = element_line(colour = '#ff9214'),
        axis.title.y = element_text(colour = '#003f5c'),
        axis.text.y = element_text(colour = '#003f5c'),
        axis.line.y = element_line(colour = '#003f5c'),
        axis.ticks.y = element_line(colour = '#003f5c'))

# Timor-Leste ------------------------------------------------------------------

# Highest increase in freedom of expression

base %>% 
  filter(country == 'Timor-Leste') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2024, 2)) +
  scale_y_continuous(name = 'Freedom of expression', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Linkages to China', 
                                         breaks = seq(0, 1, .1)),
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y.right = element_text(colour = '#ff9214'),
        axis.text.y.right = element_text(colour = '#ff9214'),
        axis.line.y.right = element_line(colour = '#ff9214'),
        axis.ticks.y.right = element_line(colour = '#ff9214'),
        axis.title.y = element_text(colour = '#003f5c'),
        axis.text.y = element_text(colour = '#003f5c'),
        axis.line.y = element_line(colour = '#003f5c'),
        axis.ticks.y = element_line(colour = '#003f5c'))


# North Korea ------------------------------------------------------------------

base %>% 
  filter(country == 'North Korea') %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2024, 2)) +
  scale_y_continuous(name = 'Freedom of expression', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Linkages to China', 
                                         breaks = seq(0, 1, .1)),
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y.right = element_text(colour = '#ff9214'),
        axis.text.y.right = element_text(colour = '#ff9214'),
        axis.line.y.right = element_line(colour = '#ff9214'),
        axis.ticks.y.right = element_line(colour = '#ff9214'),
        axis.title.y = element_text(colour = '#003f5c'),
        axis.text.y = element_text(colour = '#003f5c'),
        axis.line.y = element_line(colour = '#003f5c'),
        axis.ticks.y = element_line(colour = '#003f5c'))

# Facet plot -------------------------------------------------------------------

base %>% 
  filter(country %in% c('Timor-Leste', 'Nicaragua', 'Cambodia','North Korea')) %>% 
  ggplot(aes(x = year)) +
  facet_wrap(~factor(country, levels = c('Timor-Leste', 'Nicaragua', 'Cambodia','North Korea')), scales = 'free') +
  geom_line(aes(y = freedom), colour = '#003f5c', linewidth = 1.5) +
  geom_line(aes(y = fbic), colour = '#ff9214', linewidth = 1.5) +
  scale_x_continuous(breaks = seq(1994, 2024, 5)) +
  scale_y_continuous(name = 'Freedom of expression', 
                     sec.axis = sec_axis(~ .,
                                         name = 'Linkages to China', 
                                         breaks = seq(0, 1, .1)),
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  labs(x = 'Year') +
  theme_classic(base_family = 'serif') +
  theme(axis.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        axis.title.y.right = element_text(colour = '#ff9214'),
        axis.text.y.right = element_text(colour = '#ff9214'),
        axis.line.y.right = element_line(colour = '#ff9214'),
        axis.ticks.y.right = element_line(colour = '#ff9214'),
        axis.title.y = element_text(colour = '#003f5c'),
        axis.text.y = element_text(colour = '#003f5c'),
        axis.line.y = element_line(colour = '#003f5c'),
        axis.ticks.y = element_line(colour = '#003f5c'),
        strip.background = element_blank(),
        strip.text = element_text(size = 15, face = 'bold'),
        panel.spacing.x = unit(6, 'mm'))
