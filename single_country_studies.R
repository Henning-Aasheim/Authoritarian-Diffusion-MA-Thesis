# CONFIG -----------------------------------------------------------------------

library(tidyverse)
library(gt)

# DATA -------------------------------------------------------------------------

# Loads the base dataset
load('data/base.Rdata')

## Country ranking --------------------------------------------------------------

base %>% 
  select(c(country, freedom)) %>%
  filter(year %in% c(1994, 2024))

%>% 
  group_by(country) %>% 
  summarise(diff(freedom)) %>% 
  arrange(-freedom) %>% 
  ## slice_max(n = 10, order_by = fbic) %>% 
  gt() %>% 
  tab_header('Freedom score difference')

# CAMBODIA ---------------------------------------------------------------------

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

# USA --------------------------------------------------------------------------

base %>% 
  filter(country == 'United States of America') %>% 
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

# Norway -----------------------------------------------------------------------

base %>% 
  filter(country == 'Norway') %>% 
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

# USA --------------------------------------------------------------------------

base %>% 
  filter(country == 'North Korea') %>% 
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

