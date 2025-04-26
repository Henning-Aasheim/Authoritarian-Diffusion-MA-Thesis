library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables
library(marginaleffects)
library(tidyverse)

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

fixest_h1_m1 <- feols(f(freedom, 1) ~ fbic | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m2 <- feols(f(freedom, 1) ~ fbic + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m3 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m4 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m5 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m6 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


fixest_h1 <- list(
  'Model 1.1' = fixest_h1_m1,
  'Model 1.2' = fixest_h1_m2,
  'Model 1.3' = fixest_h1_m3,
  'Model 1.4' = fixest_h1_m4,
  'Model 1.5' = fixest_h1_m5,
  'Model 1.6' = fixest_h1_m6
)

fixest_h1_map <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(fixest_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# The high R squared is a result of the fixed effects, where I control for many 

# HYPOTHESIS 2 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

fixest_h2_m1 <- feols(f(freedom, 1) ~ fbic*factor(regime) | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m2 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log |
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m3 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m4 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda | 
                          country + year, 
                        data     = base,
                        cluster  = 'country',
                        panel.id = ~country+year)

fixest_h2_m5 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2 <- list(
  'Model 2.1' = fixest_h2_m1,
  'Model 2.2' = fixest_h2_m2,
  'Model 2.3' = fixest_h2_m3,
  'Model 2.4' = fixest_h2_m4,
  'Model 2.5' = fixest_h2_m5
)

fixest_h2_map <- list(
  'fbic'                 = 'Linkages to China',
  'factor(regime)1'      = 'Electoral autocracy',
  'factor(regime)2'      = 'Electoral democracy',
  'factor(regime)3'      = 'Liberal democracy',
  'fbic:factor(regime)1' = 'China x El.Aut.',
  'fbic:factor(regime)2' = 'China x El.Dem.',
  'fbic:factor(regime)3' = 'China x Lib.Dem.',
  'gdppc_log'            = 'log(GDP per capita)',
  'rents'                = 'Resource rents',
  'oda'                  = 'Aid',
  'west_2_fbic'          = 'Linkages (West)'
)

modelsummary(fixest_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


## Simple 2 --------------------------------------------------------------------
base2 <- base %>% 
  mutate(regime = factor(regime))

base2 <- panel(base2, ~country+year)


fixest_h2_m1 <- feols(f(freedom, 1) ~ i(regime, fbic) | 
                        country + year, 
                      data     = base2, 
                      cluster  = 'country')

fixest_h2_m1.2 <- feols(f(freedom, 1) ~ fbic + i(regime, fbic, ref = 0) | 
                        country + year, 
                      data     = base2, 
                      cluster  = 'country')

fixest_h2_m1.3 <- feols(f(freedom, 1) ~ fbic + regime + i(regime, fbic, ref = 0) | # This is the same as using freedom ~ fbic*regime
                          country + year, 
                        data     = base2, 
                        cluster  = 'country')

fixest_h2_m2 <- feols(f(freedom, 1) ~ i(regime, fbic) + gdppc_log |
                        country + year, 
                      data     = base2, 
                      cluster  = 'country')

fixest_h2_m3 <- feols(f(freedom, 1) ~ i(regime, fbic) + gdppc_log + rents | 
                        country + year, 
                      data     = base2, 
                      cluster  = 'country')

fixest_h2_m4 <- feols(f(freedom, 1) ~ i(regime, fbic) + gdppc_log + rents + oda | 
                        country + year, 
                      data     = base2,
                      cluster  = 'country')

fixest_h2_m5 <- feols(f(freedom, 1) ~ i(regime, fbic) + gdppc_log + rents + oda + west_2_fbic | 
                        country + year, 
                      data     = base2, 
                      cluster  = 'country')

fixest_h2 <- list(
  'Model 2.1' = fixest_h2_m1,
  'Model 2.2' = fixest_h2_m2,
  'Model 2.3' = fixest_h2_m3,
  'Model 2.4' = fixest_h2_m4,
  'Model 2.5' = fixest_h2_m5
)

fixest_h2_map <- list(
  'fbic'                 = 'Linkages to China',
  'regime1'      = 'Electoral autocracy',
  'regime2'      = 'Electoral democracy',
  'regime3'      = 'Liberal democracy',
  'fbic:regime1' = 'China x El.Aut.',
  'fbic:regime2' = 'China x El.Dem.',
  'fbic:regime3' = 'China x Lib.Dem.',
  'gdppc_log'            = 'log(GDP per capita)',
  'rents'                = 'Resource rents',
  'oda'                  = 'Aid',
  'west_2_fbic'          = 'Linkages (West)'
)

modelsummary(fixest_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             #coef_map = fixest_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# RESIDUALS --------------------------------------------------------------------

## H1 --------------------------------------------------------------------------

plot(fitted(fixest_h1_m1), resid(fixest_h1_m1))

plot(fitted(fixest_h1_m2), resid(fixest_h1_m2))

plot(fitted(fixest_h1_m3), resid(fixest_h1_m3))

plot(fitted(fixest_h1_m4), resid(fixest_h1_m4))

plot(fitted(fixest_h1_m5), resid(fixest_h1_m5))

plot(fitted(fixest_h1_m6), resid(fixest_h1_m6))

## H2 --------------------------------------------------------------------------

plot(fitted(fixest_h2_m1), resid(fixest_h2_m1))

plot(fitted(fixest_h2_m2), resid(fixest_h2_m2))

plot(fitted(fixest_h2_m3), resid(fixest_h2_m3))

plot(fitted(fixest_h2_m4), resid(fixest_h2_m4))

plot(fitted(fixest_h2_m5), resid(fixest_h2_m5))

# OUTLIERS ---------------------------------------------------------------------

library(ggrepel)


base$residuals <- residuals(fixest_h2_m5_delta, na.rm = F)
base$fit <- fitted(fixest_h2_m5_delta, na.rm = F)

base %>% 
  ggplot(aes(x = fit, y = residuals)) +
  geom_point(alpha = .3) +
  geom_point(data = base %>% filter(residuals > .42 | residuals < -.45), 
             aes(colour = country), size = 3) +
  geom_text_repel(data = base %>% filter(residuals > .42 | residuals < -.45),
            aes(label = paste0(country, ' ',year), colour = country, family = 'serif'),
            show.legend = F, size = 5) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 1, .2)) +
  scale_y_continuous(breaks = seq(-.5, .5, .1)) +
  scale_colour_manual(values = c('#ff6e54', '#ff9214', '#003f5c', '#dd5182', '#955196')) +
  labs(x = 'Fit',
       y = 'Residuals',
       colour = 'Country:') +
  theme_classic(base_family = 'serif') +
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = 'pt'))

# PLOT -------------------------------------------------------------------------

## H1 --------------------------------------------------------------------------

library(broom)

a <- tidy(fixest_h2_m5_delta, conf.level = .95, conf.int = T)

a %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2) +
  geom_vline(xintercept = 0) +
  theme_classic()

