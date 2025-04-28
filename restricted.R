library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables
library(tidyverse)

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

base_restricted <- base %>% 
  filter(year > 2009)

# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

restricted_h1_m1 <- feols(f(freedom, 1) ~ fbic | 
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h1_m2 <- feols(f(freedom, 1) ~ fbic + gdppc_log | 
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h1_m3 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents |
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h1_m4 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda | 
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h1_m5 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic | 
                        country+ year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h1_m6 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)


restricted_h1 <- list(
  'Model 1.1' = restricted_h1_m1,
  'Model 1.2' = restricted_h1_m2,
  'Model 1.3' = restricted_h1_m3,
  'Model 1.4' = restricted_h1_m4,
  'Model 1.5' = restricted_h1_m5,
  'Model 1.6' = restricted_h1_m6
)

restricted_h1_map <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(restricted_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = restricted_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(restricted_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = restricted_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# HYPOTHESIS 2 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

restricted_h2_m1 <- feols(f(freedom, 1) ~ fbic*factor(regime) | 
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h2_m2 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log |
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h2_m3 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents | 
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h2_m4 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda | 
                        country + year, 
                      data     = base_restricted,
                      cluster  = 'country',
                      panel.id = ~country+year)

restricted_h2_m5 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                        country + year, 
                      data     = base_restricted, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

restricted_h2 <- list(
  'Model 2.1' = restricted_h2_m1,
  'Model 2.2' = restricted_h2_m2,
  'Model 2.3' = restricted_h2_m3,
  'Model 2.4' = restricted_h2_m4,
  'Model 2.5' = restricted_h2_m5
)

restricted_h2_map <- list(
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

modelsummary(restricted_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = restricted_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(restricted_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = restricted_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')