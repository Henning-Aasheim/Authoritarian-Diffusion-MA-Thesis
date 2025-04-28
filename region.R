library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables
library(tidyverse)

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

region_h1_m1 <- feols(f(freedom, 1) ~ fbic*factor(region) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

region_h1_m2 <- feols(f(freedom, 1) ~ fbic*factor(region) + gdppc_log | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

region_h1_m3 <- feols(f(freedom, 1) ~ fbic*factor(region) + gdppc_log + rents |
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

region_h1_m4 <- feols(f(freedom, 1) ~ fbic*factor(region) + gdppc_log + rents + oda | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

region_h1_m5 <- feols(f(freedom, 1) ~ fbic*factor(region) + gdppc_log + rents + oda + west_2_fbic | 
                        country+ year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

region_h1_m6 <- feols(f(freedom, 1) ~ fbic*factor(region) + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)


region_h1 <- list(
  'Model 1.1' = region_h1_m1,
  'Model 1.2' = region_h1_m2,
  'Model 1.3' = region_h1_m3,
  'Model 1.4' = region_h1_m4,
  'Model 1.5' = region_h1_m5,
  'Model 1.6' = region_h1_m6
)

region_h1_map <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(region_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             #coef_map = region_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(region_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = region_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')
