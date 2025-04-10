# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables
library(tidyverse)

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

base <- base %>% 
  mutate(delta_gdppc = gdppc_log - lag(gdppc_log, n = 3),
         delta_rents = rents - lag(rents, n = 3),
         delta_oda   = oda - lag(oda, n = 3),
         delta_west  = west_2_fbic - lag(west_2_fbic, n = 3))

# HYPOTHESIS 1 -----------------------------------------------------------------

## Model 1 ---------------------------------------------------------------------

delta_h1_m1 <- feols(f(freedom, 1) ~ delta_fbic | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h1_m2 <- feols(f(freedom, 1) ~ delta_fbic + delta_gdppc | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country',
                            panel.id = ~country+year)

delta_h1_m3 <- feols(f(freedom, 1) ~ delta_fbic + delta_gdppc + delta_rents | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h1_m4 <- feols(f(freedom, 1) ~ delta_fbic + delta_gdppc + delta_rents + delta_oda | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h1_m5 <- feols(f(freedom, 1) ~ delta_fbic + delta_gdppc + delta_rents + delta_oda + delta_west |
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h1_m6 <- feols(f(freedom, 1) ~ delta_fbic + delta_gdppc + delta_rents + delta_oda + delta_west + factor(regime) |
                              country + year, 
                            data     = base, 
                            cluster  = 'country',
                            panel.id = ~country+year)


delta_h1 <- list(
  'Model 1.7'  = delta_h1_m1,
  'Model 1.8'  = delta_h1_m2,
  'Model 1.9'  = delta_h1_m3,
  'Model 1.10' = delta_h1_m4,
  'Model 1.11' = delta_h1_m5,
  'Model 1.12' = delta_h1_m6
)

delta_h1_map <- list(
  'delta_fbic'      = 'Linkages to China',
  'delta_gdppc'     = 'log(GDP per capita)',
  'delta_rents'     = 'Resource rents',
  'delta_oda'       = 'Aid',
  'delta_west'      = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(delta_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = delta_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(delta_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = delta_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

## Model 2 ---------------------------------------------------------------------


delta_h1_m7 <- feols(f(freedom, 1) ~ d(fbic, 3) | 
                       country + year, 
                     data     = base, 
                     cluster  = 'country', 
                     panel.id = ~country+year)

delta_h1_m8 <- feols(f(freedom, 1) ~ d(fbic, 3) + gdppc_log | 
                       country + year, 
                     data     = base, 
                     cluster  = 'country',
                     panel.id = ~country+year)

delta_h1_m9 <- feols(f(freedom, 1) ~ d(fbic, 3) + gdppc_log + rents | 
                       country + year, 
                     data     = base, 
                     cluster  = 'country', 
                     panel.id = ~country+year)

delta_h1_m10 <- feols(f(freedom, 1) ~ d(fbic, 3) + gdppc_log + rents + oda | 
                       country + year, 
                     data     = base, 
                     cluster  = 'country', 
                     panel.id = ~country+year)

delta_h1_m11 <- feols(f(freedom, 1) ~ d(fbic, 3) + gdppc_log + rents + oda + west_2_fbic |
                       country + year, 
                     data     = base, 
                     cluster  = 'country', 
                     panel.id = ~country+year)

delta_h1_m12 <- feols(f(freedom, 1) ~ d(fbic, 3) + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                       country + year, 
                     data     = base, 
                     cluster  = 'country',
                     panel.id = ~country+year)


delta_h1_2 <- list(
  'Model 1.7'  = delta_h1_m7,
  'Model 1.8'  = delta_h1_m8,
  'Model 1.9'  = delta_h1_m9,
  'Model 1.10' = delta_h1_m10,
  'Model 1.11' = delta_h1_m11,
  'Model 1.12' = delta_h1_m12
)

delta_h1_2_map <- list(
  'd(fbic, 3)'      = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(delta_h1_2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = delta_h1_2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(delta_h1_2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = delta_h1_2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

## Model 3 ---------------------------------------------------------------------


delta_h1_m13 <- feols(f(freedom, 1) ~ d(fbic, 3) | 
                       country + year, 
                     data     = base, 
                     cluster  = 'country', 
                     panel.id = ~country+year)

delta_h1_m14 <- feols(f(freedom, 1) ~ d(fbic, 3) + d(gdppc_log, 3) | 
                       country + year, 
                     data     = base, 
                     cluster  = 'country',
                     panel.id = ~country+year)

delta_h1_m15 <- feols(f(freedom, 1) ~ d(fbic, 3) + d(gdppc_log, 3) + d(rents, 3) | 
                       country + year, 
                     data     = base, 
                     cluster  = 'country', 
                     panel.id = ~country+year)

delta_h1_m16 <- feols(f(freedom, 1) ~ d(fbic, 3) + d(gdppc_log, 3) + d(rents, 3) + d(oda, 3) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

delta_h1_m17 <- feols(f(freedom, 1) ~ d(fbic, 3) + d(gdppc_log, 3) + d(rents, 3) + d(oda, 3) + d(west_2_fbic, 3) |
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

delta_h1_m18 <- feols(f(freedom, 1) ~ d(fbic, 3) + d(gdppc_log, 3) + d(rents, 3) + d(oda, 3) + d(west_2_fbic, 3) + factor(regime) |
                        country + year, 
                      data     = base, 
                      cluster  = 'country',
                      panel.id = ~country+year)


delta_h1_3 <- list(
  'Model 1.7'  = delta_h1_m13,
  'Model 1.8'  = delta_h1_m14,
  'Model 1.9'  = delta_h1_m15,
  'Model 1.10' = delta_h1_m16,
  'Model 1.11' = delta_h1_m17,
  'Model 1.12' = delta_h1_m18
)

delta_h1_3_map <- list(
  'd(fbic, 3)'        = 'Linkages to China',
  'd(gdppc_log, 3)'   = 'log(GDP per capita)',
  'd(rents, 3)'       = 'Resource rents',
  'd(oda, 3)'         = 'Aid',
  'd(west_2_fbic, 3)' = 'Linkages (West)',
  'factor(regime)1'   = 'Electoral autocracy',
  'factor(regime)2'   = 'Electoral democracy',
  'factor(regime)3'   = 'Liberal democracy'
)

modelsummary(delta_h1_3, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = delta_h1_3_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(delta_h1_3, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = delta_h1_3_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# HYPOTHESIS 2 -----------------------------------------------------------------

delta_h2_m1 <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h2_m2 <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + delta_gdppc  |
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h2_m3 <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + delta_gdppc  + delta_rents | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h2_m4 <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + delta_gdppc  + delta_rents + delta_oda | 
                              country + year, 
                            data     = base,
                            cluster  = 'country',
                            panel.id = ~country+year)

delta_h2_m5 <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + delta_gdppc  + delta_rents + delta_oda + delta_west | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

delta_h2 <- list(
  'Model 2.6'  = delta_h2_m1,
  'Model 2.7'  = delta_h2_m2,
  'Model 2.8'  = delta_h2_m3,
  'Model 2.9'  = delta_h2_m4,
  'Model 2.10' = delta_h2_m5
)

delta_h2_map <- list(
  'delta_fbic'                 = 'Linkages to China',
  'factor(regime)1'            = 'Electoral autocracy',
  'factor(regime)2'            = 'Electoral democracy',
  'factor(regime)3'            = 'Liberal democracy',
  'delta_fbic:factor(regime)1' = 'China x El.Aut.',
  'delta_fbic:factor(regime)2' = 'China x El.Dem.',
  'delta_fbic:factor(regime)3' = 'China x Lib.Dem.',
  'delta_gdppc'                = 'log(GDP per capita)',
  'delta_rents'                = 'Resource rents',
  'delta_oda'                  = 'Aid',
  'delta_west'                 = 'Linkages (West)'
)

modelsummary(delta_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = delta_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(delta_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = delta_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# RESIDUALS --------------------------------------------------------------------

plot(fitted(delta_h1_m18), resid(delta_h1_m18))
