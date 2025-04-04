# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables
library(tidyverse)    # For creating delta_bandwidth

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

base <- base %>% 
  mutate(delta_bandwidth = bandwidth - lag(bandwidth, n = 3))

# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

bandwidth_h1_m1 <- feols(f(freedom, 1) ~ bandwidth | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_h1_m2 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_h1_m3 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents |
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_h1_m4 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents + oda | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_h1_m5 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth | 
                        country+ year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_h1_m6 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)


bandwidth_h1 <- list(
  'Model A.4.1' = bandwidth_h1_m1,
  'Model A.4.2' = bandwidth_h1_m2,
  'Model A.4.3' = bandwidth_h1_m3,
  'Model A.4.4' = bandwidth_h1_m4,
  'Model A.4.5' = bandwidth_h1_m5,
  'Model A.4.6' = bandwidth_h1_m6
)

bandwidth_h1_map <- list(
  'bandwidth'        = 'Linkages to China',
  'gdppc_log'        = 'log(GDP per capita)',
  'rents'            = 'Resource rents',
  'oda'              = 'Aid',
  'west_2_bandwidth' = 'Linkages (West)',
  'factor(regime)1'  = 'Electoral autocracy',
  'factor(regime)2'  = 'Electoral democracy',
  'factor(regime)3'  = 'Liberal democracy'
)

modelsummary(bandwidth_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(bandwidth_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


### Delta ----------------------------------------------------------------------

bandwidth_h1_m1_delta <- feols(f(freedom, 1) ~ delta_bandwidth | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_h1_m2_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country',
                            panel.id = ~country+year)

bandwidth_h1_m3_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_h1_m4_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_h1_m5_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth |
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_h1_m6_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                              country + year, 
                            data     = base, 
                            cluster  = 'country',
                            panel.id = ~country+year)


bandwidth_h1_delta <- list(
  'Model A.4.7'  = bandwidth_h1_m1_delta,
  'Model A.4.8'  = bandwidth_h1_m2_delta,
  'Model A.4.9'  = bandwidth_h1_m3_delta,
  'Model A.4.10' = bandwidth_h1_m4_delta,
  'Model A.4.11' = bandwidth_h1_m5_delta,
  'Model A.4.12' = bandwidth_h1_m6_delta
)

bandwidth_h1_delta_map <- list(
  'delta_bandwidth'  = 'Linkages to China',
  'gdppc_log'        = 'log(GDP per capita)',
  'rents'            = 'Resource rents',
  'oda'              = 'Aid',
  'west_2_bandwidth' = 'Linkages (West)',
  'factor(regime)1'  = 'Electoral autocracy',
  'factor(regime)2'  = 'Electoral democracy',
  'factor(regime)3'  = 'Liberal democracy'
)

modelsummary(bandwidth_h1_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = bandwidth_h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))


modelsummary(bandwidth_h1_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = bandwidth_h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


# HYPOTHESIS 2 -----------------------------------------------------------------

### Simple ---------------------------------------------------------------------

bandwidth_h2_m1 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) | 
                                 country + year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

bandwidth_h2_m2 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log |
                                 country + year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

bandwidth_h2_m3 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log + rents | 
                                 country + year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

bandwidth_h2_m4 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log + rents + oda | 
                                 country + year, 
                               data     = base,
                               cluster  = 'country',
                               panel.id = ~country+year)

bandwidth_h2_m5 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log + rents + oda + west_2_bandwidth | 
                                 country+ year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

bandwidth_h2 <- list(
  'Model A.4.13' = bandwidth_h2_m1,
  'Model A.4.14' = bandwidth_h2_m2,
  'Model A.4.15' = bandwidth_h2_m3,
  'Model A.4.16' = bandwidth_h2_m4,
  'Model A.4.17' = bandwidth_h2_m5
)

bandwidth_h2_map <- list(
  'bandwidth'                 = 'Linkages to China',
  'factor(regime)1'           = 'Electoral autocracy',
  'factor(regime)2'           = 'Electoral democracy',
  'factor(regime)3'           = 'Liberal democracy',
  'bandwidth:factor(regime)1' = 'China x El.Aut.',
  'bandwidth:factor(regime)2' = 'China x El.Dem.',
  'bandwidth:factor(regime)3' = 'China x Lib.Dem.',
  'gdppc_log'                 = 'log(GDP per capita)',
  'rents'                     = 'Resource rents',
  'oda'                       = 'Aid',
  'west_2_bandwidth'          = 'Linkages (West)'
)

modelsummary(bandwidth_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(bandwidth_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

### Delta -----------------------------------------------------------------------

bandwidth_h2_m1_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) | 
                                       country + year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

bandwidth_h2_m2_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log |
                                       country + year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

bandwidth_h2_m3_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log + rents | 
                                       country + year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

bandwidth_h2_m4_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log + rents + oda | 
                                       country + year, 
                                     data     = base,
                                     cluster  = 'country',
                                     panel.id = ~country+year)

bandwidth_h2_m5_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log + rents + oda + west_2_bandwidth | 
                                       country+ year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

bandwidth_h2_delta <- list(
  'Model A.4.18' = bandwidth_h2_m1_delta,
  'Model A.4.19' = bandwidth_h2_m2_delta,
  'Model A.4.20' = bandwidth_h2_m3_delta,
  'Model A.4.21' = bandwidth_h2_m4_delta,
  'Model A.4.22' = bandwidth_h2_m5_delta
)

bandwidth_h2_delta_map <- list(
  'delta_bandwidth'                 = 'Linkages to China',
  'factor(regime)1'                 = 'Electoral autocracy',
  'factor(regime)2'                 = 'Electoral democracy',
  'factor(regime)3'                 = 'Liberal democracy',
  'delta_bandwidth:factor(regime)1' = 'China x El.Aut.',
  'delta_bandwidth:factor(regime)2' = 'China x El.Dem.',
  'delta_bandwidth:factor(regime)3' = 'China x Lib.Dem.',
  'gdppc_log'                       = 'log(GDP per capita)',
  'rents'                           = 'Resource rents',
  'oda'                             = 'Aid',
  'west_2_bandwidth'                = 'Linkages (West)'
)

modelsummary(bandwidth_h2_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h2_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(bandwidth_h2_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h2_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# LAG --------------------------------------------------------------------------

# Lagged models not included in the thesis

## Hypothesis 1 ----------------------------------------------------------------

bandwidth_h1_m1_lag <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                               country + year, 
                             data     = base, 
                             cluster  = 'country', 
                             panel.id = ~country+year)

bandwidth_h1_m3_lag <- feols(f(freedom, 2) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)

bandwidth_h1_m4_lag <- feols(f(freedom, 3) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)

bandwidth_h1_m4_lag <- feols(f(freedom, 4) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)

bandwidth_h1_m5_lag <- feols(f(freedom, 5) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)


bandwidth_h1_lag <- list(
  'Model 1' = bandwidth_h1_m1_lag,
  'Model 2' = bandwidth_h1_m3_lag,
  'Model 3' = bandwidth_h1_m3_lag,
  'Model 4' = bandwidth_h1_m4_lag,
  'Model 5' = bandwidth_h1_m5_lag
)

bandwidth_h1_lag_map <- list(
  'bandwidth'        = 'Linkages to China',
  'gdppc_log'        = 'log(GDP per capita)',
  'rents'            = 'Resource rents',
  'oda'              = 'Aid',
  'west_2_bandwidth' = 'Linkages (West)',
  'factor(regime)1'  = 'Electoral autocracy',
  'factor(regime)2'  = 'Electoral democracy',
  'factor(regime)3'  = 'Liberal democracy'
)

modelsummary(bandwidth_h1_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h1_lag_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(bandwidth_h1_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_h1_lag_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')
