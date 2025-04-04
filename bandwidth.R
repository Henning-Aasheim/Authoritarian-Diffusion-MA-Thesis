# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

bandwidth_m1 <- feols(f(freedom, 1) ~ bandwidth | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_m2 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_m3 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents |
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_m4 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents + oda | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_m5 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth | 
                        country+ year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

bandwidth_m6 <- feols(f(freedom, 1) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)


bandwidth_models <- list(
  'Model A.4.1' = bandwidth_m1,
  'Model A.4.2' = bandwidth_m2,
  'Model A.4.3' = bandwidth_m3,
  'Model A.4.4' = bandwidth_m4,
  'Model A.4.5' = bandwidth_m5,
  'Model A.4.6' = bandwidth_m6
)

bandwidth_map <- list(
  'bandwidth'        = 'Linkages to China',
  'gdppc_log'        = 'log(GDP per capita)',
  'rents'            = 'Resource rents',
  'oda'              = 'Aid',
  'west_2_bandwidth' = 'Linkages (West)',
  'factor(regime)1'  = 'Electoral autocracy',
  'factor(regime)2'  = 'Electoral democracy',
  'factor(regime)3'  = 'Liberal democracy'
)

modelsummary(bandwidth_models, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(bandwidth_models, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


### Delta ----------------------------------------------------------------------

bandwidth_m1_delta <- feols(f(freedom, 1) ~ delta_bandwidth | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_m2_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country',
                            panel.id = ~country+year)

bandwidth_m3_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_m4_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda | 
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_m5_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth |
                              country + year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

bandwidth_m6_delta <- feols(f(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                              country + year, 
                            data     = base, 
                            cluster  = 'country',
                            panel.id = ~country+year)


bandwidth_models_delta <- list(
  'Model A.4.7'  = bandwidth_m1_delta,
  'Model A.4.8'  = bandwidth_m2_delta,
  'Model A.4.9'  = bandwidth_m3_delta,
  'Model A.4.10' = bandwidth_m4_delta,
  'Model A.4.11' = bandwidth_m5_delta,
  'Model A.4.12' = bandwidth_m6_delta
)

bandwidth_map_delta <- list(
  'delta_bandwidth'  = 'Linkages to China',
  'gdppc_log'        = 'log(GDP per capita)',
  'rents'            = 'Resource rents',
  'oda'              = 'Aid',
  'west_2_bandwidth' = 'Linkages (West)',
  'factor(regime)1'  = 'Electoral autocracy',
  'factor(regime)2'  = 'Electoral democracy',
  'factor(regime)3'  = 'Liberal democracy'
)

modelsummary(bandwidth_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = bandwidth_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))


modelsummary(bandwidth_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = bandwidth_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


# HYPOTHESIS 2 -----------------------------------------------------------------

### Simple ---------------------------------------------------------------------

robust_interaction_m1 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) | 
                                 country + year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

robust_interaction_m2 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log |
                                 country + year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

robust_interaction_m3 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log + rents | 
                                 country + year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

robust_interaction_m4 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log + rents + oda | 
                                 country + year, 
                               data     = base,
                               cluster  = 'country',
                               panel.id = ~country+year)

robust_interaction_m5 <- feols(f(freedom, 1) ~ bandwidth*factor(regime) + gdppc_log + rents + oda + west_2_bandwidth | 
                                 country+ year, 
                               data     = base, 
                               cluster  = 'country', 
                               panel.id = ~country+year)

robust_interaction_models <- list(
  'Model A.4.13' = robust_interaction_m1,
  'Model A.4.14' = robust_interaction_m2,
  'Model A.4.15' = robust_interaction_m3,
  'Model A.4.16' = robust_interaction_m4,
  'Model A.4.17' = robust_interaction_m5
)

robust_interaction_map <- list(
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

modelsummary(robust_interaction_models, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = robust_interaction_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(robust_interaction_models, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = robust_interaction_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

### Delta -----------------------------------------------------------------------

robust_interaction_m1_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) | 
                                       country + year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

robust_interaction_m2_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log |
                                       country + year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

robust_interaction_m3_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log + rents | 
                                       country + year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

robust_interaction_m4_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log + rents + oda | 
                                       country + year, 
                                     data     = base,
                                     cluster  = 'country',
                                     panel.id = ~country+year)

robust_interaction_m5_delta <- feols(f(freedom, 1) ~ delta_bandwidth*factor(regime) + gdppc_log + rents + oda + west_2_bandwidth | 
                                       country+ year, 
                                     data     = base, 
                                     cluster  = 'country', 
                                     panel.id = ~country+year)

robust_interaction_models_delta <- list(
  'Model A.4.18'  = robust_interaction_m1_delta,
  'Model A.4.19'  = robust_interaction_m2_delta,
  'Model A.4.20'  = robust_interaction_m3_delta,
  'Model A.4.21'  = robust_interaction_m4_delta,
  'Model A.4.22' = robust_interaction_m5_delta
)

robust_interaction_map_delta <- list(
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

modelsummary(robust_interaction_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = robust_interaction_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(robust_interaction_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = robust_interaction_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# LAG --------------------------------------------------------------------------

# Lagged models not included in the thesis

## Hypothesis 1 ----------------------------------------------------------------

bandwidth_m1_lag <- feols(f(freedom, 2) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)

bandwidth_m2_lag <- feols(f(freedom, 3) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)

bandwidth_m3_lag <- feols(f(freedom, 4) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)

bandwidth_m4_lag <- feols(f(freedom, 5) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                            country + year, 
                          data     = base, 
                          cluster  = 'country', 
                          panel.id = ~country+year)


bandwidth_models_lag <- list(
  'Model 1' = bandwidth_m6,
  'Model 2' = bandwidth_m1_lag,
  'Model 3' = bandwidth_m2_lag,
  'Model 4' = bandwidth_m3_lag,
  'Model 5' = bandwidth_m4_lag
)

bandwidth_map_lag <- list(
  'bandwidth'        = 'Linkages to China',
  'gdppc_log'        = 'log(GDP per capita)',
  'rents'            = 'Resource rents',
  'oda'              = 'Aid',
  'west_2_bandwidth' = 'Linkages (West)',
  'factor(regime)1'  = 'Electoral autocracy',
  'factor(regime)2'  = 'Electoral democracy',
  'factor(regime)3'  = 'Liberal democracy'
)

modelsummary(bandwidth_models_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_map_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(bandwidth_models_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = bandwidth_map_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

## Hypothesis 2 ----------------------------------------------------------------

bandwidth_m6_delta_2 <- feols(f(freedom, 2) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                                country + year, 
                              data     = base,
                              cluster  = 'country', 
                              panel.id = ~country+year)

bandwidth_m6_delta_3 <- feols(f(freedom, 3) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                                country + year, 
                              data     = base,
                              cluster  = 'country', 
                              panel.id = ~country+year)

bandwidth_m6_delta_4 <- feols(f(freedom, 4) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

bandwidth_m6_delta_5 <- feols(f(freedom, 5) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)


bandwidth_models_delta_lag <- list(
  'Model 1.12' = bandwidth_m6_delta,
  'Model 1.13' = bandwidth_m6_delta_2,
  'Model 1.14' = bandwidth_m6_delta_3,
  'Model 1.15' = bandwidth_m6_delta_4,
  'Model 1.16' = bandwidth_m6_delta_5
)

bandwidth_map_delta_lag <- list(
  'delta_bandwidth'  = 'Linkages to China',
  'gdppc_log'        = 'log(GDP per capita)',
  'rents'            = 'Resource rents',
  'oda'              = 'Aid',
  'west_2_bandwidth' = 'Linkages (West)',
  'factor(regime)1'  = 'Electoral autocracy',
  'factor(regime)2'  = 'Electoral democracy',
  'factor(regime)3'  = 'Liberal democracy'
)

modelsummary(bandwidth_models_delta_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = bandwidth_map_delta_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))