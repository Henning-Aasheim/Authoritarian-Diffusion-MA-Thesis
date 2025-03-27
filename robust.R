# CONFIG -----------------------------------------------------------------------

library(fixest)
library(modelsummary)
library(sandwich)

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

# DATA -------------------------------------------------------------------------

# Adding change in the fbic variable to the base dataset
base <- base %>% 
  mutate(delta_fbic_1 = fbic - lag(fbic, n = 1),
         delta_fbic_2 = fbic - lag(fbic, n = 2),
         delta_fbic   = fbic - lag(fbic, n = 3),
         delta_fbic_4 = fbic - lag(fbic, n = 4),
         delta_fbic_5 = fbic - lag(fbic, n = 5),
         delta_bandwidth = bandwidth - lag(bandwidth, n = 3))

# bandwidth -------------------------------------------------------------------------

## Simple without lag ----------------------------------------------------------

robust_m1 <- feols(freedom ~ fbic | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

robust_m2 <- feols(freedom ~ fbic + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

robust_m3 <- feols(freedom ~ fbic + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

robust_m4 <- feols(freedom ~ fbic + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

robust_m5 <- feols(freedom ~ fbic + gdppc_log + rents + oda + west_2_fbic | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

robust_m6 <- feols(freedom ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


robust_models <- list(
  'Model 1.1' = robust_m1,
  'Model 1.2' = robust_m2,
  'Model 1.3' = robust_m3,
  'Model 1.4' = robust_m4,
  'Model 1.5' = robust_m5,
  'Model 1.6' = robust_m6
)

robust_map <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(robust_models, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = robust_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

## Delta without lag -----------------------------------------------------------

robust_m1_delta <- feols(freedom ~ delta_fbic | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

robust_m2_delta <- feols(freedom ~ delta_fbic + gdppc_log | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

robust_m3_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

robust_m4_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

robust_m5_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic |
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

robust_m6_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)


robust_delta <- list(
  'Model 1.7'  = robust_m1_delta,
  'Model 1.8'  = robust_m2_delta,
  'Model 1.9'  = robust_m3_delta,
  'Model 1.10' = robust_m4_delta,
  'Model 1.11' = robust_m5_delta,
  'Model 1.12' = robust_m6_delta
)

robust_delta_map <- list(
  'delta_fbic'      = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(robust_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = robust_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

## Delta 1-5 -------------------------------------------------------------------

robust_m6_delta_1 <- feols(l(freedom, 1) ~ delta_fbic_1 + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_2 <- feols(l(freedom, 1) ~ delta_fbic_2 + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_3 <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_4 <- feols(l(freedom, 1) ~ delta_fbic_4 + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_5 <- feols(l(freedom, 1) ~ delta_fbic_5 + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)


robust_models_delta <- list(
  'Model R.1' = robust_m6_delta_1,
  'Model R.2' = robust_m6_delta_2,
  'Model R.3' = robust_m6_delta_3,
  'Model R.4' = robust_m6_delta_4,
  'Model R.5' = robust_m6_delta_5
)

robust_map_delta <- list(
  'delta_fbic_1'    = 'Linkages to China',
  'delta_fbic_2'    = 'Linkages to China',
  'delta_fbic'      = 'Linkages to China',
  'delta_fbic_4'    = 'Linkages to China',
  'delta_fbic_5'    = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(robust_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = robust_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

## Delta 1-5 interaction -------------------------------------------------------

robust_m5_delta_1 <- feols(l(freedom, 1) ~ delta_fbic_1*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_2 <- feols(l(freedom, 1) ~ delta_fbic_2*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_3 <- feols(l(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_4 <- feols(l(freedom, 1) ~ delta_fbic_4*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_5 <- feols(l(freedom, 1) ~ delta_fbic_5*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_interaction_delta <- list(
  'Model R.6'  = robust_m5_delta_1,
  'Model R.7'  = robust_m5_delta_2,
  'Model R.8'  = robust_m5_delta_3,
  'Model R.9'  = robust_m5_delta_4,
  'Model R.10' = robust_m5_delta_5
)

robust_interaction_delta_map <- list(
  'delta_fbic_1'                 = 'Linkages to China',
  'delta_fbic_2'                 = 'Linkages to China',
  'delta_fbic'                   = 'Linkages to China',
  'delta_fbic_4'                 = 'Linkages to China',
  'delta_fbic_5'                 = 'Linkages to China',
  'factor(regime)1'              = 'Electoral autocracy',
  'factor(regime)2'              = 'Electoral democracy',
  'factor(regime)3'              = 'Liberal democracy',
  'delta_fbic_1:factor(regime)1' = 'China x El.Aut.',
  'delta_fbic_2:factor(regime)1' = 'China x El.Aut.',
  'delta_fbic:factor(regime)1'   = 'China x El.Aut.',
  'delta_fbic_4:factor(regime)1' = 'China x El.Aut.',
  'delta_fbic_5:factor(regime)1' = 'China x El.Aut.',
  'delta_fbic_1:factor(regime)2' = 'China x El.Dem.',
  'delta_fbic_2:factor(regime)2' = 'China x El.Dem.',
  'delta_fbic:factor(regime)2'   = 'China x El.Dem.',
  'delta_fbic_4:factor(regime)2' = 'China x El.Dem.',
  'delta_fbic_5:factor(regime)2' = 'China x El.Dem.',
  'delta_fbic_1:factor(regime)3' = 'China x Lib.Dem.',
  'delta_fbic_2:factor(regime)3' = 'China x Lib.Dem.',
  'delta_fbic:factor(regime)3'   = 'China x Lib.Dem.',
  'delta_fbic_4:factor(regime)3' = 'China x Lib.Dem.',
  'delta_fbic_5:factor(regime)3' = 'China x Lib.Dem.',
  'gdppc_log'                    = 'log(GDP per capita)',
  'rents'                        = 'Resource rents',
  'oda'                          = 'Aid',
  'west_2_fbic'                  = 'Linkages (West)'
)

modelsummary(robust_interaction_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = robust_interaction_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

# BANDWIDTH --------------------------------------------------------------------

## Simple ----------------------------------------------------------------------

bandwidth_m1 <- feols(l(freedom, 1) ~ bandwidth | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

bandwidth_m2 <- feols(l(freedom, 1) ~ bandwidth + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

bandwidth_m3 <- feols(l(freedom, 1) ~ bandwidth + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

bandwidth_m4 <- feols(l(freedom, 1) ~ bandwidth + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

bandwidth_m5 <- feols(l(freedom, 1) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

bandwidth_m6 <- feols(l(freedom, 1) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


bandwidth_models <- list(
  'Model 1.1' = bandwidth_m1,
  'Model 1.2' = bandwidth_m2,
  'Model 1.3' = bandwidth_m3,
  'Model 1.4' = bandwidth_m4,
  'Model 1.5' = bandwidth_m5,
  'Model 1.6' = bandwidth_m6
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

## Lagged simple ---------------------------------------------------------------

bandwidth_m1_lag <- feols(l(freedom, 2) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

bandwidth_m2_lag <- feols(l(freedom, 3) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

bandwidth_m3_lag <- feols(l(freedom, 4) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

bandwidth_m4_lag <- feols(l(freedom, 5) ~ bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
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

## Delta -----------------------------------------------------------------------

bandwidth_m1_delta <- feols(l(freedom, 1) ~ delta_bandwidth | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

bandwidth_m2_delta <- feols(l(freedom, 1) ~ delta_bandwidth + gdppc_log | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

bandwidth_m3_delta <- feols(l(freedom, 1) ~ delta_bandwidth + gdppc_log + rents | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

bandwidth_m4_delta <- feols(l(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

bandwidth_m5_delta <- feols(l(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth |
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

bandwidth_m6_delta <- feols(l(freedom, 1) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)


bandwidth_models_delta <- list(
  'Model 1.7'  = bandwidth_m1_delta,
  'Model 1.8'  = bandwidth_m2_delta,
  'Model 1.9'  = bandwidth_m3_delta,
  'Model 1.10' = bandwidth_m4_delta,
  'Model 1.11' = bandwidth_m5_delta,
  'Model 1.12' = bandwidth_m6_delta
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

## Lagged delta ----------------------------------------------------------------

bandwidth_m6_delta_2 <- feols(l(freedom, 2) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

bandwidth_m6_delta_3 <- feols(l(freedom, 3) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

bandwidth_m6_delta_4 <- feols(l(freedom, 4) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

bandwidth_m6_delta_5 <- feols(l(freedom, 5) ~ delta_bandwidth + gdppc_log + rents + oda + west_2_bandwidth + factor(regime) |
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
