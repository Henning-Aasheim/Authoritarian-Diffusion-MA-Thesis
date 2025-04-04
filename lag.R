# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

# HYPOTHESIS 1 -----------------------------------------------------------------

## Lagged simple ---------------------------------------------------------------

fixest_m1_lag <- feols(f(freedom, 2) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

fixest_m2_lag <- feols(f(freedom, 3) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

fixest_m3_lag <- feols(f(freedom, 4) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

fixest_m4_lag <- feols(f(freedom, 5) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)


fixest_models_lag <- list(
  'Model A.1.1' = fixest_m6,
  'Model A.1.2' = fixest_m1_lag,
  'Model A.1.3' = fixest_m2_lag,
  'Model A.1.4' = fixest_m3_lag,
  'Model A.1.5' = fixest_m4_lag
)

fixest_map_lag <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(fixest_models_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_models_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

## Lagged delta ----------------------------------------------------------------

fixest_m6_delta_2 <- feols(f(freedom, 2) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

fixest_m6_delta_3 <- feols(f(freedom, 3) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

fixest_m6_delta_4 <- feols(f(freedom, 4) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

fixest_m6_delta_5 <- feols(f(freedom, 5) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)


fixest_models_delta_lag <- list(
  'Model A.1.6'  = fixest_m6_delta,
  'Model A.1.7'  = fixest_m6_delta_2,
  'Model A.1.8'  = fixest_m6_delta_3,
  'Model A.1.9'  = fixest_m6_delta_4,
  'Model A.1.10' = fixest_m6_delta_5
)

fixest_map_delta_lag <- list(
  'delta_fbic'      = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(fixest_models_delta_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = fixest_map_delta_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_models_delta_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = fixest_map_delta_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# HYPOTHESIS 2 -----------------------------------------------------------------

## Lagged simple ---------------------------------------------------------------

interaction_m1_lag <- feols(f(freedom, 2) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

interaction_m2_lag <- feols(f(freedom, 3) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

interaction_m3_lag <- feols(f(freedom, 4) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

interaction_m4_lag <- feols(f(freedom, 5) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

interaction_models_lag <- list(
  'Model A.1.11'  = interaction_m5,
  'Model A.1.12'  = interaction_m1_lag,
  'Model A.1.13'  = interaction_m2_lag,
  'Model A.1.14'  = interaction_m3_lag,
  'Model A.1.15'  = interaction_m4_lag
)

interaction_map_lag <- list(
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

modelsummary(interaction_models_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(interaction_models_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

## Lagged delta --------------------------------------------------------


interaction_m1_lag_delta <- feols(f(freedom, 2) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

interaction_m2_lag_delta <- feols(f(freedom, 3) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

interaction_m3_lag_delta <- feols(f(freedom, 4) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

interaction_m4_lag_delta <- feols(f(freedom, 5) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

interaction_models_lag_delta <- list(
  'Model A.1.16' = interaction_m5_delta,
  'Model A.1.17' = interaction_m1_lag_delta,
  'Model A.1.18' = interaction_m2_lag_delta,
  'Model A.1.19' = interaction_m3_lag_delta,
  'Model A.1.20' = interaction_m4_lag_delta
)

interaction_map_lag_delta <- list(
  'delta_fbic'                 = 'Linkages to China',
  'factor(regime)1'            = 'Electoral autocracy',
  'factor(regime)2'            = 'Electoral democracy',
  'factor(regime)3'            = 'Liberal democracy',
  'delta_fbic:factor(regime)1' = 'China x El.Aut.',
  'delta_fbic:factor(regime)2' = 'China x El.Dem.',
  'delta_fbic:factor(regime)3' = 'China x Lib.Dem.',
  'gdppc_log'                  = 'log(GDP per capita)',
  'rents'                      = 'Resource rents',
  'oda'                        = 'Aid',
  'west_2_fbic'                = 'Linkages (West)'
)

modelsummary(interaction_models_lag_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map_lag_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(interaction_models_lag_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map_lag_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')