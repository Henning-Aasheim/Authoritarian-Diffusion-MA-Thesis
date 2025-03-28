library(fixest)
library(modelsummary)
library(sandwich)
library(tidyverse)

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

base <- base %>% 
  mutate(delta_fbic = fbic - lag(fbic, n = 3))

# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

fixest_m1 <- feols(l(freedom, 1) ~ fbic | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_m2 <- feols(l(freedom, 1) ~ fbic + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_m3 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_m4 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_m5 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_m6 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


fixest_models <- list(
  'Model 1.1' = fixest_m1,
  'Model 1.2' = fixest_m2,
  'Model 1.3' = fixest_m3,
  'Model 1.4' = fixest_m4,
  'Model 1.5' = fixest_m5,
  'Model 1.6' = fixest_m6
)

fixest_map <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(fixest_models, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

# The high R squared is a result of the fixed effects, where I control for many 

## Lagged simple ---------------------------------------------------------------

fixest_m1_lag <- feols(l(freedom, 2) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

fixest_m2_lag <- feols(l(freedom, 3) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

fixest_m3_lag <- feols(l(freedom, 4) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

fixest_m4_lag <- feols(l(freedom, 5) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)


fixest_models_lag <- list(
  'Model A.1' = fixest_m6,
  'Model A.2' = fixest_m1_lag,
  'Model A.3' = fixest_m2_lag,
  'Model A.4' = fixest_m3_lag,
  'Model A.5' = fixest_m4_lag
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

## Delta -----------------------------------------------------------------------

fixest_m1_delta <- feols(l(freedom, 1) ~ delta_fbic | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_m2_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

fixest_m3_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_m4_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_m5_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic |
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_m6_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)


fixest_models_delta <- list(
  'Model 1.7'  = fixest_m1_delta,
  'Model 1.8'  = fixest_m2_delta,
  'Model 1.9'  = fixest_m3_delta,
  'Model 1.10' = fixest_m4_delta,
  'Model 1.11' = fixest_m5_delta,
  'Model 1.12' = fixest_m6_delta
)

fixest_map_delta <- list(
  'delta_fbic'      = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(fixest_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = fixest_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

## Lagged delta ----------------------------------------------------------------

fixest_m6_delta_2 <- feols(l(freedom, 2) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

fixest_m6_delta_3 <- feols(l(freedom, 3) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

fixest_m6_delta_4 <- feols(l(freedom, 4) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

fixest_m6_delta_5 <- feols(l(freedom, 5) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
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



# HYPOTHESIS 2 & 3 -------------------------------------------------------------

## Simple -----------------------------------------------------------------------

interaction_m1 <- feols(l(freedom, 1) ~ fbic*factor(regime) | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

interaction_m2 <- feols(l(freedom, 1) ~ fbic*factor(regime) + gdppc_log |
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

interaction_m3 <- feols(l(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

interaction_m4 <- feols(l(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda | 
                          country + year, 
                        data     = base,
                        cluster  = 'country',
                        panel.id = ~country+year)

interaction_m5 <- feols(l(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                          country+ year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

interaction_models <- list(
  'Model 2.1' = interaction_m1,
  'Model 2.2' = interaction_m2,
  'Model 2.3' = interaction_m3,
  'Model 2.4' = interaction_m4,
  'Model 2.5' = interaction_m5
)

interaction_map <- list(
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

modelsummary(interaction_models, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

## Lagged simple -----------------------------------------------------------------------

interaction_m1_lag <- feols(l(freedom, 2) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

interaction_m2_lag <- feols(l(freedom, 3) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

interaction_m3_lag <- feols(l(freedom, 4) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

interaction_m4_lag <- feols(l(freedom, 5) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
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

## Delta -----------------------------------------------------------------------

interaction_m1_delta <- feols(l(freedom, 1) ~ delta_fbic*factor(regime) | 
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_m2_delta <- feols(l(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log |
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_m3_delta <- feols(l(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents | 
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_m4_delta <- feols(l(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda | 
                                country + year, 
                              data     = base,
                              cluster  = 'country',
                              panel.id = ~country+year)

interaction_m5_delta <- feols(l(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                country+ year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_models_delta <- list(
  'Model 2.6'  = interaction_m1_delta,
  'Model 2.7'  = interaction_m2_delta,
  'Model 2.8'  = interaction_m3_delta,
  'Model 2.9'  = interaction_m4_delta,
  'Model 2.10' = interaction_m5_delta
)

interaction_map_delta <- list(
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

modelsummary(interaction_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))


## Lagged delta --------------------------------------------------------


interaction_m1_lag <- feols(l(freedom, 2) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                country+ year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_m2_lag <- feols(l(freedom, 3) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                country+ year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_m3_lag <- feols(l(freedom, 4) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                country+ year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_m4_lag <- feols(l(freedom, 5) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                country+ year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

interaction_models_lag <- list(
  'Model A.1.16' = interaction_m5_delta,
  'Model A.1.17' = interaction_m1_lag,
  'Model A.1.18' = interaction_m2_lag,
  'Model A.1.19' = interaction_m3_lag,
  'Model A.1.20' = interaction_m4_lag
)

interaction_map_lag <- list(
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

modelsummary(interaction_models_lag, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map_lag,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))
