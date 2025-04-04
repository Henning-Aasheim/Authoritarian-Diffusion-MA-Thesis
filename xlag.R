# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')




# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple without lag ---------------------------------------------------------

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
  'Model A.2.1' = robust_m1,
  'Model A.2.2' = robust_m2,
  'Model A.2.3' = robust_m3,
  'Model A.2.4' = robust_m4,
  'Model A.2.5' = robust_m5,
  'Model A.2.6' = robust_m6
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
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


## Delta without lag ----------------------------------------------------------

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
  'Model A.2.7'  = robust_m1_delta,
  'Model A.2.8'  = robust_m2_delta,
  'Model A.2.9'  = robust_m3_delta,
  'Model A.2.10' = robust_m4_delta,
  'Model A.2.11' = robust_m5_delta,
  'Model A.2.12' = robust_m6_delta
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
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


# HYPOTHESIS 2 -----------------------------------------------------------------

## Simple without lag ---------------------------------------------------------

interaction_m1_r <- feols(freedom ~ fbic*factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

interaction_m2_r <- feols(freedom ~ fbic*factor(regime) + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

interaction_m3_r <- feols(freedom ~ fbic*factor(regime) + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

interaction_m4_r <- feols(freedom ~ fbic*factor(regime) + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

interaction_m5_r <- feols(freedom ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


interaction_models_r <- list(
  'Model A.2.13' = interaction_m1_r,
  'Model A.2.14' = interaction_m2_r,
  'Model A.2.15' = interaction_m3_r,
  'Model A.2.16' = interaction_m4_r,
  'Model A.2.17' = interaction_m5_r
)

interaction_map_r <- list(
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

modelsummary(interaction_models_r, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = interaction_map_r,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


## Delta without lag ----------------------------------------------------------

interaction_m1_delta_r <- feols(freedom ~ delta_fbic*factor(regime) | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

interaction_m2_delta_r <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

interaction_m3_delta_r <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log + rents | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

interaction_m4_delta_r <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log + rents + oda | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

interaction_m5_delta_r <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic |
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)


interaction_delta_r <- list(
  'Model A.2.18'  = interaction_m1_delta_r,
  'Model A.2.19'  = interaction_m2_delta_r,
  'Model A.2.20'  = interaction_m3_delta_r,
  'Model A.2.21' = interaction_m4_delta_r,
  'Model A.2.22' = interaction_m5_delta_r
)

interaction_delta_map_r <- list(
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

modelsummary(interaction_delta_r, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = interaction_delta_map_r,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')




