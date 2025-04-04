# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')




# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple without lag ---------------------------------------------------------

xlag_h1_m1 <- feols(freedom ~ fbic | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h1_m2 <- feols(freedom ~ fbic + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h1_m3 <- feols(freedom ~ fbic + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h1_m4 <- feols(freedom ~ fbic + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h1_m5 <- feols(freedom ~ fbic + gdppc_log + rents + oda + west_2_fbic | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h1_m6 <- feols(freedom ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


xlag_h1 <- list(
  'Model A.2.1' = xlag_h1_m1,
  'Model A.2.2' = xlag_h1_m2,
  'Model A.2.3' = xlag_h1_m3,
  'Model A.2.4' = xlag_h1_m4,
  'Model A.2.5' = xlag_h1_m5,
  'Model A.2.6' = xlag_h1_m6
)

xlag_h1_map <- list(
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

xlag_h1_m1_delta <- feols(freedom ~ delta_fbic | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

xlag_h1_m2_delta <- feols(freedom ~ delta_fbic + gdppc_log | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

xlag_h1_m3_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

xlag_h1_m4_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

xlag_h1_m5_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic |
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

xlag_h1_m6_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)


xlag_h1_delta <- list(
  'Model A.2.7'  = xlag_h1_m1_delta,
  'Model A.2.8'  = xlag_h1_m2_delta,
  'Model A.2.9'  = xlag_h1_m3_delta,
  'Model A.2.10' = xlag_h1_m4_delta,
  'Model A.2.11' = xlag_h1_m5_delta,
  'Model A.2.12' = xlag_h1_m6_delta
)

xlag_h1_delta_map <- list(
  'delta_fbic'      = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(xlag_h1_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = xlag_h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


# HYPOTHESIS 2 -----------------------------------------------------------------

## Simple without lag ---------------------------------------------------------

xlag_h2_m1 <- feols(freedom ~ fbic*factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h2_m2 <- feols(freedom ~ fbic*factor(regime) + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h2_m3 <- feols(freedom ~ fbic*factor(regime) + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h2_m4 <- feols(freedom ~ fbic*factor(regime) + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

xlag_h2_m5 <- feols(freedom ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


xlag_h2 <- list(
  'Model A.2.13' = xlag_h2_m1,
  'Model A.2.14' = xlag_h2_m2,
  'Model A.2.15' = xlag_h2_m3,
  'Model A.2.16' = xlag_h2_m4,
  'Model A.2.17' = xlag_h2_m5
)

xlag_h2_map <- list(
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

modelsummary(xlag_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = xlag_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')


## Delta without lag ----------------------------------------------------------

xlag_h2_m1_delta <- feols(freedom ~ delta_fbic*factor(regime) | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

xlag_h2_m2_delta <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

xlag_h2_m3_delta <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log + rents | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

xlag_h2_m4_delta <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log + rents + oda | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

xlag_h2_m5_delta <- feols(freedom ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic |
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)


xlag_h2_delta <- list(
  'Model A.2.18' = xlag_h2_m1_delta,
  'Model A.2.19' = xlag_h2_m2_delta,
  'Model A.2.20' = xlag_h2_m3_delta,
  'Model A.2.21' = xlag_h2_m4_delta,
  'Model A.2.22' = xlag_h2_m5_delta
)

xlag_h2_delta_map <- list(
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

modelsummary(xlag_h2_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = xlag_h2_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')




