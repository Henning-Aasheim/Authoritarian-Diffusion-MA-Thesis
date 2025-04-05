# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

# HYPOTHESIS 1 -----------------------------------------------------------------

## Lagged simple ---------------------------------------------------------------

lead_h1_m1 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

lead_h1_m2 <- feols(f(freedom, 2) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

lead_h1_m3 <- feols(f(freedom, 3) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

lead_h1_m4 <- feols(f(freedom, 4) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)

lead_h1_m5 <- feols(f(freedom, 5) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                         country + year, 
                       data     = base, 
                       cluster  = 'country', 
                       panel.id = ~country+year)


lead_h1 <- list(
  'Model A.1.1' = lead_h1_m1,
  'Model A.1.2' = lead_h1_m2,
  'Model A.1.3' = lead_h1_m3,
  'Model A.1.4' = lead_h1_m4,
  'Model A.1.5' = lead_h1_m5
)

lead_h1_map <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(lead_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = lead_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(lead_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = lead_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

## Lagged delta ----------------------------------------------------------------

lead_h1_m1_delta <- feols(f(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

lead_h1_m2_delta <- feols(f(freedom, 2) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

lead_h1_m3_delta <- feols(f(freedom, 3) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

lead_h1_m4_delta <- feols(f(freedom, 4) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

lead_h1_m5_delta <- feols(f(freedom, 5) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

lead_h1_delta <- list(
  'Model A.1.6'  = lead_h1_m1_delta,
  'Model A.1.7'  = lead_h1_m2_delta,
  'Model A.1.8'  = lead_h1_m3_delta,
  'Model A.1.9'  = lead_h1_m4_delta,
  'Model A.1.10' = lead_h1_m5_delta
)

lead_h1_delta_map <- list(
  'delta_fbic'      = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(lead_h1_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = lead_h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(lead_h1_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = lead_h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# HYPOTHESIS 2 -----------------------------------------------------------------

## Lagged simple ---------------------------------------------------------------

lead_h2_m1 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                          country+ year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

lead_h2_m2 <- feols(f(freedom, 2) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

lead_h2_m3 <- feols(f(freedom, 3) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

lead_h2_m4 <- feols(f(freedom, 4) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

lead_h2_m5 <- feols(f(freedom, 5) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                              country+ year, 
                            data     = base, 
                            cluster  = 'country', 
                            panel.id = ~country+year)

lead_h2 <- list(
  'Model A.1.11'  = lead_h2_m1,
  'Model A.1.12'  = lead_h2_m2,
  'Model A.1.13'  = lead_h2_m3,
  'Model A.1.14'  = lead_h2_m4,
  'Model A.1.15'  = lead_h2_m5
)

lead_h2_map <- list(
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

modelsummary(lead_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = lead_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(lead_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = lead_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

## Lagged delta --------------------------------------------------------


lead_h2_m1_delta <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                country+ year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

lead_h2_m2_delta <- feols(f(freedom, 2) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

lead_h2_m3_delta <- feols(f(freedom, 3) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

lead_h2_m4_delta <- feols(f(freedom, 4) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

lead_h2_m5_delta <- feols(f(freedom, 5) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                    country+ year, 
                                  data     = base, 
                                  cluster  = 'country', 
                                  panel.id = ~country+year)

lead_h2_delta <- list(
  'Model A.1.16' = lead_h2_m1_delta,
  'Model A.1.17' = lead_h2_m2_delta,
  'Model A.1.18' = lead_h2_m3_delta,
  'Model A.1.19' = lead_h2_m4_delta,
  'Model A.1.20' = lead_h2_m5_delta
)

lead_h2_delta_map <- list(
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

modelsummary(lead_h2_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = lead_h2_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(lead_h2_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = lead_h2_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')
