library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

# HYPOTHESIS 1 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

fixest_h1_m1 <- feols(f(freedom, 1) ~ fbic | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m2 <- feols(f(freedom, 1) ~ fbic + gdppc_log | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m3 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents |
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m4 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m5 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic | 
                     country+ year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)

fixest_h1_m6 <- feols(f(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                     country + year, 
                   data     = base, 
                   cluster  = 'country', 
                   panel.id = ~country+year)


fixest_h1 <- list(
  'Model 1.1' = fixest_h1_m1,
  'Model 1.2' = fixest_h1_m2,
  'Model 1.3' = fixest_h1_m3,
  'Model 1.4' = fixest_h1_m4,
  'Model 1.5' = fixest_h1_m5,
  'Model 1.6' = fixest_h1_m6
)

fixest_h1_map <- list(
  'fbic'            = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(fixest_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_h1, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h1_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# The high R squared is a result of the fixed effects, where I control for many 


## Delta -----------------------------------------------------------------------

fixest_h1_m1_delta <- feols(f(freedom, 1) ~ delta_fbic | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_h1_m2_delta <- feols(f(freedom, 1) ~ delta_fbic + gdppc_log | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)

fixest_h1_m3_delta <- feols(f(freedom, 1) ~ delta_fbic + gdppc_log + rents | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_h1_m4_delta <- feols(f(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda | 
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_h1_m5_delta <- feols(f(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic |
                           country + year, 
                         data     = base, 
                         cluster  = 'country', 
                         panel.id = ~country+year)

fixest_h1_m6_delta <- feols(f(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                           country + year, 
                         data     = base, 
                         cluster  = 'country',
                         panel.id = ~country+year)


fixest_h1_delta <- list(
  'Model 1.7'  = fixest_h1_m1_delta,
  'Model 1.8'  = fixest_h1_m2_delta,
  'Model 1.9'  = fixest_h1_m3_delta,
  'Model 1.10' = fixest_h1_m4_delta,
  'Model 1.11' = fixest_h1_m5_delta,
  'Model 1.12' = fixest_h1_m6_delta
)

fixest_h1_delta_map <- list(
  'delta_fbic'      = 'Linkages to China',
  'gdppc_log'       = 'log(GDP per capita)',
  'rents'           = 'Resource rents',
  'oda'             = 'Aid',
  'west_2_fbic'     = 'Linkages (West)',
  'factor(regime)1' = 'Electoral autocracy',
  'factor(regime)2' = 'Electoral democracy',
  'factor(regime)3' = 'Liberal democracy'
)

modelsummary(fixest_h1_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = fixest_h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_h1_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = fixest_h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')




# HYPOTHESIS 2 -----------------------------------------------------------------

## Simple ----------------------------------------------------------------------

fixest_h2_m1 <- feols(f(freedom, 1) ~ fbic*factor(regime) | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m2 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log |
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m3 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m4 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda | 
                          country + year, 
                        data     = base,
                        cluster  = 'country',
                        panel.id = ~country+year)

fixest_h2_m5 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                          country+ year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2 <- list(
  'Model 2.1' = fixest_h2_m1,
  'Model 2.2' = fixest_h2_m2,
  'Model 2.3' = fixest_h2_m3,
  'Model 2.4' = fixest_h2_m4,
  'Model 2.5' = fixest_h2_m5
)

fixest_h2_map <- list(
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

modelsummary(fixest_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')



## Delta -----------------------------------------------------------------------

fixest_h2_m1_delta <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) | 
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

fixest_h2_m2_delta <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log |
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

fixest_h2_m3_delta <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents | 
                                country + year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

fixest_h2_m4_delta <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda | 
                                country + year, 
                              data     = base,
                              cluster  = 'country',
                              panel.id = ~country+year)

fixest_h2_m5_delta <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                                country+ year, 
                              data     = base, 
                              cluster  = 'country', 
                              panel.id = ~country+year)

fixest_h2_delta <- list(
  'Model 2.6'  = fixest_h2_m1_delta,
  'Model 2.7'  = fixest_h2_m2_delta,
  'Model 2.8'  = fixest_h2_m3_delta,
  'Model 2.9'  = fixest_h2_m4_delta,
  'Model 2.10' = fixest_h2_m5_delta
)

fixest_h2_delta_map <- list(
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

modelsummary(fixest_h2_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h2_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(fixest_h2_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = fixest_h2_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')



