# CONFIG -----------------------------------------------------------------------

library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

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

# HYPOTHESIS 1 -----------------------------------------------------------------

## Delta 1-5 -------------------------------------------------------------------

robust_m6_delta_1 <- feols(f(freedom, 1) ~ delta_fbic_1 + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_2 <- feols(f(freedom, 1) ~ delta_fbic_2 + gdppc_log + rents + oda + west_2_fbic + factor(regime) | 
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_3 <- feols(f(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base,
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_4 <- feols(f(freedom, 1) ~ delta_fbic_4 + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m6_delta_5 <- feols(f(freedom, 1) ~ delta_fbic_5 + gdppc_log + rents + oda + west_2_fbic + factor(regime) |
                             country + year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)


robust_models_delta <- list(
  'Model A.3.1' = robust_m6_delta_1,
  'Model A.3.2' = robust_m6_delta_2,
  'Model A.3.3' = robust_m6_delta_3,
  'Model A.3.4' = robust_m6_delta_4,
  'Model A.3.5' = robust_m6_delta_5
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

modelsummary(robust_models_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001),
             coef_map = robust_map_delta,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# HYPOTHESIS 2 -----------------------------------------------------------------

## Delta 1-5 interaction -------------------------------------------------------

robust_m5_delta_1 <- feols(f(freedom, 1) ~ delta_fbic_1*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_2 <- feols(f(freedom, 1) ~ delta_fbic_2*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_3 <- feols(f(freedom, 1) ~ delta_fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_4 <- feols(f(freedom, 1) ~ delta_fbic_4*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_m5_delta_5 <- feols(f(freedom, 1) ~ delta_fbic_5*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                             country+ year, 
                           data     = base, 
                           cluster  = 'country', 
                           panel.id = ~country+year)

robust_interaction_delta <- list(
  'Model A.3.6'  = robust_m5_delta_1,
  'Model A.3.7'  = robust_m5_delta_2,
  'Model A.3.8'  = robust_m5_delta_3,
  'Model A.3.9'  = robust_m5_delta_4,
  'Model A.3.10' = robust_m5_delta_5
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

modelsummary(robust_interaction_delta, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = robust_interaction_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')