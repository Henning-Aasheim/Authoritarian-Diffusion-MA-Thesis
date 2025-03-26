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

# Simple -----------------------------------------------------------------------

fixest_m1 <- feols(l(freedom, 1) ~ fbic | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m2 <- feols(l(freedom, 1) ~ fbic + gdppc_log | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m3 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m4 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m5 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic | country+ year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m6 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | country + year, data = base, cluster = 'country', panel.id = ~country+year)


fixest_models <- list(
  'Model 1' = fixest_m1,
  'Model 2' = fixest_m2,
  'Model 3' = fixest_m3,
  'Model 4' = fixest_m4,
  'Model 5' = fixest_m5,
  'Model 6' = fixest_m6
)

h1_simple_map <- list(
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
             coef_map = h1_simple_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 'adj.r.squared', 'r2.within.adjusted'))

# The high R squared is a result of the fixed effects, where I control for many 

# Simple delta_fbic ------------------------------------------------------------

fixest_m1_delta <- feols(l(freedom, 1) ~ delta_fbic | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m2_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m3_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m4_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m5_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m6_delta <- feols(l(freedom, 1) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | country + year, data = base, cluster = 'country', panel.id = ~country+year)


fixest_models_delta <- list(
  'Model 7' = fixest_m1_delta,
  'Model 8' = fixest_m2_delta,
  'Model 9' = fixest_m3_delta,
  'Model 10' = fixest_m4_delta,
  'Model 11' = fixest_m5_delta,
  'Model 12' = fixest_m6_delta
)

h1_delta_map <- list(
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
             coef_map = h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 'adj.r.squared', 'r2.within.adjusted'))

# lagged delta_fbic ------------------------------------------------------------

fixest_m6_delta_2 <- feols(l(freedom, 2) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m6_delta_3 <- feols(l(freedom, 3) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m6_delta_4 <- feols(l(freedom, 4) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m6_delta_5 <- feols(l(freedom, 5) ~ delta_fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | country + year, data = base, cluster = 'country', panel.id = ~country+year)


fixest_models_delta <- list(
  'Model 12' = fixest_m6_delta,
  'Model 13' = fixest_m6_delta_2,
  'Model 14' = fixest_m6_delta_3,
  'Model 15' = fixest_m6_delta_4,
  'Model 16' = fixest_m6_delta_5
)

h1_delta_map <- list(
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
             coef_map = h1_delta_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 'adj.r.squared', 'r2.within.adjusted'))



# Test -------------------------------------------------------------------------

# Simple -----------------------------------------------------------------------

fixest_m1 <- feols(l(freedom, 1) ~ fbic | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m2 <- feols(l(freedom, 1) ~ fbic + gdppc_log | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m3 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m4 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda | country + year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m5 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic | country+ year, data = base, cluster = 'country', panel.id = ~country+year)

fixest_m6 <- feols(l(freedom, 1) ~ fbic + gdppc_log + rents + oda + west_2_fbic + factor(regime) | country + year, data = base, cluster = 'country', panel.id = ~country+year)


fixest_models <- list(
  'Model 1' = fixest_m1,
  'Model 2' = fixest_m2,
  'Model 3' = fixest_m3,
  'Model 4' = fixest_m4,
  'Model 5' = fixest_m5,
  'Model 6' = fixest_m6
)

h1_simple_map <- list(
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
             coef_map = h1_simple_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 'adj.r.squared', 'r2.within.adjusted'))

## Lag 3 -----------------------------------------------------------------------

fixest_int_lag_0 <- feols(freedom ~ fbic*factor(regime) + west_2_fbic + gdppc_log + rents + oda | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_1 <- feols(freedom ~ l(fbic, 1)*factor(l(regime, 1)) + l(west_2_fbic, 1) + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_2 <- feols(freedom ~ l(fbic, 2)*factor(l(regime, 2)) + l(west_2_fbic, 2) + l(gdppc_log, 2) + l(rents, 2) + l(oda, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_3 <- feols(freedom ~ l(fbic, 3)*factor(l(regime, 3)) + l(west_2_fbic, 3) + l(gdppc_log, 3) + l(rents, 3) + l(oda, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_4 <- feols(freedom ~ l(fbic, 4)*factor(l(regime, 4)) + l(west_2_fbic, 4) + l(gdppc_log, 4) + l(rents, 4) + l(oda, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_5 <- feols(freedom ~ l(fbic, 5)*factor(l(regime, 5)) + l(west_2_fbic, 5) + l(gdppc_log, 5) + l(rents, 5) + l(oda, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_models_int_lag <- list(
  'No lag' = fixest_int_lag_0,
  '1 year lag' = fixest_int_lag_1,
  '2 year lag' = fixest_int_lag_2,
  '3 year lag' = fixest_int_lag_3,
  '4 year lag' = fixest_int_lag_4,
  '5 year lag' = fixest_int_lag_5
)

fixest_int_map <- list(
  'fbic'       = 'fbic',
  'l(fbic, 1)' = 'fbic',
  'l(fbic, 2)' = 'fbic',
  'l(fbic, 3)' = 'fbic',
  'l(fbic, 4)' = 'fbic',
  'l(fbic, 5)' = 'fbic',
  'factor(regime)1' = 'R1',
  'factor(l(regime, 1))1' = 'R1',
  'factor(l(regime, 2))1' = 'R1',
  'factor(l(regime, 3))1' = 'R1',
  'factor(l(regime, 4))1' = 'R1',
  'factor(l(regime, 5))1' = 'R1',
  'factor(regime)2' = 'R2',
  'factor(l(regime, 1))2' = 'R2',
  'factor(l(regime, 2))2' = 'R2',
  'factor(l(regime, 3))2' = 'R2',
  'factor(l(regime, 4))2' = 'R2',
  'factor(l(regime, 5))2' = 'R2',
  'factor(regime)3' = 'R3',
  'factor(l(regime, 1))3' = 'R3',
  'factor(l(regime, 2))3' = 'R3',
  'factor(l(regime, 3))3' = 'R3',
  'factor(l(regime, 4))3' = 'R3',
  'factor(l(regime, 5))3' = 'R3',
  'fbic:factor(regime)1' = 'fbic:R1',
  'l(fbic, 1):factor(l(regime, 1))1' = 'fbic:R1',
  'l(fbic, 2):factor(l(regime, 2))1' = 'fbic:R1',
  'l(fbic, 3):factor(l(regime, 3))1' = 'fbic:R1',
  'l(fbic, 4):factor(l(regime, 4))1' = 'fbic:R1',
  'l(fbic, 5):factor(l(regime, 5))1' = 'fbic:R1',
  'fbic:factor(regime)2' = 'fbic:R2',
  'l(fbic, 1):factor(l(regime, 1))2' = 'fbic:R2',
  'l(fbic, 2):factor(l(regime, 2))2' = 'fbic:R2',
  'l(fbic, 3):factor(l(regime, 3))2' = 'fbic:R2',
  'l(fbic, 4):factor(l(regime, 4))2' = 'fbic:R2',
  'l(fbic, 5):factor(l(regime, 5))2' = 'fbic:R2',
  'fbic:factor(regime)3' = 'fbic:R3',
  'l(fbic, 1):factor(l(regime, 1))3' = 'fbic:R3',
  'l(fbic, 2):factor(l(regime, 2))3' = 'fbic:R3',
  'l(fbic, 3):factor(l(regime, 3))3' = 'fbic:R3',
  'l(fbic, 4):factor(l(regime, 4))3' = 'fbic:R3',
  'l(fbic, 5):factor(l(regime, 5))3' = 'fbic:R3',
  'west_2_fbic' = 'west',
  'l(west_2_fbic, 1)' = 'west',
  'l(west_2_fbic, 2)' = 'west',
  'l(west_2_fbic, 3)' = 'west',
  'l(west_2_fbic, 4)' = 'west',
  'l(west_2_fbic, 5)' = 'west',
  'gdppc_log' = 'gdppc',
  'l(gdppc_log, 1)' = 'gdppc',
  'l(gdppc_log, 2)' = 'gdppc',
  'l(gdppc_log, 3)' = 'gdppc',
  'l(gdppc_log, 4)' = 'gdppc',
  'l(gdppc_log, 5)' = 'gdppc',
  'rents' = 'rents',
  'l(rents, 1)' = 'rents',
  'l(rents, 2)' = 'rents',
  'l(rents, 3)' = 'rents',
  'l(rents, 4)' = 'rents',
  'l(rents, 5)' = 'rents',
  'oda' = 'oda',
  'l(oda, 1)' = 'oda',
  'l(oda, 2)' = 'oda',
  'l(oda, 3)' = 'oda',
  'l(oda, 4)' = 'oda',
  'l(oda, 5)' = 'oda'
)

modelsummary(fixest_models_int_lag, 
             coef_map = fixest_int_map,
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))
