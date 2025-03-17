library(fixest)
library(modelsummary)
library(sandwich)
library(tidyverse)

load('data/base.RData')

base <- base %>% 
  mutate(delta_fbic = fbic - lag(fbic, n = 3))

# Simple -----------------------------------------------------------------------

fixest_m1 <- feols(freedom ~ fbic | iso3c + year, data = base, cluster = 'iso3c')

fixest_m2 <- feols(freedom ~ fbic + gdppc_log | iso3c + year, data = base, cluster = 'iso3c')

fixest_m3 <- feols(freedom ~ fbic + gdppc_log + rents | iso3c + year, data = base, cluster = 'iso3c')

fixest_m4 <- feols(freedom ~ fbic + gdppc_log + rents + oda | iso3c + year, data = base, cluster = 'iso3c')

fixest_m5 <- feols(freedom ~ fbic + gdppc_log + rents + oda + west_2 | iso3c + year, data = base, cluster = 'iso3c')

fixest_m6 <- feols(freedom ~ fbic + gdppc_log + rents + oda + west_2 + regime | iso3c + year, data = base, cluster = 'iso3c')


fixest_models <- list(
  'm1' = fixest_m1,
  'm2' = fixest_m2,
  'm3' = fixest_m3,
  'm4' = fixest_m4,
  'm5' = fixest_m5,
  'm6' = fixest_m6
)

modelsummary(fixest_models, stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))

# The high R squared is a result of the fixed effects, where I control for many 

# Simple delta_fbic ------------------------------------------------------------

fixest_m1_delta <- feols(freedom ~ delta_fbic | iso3c + year, data = base, cluster = 'iso3c')

fixest_m2_delta <- feols(freedom ~ delta_fbic + gdppc_log | iso3c + year, data = base, cluster = 'iso3c')

fixest_m3_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents | iso3c + year, data = base, cluster = 'iso3c')

fixest_m4_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda | iso3c + year, data = base, cluster = 'iso3c')

fixest_m5_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda + west_2 | iso3c + year, data = base, cluster = 'iso3c')

fixest_m6_delta <- feols(freedom ~ delta_fbic + gdppc_log + rents + oda + west_2 + regime | iso3c + year, data = base, cluster = 'iso3c')


fixest_models_delta <- list(
  'm1' = fixest_m1_delta,
  'm2' = fixest_m2_delta,
  'm3' = fixest_m3_delta,
  'm4' = fixest_m4_delta,
  'm5' = fixest_m5_delta,
  'm6' = fixest_m6_delta
)

modelsummary(fixest_models_delta, stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))

# Lag 1 ------------------------------------------------------------------------

fixest_m1_lag_1 <- feols(freedom ~ l(fbic, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m2_lag_1 <- feols(freedom ~ l(fbic, 1) + l(west_2, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m3_lag_1 <- feols(freedom ~ l(fbic, 1) + l(west_2, 1) + l(gdppc_log, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m4_lag_1 <- feols(freedom ~ l(fbic, 1) + l(west_2, 1) + l(gdppc_log, 1) + l(rents, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m5_lag_1 <- feols(freedom ~ l(fbic, 1)  + l(west_2, 1) + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m6_lag_1 <- feols(freedom ~ l(fbic, 1)  + l(west_2, 1) + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) + l(regime, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_models_lag_1 <- list(
  'm1' = fixest_m1_lag_1,
  'm2' = fixest_m2_lag_1,
  'm3' = fixest_m3_lag_1,
  'm4' = fixest_m4_lag_1,
  'm5' = fixest_m5_lag_1,
  'm6' = fixest_m6_lag_1
)

modelsummary(fixest_models_lag_1, stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))

# Lag 2 ------------------------------------------------------------------------

fixest_m1_lag_2 <- feols(freedom ~ l(fbic, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m2_lag_2 <- feols(freedom ~ l(fbic, 2) + l(west_2, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m3_lag_2 <- feols(freedom ~ l(fbic, 2) + l(west_2, 2) + l(gdppc_log, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m4_lag_2 <- feols(freedom ~ l(fbic, 2) + l(west_2, 2) + l(gdppc_log, 2) + l(rents, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m5_lag_2 <- feols(freedom ~ l(fbic, 2) + l(west_2, 2) + l(gdppc_log, 2) + l(rents, 2) + l(oda, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m6_lag_2 <- feols(freedom ~ l(fbic, 2) + l(west_2, 2) + l(gdppc_log, 2) + l(rents, 2) + l(oda, 2) + l(regime, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_models_lag_2 <- list(
  'm1' = fixest_m1_lag_2,
  'm2' = fixest_m2_lag_2,
  'm3' = fixest_m3_lag_2,
  'm4' = fixest_m4_lag_2,
  'm5' = fixest_m5_lag_2,
  'm6' = fixest_m6_lag_2
)

modelsummary(fixest_models_lag_2, stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))

# Lag 3 ------------------------------------------------------------------------

fixest_m1_lag_3 <- feols(freedom ~ l(fbic, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m2_lag_3 <- feols(freedom ~ l(fbic, 3) + l(west_2, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m3_lag_3 <- feols(freedom ~ l(fbic, 3) + l(west_2, 3) + l(gdppc_log, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m4_lag_3 <- feols(freedom ~ l(fbic, 3) + l(west_2, 3) + l(gdppc_log, 3) + l(rents, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m5_lag_3 <- feols(freedom ~ l(fbic, 3) + l(west_2, 3) + l(gdppc_log, 3) + l(rents, 3) + l(oda, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m6_lag_3 <- feols(freedom ~ l(fbic, 3) + l(west_2, 3) + l(gdppc_log, 3) + l(rents, 3) + l(oda, 3) + l(regime, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_models_lag_3 <- list(
  'm1' = fixest_m1_lag_3,
  'm2' = fixest_m2_lag_3,
  'm3' = fixest_m3_lag_3,
  'm4' = fixest_m4_lag_3,
  'm5' = fixest_m5_lag_3,
  'm6' = fixest_m6_lag_3
)

modelsummary(fixest_models_lag_3, stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))

# Lag 4 ------------------------------------------------------------------------

fixest_m1_lag_4 <- feols(freedom ~ l(fbic, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m2_lag_4 <- feols(freedom ~ l(fbic, 4) + l(west_2, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m3_lag_4 <- feols(freedom ~ l(fbic, 4) + l(west_2, 4) + l(gdppc_log, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m4_lag_4 <- feols(freedom ~ l(fbic, 4) + l(west_2, 4) + l(gdppc_log, 4) + l(rents, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m5_lag_4 <- feols(freedom ~ l(fbic, 4) + l(west_2, 4) + l(gdppc_log, 4) + l(rents, 4) + l(oda, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m6_lag_4 <- feols(freedom ~ l(fbic, 4) + l(west_2, 4) + l(gdppc_log, 4) + l(rents, 4) + l(oda, 4) + l(regime, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_models_lag_4 <- list(
  'm1' = fixest_m1_lag_4,
  'm2' = fixest_m2_lag_4,
  'm3' = fixest_m3_lag_4,
  'm4' = fixest_m4_lag_4,
  'm5' = fixest_m5_lag_4,
  'm6' = fixest_m6_lag_4
)

modelsummary(fixest_models_lag_4, stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))

# Lag 5 ------------------------------------------------------------------------

fixest_m1_lag_5 <- feols(freedom ~ l(fbic, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m2_lag_5 <- feols(freedom ~ l(fbic, 5) + l(west_2, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m3_lag_5 <- feols(freedom ~ l(fbic, 5) + l(west_2, 5) + l(gdppc_log, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m4_lag_5 <- feols(freedom ~ l(fbic, 5) + l(west_2, 5) + l(gdppc_log, 5) + l(rents, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m5_lag_5 <- feols(freedom ~ l(fbic, 5) + l(west_2, 5) + l(gdppc_log, 5) + l(rents, 5) + l(oda, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_m6_lag_5 <- feols(freedom ~ l(fbic, 5) + l(west_2, 5) + l(gdppc_log, 5) + l(rents, 5) + l(oda, 5) + l(regime, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_models_lag_5 <- list(
  'm1' = fixest_m1_lag_5,
  'm2' = fixest_m2_lag_5,
  'm3' = fixest_m3_lag_5,
  'm4' = fixest_m4_lag_5,
  'm5' = fixest_m5_lag_5,
  'm6' = fixest_m6_lag_5
)

modelsummary(fixest_models_lag_5, stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001))

# Test -------------------------------------------------------------------------

## Lag 3 -----------------------------------------------------------------------

fixest_int_lag_0 <- feols(freedom ~ fbic*factor(regime) + west_2 + gdppc_log + rents + oda | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_1 <- feols(freedom ~ l(fbic, 1)*factor(l(regime, 1)) + l(west_2, 1) + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_2 <- feols(freedom ~ l(fbic, 2)*factor(l(regime, 2)) + l(west_2, 2) + l(gdppc_log, 2) + l(rents, 2) + l(oda, 2) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_3 <- feols(freedom ~ l(fbic, 3)*factor(l(regime, 3)) + l(west_2, 3) + l(gdppc_log, 3) + l(rents, 3) + l(oda, 3) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_4 <- feols(freedom ~ l(fbic, 4)*factor(l(regime, 4)) + l(west_2, 4) + l(gdppc_log, 4) + l(rents, 4) + l(oda, 4) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

fixest_int_lag_5 <- feols(freedom ~ l(fbic, 5)*factor(l(regime, 5)) + l(west_2, 5) + l(gdppc_log, 5) + l(rents, 5) + l(oda, 5) | iso3c + year, data = base, cluster = 'iso3c', panel.id = ~iso3c+year)

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
  'west_2' = 'west',
  'l(west_2, 1)' = 'west',
  'l(west_2, 2)' = 'west',
  'l(west_2, 3)' = 'west',
  'l(west_2, 4)' = 'west',
  'l(west_2, 5)' = 'west',
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
