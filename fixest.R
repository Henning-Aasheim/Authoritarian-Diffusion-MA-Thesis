library(fixest)       # Runs fixed-effects
library(modelsummary) # Outputs regression tables
library(marginaleffects)
library(tidyverse)

# To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries 
# in `\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")

load('data/base.RData')

base_regime_binary <- base %>% 
  mutate(hybrid = ifelse(regime %in% c(1, 2), 1, 0))

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

## lagged controls -------------------------------------------------------------

fixest_h1_m7 <- feols(f(freedom, 1) ~ fbic | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

fixest_h1_m8 <- feols(f(freedom, 1) ~ fbic + l(gdppc_log, 1) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

fixest_h1_m9 <- feols(f(freedom, 1) ~ fbic + l(gdppc_log, 1) + l(rents, 1) |
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

fixest_h1_m10 <- feols(f(freedom, 1) ~ fbic + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

fixest_h1_m11 <- feols(f(freedom, 1) ~ fbic + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) + l(west_2_fbic, 1) | 
                        country+ year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

fixest_h1_m12 <- feols(f(freedom, 1) ~ fbic + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) + l(west_2_fbic, 1) + l(factor(regime), 1) | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)


fixest_h1_lc <- list(
  'Model 1.1' = fixest_h1_m7,
  'Model 1.2' = fixest_h1_m8,
  'Model 1.3' = fixest_h1_m9,
  'Model 1.4' = fixest_h1_m10,
  'Model 1.5' = fixest_h1_m11,
  'Model 1.6' = fixest_h1_m12
)

fixest_h1_lc_map <- list(
  'fbic'                  = 'Linkages to China',
  'l(gdppc_log, 1)'       = 'log(GDP per capita)',
  'l(rents, 1)'           = 'Resource rents',
  'l(oda, 1)'             = 'Aid',
  'l(west_2_fbic, 1)'     = 'Linkages (West)',
  'l(factor(regime), 1)1' = 'Electoral autocracy',
  'l(factor(regime), 1)2' = 'Electoral democracy',
  'l(factor(regime), 1)3' = 'Liberal democracy'
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
                        country + year, 
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

## Lagged controls -------------------------------------------------------------

fixest_h2_m6 <- feols(f(freedom, 1) ~ fbic*factor(regime) | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m7 <- feols(f(freedom, 1) ~ fbic*factor(regime) + l(gdppc_log, 1) | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m8 <- feols(f(freedom, 1) ~ fbic*factor(regime) + l(gdppc_log, 1) + l(rents, 1) | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2_m9 <- feols(f(freedom, 1) ~ fbic*factor(regime) + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) |  
                          country + year, 
                        data     = base,
                        cluster  = 'country',
                        panel.id = ~country+year)

fixest_h2_m10 <- feols(f(freedom, 1) ~ fbic*factor(regime) + l(gdppc_log, 1) + l(rents, 1) + l(oda, 1) + l(west_2_fbic, 1) | 
                          country + year, 
                        data     = base, 
                        cluster  = 'country', 
                        panel.id = ~country+year)

fixest_h2 <- list(
  'Model 2.1' = fixest_h2_m6,
  'Model 2.2' = fixest_h2_m7,
  'Model 2.3' = fixest_h2_m8,
  'Model 2.4' = fixest_h2_m9,
  'Model 2.5' = fixest_h2_m10
)

fixest_h2_map <- list(
  'fbic'                 = 'Linkages to China',
  'factor(regime)1'      = 'Electoral autocracy',
  'factor(regime)2'      = 'Electoral democracy',
  'factor(regime)3'      = 'Liberal democracy',
  'fbic:factor(regime)1' = 'China x El.Aut.',
  'fbic:factor(regime)2' = 'China x El.Dem.',
  'fbic:factor(regime)3' = 'China x Lib.Dem.',
  'l(gdppc_log, 1)'            = 'log(GDP per capita)',
  'l(rents, 1)'                = 'Resource rents',
  'l(oda, 1)'                  = 'Aid',
  'l(west_2_fbic, 1)'          = 'Linkages (West)'
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


## Interaction experiment ------------------------------------------------------

# Here I test different ways to use interaction with fixest. I think the safest
# option is to use the specification I am already using, as this corresponds to
# The results I get from using the standard notation.

base2 <- base %>% 
  mutate(regime = factor(regime))

base2 <- panel(base2, ~country+year)


fixest_h2_m1.1 <- feols(f(freedom, 1) ~ i(regime, fbic) | 
                        country + year, 
                      data     = base2, 
                      cluster  = 'country')

fixest_h2_m1.2 <- feols(f(freedom, 1) ~ fbic + i(regime, fbic, ref = 0) | 
                        country + year, 
                      data     = base2, 
                      cluster  = 'country')

fixest_h2_m1.3 <- feols(f(freedom, 1) ~ fbic + regime + i(regime, fbic, ref = 0) | # This is the same as using freedom ~ fbic*regime
                          country + year, 
                        data     = base2, 
                        cluster  = 'country')

## Hybrid ----------------------------------------------------------------------

hybrid_h2_m1 <- feols(f(freedom, 1) ~ fbic*factor(hybrid) | 
                        country + year, 
                      data     = base_regime_binary, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

hybrid_h2_m2 <- feols(f(freedom, 1) ~ fbic*factor(hybrid) + gdppc_log |
                        country + year, 
                      data     = base_regime_binary, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

hybrid_h2_m3 <- feols(f(freedom, 1) ~ fbic*factor(hybrid) + gdppc_log + rents | 
                        country + year, 
                      data     = base_regime_binary, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

hybrid_h2_m4 <- feols(f(freedom, 1) ~ fbic*factor(hybrid) + gdppc_log + rents + oda | 
                        country + year, 
                      data     = base_regime_binary,
                      cluster  = 'country',
                      panel.id = ~country+year)

hybrid_h2_m5 <- feols(f(freedom, 1) ~ fbic*factor(hybrid) + gdppc_log + rents + oda + west_2_fbic | 
                        country + year, 
                      data     = base_regime_binary, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

hybrid_h2 <- list(
  'Model 2.1' = hybrid_h2_m1,
  'Model 2.2' = hybrid_h2_m2,
  'Model 2.3' = hybrid_h2_m3,
  'Model 2.4' = hybrid_h2_m4,
  'Model 2.5' = hybrid_h2_m5
)

hybrid_h2_map <- list(
  'fbic'                 = 'China x Consolidated',
  'factor(hybrid)1'      = 'Hybrid',
  'fbic:factor(hybrid)1' = 'China x Hybrid',
  'gdppc_log'            = 'log(GDP per capita)',
  'rents'                = 'Resource rents',
  'oda'                  = 'Aid',
  'west_2_fbic'          = 'Linkages (West)'
)

modelsummary(hybrid_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = hybrid_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'))

modelsummary(hybrid_h2, 
             stars = c("x" = .1, "*" = .05,"**" = .01, '***' = .001), 
             coef_map = hybrid_h2_map,
             gof_map = c('nobs', 'vcov.type', 'FE: country', 'FE: year', 
                         'adj.r.squared', 'r2.within.adjusted'),
             output = 'latex')

# RESIDUALS --------------------------------------------------------------------

## H1 --------------------------------------------------------------------------

plot(fitted(fixest_h1_m1), resid(fixest_h1_m1))

plot(fitted(fixest_h1_m2), resid(fixest_h1_m2))

plot(fitted(fixest_h1_m3), resid(fixest_h1_m3))

plot(fitted(fixest_h1_m4), resid(fixest_h1_m4))

plot(fitted(fixest_h1_m5), resid(fixest_h1_m5))

plot(fitted(fixest_h1_m6), resid(fixest_h1_m6))

## H2 --------------------------------------------------------------------------

plot(fitted(fixest_h2_m1), resid(fixest_h2_m1))

plot(fitted(fixest_h2_m2), resid(fixest_h2_m2))

plot(fitted(fixest_h2_m3), resid(fixest_h2_m3))

plot(fitted(fixest_h2_m4), resid(fixest_h2_m4))

plot(fitted(fixest_h2_m5), resid(fixest_h2_m5))

# OUTLIERS ---------------------------------------------------------------------

library(ggrepel)


base$residuals <- residuals(fixest_h2_m5, na.rm = F)
base$fit <- fitted(fixest_h2_m5, na.rm = F)

base %>% 
  ggplot(aes(x = fit, y = residuals)) +
  geom_point(alpha = .3) +
  geom_point(data = base %>% filter(residuals > .42 | residuals < -.45), 
             aes(colour = country), size = 3) +
  geom_text_repel(data = base %>% filter(residuals > .42 | residuals < -.45),
            aes(label = paste0(country, ' ',year), colour = country, family = 'serif'),
            show.legend = F, size = 5) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 1, .2)) +
  scale_y_continuous(breaks = seq(-.5, .5, .1)) +
  scale_colour_manual(values = c('#ff6e54', '#003f5c', '#ff9214', '#dd5182', '#955196', '#444e86')) +
  labs(x = 'Fitted Value',
       y = 'Residuals',
       colour = 'Country:') +
  theme_classic(base_family = 'serif') +
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = 'pt'))

if(!file.exists('illustrations/residuals.jpeg')){
  ggsave('illustrations/residuals.jpeg', units = 'px', width = 2700, height = 2000, dpi = 300)
}

perfect <- base %>% 
  mutate(freedom = lead(freedom, 1)) %>% 
  filter(residuals < .005 & residuals > -.005)

# PLOT -------------------------------------------------------------------------

## H2 --------------------------------------------------------------------------

library(broom)

a <- tidy(fixest_h2_m5_delta, conf.level = .95, conf.int = T)

a %>% 
  ggplot(aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2) +
  geom_vline(xintercept = 0) +
  theme_classic()

