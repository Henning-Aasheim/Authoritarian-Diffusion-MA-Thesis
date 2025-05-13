# CONFIG -----------------------------------------------------------------------
library(fixest)
library(margins)
library(sandwich)
library(marginaleffects)

# MODEL ------------------------------------------------------------------------

fixest_h2_m5 <- feols(f(freedom, 1) ~ fbic*factor(regime) + gdppc_log + rents + oda + west_2_fbic | 
                        country + year, 
                      data     = base, 
                      cluster  = 'country', 
                      panel.id = ~country+year)

# MARGINAL EFFECTS -------------------------------------------------------------

beta_hat <- coef(fixest_h2_m5)
cov <- vcov(fixest_h2_m5)

z0 <- seq(0, 0, length.out = 1000)

dy.dx <- beta_hat['fbic'] + beta_hat['fbic']*z0
dy.dx <- beta_hat['fbic'] + beta_hat['fbic:factor(regime)1']*z1

meplot()

meplot <- function(model,var1,var2,ci=.95,
                   xlab=var2,ylab=paste("Marginal Effect of",var1),
                   main="Marginal Effect Plot",
                   me_lty=1,me_lwd=3,me_col="black",
                   ci_lty=1,ci_lwd=1,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black"){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[length(beta.hat)]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
       ylim=c(min(lwr),max(upr)),
       xlab = xlab,
       ylab = ylab,
       main = main)
  lines(z0, dy.dx, lwd = me_lwd, lty = me_lty, col = me_col)
  lines(z0, lwr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  lines(z0, upr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  abline(h=0,lty=yint_lty,lwd=yint_lwd,col=yint_col)
}

meplot(fixest_h2_m5, var1 = 'fbic', var2 = c('factor(regime)1'))

# MARGINALEFFECTS PACKAGE ------------------------------------------------------

plot_predictions(fixest_h2_m5, condition = c('freedom', 'fbic'))
