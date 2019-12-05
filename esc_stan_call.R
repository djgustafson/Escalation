## Clear memory
rm(list=ls(all=TRUE))

## Working directory
setwd("~/Documents/Grad/Escalation")

library(rstan)

## Read in data
mod_data <- readRDS("esc_mod_data.rds")



#### Model ####

## Data
X <- model.matrix(~ mod_data$food_lag + mod_data$unemployment + 
                    mod_data$food_lag*mod_data$unemployment + mod_data$org_dem +
                    mod_data$rep_prev + mod_data$rural_threat + mod_data$democracy +
                    mod_data$log_gdp_pc + mod_data$gdp_growth + mod_data$lexclpop)
y <- mod_data$viol
n <- length(y)
b <- ncol(X)
j <- length(unique(mod_data$ccode))
k <- length(unique(mod_data$eyr))
cid <- mod_data$cid
yid <- mod_data$yid

stan_data <- list(X = X, y = y, n = n, b = b, j = j, k = k, country = cid, year = yid)

## Fit
esc_stan <- stan(file="esc_ran_int.stan", data=stan_data, 
                 chains = 4, iter = 10000, seed=26,
                 control = list(adapt_delta = .9), cores=parallel::detectCores())

## Check convergence
summary(summary(esc_stan)$summary[,"Rhat"])

## Pull out coefficients
samps <- rstan::extract(esc_stan, c("beta"))
samps <- samps[[1]]
samps <- as.data.frame(samps)
colnames(samps) <- c("intercept", "food_lag", "unemployment", "organized",
                     "repression", "rural", "democracy", "log_gdp_pc", "gdp_growth",
                     "log_excl_pop", "interaction")

## Reorder
samps <- samps[,rev(c(2,3,11,4,5,6,7,8,9,10,1))]

## Save Output
saveRDS(samps, "esc_bay_mfp.RDS")









#### Appendix ####
## Fit
esc_stan_alt <- stan(file="esc_ran_int_alt.stan", data=stan_data, 
                 chains = 4, iter = 10000, seed=26,
                 control = list(adapt_delta = .9), cores=parallel::detectCores())

## Check convergence
summary(summary(esc_stan_alt)$summary[,"Rhat"])

## Pull out coefficients
samps_alt <- rstan::extract(esc_stan_alt, c("beta"))
samps_alt <- samps_alt[[1]]
samps_alt <- as.data.frame(samps_alt)
colnames(samps_alt) <- c("intercept", "food_lag", "unemployment", "organized",
                     "repression", "rural", "democracy", "log_gdp_pc", "gdp_growth",
                     "log_excl_pop", "interaction")

## Reorder
samps_alt <- samps_alt[,rev(c(2,3,11,4,5,6,7,8,9,10,1))]

## Save Output
saveRDS(samps_alt, "esc_bay_mfp_alt.RDS")

