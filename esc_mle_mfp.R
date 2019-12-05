## Clear memory
rm(list=ls(all=TRUE))

## Working directory
setwd("~/Documents/Grad/Escalation")

## Packages
library(lme4)
library(ggplot2)
library(gridExtra)
library(texreg)
library(MASS)
library(arm)
library(reshape)
library(plotROC)

## For marginal effects
source("me_plot.R")

## Read in data
mod_data <- readRDS("esc_mod_data.rds")

#### Models 1: Baseline, Rand., Nstar ####
## Baseline
baseline <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + food_lag*unemployment, family="binomial", data=mod_data)

## Baseline with region
baseline_region <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + africa + food_lag*unemployment, family="binomial", data=mod_data)

## Random Effects (separate)
rand_eff <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

## Random Effects (together)
rand_eff_cy <- glmer(viol~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + food_lag*unemployment + (1|cyid), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

## nstar
nstar <- readRDS("esc_mod_data_nstar.rds")

nstar_rob <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + nstar + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=nstar, control = glmerControl(optimizer = "bobyqa"))

texreg(list(baseline, baseline_region, rand_eff, rand_eff_cy, nstar_rob), stars=.05, dcolumn=T)




#### Marginal Effects 1 ####
## Food
## Baseline
mef1 <- me_plot(model=baseline, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Baseline Region
mef2 <- me_plot(model=baseline_region, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Random Effects (separate)
mef3 <- me_plot(model=rand_eff, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Random Effects (together)
mef4 <- me_plot(model=rand_eff_cy, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## nstar
mef5 <- me_plot(model=nstar_rob, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Unemployment
## Baseline
meu1 <- me_plot(model=baseline, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Baseline Region
meu2 <- me_plot(model=baseline_region, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Random Effects (separate)
meu3 <- me_plot(model=rand_eff, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Random Effects (together)
meu4 <- me_plot(model=rand_eff_cy, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## nstar
meu5 <- me_plot(model=nstar_rob, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

pdf("mfp_me_1.pdf", width = 8, height = 8)
grid.arrange(mef1, meu1, mef2, meu2, mef3, meu3, mef4, meu4, mef5, meu5, ncol = 2)
dev.off()













#### Models 2: Diffusion ####
## Diffusion
prot_diff_1 <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_prot_1 + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

prot_diff_6 <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_prot_6 + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

prot_diff_12 <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_prot_12 + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

esc_diff_1 <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_esc_1 + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

esc_diff_6 <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_esc_6 + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

esc_diff_12 <- glmer(viol~food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_esc_12 + food_lag*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

texreg(list(prot_diff_1, prot_diff_6, prot_diff_12, esc_diff_1, esc_diff_6, esc_diff_12), stars=.05, dcolumn=T)






#### Marginal Effects 2 ####
## Food
## Protest diffusion 1
mef6 <- me_plot(model=prot_diff_1, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Protest diffusion 6
mef7 <- me_plot(model=prot_diff_6, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Protest diffusion 12
mef8 <- me_plot(model=prot_diff_12, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 1
mef9 <- me_plot(model=esc_diff_1, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 6
mef10 <- me_plot(model=esc_diff_6, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 12
mef11 <- me_plot(model=esc_diff_12, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

## Unemployment
## Protest diffusion 1
meu6 <- me_plot(model=prot_diff_1, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Protest diffusion 6
meu7 <- me_plot(model=prot_diff_6, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Protest diffusion 12
meu8 <- me_plot(model=prot_diff_12, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 1
meu9 <- me_plot(model=esc_diff_1, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 6
meu10 <- me_plot(model=esc_diff_6, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 12
meu11 <- me_plot(model=esc_diff_12, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

pdf("mfp_me_diff.pdf", width = 8, height = 8)
grid.arrange(mef6, meu6, mef7, meu7, mef8, meu8, mef9, meu9, mef10, meu10, ncol = 2)
dev.off()
















#### Models 3: Fixed Effects ####
## Country and year fixed effects
fix_eff <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + food_lag*unemployment + as.factor(ccode) + as.factor(eyr), family="binomial", data=mod_data)

## Country fixed effects
fix_eff_c <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + food_lag*unemployment + as.factor(ccode), family="binomial", data=mod_data)

## Year fixed effects
fix_eff_y <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + food_lag*unemployment + as.factor(eyr), family="binomial", data=mod_data)

texreg(list(fix_eff, fix_eff_c, fix_eff_y),stars=.05, dcolumn=T)

#### Fixed Effects Marginal Effects ####
mef12 <- me_plot(model=fix_eff, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

mef13 <- me_plot(model=fix_eff_c, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

mef14 <- me_plot(model=fix_eff_y, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

meu12 <- me_plot(model=fix_eff, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

meu13 <- me_plot(model=fix_eff_c, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

meu14 <- me_plot(model=fix_eff_y, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

pdf("mfp_me_fix.pdf", width = 6, height = 6)
grid.arrange(mef12, meu12, mef13, meu13, mef14, meu14, ncol = 2)
dev.off()









#### Models 4: issues, strikes, npart, lagged escalation ####
iss <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + iss_econ + iss_food + food_lag*unemployment, family="binomial", data=mod_data)

meissf <- me_plot(model=iss, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

meissu <- me_plot(model=iss, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

mod_data_strikes <- readRDS("esc_mod_data_strikes.rds")

strikes <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + food_lag*unemployment, family="binomial", data=mod_data_strikes)

mestrf <- me_plot(model=strikes, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

mestru <- me_plot(model=strikes, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

mod_data_npart <- readRDS("esc_mod_data_npart.rds")

npart <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + as.factor(npart) + food_lag*unemployment, family="binomial", data=mod_data_npart)

menpf <- me_plot(model=npart, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

menpu <- me_plot(model=npart, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

lagesc <- glm(viol ~ food_lag + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + esc_lag + food_lag*unemployment, family="binomial", data=mod_data)

melef <- me_plot(model=lagesc, var1 = "food_lag", var2 = "unemployment", name1 = "Food Price Change", name2 = "Unemployment Rate", text_size = 6)

meleu <- me_plot(model=lagesc, var2 = "food_lag", var1 = "unemployment", name2 = "Food Price Change", name1 = "Unemployment Rate", text_size = 6)

texreg(list(iss, strikes, npart, lagesc),stars=.05, dcolumn=T)

pdf("mfp_me_4.pdf", width = 7, height = 7)
grid.arrange(meissf, meissu, mestrf, mestru, menpf, menpu, melef, meleu, ncol = 2)
dev.off()



#### Out-of-Sample Performance ####
## Baseline
set.seed(26)

nfolds <- 10

folds <- caret::createFolds(mod_data$viol, k = nfolds)

rocs <- list()

aucs <- list()

preds <- list()

test_dfs <- list()

for(i in 1:nfolds){
  train <- mod_data[-folds[[i]],]
  test_dfs[[i]] <- mod_data[folds[[i]],]
  
  train_fit <- glm(formula(baseline), family = "binomial", data = train)
  
  preds[[i]] <- predict(train_fit, newdata = test_dfs[[i]], types = c("response"))
  
  rocs[[i]] <- ggplot()+
    geom_roc(aes_string(m = preds[[i]], 
                        d = as.numeric(as.character(test_dfs[[i]]$viol))), n.cuts=0)+
    theme_bw()
  
  aucs[[i]] <- calc_auc(rocs[[i]])
}

aucs <- do.call(rbind, aucs)

mean_auc <- mean(aucs$AUC)

roc <- ggplot()+
  labs(x = "False Positive Fraction", y = "True Positive Fraction", 
       title = paste("Baseline: AUC = ", round(mean_auc, 3)))+
  geom_roc(aes(m = preds[[1]], d = as.numeric(as.character(test_dfs[[1]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[2]], d = as.numeric(as.character(test_dfs[[2]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[3]], d = as.numeric(as.character(test_dfs[[3]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[4]], d = as.numeric(as.character(test_dfs[[4]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[5]], d = as.numeric(as.character(test_dfs[[5]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[6]], d = as.numeric(as.character(test_dfs[[6]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[7]], d = as.numeric(as.character(test_dfs[[7]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[8]], d = as.numeric(as.character(test_dfs[[8]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[9]], d = as.numeric(as.character(test_dfs[[9]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds[[10]], d = as.numeric(as.character(test_dfs[[10]]$viol))), 
           n.cuts=0)+
  geom_abline(aes(intercept = 0, slope = 1))+
  theme_bw()
  






## Fixed
rocs_f <- list()

aucs_f <- list()

preds_f <- list()

test_dfs_f <- list()

for(i in 1:nfolds){
  train <- mod_data[-folds[[i]],]
  test_dfs_f[[i]] <- mod_data[folds[[i]],]
  
  train_fit <- glm(formula(fix_eff), family = "binomial", data = train)
  
  preds_f[[i]] <- predict(train_fit, newdata = test_dfs_f[[i]], type = c("response"))
  
  rocs_f[[i]] <- ggplot()+
    geom_roc(aes_string(m = preds_f[[i]], 
                        d = as.numeric(as.character(test_dfs_f[[i]]$viol))), n.cuts=0)+
    theme_bw()
  
  aucs_f[[i]] <- calc_auc(rocs_f[[i]])
}

aucs_f <- do.call(rbind, aucs_f)

mean_auc_f <- mean(aucs_f$AUC)

roc_f <- ggplot()+
  labs(x = "False Positive Fraction", y = "True Positive Fraction", 
       title = paste("Fixed: AUC = ", round(mean_auc_f, 3)))+
  geom_roc(aes(m = preds_f[[1]], 
               d = as.numeric(as.character(test_dfs_f[[1]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[2]], 
               d = as.numeric(as.character(test_dfs_f[[2]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[3]], 
               d = as.numeric(as.character(test_dfs_f[[3]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[4]], 
               d = as.numeric(as.character(test_dfs_f[[4]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[5]], 
               d = as.numeric(as.character(test_dfs_f[[5]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[6]], 
               d = as.numeric(as.character(test_dfs_f[[6]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[7]], 
               d = as.numeric(as.character(test_dfs_f[[7]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[8]], 
               d = as.numeric(as.character(test_dfs_f[[8]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[9]], 
               d = as.numeric(as.character(test_dfs_f[[9]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_f[[10]], 
               d = as.numeric(as.character(test_dfs_f[[10]]$viol))), n.cuts=0)+
  geom_abline(aes(intercept = 0, slope = 1))+
  theme_bw()

  
  

## Random
rocs_r <- list()

aucs_r <- list()

preds_r <- list()

test_dfs_r <- list()

for(i in 1:nfolds){
  train <- mod_data[-folds[[i]],]
  test_dfs_r[[i]] <- mod_data[folds[[i]],]
  
  train_fit <- glmer(formula(rand_eff), family = "binomial", data = train,
                     control = glmerControl(optimizer = "bobyqa"))
  
  preds_r[[i]] <- predict(train_fit, newdata = test_dfs_r[[i]], 
                          type = c("response"), allow.new.levels=T)
  
  rocs_r[[i]] <- ggplot()+
    geom_roc(aes_string(m = preds_r[[i]], 
                        d = as.numeric(as.character(test_dfs_r[[i]]$viol))), n.cuts=0)+
    theme_bw()
  
  aucs_r[[i]] <- calc_auc(rocs_r[[i]])
}

aucs_r <- do.call(rbind, aucs_r)

mean_auc_r <- mean(aucs_r$AUC)

roc_r <- ggplot()+
  labs(x = "False Positive Fraction", y = "True Positive Fraction", 
       title = paste("Random: AUC = ", round(mean_auc_r, 3)))+
  geom_roc(aes(m = preds_r[[1]], 
               d = as.numeric(as.character(test_dfs_r[[1]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[2]], 
               d = as.numeric(as.character(test_dfs_r[[2]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[3]], 
               d = as.numeric(as.character(test_dfs_r[[3]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[4]], 
               d = as.numeric(as.character(test_dfs_r[[4]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[5]], 
               d = as.numeric(as.character(test_dfs_r[[5]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[6]], 
               d = as.numeric(as.character(test_dfs_r[[6]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[7]], 
               d = as.numeric(as.character(test_dfs_r[[7]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[8]], 
               d = as.numeric(as.character(test_dfs_r[[8]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[9]], 
               d = as.numeric(as.character(test_dfs_r[[9]]$viol))), n.cuts=0)+
  geom_roc(aes(m = preds_r[[10]], 
               d = as.numeric(as.character(test_dfs_r[[10]]$viol))), n.cuts=0)+
  geom_abline(aes(intercept = 0, slope = 1))+
  theme_bw()









## Bayesian CV
rocs_b <- list()

aucs_b <- list()

probs_b <- list()

test_dfs_b <- list()

for(i in 1:nfolds){
  train <- mod_data[-folds[[i]],]
  test_dfs_b[[i]] <- mod_data[folds[[i]],]
  
  X <- model.matrix(~ train$food_lag + train$unemployment + 
                      train$food_lag*train$unemployment + train$org_dem +
                      train$rep_prev + train$rural_threat + train$democracy +
                      train$log_gdp_pc + train$gdp_growth + train$lexclpop)
  y <- train$viol
  n <- length(y)
  b <- ncol(X)
  j <- length(unique(train$ccode))
  k <- length(unique(train$eyr))
  cid <- train$cid
  yid <- train$yid
  
  Xtest <- model.matrix(~ test_dfs_f[[i]]$food_lag + test_dfs_f[[i]]$unemployment + 
                          test_dfs_f[[i]]$food_lag*test_dfs_f[[i]]$unemployment + 
                          test_dfs_f[[i]]$org_dem + test_dfs_f[[i]]$rep_prev + 
                          test_dfs_f[[i]]$rural_threat + test_dfs_f[[i]]$democracy +
                          test_dfs_f[[i]]$log_gdp_pc + test_dfs_f[[i]]$gdp_growth +
                          test_dfs_f[[i]]$lexclpop)
  
  ytest <- test_dfs_f[[i]]$viol
  ntest <- length(ytest)
  jtest <- length(unique(test_dfs_f[[i]]$ccode))
  ktest <- length(unique(test_dfs_f[[i]]$eyr))
  cidtest <- test_dfs_f[[i]]$cid
  yidtest <- test_dfs_f[[i]]$yid
  
  stan_data <- list(X = X, y = y, n = n, b = b, j = j, k = k, country = cid, year = yid,
                    Xtest = Xtest, ytest = ytest, ntest = ntest, jtest = jtest, 
                    ktest = ktest, countrytest = cidtest, yeartest = yidtest)
  
  ## Fit
  esc_stan <- stan(file="esc_ran_int_cv.stan", data=stan_data, 
                   chains = 4, iter = 5000, seed=26,
                   cores=parallel::detectCores())
  
  probs <- rstan::extract(esc_stan, c("ptest"))
  probs_b[[i]] <- probs[[1]]
  
  m = apply(probs_b[[i]], 2, mean)
  d = test_dfs_f[[i]]$viol
  
  rocs_b[[i]] <- ggplot()+
    geom_roc(aes_string(m = m, 
                        d = d), n.cuts=0)+
    theme_bw()
  
  aucs_b[[i]] <- calc_auc(rocs_b[[i]])
}

aucs_b <- do.call(rbind, aucs_b)

mean_auc_b <- mean(aucs_b$AUC)

roc_b <- ggplot()+
  labs(x = "False Positive Fraction", y = "True Positive Fraction", 
       title = paste("Bayesian: AUC = ", round(mean_auc_b, 3)))+
  geom_roc(aes(m = apply(probs_b[[1]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[1]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[2]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[2]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[3]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[3]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[4]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[4]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[5]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[5]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[6]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[6]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[7]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[7]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[8]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[8]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[9]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[9]]$viol))), n.cuts=0)+
  geom_roc(aes(m = apply(probs_b[[10]],2,mean), 
               d = as.numeric(as.character(test_dfs_b[[10]]$viol))), n.cuts=0)+
  geom_abline(aes(intercept = 0, slope = 1))+
  theme_bw()

pdf("roc_comparison.pdf", width = 8, height = 6)
grid.arrange(roc_b, roc_r, roc_f, roc, ncol=2)
dev.off()
