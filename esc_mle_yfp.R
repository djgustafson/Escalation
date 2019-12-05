## Clear memory
rm(list=ls(all=TRUE))

## Working directory
setwd("~/Documents/Grad/Escalation")

## Packages
library(lme4)
library(ggplot2)
library(grid)
library(gridExtra)
library(texreg)
library(MASS)
library(arm)
library(reshape)
library(plotROC)

## For marginal effects
source("me_plot.R")

## Read in data
mod_data <- readRDS("esc_mod_data_yfp.rds")

#### Models 1: Baseline, Rand., Nstar ####
## Baseline
baseline <- glm(viol ~ dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + dom_ind*unemployment, family="binomial", data=mod_data)

## Baseline with region
baseline_region <- glm(viol ~ dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + africa + dom_ind*unemployment, family="binomial", data=mod_data)

## Random Effects (separate)
rand_eff <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

## Random Effects (together)
rand_eff_cy <- glmer(viol~ dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + dom_ind*unemployment + (1|cyid), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

## nstar
nstar <- readRDS("esc_mod_data_nstar_yfp.rds")

nstar_rob <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + nstar + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=nstar, control = glmerControl(optimizer = "bobyqa"))

texreg(list(baseline, baseline_region, rand_eff, rand_eff_cy, nstar_rob), stars=.05, dcolumn=T)

#### Marginal Effects 1 ####
## Food
## Baseline
mef1 <- me_plot(model=baseline, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Baseline Region
mef2 <- me_plot(model=baseline_region, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Random Effects (separate)
mef3 <- me_plot(model=rand_eff, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Random Effects (together)
mef4 <- me_plot(model=rand_eff_cy, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## nstar
mef5 <- me_plot(model=nstar_rob, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Unemployment
## Baseline
meu1 <- me_plot(model=baseline, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Baseline Region
meu2 <- me_plot(model=baseline_region, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Random Effects (separate)
meu3 <- me_plot(model=rand_eff, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Random Effects (together)
meu4 <- me_plot(model=rand_eff_cy, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## nstar
meu5 <- me_plot(model=nstar_rob, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

pdf("yfp_me_1.pdf", width = 8, height = 8)
grid.arrange(mef1, meu1, mef2, meu2, mef3, meu3, mef4, meu4, mef5, meu5, ncol = 2)
dev.off()











#### Models 2: Diffusion ####
## Diffusion
prot_diff_1 <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_prot_1 + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

prot_diff_6 <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_prot_6 + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

prot_diff_12 <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_prot_12 + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

esc_diff_1 <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_esc_1 + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

esc_diff_6 <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_esc_6 + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

esc_diff_12 <- glmer(viol~dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + cont_esc_12 + dom_ind*unemployment +  (1|ccode) + (1|eyr), family=binomial, data=mod_data, control = glmerControl(optimizer = "bobyqa"))

texreg(list(prot_diff_1, prot_diff_6, prot_diff_12, esc_diff_1, esc_diff_6, esc_diff_12), stars=.05, dcolumn=T)

#### Marginal Effects 2 ####
## Food
## Protest diffusion 1
mef6 <- me_plot(model=prot_diff_1, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Protest diffusion 6
mef7 <- me_plot(model=prot_diff_6, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Protest diffusion 12
mef8 <- me_plot(model=prot_diff_12, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 1
mef9 <- me_plot(model=esc_diff_1, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 6
mef10 <- me_plot(model=esc_diff_6, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 12
mef11 <- me_plot(model=esc_diff_12, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

## Unemployment
## Protest diffusion 1
meu6 <- me_plot(model=prot_diff_1, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Protest diffusion 6
meu7 <- me_plot(model=prot_diff_6, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Protest diffusion 12
meu8 <- me_plot(model=prot_diff_12, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 1
meu9 <- me_plot(model=esc_diff_1, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 6
meu10 <- me_plot(model=esc_diff_6, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

## Escalation diffusion 12
meu11 <- me_plot(model=esc_diff_12, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

pdf("yfp_me_diff.pdf", width = 8, height = 8)
grid.arrange(mef6, meu6, mef7, meu7, mef8, meu8, mef9, meu9, mef10, meu10, ncol = 2)
dev.off()









#### Fixed Effects Models ####
## Country and year fixed effects
fix_eff <- glm(viol ~ dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + dom_ind*unemployment + as.factor(ccode) + as.factor(eyr), family="binomial", data=mod_data)

## Country fixed effects
fix_eff_c <- glm(viol ~ dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + dom_ind*unemployment + as.factor(ccode), family="binomial", data=mod_data)

## Year fixed effects
fix_eff_y <- glm(viol ~ dom_ind + unemployment + org_dem + rep_prev + rural_threat + democracy + log_gdp_pc + gdp_growth + lexclpop + dom_ind*unemployment + as.factor(eyr), family="binomial", data=mod_data)

texreg(list(fix_eff, fix_eff_c, fix_eff_y),stars=.05, dcolumn=T)

#### Fixed Effects Marginal Effects ####
mef12 <- me_plot(model=fix_eff, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

mef13 <- me_plot(model=fix_eff_c, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

mef14 <- me_plot(model=fix_eff_y, var1 = "dom_ind", var2 = "unemployment", name1 = "Relative Food Price", name2 = "Unemployment Rate", text_size = 6)

meu12 <- me_plot(model=fix_eff, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

meu13 <- me_plot(model=fix_eff_c, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

meu14 <- me_plot(model=fix_eff_y, var2 = "dom_ind", var1 = "unemployment", name2 = "Relative Food Price", name1 = "Unemployment Rate", text_size = 6)

pdf("yfp_me_fix.pdf", width = 6, height = 6)
grid.arrange(mef12, meu12, mef13, meu13, mef14, meu14, ncol = 2)
dev.off()




#### STOP HERE ####