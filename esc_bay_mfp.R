## Clear memory
rm(list=ls(all=TRUE))

## Working directory
setwd("~/Documents/Grad/Escalation")

library(rstan)
library(ggridges)
library(ggplot2)
library(gridExtra)

## Read in data
samps <- readRDS("esc_bay_mfp.rds")
mod_data <- readRDS("esc_mod_data.rds")


#### Post-Estimation ####
## Titles for Beta plots
beta_titles <- c("Constant", "ln(% Excluded)", "GDP Growth", "ln(GDP)", 
                 "Democracy", "Rural", "Previous Repression", "Organized", 
                 "Interaction", "Unemployment", "Lagged Food Price Change")

## QI
point_ests <- apply(samps, 2, FUN="mean")
cred_lo <- apply(samps, 2, quantile, prob = .025)
cred_hi <- apply(samps, 2, quantile, prob = .975)
sds <- apply(samps, 2, FUN="sd")

bay_tab <- texreg::createTexreg(coef.names = rev(beta_titles),
                     coef = rev(point_ests),
                     ci.low = rev(cred_lo),
                     ci.up = rev(cred_hi))

texreg::texreg(bay_tab, dcolumn = T)






#### Ridge Plot ####
ridge_df <- reshape2::melt(samps)

esc_ridge <- ggplot()+
  labs(x="", y="")+
  geom_density_ridges(aes(x=ridge_df$value, y=ridge_df$variable))+
  geom_vline(aes(xintercept=0))+
  theme_bw()+
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),
        panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())+
  scale_y_discrete(labels=beta_titles)

pdf("esc_ridge_mfp.pdf", width=8, height=6)
esc_ridge
dev.off()

#### Specific Densities ####
## Sequence the number of samples
x <- seq(1:(nrow(samps)/4))

food_dens <- ggplot() +
  labs(x="", y="Density")+
  geom_density(aes_string(samps[x,11]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) + x,11]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) * 2 + x,11]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) * 3 + x,11]), fill="black", alpha=.3)+
  geom_vline(aes(xintercept=c(point_ests[11], cred_hi[11], cred_lo[11])), 
             linetype=c(2,1,1))+
  theme_bw() + 
  scale_y_continuous(breaks = c(), labels = c())+
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), 
        axis.title.y=element_text(size=18), panel.grid.minor.x=element_blank(), 
        panel.grid.minor.y=element_blank(),legend.position="bottom")

pdf("food_dens.pdf", width=8, height=6)
food_dens
dev.off()

unem_dens <- ggplot() +
  labs(x="", y="Density")+
  geom_density(aes_string(samps[x,10]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) + x,10]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) * 2 + x,10]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) * 3 + x,10]), fill="black", alpha=.3)+
  geom_vline(aes(xintercept=c(point_ests[10], cred_hi[10], cred_lo[10])), 
             linetype=c(2,1,1))+
  theme_bw() + 
  scale_y_continuous(breaks = c(), labels = c())+
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), 
        axis.title.y=element_text(size=18), panel.grid.minor.x=element_blank(), 
        panel.grid.minor.y=element_blank(),legend.position="bottom")

pdf("unem_dens.pdf", width=8, height=6)
unem_dens
dev.off()

org_dens <- ggplot() +
  labs(x="", y="Density")+
  geom_density(aes_string(samps[x,8]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) + x,8]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) * 2 + x,8]), fill="black", alpha=.3)+
  geom_density(aes_string(samps[max(x) * 3 + x,8]), fill="black", alpha=.3)+
  geom_vline(aes(xintercept=c(point_ests[8], cred_hi[8], cred_lo[8])), linetype=c(2,1,1))+
  theme_bw() + 
  scale_y_continuous(breaks = c(), labels = c())+
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), 
        axis.title.y=element_text(size=18), panel.grid.minor.x=element_blank(), 
        panel.grid.minor.y=element_blank(),legend.position="bottom")

pdf("org_dens.pdf", width=8, height=6)
org_dens
dev.off()




#### Marginal Effects ####
## Marginal Effect of Food Price
## Values of unemployment
seq_unem <- seq(min(mod_data$unemployment), max(mod_data$unemployment), 
                length.out = nrow(mod_data))

## Conditional effect of food across unemployment
food_sim <- matrix(rep(NA, nrow(samps)*length(seq_unem)), nrow = nrow(samps))

for(i in 1:length(seq_unem)){
  food_sim[, i] <- samps$food_lag + samps$interaction * seq_unem[i]
}

## Get mean and upper/lower ci
food_me <- apply(food_sim, 2, mean)
food_me_lo <- apply(food_sim, 2, function(x) quantile(x, probs = c(0.025)))
food_me_hi <- apply(food_sim, 2, function(x) quantile(x, probs = c(0.975)))

food_me_plot <- ggplot() +
  labs(x="Observed Values of Unemployment", y="Marginal Effect of Food Price Change") +
  geom_line(aes(seq_unem,food_me)) +
  geom_ribbon(aes(x=seq_unem, ymin=food_me_lo, ymax=food_me_hi), alpha=.5) +
  geom_rug(aes(mod_data$unemployment, food_me), sides="b") +
  theme_bw() + 
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), 
        axis.title.y=element_text(size=18), 
        plot.title=element_text(hjust=0.5, size=24),panel.grid.minor.x=element_blank(), 
        panel.grid.minor.y=element_blank(),legend.position="bottom")

pdf("food_me.pdf", width=8, height=6)
food_me_plot
dev.off()




## Marginal Effect of Unemployment
## Values of food price
seq_food<- seq(min(mod_data$food_lag, na.rm=T), max(mod_data$food_lag,na.rm=T), length.out = nrow(mod_data))

## Conditional effect of unemployment across food
unem_sim <- matrix(rep(NA, nrow(samps)*length(seq_food)), nrow = nrow(samps))

for(i in 1:length(seq_food)){
  unem_sim[, i] <- samps$unemployment + samps$interaction * seq_food[i]
}

## Get mean and upper/lower ci
unem_me <- apply(unem_sim, 2, mean)
unem_me_lo <- apply(unem_sim, 2, function(x) quantile(x, probs = c(0.025)))
unem_me_hi <- apply(unem_sim, 2, function(x) quantile(x, probs = c(0.975)))

unem_me_plot <- ggplot() +
  labs(x="Observed Values of Food Price Change", y="Marginal Effect of Unemployment") +
  geom_line(aes(seq_food,unem_me)) +
  geom_ribbon(aes(x=seq_food, ymin=unem_me_lo, ymax=unem_me_hi), alpha=.5) +
  geom_rug(aes(mod_data$food_lag, unem_me), sides="b") +
  theme_bw() + 
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), 
        axis.title.y=element_text(size=18), 
        plot.title=element_text(hjust=0.5, size=24),panel.grid.minor.x=element_blank(), 
        panel.grid.minor.y=element_blank(),legend.position="bottom")

pdf("unem_me.pdf", width=8, height=6)
unem_me_plot
dev.off()






#### Predicted Probabilities ####
## Food price (by unemployment)
## Unemployment at mean
## Generate dataframe with simulated values
viol_sim <- cbind(seq_food, 
                  rep(mean(mod_data$unemployment), length(seq_food)),
                  mean(mod_data$unemployment) * seq_food,
                  rep(median(mod_data$org_dem), length(seq_food)),
                  rep(median(mod_data$rep_prev), length(seq_food)),
                  rep(median(mod_data$rural_threat), length(seq_food)),
                  rep(median(mod_data$democracy), length(seq_food)),
                  rep(mean(mod_data$log_gdp_pc), length(seq_food)),
                  rep(mean(mod_data$gdp_growth), length(seq_food)),
                  rep(mean(mod_data$lexclpop), length(seq_food)),
                  1)
                  

## Create a matrix of just coefficients and intercept
bay_coef <- samps[,c(11:1)]

## Multiply X by the betas
xb <- t(viol_sim %*% t(bay_coef))

## Get CIs (for plotting)
viol_ci_food <- apply(xb, 2, quantile, probs=c(.025,.5, .975))

## Transform linear prediction to probability
viol_pp_food <- exp(viol_ci_food)/(1+exp(viol_ci_food))

## Unemployment at .05 quantile
## Generate dataframe with simulated values
viol_sim_2 <- cbind(seq_food, 
                    rep(quantile(mod_data$unemployment, probs = .05), length(seq_food)),
                    quantile(mod_data$unemployment, probs = .05) * seq_food,
                    rep(median(mod_data$org_dem), length(seq_food)),
                    rep(median(mod_data$rep_prev), length(seq_food)),
                    rep(median(mod_data$rural_threat), length(seq_food)),
                    rep(median(mod_data$democracy), length(seq_food)),
                    rep(mean(mod_data$log_gdp_pc), length(seq_food)),
                    rep(mean(mod_data$gdp_growth), length(seq_food)),
                    rep(mean(mod_data$lexclpop), length(seq_food)),
                    1)

## Multiply X by the betas
xb_2 <- t(viol_sim_2%*% t(bay_coef))

## Get CIs (for plotting)
viol_ci_food_2 <- apply(xb_2, 2, quantile, probs=c(.025,.5, .975))

## Transform linear prediction to probability
viol_pp_food_2 <- exp(viol_ci_food_2)/(1+exp(viol_ci_food_2))

## Unemployment at .95 quantile
## Generate dataframe with simulated values
viol_sim_3 <- cbind(seq_food, 
                    rep(quantile(mod_data$unemployment, probs = .95), length(seq_food)),
                    quantile(mod_data$unemployment, probs = .95) * seq_food,
                    rep(median(mod_data$org_dem), length(seq_food)),
                    rep(median(mod_data$rep_prev), length(seq_food)),
                    rep(median(mod_data$rural_threat), length(seq_food)),
                    rep(median(mod_data$democracy), length(seq_food)),
                    rep(mean(mod_data$log_gdp_pc), length(seq_food)),
                    rep(mean(mod_data$gdp_growth), length(seq_food)),
                    rep(mean(mod_data$lexclpop), length(seq_food)),
                    1)

## Multiply X by the betas
xb_3 <- t(viol_sim_3%*% t(bay_coef))

## Get CIs (for plotting)
viol_ci_food_3 <- apply(xb_3, 2, quantile, probs=c(.025,.5, .975))

## Transform linear prediction to probability
viol_pp_food_3 <- exp(viol_ci_food_3)/(1+exp(viol_ci_food_3))

## Plot
food_pp_bay_plot <- ggplot()+
  labs(x="Observed Values of Food Price Change", y="Probability of Escalation")+
  geom_line(aes(seq_food, viol_pp_food[2,]))+
  geom_ribbon(aes(seq_food, ymin=viol_pp_food[1,], ymax=viol_pp_food[3,], 
                  fill="Mean Unemployment"), alpha=.7)+
  geom_line(aes(seq_food, viol_pp_food_2[2,]))+
  geom_ribbon(aes(seq_food, ymin=viol_pp_food_2[1,], ymax=viol_pp_food_2[3,], 
                  fill="Low Unemployment"), alpha=.7)+
  geom_line(aes(seq_food, viol_pp_food_3[2,]))+
  geom_ribbon(aes(seq_food, ymin=viol_pp_food_3[1,], ymax=viol_pp_food_3[3,], 
                  fill="High Unemployment"), alpha=.7)+
  geom_rug(aes(x=mod_data$food_lag))+
  theme_bw()+
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), 
        axis.title.y=element_text(size=18), 
        plot.title=element_text(hjust=0.5, size=24),panel.grid.minor.x=element_blank(), 
        panel.grid.minor.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))+
  scale_fill_manual(values=c("High Unemployment"="gray13","Low Unemployment"="gray87", 
                             "Mean Unemployment"="gray50"), name="", 
                    breaks=c("High Unemployment", "Mean Unemployment","Low Unemployment"))+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

pdf("food_pp_bay.pdf", width=8, height=6)
food_pp_bay_plot
dev.off()

## Specific values
viol_pp_food[2,1]; viol_pp_food[2,2404]
viol_pp_food_3[2,1]; viol_pp_food_3[2,2404]





## Unemployment (by food price change)
## Food Price Change at mean
## Generate dataframe with simulated values
viol_sim_4 <- cbind(rep(mean(mod_data$food_lag), length(seq_food)), 
      seq_unem,
      mean(mod_data$food_lag) * seq_unem,
      rep(median(mod_data$org_dem), length(seq_food)),
      rep(median(mod_data$rep_prev), length(seq_food)),
      rep(median(mod_data$rural_threat), length(seq_food)),
      rep(median(mod_data$democracy), length(seq_food)),
      rep(mean(mod_data$log_gdp_pc), length(seq_food)),
      rep(mean(mod_data$gdp_growth), length(seq_food)),
      rep(mean(mod_data$lexclpop), length(seq_food)),
      1)

## Multiply X by the betas
xb_4 <- t(viol_sim_4%*% t(bay_coef))

## Get CIs (for plotting)
viol_ci_unem_4 <- apply(xb_4, 2, quantile, probs=c(.025,.5, .975))

## Transform linear prediction to probability
viol_pp_unem_4 <- exp(viol_ci_unem_4)/(1+exp(viol_ci_unem_4))

## Food Price Change at .05 quantile
viol_sim_5 <- cbind(rep(quantile(mod_data$food_lag, probs = .05), length(seq_food)), 
                    seq_unem,
                    quantile(mod_data$food_lag, probs = .05) * seq_unem,
                    rep(median(mod_data$org_dem), length(seq_food)),
                    rep(median(mod_data$rep_prev), length(seq_food)),
                    rep(median(mod_data$rural_threat), length(seq_food)),
                    rep(median(mod_data$democracy), length(seq_food)),
                    rep(mean(mod_data$log_gdp_pc), length(seq_food)),
                    rep(mean(mod_data$gdp_growth), length(seq_food)),
                    rep(mean(mod_data$lexclpop), length(seq_food)),
                    1)

## Multiply X by the betas 
xb_5 <- t(viol_sim_5%*% t(bay_coef))

## Get CIs (for plotting)
viol_ci_unem_5 <- apply(xb_5, 2, quantile, probs=c(.025,.5, .975))

## Transform linear prediction to probability
viol_pp_unem_5 <- exp(viol_ci_unem_5)/(1+exp(viol_ci_unem_5))

## Food Price Change at .95 quantile
## Generate dataframe with simulated values
viol_sim_6 <- cbind(rep(quantile(mod_data$food_lag, probs = .95), length(seq_food)), 
                    seq_unem,
                    quantile(mod_data$food_lag, probs = .95) * seq_unem,
                    rep(median(mod_data$org_dem), length(seq_food)),
                    rep(median(mod_data$rep_prev), length(seq_food)),
                    rep(median(mod_data$rural_threat), length(seq_food)),
                    rep(median(mod_data$democracy), length(seq_food)),
                    rep(mean(mod_data$log_gdp_pc), length(seq_food)),
                    rep(mean(mod_data$gdp_growth), length(seq_food)),
                    rep(mean(mod_data$lexclpop), length(seq_food)),
                    1)

## Multiply X by the betas
xb_6 <- t(viol_sim_6%*% t(bay_coef))

## Get CIs (for plotting)
viol_ci_unem_6 <- apply(xb_6, 2, quantile, probs=c(.025,.5, .975))

## Transform linear prediction to probability
viol_pp_unem_6 <- exp(viol_ci_unem_6)/(1+exp(viol_ci_unem_6))

unem_pp_bay <- ggplot()+
  labs(x="Observed Values of Unemployment", y="Probability of Escalation")+
  geom_line(aes(seq_unem, viol_pp_unem_4[2,]))+
  geom_ribbon(aes(seq_unem, ymin=viol_pp_unem_4[1,], ymax=viol_pp_unem_4[3,], 
                  fill="Mean Food Price Change"), alpha=.7)+
  geom_line(aes(seq_unem, viol_pp_unem_5[2,]))+
  geom_ribbon(aes(seq_unem, ymin=viol_pp_unem_5[1,], ymax=viol_pp_unem_5[3,], 
                  fill="Low Food Price Change"), alpha=.7)+
  geom_line(aes(seq_unem, viol_pp_unem_6[2,]))+
  geom_ribbon(aes(seq_unem, ymin=viol_pp_unem_6[1,], ymax=viol_pp_unem_6[3,], 
                  fill="High Food Price Change"), alpha=.7)+
  geom_rug(aes(x=mod_data$unemployment))+
  theme_bw()+
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), 
        axis.title.y=element_text(size=18), 
        plot.title=element_text(hjust=0.5, size=24),
        panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))+
  scale_fill_manual(values=c("High Food Price Change"="gray13",
                             "Low Food Price Change"="gray87", 
                             "Mean Food Price Change"="gray50"), name="", 
                    breaks=c("High Food Price Change", "Mean Food Price Change",
                             "Low Food Price Change"))+
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

pdf("unem_pp_bay.pdf", width=8, height=6)
unem_pp_bay
dev.off()

## Specific values
viol_pp_unem_4[2,1]; viol_pp_unem_4[2,2404]
viol_pp_unem_6[2,1]; viol_pp_unem_6[2,2404]





## Organized ##
## Generate vector with the simulated range of Organized
seq_org <- 0:1

## Generate dataframe with simulated values
viol_sim_7 <- cbind(rep(mean(mod_data$food_lag), length(seq_org)), 
                    rep(mean(mod_data$unemployment), length(seq_org)),
                    rep(mean(mod_data$food_lag) * mean(mod_data$unemployment), 
                        length(seq_org)),
                    seq_org,
                    rep(median(mod_data$rep_prev), length(seq_org)),
                    rep(median(mod_data$rural_threat), length(seq_org)),
                    rep(median(mod_data$democracy), length(seq_org)),
                    rep(mean(mod_data$log_gdp_pc), length(seq_org)),
                    rep(mean(mod_data$gdp_growth), length(seq_org)),
                    rep(mean(mod_data$lexclpop), length(seq_org)),
                    1)

## Multiply X by the betas
xb_7 <- t(viol_sim_7%*% t(bay_coef))

## Plot
org_pred <- exp(xb_7)/(1+exp(xb_7))

org_pp_bay <- ggplot()+
  labs(x="", y="Probability of Escalation")+
  geom_boxplot(aes(x=1, y=org_pred[,1]))+
  geom_boxplot(aes(x=2, y=org_pred[,2]))+
  scale_x_continuous(breaks=c(1:2), labels=c("Spontaneous","Organized")) +  
  theme_bw()+
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18), axis.title.y=element_text(size=18), plot.title=element_text(hjust=0.5, size=24),panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())

pdf("org_pp_bay.pdf", width=8, height=6)
org_pp_bay
dev.off()

## Specific values
mean(org_pred[,1])
mean(org_pred[,2])











#### APPENDIX: ####

#### Traceplots ####
## Empty list for trace plots for betas
trace_beta <- list()

## Loop to create the 3 plots
for(i in 1:ncol(samps)){
  trace_beta[[i]] <- ggplot() +
    labs(x="", y="", title=beta_titles[i]) +
    geom_line(aes_string(x=x, y=samps[x,i]), color="orange", alpha=.5)+
    geom_line(aes_string(x=x, y=samps[max(x) + x,i]), color="red", alpha=.5)+
    geom_line(aes_string(x=x, y=samps[max(x) * 2 + x,i]), color="black", alpha=.5)+
    geom_line(aes_string(x=x, y=samps[max(x) * 3 + x,i]), color="blue", alpha=.5)+
    geom_hline(aes_string(yintercept=point_ests[i]))+
    theme_bw() + 
    theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18), 
          axis.title.y=element_text(size=18), 
          plot.title=element_text(hjust=0.5, size=24),
          panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(),
          legend.position="bottom")
}

png("trace_beta.png", width=1080, height=1080)
grid.arrange(trace_beta[[11]], trace_beta[[10]], trace_beta[[9]], trace_beta[[8]],
             trace_beta[[7]], trace_beta[[6]], trace_beta[[5]], trace_beta[[4]],
             trace_beta[[3]], trace_beta[[2]], trace_beta[[1]], ncol=3)
dev.off()





#### Density Plots ####
## Empty list for trace plots for betas
dens_beta <- list()

## Loop to create the 3 plots
for(i in 1:ncol(samps)){
  dens_beta[[i]] <- ggplot() +
    labs(x="", y="", title=beta_titles[i]) +
    geom_density(aes_string(samps[x,i]), fill="orange", alpha=.5)+
    geom_density(aes_string(samps[max(x) + x,i]), fill="red", alpha=.5)+
    geom_density(aes_string(samps[max(x) * 2 + x,i]), fill="black", alpha=.5)+
    geom_density(aes_string(samps[max(x) * 3 + x,i]), fill="blue", alpha=.5)+
    geom_vline(aes_string(xintercept=point_ests[i]))+
    geom_vline(aes_string(xintercept=cred_lo[i]), linetype = 2)+
    geom_vline(aes_string(xintercept=cred_hi[i]), linetype = 2)+
    theme_bw() + 
    scale_y_continuous(labels = c(), breaks = c())+
    theme(axis.text.x=element_text(size=18), axis.text.y=element_text(size=18), 
          axis.title.y=element_text(size=18), 
          plot.title=element_text(hjust=0.5, size=20),
          panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
          legend.position="bottom")
}

pdf("dens_beta.pdf", width=12, height=12)
grid.arrange(dens_beta[[11]], dens_beta[[10]], dens_beta[[9]], dens_beta[[8]],
             dens_beta[[7]], dens_beta[[6]], dens_beta[[5]], dens_beta[[4]],
             dens_beta[[3]], dens_beta[[2]], dens_beta[[1]], ncol=3)
dev.off()

## Autocorrelation Plots (not in appendix)
## stan_ac(esc_stan, pars = "beta")

## Alternative Specification

samps_alt <- readRDS("esc_bay_mfp_alt.rds")


## QI
point_ests_alt <- apply(samps_alt, 2, FUN="mean")
cred_lo_alt <- apply(samps_alt, 2, quantile, prob = .025)
cred_hi_alt <- apply(samps_alt, 2, quantile, prob = .975)
sds_alt <- apply(samps_alt, 2, FUN="sd")

bay_tab_alt <- texreg::createTexreg(coef.names = rev(beta_titles),
                                coef = rev(point_ests_alt),
                                ci.low = rev(cred_lo_alt),
                                ci.up = rev(cred_hi_alt))

texreg::texreg(bay_tab_alt, dcolumn = T)
