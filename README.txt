This file provides instructions for replicating "Hunger to Violence: Explaining the Violent Escalation of Nonviolent Demonstrations."

MAIN TEXT:

- esc_mod_data.rds is the dataset
- esc_ran_int.stan is the Stan model
- esc_stan_call.R fits the Stan model and produces the MCMC samples required to replicate the statistical results
- esc_bay_mfp.R produces the main results in the paper

SUPPORTING INFORMATION:

- me_plot.R is a function used to produce the marginal effects plots in the appendix
- esc_mod_data_npart.rds is the dataset including the number of partipants
- esc_mod_data_strikes.rds is the dataset including the strikes in the sample
- esc_mod_data_nstar.rds is the dataset including N*
- esc_ran_int_alt.stan is the alternative Stan model
- esc_mle_mfp.R produces the MLE results for monthly food prices in the appendix
- esc_mod_data_yfp.rds is the dataset including the yearly food prices measure
- esc_mod_data_nstar_yfp.rds is the dataset including the yearly food prices measure and N*
- esc_mle_yfp.R produces the MLE results for yearly food prices in the appendix
