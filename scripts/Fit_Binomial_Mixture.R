library(here)
Path_DATA <- here("data")

pacman::p_load(brms, tidyverse, bridgesampling)

df_binomial <- read.table(file = here("data","simData_Binomial.txt"), header = TRUE, sep = ",") %>%
  mutate(setsize_factor = as.factor(setsize))

df_binomial_agg <- df_binomial %>% 
  group_by(ID, setsize) %>% 
  summarise(meanPC = mean(nHits/setsize))

# quick descriptive plot of the data
ggplot(data = df_binomial_agg,
       aes(x = as.factor(setsize), y = meanPC, group = 1)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.4) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun = "mean", geom = "line") +
  labs(x = "Set Size", y = "Average Proportion Correct")

# extract the data generating parameters from the data
genPars_sub <- df_binomial %>% 
  filter(trialNum == 1) %>% 
  select(ID, setsize, true_pCorr, true_pLapse)

# set up parallelization for the MCMC chains
options(mc.cores = parallel::detectCores())

# 1) Regular Binomial model --------------------------------------------------------------

# fit model assuming a single binomial distribution
fit_binomial <- brm(
  formula = nHits|trials(setsize) ~ 0 + setsize_factor + (0 + setsize_factor | ID),
  data = df_binomial,
  family = binomial()
)

# evalutae model fit
pp_check(fit_binomial)

# get summary for estimated effects
summary(fit_binomial)
inv_logit_scaled(fixef(fit_binomial))

# 2) Binomial Mixture Modeling -----------------------------------------------------------
#' this mixture model separates trials stemming from a guessing process
#' versus trials stemming from a memory state above guessing performance

# Set up the mixture family
mix_binomial <- mixture(binomial, binomial, order = "none")

# Set up the formula for predicting parameters
bf_binomial <- brms::bf(nHits|trials(setsize) ~ 0 + setsize_factor + (0 + setsize_factor | ID),
            mu2 ~ 1,
            theta1 ~ 0 + setsize_factor + (0 + setsize_factor | ID))

get_prior(formula = bf_binomial,
          data = df_binomial,
          family = mix_binomial)

# identify mixtures using priors
mix_priors <- prior(constant(0), class = Intercept, dpar = "mu2")

# fit mixture model using the brm command
fit_binomial_mixModel <- brm(
  formula = bf_binomial,
  data = df_binomial,
  family = mix_binomial,
  prior = mix_priors
)

# evalutae model fit
pp_check(fit_binomial_mixModel)

# add LOO estimation to compare models
fit_binomial <- add_criterion(fit_binomial,"loo")
fit_binomial_mixModel <- add_criterion(fit_binomial_mixModel,"loo")

# compare models via LOO
loo_compare(fit_binomial_mixModel, fit_binomial)

# look at the results
summary(fit_binomial_mixModel)

# extract fixed effects (i.e. group level means)
fixed_FX <- fixef(fit_binomial_mixModel)
fixed_FX_scaled <- inv_logit_scaled(fixed_FX)
fixed_FX_scaled

# compare with the mean of the true data generating parameters
genPars_sub %>% 
  group_by(setsize) %>% 
  summarise(mean_pCorr = mean(true_pCorr))
genPars_sub %>% 
  group_by(setsize) %>% 
  summarise(mean_pLaps = mean(true_pLapse))

# extract random effects
randomFX <- ranef(fit_binomial_mixModel)

# compute random effects for each subject for the probability to respond correctly
randomFX_pCorr <- randomFX$ID[,,"mu1_Intercept"] + fixed_FX["mu1_Intercept","Estimate"]
randomFX_pCorr_scaled <- inv_logit_scaled(randomFX_pCorr)
randomFX_pCorr_scaled

# plot estimated subject effects against data generating parameters
plot(genPars_sub$true_pCorr, randomFX_pCorr_scaled[,"Estimate"])
cor(genPars_sub$true_pCorr, randomFX_pCorr_scaled[,"Estimate"])

# 
randomFX_pLaps <- randomFX$ID[,,"theta1_Intercept"] + fixed_FX["theta1_Intercept","Estimate"]
randomFX_pLaps_scaled <- inv_logit_scaled(randomFX_pLaps)
randomFX_pLaps_scaled

plot(1 - genPars_sub$true_pLapse, randomFX_pLaps_scaled[,"Estimate"])
cor(1 - genPars_sub$true_pLapse, randomFX_pLaps_scaled[,"Estimate"])
