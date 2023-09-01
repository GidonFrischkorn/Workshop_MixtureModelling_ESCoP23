library(here)
Path_DATA <- here("data")

# Specify settings for simulation
nSub = 50
nTrials = 40
setsize = c(4,8)

mean_pCorr = c(0.80, 0.75)
cor = c(0.7, 0.5)
mean_pLapse = c(0.15, 0.075)

pGuess <- 0.5

# randomly draw the probability for correct responses for each subject
sub_pCorr_logit <- faux::rnorm_multi(n = nSub,
                                     mu = brms::logit_scaled(mean_pCorr), sd = c(0.3, 0.2),
                                     r = cor[1])
#cor(sub_pCorr_logit)
#brms::inv_logit_scaled(apply(sub_pCorr_logit,2,mean))
sub_pCorr <- brms::inv_logit_scaled(sub_pCorr_logit)

sub_pLapse_logit <- faux::rnorm_multi(n = nSub,
                                     mu = brms::logit_scaled(mean_pLapse), sd = c(0.1, 0.1),
                                     r = cor[2])
#cor(sub_pCorr_logit)
#brms::inv_logit_scaled(apply(sub_pCorr_logit,2,mean))
sub_pLapse <- brms::inv_logit_scaled(sub_pLapse_logit)

# pre-allocate data frame to store simulated behavior
simData <- data.frame(
  ID = integer(),
  trialNum = integer(),
  nHits = integer(),
  setsize = integer(),
  isLaps = integer(),
  true_pCorr = numeric(),
  true_pLapse = numeric()
)

# loop through all subjects to simulate behavior
for (i in 1:nSub) {
  for (j in setsize) {
    isLaps <- rbinom(n = nTrials, size = 1, prob = sub_pLapse[i, setsize == j])
    
    laps_data <- rbinom(n = sum(isLaps), size = j, prob = pGuess)
    mem_data <- rbinom(n = nTrials - sum(isLaps), size = j, prob = sub_pCorr[i,setsize == j])
    
    sub_data <- c(laps_data, mem_data)
    
    df_subData <- data.frame(
      ID = i,
      trialNum = 1:nTrials,
      nHits = sub_data,
      setsize = j,
      isLaps = c(rep(1,times = sum(isLaps)),rep(0,times = nTrials - sum(isLaps))),
      true_pCorr = sub_pCorr[i,setsize == j],
      true_pLapse = sub_pLapse[i, setsize == j]
    )
    
    simData <- rbind(simData,df_subData)
  }
}

# plot simulated data
library(tidyverse)

ggplot(data = simData,
       aes(x = nHits/setsize)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,1.05)) +
  labs(x = "proportion correct",
       y = "frequency",
       fill = "Random guessing") + theme_classic()

ggplot(data = simData,
       aes(x = nHits/setsize, fill = factor(isLaps, labels = c("no","yes")))) +
  geom_histogram(position = position_dodge()) +
  coord_cartesian(xlim = c(0,1.05)) +
  labs(x = "proportion correct",
       y = "frequency",
       fill = "Random guessing") + theme_classic()

write.table(simData, file = here("data","simData_Binomial.txt"), sep = ",", row.names = FALSE, col.names = TRUE)
