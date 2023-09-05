
#############################################################################!
# 0) R Setup                                                             ####
#############################################################################!

# load packages (e.g. here, brms, tidyverse, tidybayes, patchwork, gghalves, bmm)


# read data into a data.frame


# transform the data
# the dependent variable should be response error relative to target
# the non-targets errors should be error relative to non-target values
# if subtracting response from target directly, use the wrap() function from bmm
# to wrap the data into the range of -pi to pi





#############################################################################!
# 1) Model specification and estimation                                  ####
#############################################################################!

# specify model formula


# fit the desired model with bmm



#############################################################################!
# 2) Model evaluation                                                    ####
#############################################################################!

## 2.1) fit & summary ----------------------------------------------------------
# plot the posterior predictive check to evaluate overall model fit


# print results summary



## 2.2) Extract parameter estimates --------------------------------------------
# extract the fixed effects from the model and determine the rows that contain
# the relevant parameter estimates


# transform parameters because brms uses special link functions


# print parameter estimates


## 2.3) plot parameter estimates -----------------------------------------------

# plot sd results


# plot pnt results


# plot pg results


#############################################################################!
# 3) Inference                                                           ####
#############################################################################!

# make conclusions about differences between conditions in parameter values