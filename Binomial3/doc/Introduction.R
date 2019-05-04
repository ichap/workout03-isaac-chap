## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, error = TRUE)
library(Binomial3)

## ----check private funcs-------------------------------------------------
# check_prob
# check_trials
# check_success

## ----aux private funcs---------------------------------------------------
# aux_mean
# aux_mode
# aux_variance 

## ----binchoose-----------------------------------------------------------
bin_choose(6, 5)
bin_choose(4, 5)

## ----plot with bindis bincum---------------------------------------------
dis1 <- bin_distribution(trials = 5, prob = 0.5)
cum1 <- bin_cumulative(trials = 5, prob = 0.5)
plot.bindis(dis1)
plot.bincum(cum1)

## ----printsummary--------------------------------------------------------

bin1 <- bin_variable(10, 0.3)
print.binvar(bin1)

binsum1 <- summary.binvar(bin1)
print.summary.binvar(binsum1)

## ----bin other functions-------------------------------------------------

bin_mean(10, 0.4)
bin_variance(10, 0.4)
bin_mode(10, 0.4)
bin_skewness(10, 0.4)
bin_kurtosis(10, 0.4)

