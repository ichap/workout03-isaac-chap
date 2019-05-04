
library(ggplot2)

#checks prob variable if it has a valid value between 0 and 1
check_prob <- function(prob) {
  if (prob >= 0 & prob <= 1) {
    return(TRUE)
  }
  else {
    stop('prob has to be a number betwen 0 and 1')
  }
}

#checks trial variable if it has a valid value of a positive integer
check_trials <- function(trials) {
  if (trials >= 0 & as.integer(trials) == trials) {
    return(TRUE)
  }
  else {
    stop('invalid trials value')
  }
}

#checks success variable in comparison with trials
check_success <- function(success, trials) {
  t1 <- rep(trials, length(success))

  if (all(t1>= success) & all(success - floor(success) == 0)) {
    return(TRUE)
  }
  else {
    stop('invalid success values')
  }
}


#returns the mean based on variable trials and prob
aux_mean <- function(trials, prob) {
  return(trials*prob)
}

#returns the variance based on variable trials and prob
aux_variance <- function(trials, prob) {
  return(trials*prob*(1-prob))
}

#returns the mode based on variable trials and prob
aux_mode <- function(trials, prob) {
  return(round(trials*prob + prob))
}

#returns the skewness value based on variable trials and prob
aux_skewness <- function(trials, prob) {
  return((1 - 2*prob)/((trials*prob*(1-prob))^0.5))
}

#returns the kurtosis value based on variable trials and prob
aux_kurtosis <- function(trials, prob) {
  return((1 - 6*prob*(1-prob))/(trials*prob*(1-prob)))
}

#' @title binomial choose function
#' @description Finds choose value of trials and success
#' @param trials: how many times experiment repeated
#' @param success: how many successes out of trials
#' @return the value of combinations
#' @export
# bin_choose(trials = 5, success = 2)
# bin_choose(5, 0)
# bin_choose(5, 1:3)

bin_choose <- function(trials, success) {
  check_trials(trials)
  check_success(success, trials)
  return(factorial(trials)/(factorial(trials-success)*factorial(success)))
}

#' @title binomial probability function
#' @description Finds the value based on success, trials and probability of binomial distribution
#' @param trials: how many times experiment repeated
#' @param success: how many successes out of trials
#' @param prob: probability of success
#' @return the probability of number of successes
#' @export
#' @examples
# bin_probability(success = 2, trials = 5, prob = 0.5)
# bin_probability(success = 0:2, trials = 5, prob = 0.5)
# bin_probability(success = 55, trials = 100, prob = 0.45)

bin_probability <- function(success, trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop('need valid trials')
  }
  if (check_prob(prob) != TRUE) {
    stop('need valid probability')
  }
  if (check_success(success, trials) != TRUE) {
    stop('need valid success')
  }
  else {
    return(bin_choose(trials, success)*prob^success*(1-prob)^(trials-success))
  }
}


#' @title binomial distribution function
#' @description Finds the probabilities of all successes
#' @param trials: how many times experiment repeated
#' @param prob: probability of success
#' @return a list of the probabilities of each success
#' @export
#' @example
# bin_distribution(trials = 5, prob = 0.5)

bin_distribution <- function(trials, prob) {
  m <- matrix(nrow = trials + 1, ncol = 2)
  colnames(m) <- c("success", "probability")
  for (i in 0:trials) {
    m[i+1, 1] = i
    m[i+1, 2] = bin_probability(i, trials, prob)
  }
  print(data.frame(m))
}

#' @title plot bindis
#' @export
plot.bindis <- function(dis) {
  ggplot(data = dis) +
    geom_bar(stat = "identity", aes(x=success, y= probability))
}

#' @title binomial cumulative function
#' @description Finds the cumulative probabilities of all successes
#' @param trials: how many times experiment repeated
#' @param prob: probability of success
#' @return a list of the probabilities and cumulative probabilities of each success
#' @export
#' @example
# bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials = 1, prob = 1) {
  m <- matrix(nrow = trials + 1, ncol = 3)
  for (i in 0:trials) {
    m[i+1, 1] = i
    m[i+1, 2] = bin_probability(i, trials, prob)
    m[i+1, 3] = sum(m[1:(i+1), 2])
  }
  colnames(m) <- c("success", "probability", "cumulative")
  print(data.frame(m))
}

#' @title plot bincum
#' @export
plot.bincum <- function(dis) {
  ggplot(data = dis) +
    geom_line(stat = "identity", aes(x=success, y= cumulative)) +
    geom_point(aes(x=success, y= cumulative))
}

#' @title binomial variable function
#' @description displays trial and probability
#' @param trials: how many times experiment repeated
#' @param prob: probability of success
#' @return trial and probability variable in a list format
#' @export
#' @example
# bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  list('trials'=trials, 'prob'=prob)
}

#' @title print binvar
#' @export
print.binvar <- function(binvar) {
  trials = binvar[['trials']]
  prob = binvar[['prob']]
  cat(paste0("Binomial Variable\n\nParameters\n- prob of success: ", prob, "\n",
             "- number of trials: ", trials))
}

#' @title summarybinvar
#' @export
summary.binvar <- function(binvar) {
  trials = binvar[['trials']]
  prob = binvar[['prob']]
  kurt = aux_kurtosis(trials, prob)
  mean = aux_mean(trials, prob)
  mode = aux_mode(trials, prob)
  skew = aux_skewness(trials, prob)
  var  = aux_variance(trials, prob)
  list('trials'=trials, 'prob'=prob, 'kurt'=kurt, 'mean'=mean, 'mode'=mode, 'skew'=skew, 'var'=var)
}



#' @title printsummary
#' @export
print.summary.binvar <- function(binsum1) {
  cat(paste0(
    'Summary Binomial\n\nParameters\n- prob of success: ', binsum1[['prob']], "\n",
    "- number of trials: ", binsum1[['trials']],
    "\n\nMeasures\n- mean    : ", binsum1[['mean']], "\n",
    "- variance: ", binsum1[['var']], "\n",
    "- mode    : ", binsum1[['mode']], "\n",
    "- skewness: ", binsum1[['skew']], "\n",
    "- kurtosis: ", binsum1[['kurt']]))
}

# example
# bin1 <- bin_variable(10, 0.3)
# binsum1 <- summary.binvar(bin1)
# binsum1
# print.summary.binvar(binsum1)

#' @title binmean
#' @export
#returns the mean based on variable trials and prob
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title binvar
#' @export
#returns the variance based on variable trials and prob
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title binmode
#' @export
#returns the mode based on variable trials and prob
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title binskew
#' @export
#returns the skewness value based on variable trials and prob
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title binkurt
#' @export
#returns the kurtosis value based on variable trials and prob
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

