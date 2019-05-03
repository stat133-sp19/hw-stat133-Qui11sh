#PRivate fucntions

#this funcion check if a number is between 1 & 0
check_prob <- function(prob) {
  if(!is.numeric(prob) | !(0<=prob) | !(prob<=1)){
    stop("invalid prob value")
  }
  return(TRUE)
}

#checks if number of trials is valid
check_trials <- function(trials) {
  if(trials%%1!=0 | trials<=0){
    stop("invalid trial value")
  }
  return(TRUE)
}

#checks if successes are valid number
check_success <- function(success, trials) {
  if(trials%%1!=0 | trials<=0){
    stop("invalid trial value")
  }
  if(any(success%%1!=0) | any(success>trials) | any(success <0)){
    stop("invalid success value")
  }
  return(TRUE)
}

#returns auxillary mean
aux_mean <- function(trials, prob){
  return (trials * prob)
}

#returns aux variance
aux_variance <- function(trials, prob){
  return (aux_mean(trials, prob)*(1-prob))
}

#returns aux mode
aux_mode <- function(trials, prob){
  return (round((trials * prob) + prob))
}

#returns skewness
aux_skewness <- function(trials, prob){
  return ((1-(2*prob))/sqrt(aux_variance(trials, prob)))
}

#returns kurtosis
aux_kurtosis <- function(trials, prob){
  return ((1-(6*prob*(1-prob)))/(aux_variance(trials, prob)))
}


#
#
#
#
#MAIN FUNCTIONS
#
#
#
#

#' @title binomial choose
#' @description number of ways to choose k in n trials
#' @param accepts n: number of trials and k: size of combinations
#' @return returns the number of combinations of size k
#' @export 
#' @example 
#' bin_choose(n = 5, k = 2)
#' 10
#' bin_choose(5, 2:5)
#' 10, 10, 5, 1
bin_choose <- function(n, k){
  if(any(k>n | k<0) | any(!is.numeric(k))){
    stop("invalid value of k")
  }
  if(n <= 0 | !is.numeric(n)){
    stop("invalid value of n")
  }
  fac_n = factorial(n)
  fack = factorial(k)
  return (fac_n/(fack*factorial(n-k)))
}

setClass("bindis", contains = "data.frame")
setClass("bincum", contains = "bindis")
setClass("binvar")

#' @title binomial choose
#' @description number of ways to choose k in n trials
#' @param accepts n: number of trials and k: size of combinations
#' @return returns the number of combinations of size k
#' @export 
#' @example 
#' bin_choose(n = 5, k = 2)
#' 10
#' bin_choose(5, 2:5)
#' 10, 10, 5, 1
bin_probability <- function(success, trials, prob){
  if(!check_trials(trials) | !check_prob(prob) | !check_success(success, trials)) {
    stop("One of the inputs had an error")
  }
  arrang <- bin_choose(trials, success)
  return(arrang*(prob^(success))*((1-prob)^(trials-success)))
}

#' @title binomial choose
#' @description number of ways to choose k in n trials
#' @param accepts n: number of trials and k: size of combinations
#' @return returns the number of combinations of size k
#' @export 
#' @example 

bin_distribution <- function(trials, prob) {
  success <- c(0:trials)
  probability <- bin_probaility(success, trials, prob)
  dis_frame <- data.frame(success, probability)
  class(dis_frame) <- c("bindis", "data.frame")
  return (dis_frame)
}


#' @export 
setMethod("plot", "bindis", 
          function(bindis) {
            lx <- bindis$success
            ly <- bindis$probability
            barplot(ly, 1, names.arg = lx,
            ylab = "Probabilities",
            xlab = "successes")
            
          })

#' @title binomial choose
#' @description number of ways to choose k in n trials
#' @param accepts n: number of trials and k: size of combinations
#' @return returns the number of combinations of size k
#' @export 
#' @example 

bin_cumulative <- function(trials, prob) {
  distFrame <- bin_distribution(trials, prob)
  cumu <- c(distFrame$probability[1])
  for (i in c(2:length(distFrame$probability))){
    cumu <- c(cumu, cumu[i-1] + distFrame$probability[i])
  }
  distFrame$cumulative <- cumu
  class(distFrame) <- c("bincum", "data.frame")
  return(distFrame)
}

#' @export
setMethod("plot", "bincum", 
          function(bindis) {
            lx <- bindis$success
            ly <- bindis$cumulative
            plot.default(lx, ly, type = "b",
                    ylab = "Probabilities",
                    xlab = "successes")
            
          })

#' @title binomial choose
#' @description number of ways to choose k in n trials
#' @param accepts n: number of trials and k: size of combinations
#' @return returns the number of combinations of size k
#' @export 
#' @example 

bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  finList <- list(trials = trials,
                  prob = prob)
  class(finList) <- "binvar"
  return (finList)
}

setMethod("print", "binvar",
          function(x) {
            cat("\"Binomial variable\"\n\nParameters\n")
            cat("- number of trials:", x$trials, "\n")
            cat("- prob of success:", x$prob, "\n")
          })

setMethod("summary", "binvar",
          function(binvar) {
            tri <- x$trials
            p <- x$prob
            men <- aux_mean(tri, p)
            var <- aux_variance(tri, p)
            mo <- aux_mode(tri, p)
            skew <- aux_skewness(tri, p)
            kurt <- aux_kurtosis(tri, p)
            return(list(trials = tri,
                        prob = p,
                        mean = men,
                        variance = var,
                        mode = mo,
                        skewness = skew,
                        kurtosis = kurt))
          })







