## Overview

`"binomial"` is an [R](http://www.r-project.org/) package that contains functions which allow one to calculate various binomial distribution factors.

  - `bin_choose(trials, number)` calculates how many combinations of number length in trials trials. 
  - `bin_probability(success, trials, prob)` calculates the probability of the nuber of successes given a number of trials and probability of success.
  - `bin_distribution(trials, prob)` creates a `"bindis"` object of individual probabilities which can be plotted using `plot()`. This outputs a barchart.
  - `bin_cumulative(trials, prob)`creates a `"bincum"` object of the total probabilities which can be plotted using `plot()`. This outputs a point-lineplot.
  - `bin_variable(trials, prob)` creates a `"binvar"` object which contains the trials and probability of a binomial distribution. This object has the methods `print()` and `summary()` which give basic features of the binomial distribution.

## Motivation


I need a grade for this project.

## Installation

Install the development version from GitHub via the package
`"devtools"`:

``` r
# Development version from GitHub:
#install.packages("devtools") 

# Install "binomial" (without vignettes)
devtools::install_github("Qui11sh/binomial")

# Install "binomial" (with vignettes)
devtools::install_github("Qui11sh/binomial", build_vignettes = TRUE)
```

## Usage

``` r
library(binomial)

# Choose k in n
bin_choose(5, 2)
#> 10

bin_choose(5, 2:4)
#> 10  10  5


# Probability of k success in n trials given prob p
bin_probability(2, 5, 0.3)
#> 0.3087

bin_probability(2:4, 5, 0.3)
#> 0.3087 0.13230 0.02835


# Plot distribution of a bindis object
bindi <- bin_distribution(5, 0.3)
bindi

#>   success probability
#> 1       0     0.16807
#> 2       1     0.36015
#> 3       2     0.30870
#> 4       3     0.13230
#> 5       4     0.02835
#> 6       5     0.00243

plot(bindis)


# Plot distribution of a bincum object
bincu <- bin_cum(5, 0.3)
bincu

#>   success probability cumulative
#> 1       0     0.16807    0.16807
#> 2       1     0.36015    0.52822
#> 3       2     0.30870    0.83692
#> 4       3     0.13230    0.96922
#> 5       4     0.02835    0.99757
#> 6       5     0.00243    1.00000

plot(bincu)


# Get parameters and total summary of a binvar object
binva <- bin_variable(5, 0.3)
print(binva)

#> "Binomial variable"
#> 
#> Parameters
#> - number of trials: 5 
#> - prob of success: 0.3 

bisum <- summary(binva)
bisum

#>   'Binomial variable' 
#> 
#>   Parameters 
#>  - number of trials: 5 
#>  - prob of success: 0.3 
#> 
#>  Measures 
#>  - mean:  
#>  - variance: 1.05 
#>  - mode: 2 
#>  - skewness: 0.39036 
#>  - kurtosis: -0.247619


# Each of the measures can be obtained by using 
# bin_`"insert measure here"`(trials, prob)



```
