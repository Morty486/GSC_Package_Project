# GenSim 

## Introduction

This package has updated the previous function in GenCorSeqSort by addressing
the following issues:

### Problem:
1. The sequential sorting algorithm faced a challenge when the required sorting ratio 
causing potential bias,  such as sorting 78 or 79 rows out of 100.

2. Original code has issues with bivariate setting-particularly the function GenCorDataBiTri() 
doesn't work properly when 2 distributions are passed in as arguments.


### Solution:
We updated the main functions that:

- Handles fractional sorting ratios by implementing a probabilistic sorting . 
\
\
For instance, with a ratio of 
0.7856, the function sorts 78 rows with 
44% probability and 79 rows with 56% probability.


- Supports a method to select rows by sampling without replacement.
\
\
For instance, suppose we want to sample 1000 rows with a ratio of 
0.7856. We first increase the size to 10000 rows, meaning 7856 rows 
needs to be sorted. By sampling without replacement, we select 1000 rows
as our final simulation data.

\
Additionally, we fixed the issues in bivariate setting and now the revised function GenCorDataBiTri1.1() 
can correctly handle a pair of distributions.


### Other Features:

- Allow Visualization for simulation results.


## Installation

we can install our package by the following command:

```{r}
devtools::install_github("Morty486/GSC_Package_Project", build_vignettes = T)
library(GSCSim) 
```


## Visualization

Our package has a special feature of visualization, here's an example:

```{r}
# Example Usage
X <- function(n) rnorm(n, mean = 0, sd = 1)
Y <- function(n) rpois(n, lambda = 2)
Z <- function(n) runif(n, min = 0, max = 1)
set.seed(100)
a <- TableGeneration(3, list(X, Y, Z), "N(0, 1)", "Poisson(2)", "Uniform(0, 1)")

# Access bounds table
library(knitr)
kable(a$bounds_table, format = "markdown", caption = "Bounds Table")

# Access correlation table
kable(a$correlation_table, format = "markdown", caption = "Correlation Table")
```



