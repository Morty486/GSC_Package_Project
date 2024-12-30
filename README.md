# GenSim 

## Introduction

This package has updated the previous function in GenCorSeqSort by addressing
the following issues:

### Problem:
1. The sequential sorting algorithm faced a challenge when the required sorting ratio 
causing potential bias,  such as sorting 78 or 79 rows out of 100.

2. Original code has issues with bivariate setting.


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
Additionally, we fixed the issues in bivariate setting.


### Other Features:

- Allow Visualization for simulation results.


## Installation

Since our package is an update of the following package, we need to install 
the original one first:

```{r}
devtools::install_github("Morty486/GenCorSeqSort", build_vignettes = T)
```

Next, we can install our package by the following command:

```{r}
devtools::install_github("Morty486/GSC_Package_Project", build_vignettes = T)
```




