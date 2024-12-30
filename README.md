# GenSim 

## Introduction

This package has updated the previous function in GenCorSeqSort by addressing
the following issue:

### Problem:
The sequential sorting algorithm faced a challenge when the required sorting ratio 
causing potential bias,  such as sorting 78 or 79 rows out of 100.

### Solution:
We developed a new R function that:

- Handles fractional sorting ratios by implementing a probabilistic sorting . 
\
For instance, with a ratio of 
0.7856
0.7856, the function sorts 78 rows with 
44% probability and 79 rows with 56% probability.

\

- Supports a method toselect rows by sampling without replacement.
\
For instance, suppose we want to sample 1000 rows with a ratio of 
0.7856. We first increase the size to 10000 rows, meaning 7856 rows 
needs to be sorted. By sampling without replacement, we select 1000 rows
as our final simulation data







