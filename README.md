# Birdr
## Description
Mini R package for birders, primarily in the United States.
Package functions can retrieve select data from the eBird API and create maps/graphics that birders may find useful.
See the `.RMD` file for select examples
 
## Installation

You can install the the development version from
[GitHub](https://github.com/) with:

```{r}
# install.packages("devtools")
devtools::install_github("sjmarks/Birdr")
```
```{r}
library(Birdr)
```
 
### Special note: `Birdr` accesses all data using the eBird API
#### About ebird keys:
##### To use all functions in `Birdr` please obtain a key here- (https://ebird.org/api/keygen)
