# ```tureen625```, a R Package for Biostat 625

## Overview

This github repository is dedicated to Homework #4 for Biostat 625 : Big Data Computing (Fall 2019). ```tureen625``` currently consists of one relevant function called ```fit_OLS()``` that can be used to fit a linear regression model (simple or multi-variable) using the Ordinary Least Squares method.

## How to Install

To install this package, recommend following the ```R``` provided code below. First open up a ```R``` session, then proceed:

```library(devtools)```

```devtools::install_github("tahmeed14/tureen625", build_vignettes = TRUE, force = TRUE)```

```library(tureen625)```

## Quick Example

We demonstrate how to use our function in the ```R``` code below. For a more in-depth tutorial, we recommend checking out the vignette provided in the package.

```my_model <- tureen625::fit_OLS(design_X = matrix_object, Y = vector_object)```

## How to View Tutorial

To view the tutorial (vignette) associated with this package, run the following code:

```browseVignettes(package = "tureen625")```

If for any reason, you cannot access the vignette or the vignette is not downloading, we provide a PDF version of the tutorial in the github repository called ```tutorial.pdf```.

## Contact Information

If you have any questions, please contact the maintainer at ```tureen@umich.edu```
