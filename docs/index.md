--- 
title: "Mulling Over McElreath's Statistical Rethinking"
author: "Dan Burrell"
date: '2020-08-22'
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib]
biblio-style: apalike
link-citations: yes
github-repo: danBurrell/StatisticalRethinking
url: 'https://danburrell.github.io/StatisticalRethinking/'
description: "My journey working through McElreath's Statistical Rethinking text"
---

# Before you start, you should do these things

We're working through the second edition of McElreath's Statistical Rethinking text. We need to access the associated R pacakge `rethinking` and also load the `tidyverse`. Use the following code:


```r
if(!require(pacman)) install.packages("pacman")
library(pacman)

#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
#pkgbuild::has_build_tools(debug = TRUE) # Check the C++ toolchain
#library("rstan")
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
p_load(coda, mvtnorm, loo, dagitty, tidyverse, remotes)
remotes::install_github("rmcelreath/rethinking")
#remotes::install_github("stan-dev/cmdstanr")
```

Below is an example of the generic format for \stan code. The blocks need to occur in the order specified, however only the `model` block is necessary (the others are optional, depending on the needs of the modeler). 


```r
modelString = "

    data {
        ...declarations... // This is a comment
    }

    transformed data {
        ...declarations ... statements ...
    }

    parameters {
        ...declarations...
    }

    transformed parameters {
        ... declarations ... statemetns ...
    }

    model {
        ...declarations ... statements ...
    }

    generated quantities {
        ...declarations ... statements ...
    }
" # Close quote for modelString
```

Translate the model into C++ code and compile into an executable dynamic shared object (DSO) using `stan_model()` from the `rstan` package:


```r
p_load(rstan)
stanDSO = stan_model( model_code=modelString )
```

Once the DSO is created, it can be used for generating a Monte Carlo sample from the posterior distribution. For example:

```r
# Create some fictitious data:
N = 50; z = 10; y = c(rep(1,z), rep(0,N-z))
dataList = list( y=y, N=N)
stanFit = sampling( object=stanDSO, data=dataList,
                    chains=3, iter=1000, warmup=200, thin=1)

# Load rjags, coda, and DBDA2E functions
source("DBDA2E-utilities.R")
```


