---
title: 'defm: Estimation and Simulatio of Multi-Binary Response Models'
tags:
  - R
  - statistics
  - graphical models
  - Markov random fields
authors:
  - given-names: George G.
    family: Vega Yon
    affiliation: '1'
    orc-id: 0000-0002-3171-0844
affiliations:
  - index: 1
    name: Division of Epidemiology, University of Utah, Salt Lake City, UT, United States of America
---

# Introduction

Datasets featuring multiple outcomes and methods to deal with them are increasingly common [@Martin2021]. In the case of binary outcomes, the usual statistical approach is to either look into one variable at a time or, in the case of longitudinal data, use Markov random fields [MRF] to estimate transitions between states. In models with $k$ states, the number of parameters to estimate grows exponentially at a rate of $2^{2k}$. While MRF are a better modeling alternative than looking at the outcomes separately, MRF can become awkward to estimate and interpret. The `defm` R package [@R] implements the Discrete Exponential-Family Models [DEFM,] a modeling framework that simplifies the problem using sufficiency [@Holland1981; @Frank1986; @Wasserman1996; @Snijders2006; @Robins2007]. DEFMs' most popular model is the Exponential-Family Random Graph Model [ERGM], currently available in the R ergm package [@handcock2023; @ergmpkg]. This package provides the first resource for fitting DEFMs for data other than network data.

# Statement of Need

As mentioned in the introduction, multi-outcome data is more common. Although there are statistical models built for estimating multi-binary outcome models [@Bai2020; @Davenport2018; @CAREY1993; @TeixeiraPinto2009], most methods are focused on controlling for correlations between outcomes rather than hypotheses about their interactions. With DEFMs, we can answer questions such as:

- Does consuming tobacco and alcohol lead to a higher risk of using marijuana?
- Are depression and substance more likely to co-occur than depression and anxiety?
- Are phenotypes A and B jointly mediated by gender?
- Is a joint disease A and B prevalence model more appropriate than independent logistic regressions?

DEFMs provide an elegant framework to answer these questions, providing a formal strategy to assess independence between outcomes. @defmarxiv introduces the method and provides a detailed description of the model. 

# Key Features

The package's core functionality is implemented in C++. `defm` is a module part of a larger project we also maintain called [`barry`](https://github.com/USCbiostats/barry), a header-only C++ library that provides functionality to build DEFMs. Key features of this R package are:

- Estimation of DEFM using maximum likelihood.
- Simulation of DEFM data.
- Build Markov models of arbitrary order.
- Fast motif counting using the C++ backend.
- Hashing of models' normalizing constant to avoid recomputing it.

The package is available on the Comprehensive R Archive Network [[CRAN](https://cran.r-project.org/package=defm)] and [GitHub](https://github.com/UofUEpiBio/defm).


# Conclusion

The `defm` R package provides the first implementation of Discrete Exponential-Family Models for data other than network data. The package's core functionality is implemented in C++, which makes it fast. Using `defm`, researchers can model data featuring multiple binary outcomes and make hypothesis testing about their interactions. The package is available on CRAN and GitHub.

# Acknowledgements

This work was supported by the Assistant Secretary of Defense for Health Affairs endorsed by the Department of Defense, through the Psychological Health/Traumatic Brain Injury Research Program Long-Term Impact of Military-Relevant Brain Injury Consortium (LIMBIC) Award/W81XWH-18-PH/TBIRP-LIMBIC under Award No. I01 RX003443. The U.S. Army Medical Research Acquisition Activity, 839 Chandler Street, Fort Detrick MD 21702-5014 is the awarding and administering acquisition office. Opinions, interpretations, conclusions and recommendations are those of the author and are not necessarily endorsed by the Department of Defense. Any opinions, findings, conclusions recommendations expressed in this publication are those of the author(s) and do not necessarily reflect the views of the U.S. Government, the U.S. Department of Veterans Affairs or the Department of Defense and no official endorsement should be inferred.

# References