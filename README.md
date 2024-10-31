# FinNet: Financial network analysis in R got straight <img src="man/figures/logo.png" align="right" width="120" alt = ''/>

*Author: Fabio Ashtar Telarico, University of Ljubljana, FDV*

<!-- badges: start -->
![](https://img.shields.io/badge/R%20CMD-passing-brightgreen)
![](https://img.shields.io/badge/version-0.2.1-greem)
![](https://img.shields.io/badge/CRAN-0.2.1-blue)
![](https://img.shields.io/github/last-commit/fatelarico/finnet?logo=GitHub&logoColor=orange&style=plastic)

<!-- badges: end -->

# Introduction

The numerous packages helping users to deal with network data in `R` (most notably [`network`](https://statnet.org/) and [`igraph`](https://igraph.org/)) do not offer tools specific for financial networks.

For years now, authors and analysts have worked on financial data using *ad-hoc* tools or programming languages other than `R`. So, the package `FinNet` was born to provide all `R` users with the ability to study financial networks with a set of tool especially designed to this purpose. Specifically, `FinNet` offers both brand new tools and an interface to the almost limitless capabilities of `igraph` and `network`.

![Supply chains as a financial network (From: McVey, Henry H., 'State of Play', KKR, Mar 24, 2022, <https://www.kkr.com/node/4025>).](https://i.pinimg.com/736x/76/9d/99/769d99093e4abf6e762f1cf7e0ef6dd3.jpg)



The first release of `FinNet` introduces the backbone of this system, which will be expanded with new releases. A basic introduction is available in this readme. For more information, visit: [FinNet's webiste](https://fatelarico.github.io/FinNet.html)

# Installation

## From CRAN (stable version)

```         
install.packages('FinNet')
```

## From GitHub (development version)

```         
if(!require('remotes'))install.packages('remotes')
remotes::install_github('faTelarico/FinNet')
```

# Motivation

Before `FinNet` there was not `R` package especially conceived for financial-network analysis on the `CRAN`. True, similar results could be achieved combining functions from several packages. However, `FinNet` allows users (mainly analysts and researchers) to focus on what matters, their analyses, and let the package take care of tedious operations and conversion.

In a nutshell, `FinNet` is designed to put ease of use first.[^index-1] Moreover, it allows for a very lean installation since the only required packages are included with base `R`: `grDevices` and `methods`. Yet, it is extremely flexible insofar as it allows users to leverage the capabilities of other packages available on `CRAN`:

[^index-1]: See full documentation: [here]().

-   [`igraph`](https://CRAN.R-project.org/package=igraph) or [`network`](https://CRAN.R-project.org/package=network) to interface `FinNet` with these packages;
-   [`knitr`](https://CRAN.R-project.org/package=knitr) or [`pander`](https://CRAN.R-project.org/package=pander) to print better summaries to the console;
-   [`Matrix`](https://CRAN.R-project.org/package=Matrix) to optimise the storage of relations between agents;
-   [`SPB`](https://CRAN.R-project.org/package=SPB) to get better progress bars; and
-   [`yahoofinancer`](https://CRAN.R-project.org/package=yahoofinancer) to automatically retrieve data from [Yahoo! Finance](https://finance.yahoo.com/)

# Features

At this stage of development, `FinNet` provides the following:

-   Specialised S4 classes for financial agents (`firm`), their relations (`financial_matrix`), and interfaces to other packages (`network_financial` and `igraph_financial`);
-   Functions to register information about financial agents, including retrieving them from Yahoo! Finance;
-   Function to encode the relations between these agents and their owners or managers into incidence matrices;
-   Function to encode the relations between these agents (based on common ownership, board interlocks, or both) into adjacency matrices;
-   Function to encode the relations between these agents (based on common ownership, board interlocks, or both) into extended `network` or `igraph` objects.



# Comparison with potentially similar `R` packages

A simple look at the [list of packages available on `CRAN`](https://cran.r-project.org/web/packages/available_packages_by_name.html) shows that there are no packages for financial-network analysis.

The most up-to-date book on the subject, *Introduction to R for Quantitative Finance* (Daróczi et al. 2023, [125-138](https://packtpub.com/book/data/9781783280933/)) uses `igraph` do analyse the network, but offers no indications as regards data managing and network construction.

# Advanced features
With implementation in both `C++` and `R`:

-   Functions to identify systematically important agents in the network (see Komárková, Hausenblas, and Frait [2012](https://www.cnb.cz/export/sites/cnb/en/financial-stability/.galleries/fs_reports/fsr_2011-2012/fsr_2011-2012_article_i.pdf); Lai and Hu [2021](https://doi.org/10.1016/j.physa.2020.125613));
-   Different measures of centrality (see Nakamoto, Chakraborty and Ikeda [2019](https://doi.org/10.1007/s41109-019-0158-8));
-   Estimation of systemic risk and simulating financial contagion (see Kali and Reyes [2013](https://doi.org/10.1111/j.1465-7295.2009.00249.x); and Summer [2013](https://doi.org/10.1146/annurev-financial-110112-120948)).

# Next steps

The upcoming features of the package are:

-   Functions to estimate influence and influence-maximisation algorhitms (see Mizruchi and Bunting [1981] (<https://doi.org/10.2307/2392519>); Khalife, Read and Vazirgiannis [2021](https://doi.org/10.1007/s41109-021-00359-6));
-   Improve support for `network`/`sna`;
-   Introduce a graphical interface to execute most operation.

