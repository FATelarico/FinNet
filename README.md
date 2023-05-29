# FinNet: Financial network analysis in R got straight <img src="man/figures/logo.png" align="right" width="120" alt = ''/>

Author: Fabio Ashtar Telarico, University of Ljubljana, FDV

<!-- badges: start -->

![](https://img.shields.io/badge/R%20CMD-passing-brightgreen)    
![](https://img.shields.io/badge/version-0.1.0-orange)    
![](https://img.shields.io/badge/CRAN-0.1.0-blue)    
![](https://img.shields.io/github/last-commit/fatelarico/finnet?logo=GitHub&logoColor=orange&style=plastic)

<!-- badges: end -->

# Introduction

The numerous packages helping users to deal with network data in `R` (most notably [`network`](https://statnet.org/) and [`igraph`](https://igraph.org/)) do not offer tools specific for financial networks.

For years now, authors and analysts have worked on financial data using *ad-hoc* tools or programming languages other than `R`. So, the package `FinNet` was born to provide all `R` users with the ability to study financial networks with a set of tool especially designed to this purpose. Specifically, `FinNet` offers both brand new tools and an interface to the almost limitless capabilities of `igraph` and `network`.

![Supply chains as a financial network (From: McVey, Henry H., 'State of Play', KKR, Mar 24, 2022, <https://www.kkr.com/node/4025>).](https://www.kkr.com/images/insights/images-69/charts-30.png)



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

# Example workflow

## From retrieving the data to an `igraph`/`sna` object

A very simple workflow using this version of `FinNet` includes:

After having identified the firms of interest, the package can fetch all information on them as long as `yahoofinancer` is available. So:

``` r
# Check if `yahoofinancer` is installed
isTRUE(requireNamespace('yahoofinancer', quietly = TRUE))
```

```         
## [1] TRUE
```

``` r
# Create a list of the desired firms
# Note: if `SPB` is installed, a progress bar will appear
firms <- find.firms(tickers = c('TM', 'GM', 'F', 'HMC'),
                    name = c('Toyota', 'General Motors',
                             'Ford', 'Honda'))
```

```         
## 
## Progress status: 1/4
##  [==================------------------------------------------------------]  25%
## 
## Progress status: 2/4
##  [====================================------------------------------------]  50%
## 
## Progress status: 3/4
##  [======================================================------------------]  75%
## 
## Progress status: 4/4
##  [=========================================================================]  100%
```

``` r
# Identify common-ownership relations in a firm-firm matrix
FF <- FF.norm.ownership(firms)

# Create a simple-looking graph
g <- FF.graph(FF, aesthetic = 'simple')
```

Some check using the S3 methods implemented for `financial_matrix` objects and the extension of some `igraph` functions allow to verify the correctness of the graph:

``` r
# The order of the graph equals the number of rows in the FF matrix
vcount(g) == nrow(FF)
```

```         
## [1] TRUE
```

``` r
# The names of its vertex match the row names of the FF matrix
V(g)$name == rownames(FF)
```

```         
## [1] TRUE TRUE TRUE TRUE
```

## Plotting using default *nice* aesthetics

There are also useful defaults for a visual inspection of the network.

``` r
# Load dataset
data('firms_BKB')

# Identify common-ownership relations in a firm-firm matrix
FF <- FF(firms_BKB, who = 'own',
                 ties = 'naive', Matrix = TRUE)

# Create a nice-looking graph
g <- FF.graph(FF, aesthetic = 'nice')

# Plot it
plot_igraph(g, vertex.label = NA, edge.arrow.size = .6, scale_vertex = 10)
```

![Plot drawn using default 'nice' aesthetics](https://github.com/FATelarico/FinNet/assets/100512813/4e249390-d97d-4cc9-9fa2-4e8df37df3e8)

# Comparison with potentially similar `R` packages

A simple look at the [list of packages available on `CRAN`](https://cran.r-project.org/web/packages/available_packages_by_name.html) shows that there are no packages for financial-network analysis.

The most up-to-date book on the subject, *Introduction to R for Quantitative Finance* (Daróczi et al. 2023, [125-138](https://packtpub.com/book/data/9781783280933/)) uses `igraph` do analyse the network, but offers no indications as regards data managing and network construction.

# Next steps

The upcoming features of the package are:

-   Functions to estimate influence and influence-maximisation algorhitms (see Mizruchi and Bunting [1981] (<https://doi.org/10.2307/2392519>); Khalife, Read and Vazirgiannis [2021](https://doi.org/10.1007/s41109-021-00359-6));
-   Functions to identify systematically important agents in the network (see Komárková, Hausenblas, and Frait [2012](https://www.cnb.cz/export/sites/cnb/en/financial-stability/.galleries/fs_reports/fsr_2011-2012/fsr_2011-2012_article_i.pdf); Lai and Hu [2021](https://doi.org/10.1016/j.physa.2020.125613));
-   Different measures of centrality (see the `R` package [`centiserve`](https://www.centiserver.org/); and Nakamoto, Chakraborty and Ikeda [2019](https://doi.org/10.1007/s41109-019-0158-8));
-   Functions to estimate the systemic risk of financial contagion (see Kali and Reyes [2013](https://doi.org/10.1111/j.1465-7295.2009.00249.x); and Summer [2013](https://doi.org/10.1146/annurev-financial-110112-120948));
-   Improve support for `network`/`sna`;
-   Introduce a graphical interface to execute most operation.
