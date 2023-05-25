# FinNet: Financial network analysis in R got straight

author: Fabio Ashtar Telarico, University of Ljubljana, FDV

version: 0.9

![Supply chaisn as a financial network.](https://github.com/FATelarico/FinNet/assets/100512813/4750186f-39d1-4ecd-b8b9-48351e26f3eb)
Source: McVey, Henry H., 'State of Play', KKR, Mar 24, 2022, [https://www.kkr.com/node/4025](https://www.kkr.com/node/4025)


# Introduction

There are a number of package helping users to deal with network data in `R`, most notably [`network`](https://statnet.org/) and [`igraph`](https://igraph.org/). However, these very popular R packages do not offer specific tools for working with financial networks. Yet, this is one of the frontiers of network-analysis and its application to economics (see [Haberley and Wojcik 2022](https://doi.org/10.1093/oso/9780198870982.003.0001))

For years now, authors and analysts have worked on financial data using *ad-hoc* tools or programming languages other than `R`. So, the package `FinNet` was born to provide all `R` users with the ability to study financial networks with a set of tool especially designed to this purpose. Specifically, `FinNet` offers both brand new tools and an interface to the almost limitless capabilities of `igraph` and `network`.

# Installation
```r
install.packages('FinNet')
```

# Features

At this stage of development, `FinNet` provides the following:

- Specialised S4 classes for financial agents (`firm`), their relations (`financial_matrix`), and interfaces to other packages (`network_financial` and `igraph_financial`);
- Functions to register information about financial agents, including retrieving them from Yahoo! Finance;
- Function to encode the relations between these agents and their owners or managers into incidence matrices;
- Function to encode the relations between these agents (based on common ownership, board interlocks, or both) into adjacency matrices;
- Function to encode the relations between these agents (based on common ownership, board interlocks, or both) into extended `network` or `igraph` objects.

## Example workflow

### From retrieving the data to an `igraph`/`sna` object

A very simple workflow using this version of `FinNet` includes:

After having identified the firms of interest, the package can fetch all information on them as long as `yahoofinancer` is available. So:


```r
# Check if `yahoofinancer` is installed
isTRUE(requireNamespace('yahoofinancer', quietly = TRUE))
```

```
## [1] TRUE
```

```r
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

```r
# Identify common-ownership relations in a firm-firm matrix
FF <- FF.norm.ownership(firms)

# Create a simple-looking graph
g <- FF.graph(FF, aesthetic = 'simple')
```

Some check using the S3 methods implemented for `financial_matrix` objects and the extension of some `igraph` functions allow to verify the correctness of the graph:


```r
# The order of the graph equals the number of rows in the FF matrix
vcount_fin(g) == nrow(FF)
```

```
## [1] TRUE
```

```r
# The names of its vertex match the row names of the FF matrix
V_fin(g)$name == rownames(FF)
```

```
## [1] TRUE TRUE TRUE TRUE
```


### Plotting using default *nice* aesthetics 

There are also useful defaults for a visual inspection of the network.


```r
# Load dataset
data('firms_BKB')

# Identify common-ownership relations in a firm-firm matrix
FF <- FinNet::FF(firms_BKB, who = 'own',
                 ties = 'naive', Matrix = TRUE)

# Create a nice-looking graph
g <- FF.graph(FF, aesthetic = 'nice')

# Plot it
plot_igraph_fin(g, vertex.label = NA, edge.arrow.size = .6, scale_vertex = 10)
```

![workflow_2-1](https://github.com/FATelarico/FinNet/assets/100512813/34bc0f42-807e-4781-b727-06f9340be62c)
