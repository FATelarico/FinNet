% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06.4.Bonacich.R
\name{bonacich}
\alias{bonacich}
\title{Compute Bonacich centrality on a Firm-Firm network}
\usage{
bonacich(FF, exo = 1, alpha = NULL, tol = 1e-07)
}
\arguments{
\item{FF}{An object of class \code{financial_matrix} or an object of class \code{igraph_financial} (or  \code{network_financial}) produced by \code{\link{FF}} and related functions to represent the relations between firms (legal person).}

\item{exo}{A value for the exogenous parameter of the Bonacich centrality. Defaults to \code{1}.}

\item{alpha}{A value for the alpha parameter of the Bonacich centrality. Defaults to a function of the matrix's largest eigenvalue.}

\item{tol}{A value for the tolerance of the Bonacich centrality. Defaults to \code{1e-07}.}
}
\value{
A scalar vector with the Bonacich centrality of each firm.
}
\description{
Based in part on the earlier work of Katz (1953) and Hubbell (1965),
Bonacich (1972a, 1972b) argued that a point's centrality in a network
should depend on three criteria: (1) the number of links to other
points; (2) the intensity of the links; and (3) the centrality of those
with whom one is linked. This centrality has been applied to board
interlocks for the first time by Mizruchi and Bunting (1981).
}
\examples{
# Find the Bonacich centralities for all the companies that Berkshire Hathaway holds
data('firms_BKB')
FF_BKB <- FF.norm.ownership(firms_BKB)
bonacich(FF_BKB)

}
\references{
Katz, Leo. 1953. “A new status index derived from sociometric analysis.” Psychometrika 18: 39-43 \doi{10.1007/BF02289026}.

Bonacich, Phillip. 1972a. “Technique for Analyzing Overlapping Memberships.” Sociological Methodology 4: 176–85. \doi{10.2307/270732}.

Bonacich, Phillip. 1972b. “Factoring and Weighting Approaches to Status Scores and Clique Identification.” The Journal of Mathematical Sociology 2 (1): 113–20. \doi{10.1080/0022250X.1972.9989806}.

Mizruchi, Mark S., and David Bunting. 1981. “Influence in Corporate Networks: An Examination of Four Measures.” Administrative Science Quarterly 26, no. 3: 475–89. \doi{10.2307/2392519}.
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
