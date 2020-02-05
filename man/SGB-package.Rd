\name{SGB-package}
\alias{SGB-package}
\alias{SGB}
\docType{package}
\title{
Package SGB
}
\description{
 Package SGB contains a generalization of the Dirichlet distribution, called the Simplicial Generalized Beta (SGB). It is a new distribution on the simplex (i.e. on the space of compositions or positive vectors with sum of components equal to 1). The Dirichlet distribution can be constructed from a random vector of independent Gamma variables divided by their sum. The SGB follows the same construction with generalized Gamma instead of Gamma variables. The Dirichlet exponents are supplemented by an overall shape parameter and a vector of scales. The scale vector is itself a composition and can be modeled with auxiliary variables through a log-ratio transformation. 
}
\details{
\packageIndices{SGB}
}
\author{
Monique Graf

Maintainer: Monique Graf <monique.p.n.graf@bluewin.ch>
}
\references{
Graf, M. (2017). A distribution on the simplex of the Generalized Beta type. \emph{In J. A. Martin-Fernandez (Ed.), Proceedings CoDaWork 2017}, University of Girona (Spain), 71-90.

Graf, M. (2019). The Simplicial Generalized Beta distribution - R-package SGB and applications. \emph{Proceedings of the 8th International Workshop on Compositional Data Analysis (CoDaWork2019): Terrassa, 3-8 June, 2019. J.J. Egozcue, J. Graffelman and M.I. Ortego (Editors). Universitat Politecnica de Catalunya-BarcelonaTECH, 2019. 202 p. ISBN 978-84-947240-2-2. }.

Graf, M. (2020). Regression for compositions based on a generalization of the Dirichlet distribution. \emph{Statistical Methods & Applications}, (), 1-24.
}

\examples{
## Result of a regression object:
summary(oilr)
}

\keyword{Regression}
\keyword{Multivariate Techniques}
