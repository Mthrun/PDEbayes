\name{getPriors}
\alias{getPriors}
\title{getPriors}
\description{
Get a prior via class proportions.
}
\usage{
getPriors(Cls)
}
\arguments{
\item{Cls}{
[1:n]  numerical vector with n numbers defining the classification. It has k unique numbers representing the arbitrary labels of the classification.
}
}
\value{
\item{Priors}
{[1:k] Numeric vector with prior probability for each class.}
}
\author{
Michael Thrun
}
\examples{
if(requireNamespace("FCPS")){
data(Hepta)
Data=Hepta$Data
Cls=Hepta$Cls
Priors=getPriors(Cls)
}
}
\keyword{Classification}
\keyword{Bayes}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}

