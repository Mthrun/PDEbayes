\name{defineOrEstimateDistribution}
\alias{defineOrEstimateDistribution}
\title{defineOrEstimateDistribution}
\description{
The function estimates the distribution of values within a features that belong
to a specific class,i.e., the conditional probiablity of the likelihood
}
\usage{
defineOrEstimateDistribution(Feature,ClassInd,Gaussian=TRUE,Robust=FALSE,
PlotIt=FALSE,na.rm=TRUE,Threshold=0.0001)
}
\arguments{
\item{Feature}{[1:n] Numeric Vector}
\item{ClassInd}{Integer Vector with class indices}
\item{Gaussian}{(Optional: Default=TRUE). Assume gaussian distribution.}
\item{Robust}{(Optional: Default=FALSE). Robust computation if set to TRUE.}
\item{PlotIt}{(Optional: Default=FALSE). Create plot if set to TRUE.}
\item{na.rm}{(Optional: Default=TRUE). Remove na.}
\item{Threshold}{(Optional: Default=0.00001).}
}
\value{
\item{Kernels}{[1:m] Numeric vector with kernels (x-values) of a 1D pdf.}
\item{PDF}{[1:m] Numeric vector with the distribution values of a 1D pdf.}
\item{Theta}{Numeric vector with parameters of gaussian of mean and standard deviation -
NULL if no gaussian used.}
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

