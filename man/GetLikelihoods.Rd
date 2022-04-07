\name{GetLikelihoods}
\alias{GetLikelihoods}
\title{GetLikelihoods}
\description{
Yields the likelioods per feauture and class as values of distribution either
defined by gaussian or estimated form the data using pareto density estimation.
}
\usage{
GetLikelihoods(Data,Cls,Gaussian=TRUE,\dots)
}
\arguments{
\item{Data}{[1:n,1:d] matrix of training data. It consists of n cases of
d-dimensional data points. Every case has d attributes, variables or features.}
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification.
It has k unique numbers representing the arbitrary labels of the
classification.}
\item{Gaussian}{(Optional: Default=TRUE). Assume gaussian distribution.}
\item{\dots}{Further arguements for \code{\link{defineOrEstimateDistribution}}
Robust=TRUE: robustly estimated gaussians
na.rm=TRUE: remove NaNs
Threshold: threshold for which the standard deviation cannot be smaller
(defaul 0.0001)}
}
\value{
\item{c_Kernels_list}{List of d numeric matrices, one per feature, each matrix
with 1:k columns containing 
  the kernels of class 1:k}
\item{ListOfLikelihoods}{
List of d numeric matrices, one per feature, each matrix with 1:k columns containing 
distribution values (likelihood) of class 1:k }
\item{Thetas}{If Gaussian=TRUE:  List of d numeric matrices, one per feauture,
each matrix with 1:k rows containing the mean in the first column and the
standard deviation in teh seconf columd of class 1:k Otherwise: NULL}
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

