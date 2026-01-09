\name{GetLikelihoods}
\alias{GetLikelihoods}
\title{GetLikelihoods}
\description{
Yields the likelihoods per feauture and class as values of distribution either
defined by Gaussian or estimated form the data using pareto density estimation.
}
\usage{
GetLikelihoods(Data,Cls,\dots)
}
\arguments{
\item{Data}{[1:n,1:d] matrix of training data. It consists of n cases of
d-dimensional data points. Every case has d attributes, variables or features.}
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification.
It has k unique numbers representing the arbitrary labels of the
classification.}
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
\item{ParetoRadiusPerFeauture}{Numeric vector with estimated pareto radius
per feature.}
}
\details{
Due to pareto density estimation per class and  feature, usually the number of rows in each element of
\code{c_Kernels_list} and \code{ListOfLikelihoods} varies and does not equal the number of rows of data \code{n}.
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

\references{

[Stier et al., 2026]	Stier, Q.,Hoffmann, J. & Thrun, M. C.: Classifying with the Fine Structure of Distributions: Leveraging Distributional Information for Robust and Plausible Naïve Bayes, Machine Learning and Knowledge Extraction (MAKE), Vol. 8(1), 13, doi 10.3390/make8010013, MDPI, 2026. 
}