\name{interpolateDistributionOnData}
\alias{interpolateDistributionOnData}
\title{interpolateDistributionOnData}
\description{
In contrast to the statistical likelihood which lie between zero and one here the pdf is estimated for which the integral has to be one, but a sepcific value may be above one.
}
\usage{
interpolateDistributionOnData(c_Kernels_list,c_PDFS_List,Data)
}
\arguments{
\item{c_Kernels_list}
{List of d numeric matrices, one per feature, each matrix with 1:k columns containing the kernels of class 1:k, each kernel is defined as the unique value of a feature within the data}
\item{c_PDFS_List}
{List of d numeric matrices, one per feature, each matrix with 1:k columns containing the estimated probability density function of class 1:k. Density is approximated for each kernel}
\item{Data}{
[1:n,1:d] matrix of training data. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.
}
}
\value{
\item{ListOfLikelihoods}
{List of m numeric matrices with l columns containing the distribution of class
i in 1:l.}
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

