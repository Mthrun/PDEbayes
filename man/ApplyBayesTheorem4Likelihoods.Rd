\name{ApplyBayesTheorem4Likelihoods}
\alias{ApplyBayesTheorem4Likelihoods}
\title{ApplyBayesTheorem4Likelihoods}
\description{
ApplyBayesTheorem4Likelihoods
}
\usage{
ApplyBayesTheorem4Likelihoods(ListOfLikelihoods,Priors,
threshold=0.00001)
}
\arguments{
\item{ListOfLikelihoods}{List of d numeric matrices, one per feature, each matrix with 1:k columns
containing the distribution of class 1:k}
\item{Priors}{[1:k] Numeric vector with prior probability for each class.}
\item{threshold}{(Optional: Default=0.00001).}
}
\value{
\item{Posteriors}{[1:n, 1:d] Numeric matrix with posterior probability according to the
bayes theorem.}
}
\author{
Michael Thrun
}
\examples{
if(requireNamespace("FCPS")){
data(Hepta)
Data=Hepta$Data
Cls=Hepta$Cls
#parametric
V=Train_naivBayes(Data,Cls,Gaussian=TRUE)
ClsTrain=V$ClsTrain
table(Cls,ClsTrain)

#non-parametric
V=Train_naivBayes(Data,Cls,Gaussian=FALSE)
ClsTrain=V$ClsTrain
table(Cls,ClsTrain)
}
}
\keyword{Classification}
\keyword{Bayes}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}

