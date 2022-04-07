\name{fitParameters}
\alias{fitParameters}
\title{fitParameters}
\description{
Fit gaussian parameters.
}
\usage{
fitParameters(Feature,ClassInd,Robust=FALSE,na.rm=TRUE,Threshold=0.0001)
}
\arguments{
\item{Feature}{[1:n] Numeric Vector}
\item{ClassInd}{Integer Vector with class indices}
\item{Robust}{(Optional: Default=FALSE). Robust computation if set to TRUE.}
\item{na.rm}{(Optional: Default=TRUE). Remove na.}
\item{Threshold}{(Optional: Default=0.00001).}
}
\value{
\item{Parameters}{[1:2] Numeric vector with Mean and Std.}
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

