\name{ParetoRadius_faster}
\alias{ParetoRadius_faster}
\title{ParetoRadius_faster}
\description{
Predict classification with naivee bayes model.
}
\usage{
ParetoRadius_faster(Data, maximumNrSamples = 10000,
plotDistancePercentiles = FALSE, na.rm = TRUE)
}
\arguments{
\item{Data}{Numeric matrix [1:n,1:d], cases in rows, variables in columns.}
\item{maximumNrSamples}{max number for wich the distance calculation can be done, default 1000}
\item{plotDistancePercentiles}{Optional boolean. Should the plot of percentiles
of distances be done? TRUE (default) means yes, FALSE means no.}
\item{na.rm}{Optional boolean. Remove na (TRUE) or keep them (FALSE).}
}
\value{
\item{Cls}{Numeric vector with predicted class associated with newdata.}
}
\details{Function calculates the paretoRadius for passed gauss mixture model}
\author{Michael Thrun
}
\keyword{Classification}
\keyword{Bayes}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}
