\name{predict.PDEbayes}
\alias{predict.PDEbayes}
\title{predict.PDEbayes}
\description{
Predict classification with naivee bayes model.
}
\usage{
predict.PDEbayes(object, newdata, type = c("class", "response","prob"), ...)
}
\arguments{
\item{object}{Model obtained from training routine in PDEnaiveBayes package.}
\item{newdata}{[1:n,1:d] matrix of test data. It consists of n cases of
d-dimensional data points. Every case has d attributes, variables or features.}
\item{type}{Optional parameter.}
\item{\dots}{
\code{Gaussian}: Optional: Default=TRUE). Assume gaussian distribution.

\code{Plausible}: (Optional: TRUE: uses plausble bayesian theorem, FALSE non-plausible bayesian theorem

\code{Type}: (Optional: default=1, 1 = original PDE, 2 = R native density estimation

\code{Threshold}: Threshold for which the standard deviation cannot be smaller (default =1e-12)

\code{PlotIt}: Optional: Default=FALSE, TRUE: Plots Likelihoods

\code{PlotCutOff}: Optional: Scalar indicating how many features (starting from 1) should be plotted, or a numerical vector specifying the indices of the features to plot.
Note: In the second case, avoid selecting too many features, as this may cause the plot to fail

\code{ParetoRadiusPerFeauture}: Optional [1:d] numerical vector for pareto radii computed priorly, see  \code{\link[DataVisualizations]{ParetoRadius}} or  \code{{ParetoRadius_fast}}

\code{cl}: Optional: a cluster object, created by parallel, if given and \code{ParetoRadiusPerFeauture} missing, then \code{ParetoRadiusPerFeauture} is compputed multicore otherwise single core


\code{Robust}: Optional: Default=FALSE, TRUE: robust estimation of mean and std in case of \code{Gaussian=TRUE}
}
}
\value{
\item{Cls}{Numeric vector with predicted class associated with newdata.}
}
\details{
The function is implemented in a way so that one can combine training and test data although it is intended to be applied on test data only.
}
\author{
Michael Thrun
}
\seealso{
\code{\link{Train_naiveBayes}}
}
\examples{
if(requireNamespace("FCPS")){
V=FCPS::ClusterChallenge("Hepta",1000)
Data=V$Hepta
Cls=V$Cls
ind=1:length(Cls)
indtrain=sample(ind,800)
indtest=setdiff(ind,indtrain)

model=Train_naiveBayes(Data[indtrain,],Cls[indtrain],Gaussian=FALSE)
ClsTrain=model$ClsTrain
table(Cls[indtrain],ClsTrain)

ClsTest=predict.PDEbayes(object = model, newdata = Data[indtest,])
table(Cls[indtest],ClsTest)
}
}
\keyword{Classification}
\keyword{Bayes}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}
