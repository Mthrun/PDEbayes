\name{Train_naivBayes}
\alias{Train_naivBayes}
\title{Train_naivBayes}
\description{
Trains a Naive Bayes classifier.
}
\usage{
Train_naivBayes(Data, Cls, Gaussian = TRUE, ...)
}
\arguments{
\item{Data}{
[1:n,1:d] matrix of training data. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.
}
\item{Cls}{
[1:n]  numerical vector with n numbers defining the classification. It has k unique numbers representing the arbitrary labels of the classification.
}
\item{Gaussian}
{(Optional: Default=TRUE). Assume gaussian distribution.}
\item{\dots}
{Robust=TRUE: robustly estimated gaussians
na.rm=TRUE: remove NaNs
Threshold: threshold for which the standard deviation cannot be smaller (defaul 0.0001)}
}
\value{
\item{ClsTrain}
{[1:n]  numerical vector with n numbers defining the classification. It has k unique numbers representing the arbitrary labels of the classification.
}
\item{Posteriors}
{[1:n, 1:l] Numeric matrices with posterior probabilities.}
\item{Priors}
{[1:l] Numeric vector with prior probability for each class.}
\item{c_2List_Train}
{output of \link{\code{GetLikelihoods}}: a list of two elements of Kernels and Likelihoods per feature and class}
\item{Thetas}
{Parameters mean and standard deviation of the gaussian distributions per class and feaures}
}
\author{
Michael Thrun
}
\seealso{
\code{\link{Predict_naivBayes}}
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

