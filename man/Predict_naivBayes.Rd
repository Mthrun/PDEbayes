\name{Predict_naivBayes}
\alias{Predict_naivBayes}
\title{Apply_naivBayes}
\description{
Predict classification with naive bayes model.
}
\usage{
Predict_naivBayes(TestData,Priors,c_2List_Train,Thetas)
}
\arguments{
\item{TestData}
{[1:n,1:d] matrix of test data. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}
\item{Priors}
{[1:k] Numeric vector with prior probability for each class.}
\item{c_2List_Train}
{output of \link{\code{GetLikelihoods}}: a list of two elements of Kernels and Likelihoods per feature and class}
\item{Thetas}
{
If c_2List_Train is missing, alternatively the parameters mean and standard deviation of the gaussian distributions per class and feaures
}
}
\value{
\item{ClsTest}{
[1:n]  numerical vector with n numbers defining the classification. It has k unique numbers representing the arbitrary labels of the classification.
}
\item{Posteriors}{}
\item{ListOfLikelihoods}{}
}
\author{
Michael Thrun
}
\seealso{
\code{\link{Train_naivBayes}}
}
\examples{
if(requireNamespace("FCPS")){
V=FCPS::ClusterChallenge("Hepta",1000)
Data=V$Hepta
Cls=V$Cls
ind=1:length(Cls)
indtrain=sample(ind,800)
indtest=setdiff(ind,indtrain)
#parametric
model=Train_naivBayes(Data[indtrain,],Cls[indtrain],Gaussian=TRUE)
ClsTrain=model$ClsTrain
table(Cls[indtrain],ClsTrain)

res=Predict_naivBayes(Data[indtest,],model$Priors, Thetas=model$Thetas)
table(Cls[indtest],res$ClsTest)

#PDEbayes
model=Train_naivBayes(Data[indtrain,],Cls[indtrain],Gaussian=FALSE)
ClsTrain=model$ClsTrain
table(Cls[indtrain],ClsTrain)

res=Predict_naivBayes(Data[indtest,],model$Priors, c_2List_Train=model$c_2List_Train)
table(Cls[indtest],res$ClsTest)
}
}
\keyword{Classification}
\keyword{Bayes}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}
