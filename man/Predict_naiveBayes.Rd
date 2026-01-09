\name{Predict_naiveBayes}
\alias{Predict_naiveBayes}
\title{Predict_naiveBayes}
\description{
Predict classification with naive Bayes model [Stier et al., 2026].
}
\usage{
Predict_naiveBayes(Data, Model, \dots)
}
\arguments{
\item{Data}{[1:n,1:d] matrix of test data. It consists of n cases of
d-dimensional data points. Every case has d attributes, variables or features.}

\item{Model}{Optional, list with elements \code{Priors},\code{c_2List_Train},\code{Thetas}, alternative set arguements seperatly }
\item{\dots}{
\code{Priors}: Optional, if \code{Model} missing, then [1:k] Numeric vector with prior probability for each class.

\code{c_2List_Train}: Optional, if \code{Model} missing, then c_2List_Train is the output of GetLikelihoods: a list of two
three  elements of Kernels,  Likelihoods per feature and class, optional Thetas or PlausibleCenters depending on parameter setting

\code{Thetas}: Optional, if \code{Model} missing, then If c_2List_Train is missing, alternatively the parameters mean and
standard deviation of the gaussian distributions per class and feaures.

\code{PlotIt}: Optional: Default=FALSE, TRUE: Plots Likelihoods

\code{PlotCutOff}: Optional: Scalar indicating how many features (starting from 1) should be plotted, or a numerical vector specifying the indices of the features to plot.
Note: In the second case, avoid selecting too many features, as this may cause the plot to fail

}
}
\value{
\item{Cls}{[1:n]  numerical vector with n numbers defining the
classification. It has k unique numbers representing the arbitrary labels of the
classification.}
\item{Posteriors}{[1:n, 1:l] Numeric matrices with posterior probabilities.}
\item{DataLikelihoodsPerClass}{list of length \code{d}, each element is a matrix \code{[1:n,1:k]} of interpolated class likelihoods per feature \code{d}}

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

#PDEbayes
model=Train_naiveBayes(Data[indtrain,],Cls[indtrain],Gaussian=FALSE)
ClsTrain=model$ClsTrain
table(Cls[indtrain],ClsTrain)

res=Predict_naiveBayes(Data[indtest,], Model = model)
table(Cls[indtest],res$ClsTest)
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