\name{Train_naiveBayes_multicore}
\alias{Train_naiveBayes_multicore}
\title{Train_naiveBayes_multicore}
\description{
Trains a Pareto Density estimated naive Bayes model (PDENB) with multicore parallelity
}
\usage{
Train_naiveBayes_multicore(cl=NULL,Data,Cls,Predict=FALSE,Priors,UseMemshare=FALSE,...)
}
\arguments{
\item{cl}{Object instance of package parallel.}
\item{Data}{\code{[1:n,1:d]} matrix of training data. It consists of \code{n}
cases of \code{d}-dimensional data points. Every case has\code{d} attributes,
variables or features.
}
\item{Cls}{\code{[1:n]}  numerical vector with n numbers defining the
classification. It has \code{k} unique numbers representing the arbitrary labels
of the classification.
}
\item{Predict}{Optional, boolean to decide extent of output. In case of TRUE,
yields ClsTrain and Posteriors, else it yields only Model and Thetas.
Note: Only if Predict is set to TRUE, parameter EvalPlausible can be set true!
}
\item{Priors}{Optional, \code{[1:k]}  numerical vector defining the prior
probabilities of the \code{k} classes. If missing, estimated from \code{Cls}.
}
\item{UseMemshare}{Optional boolean. If set to TRUE, then package functionality
from Memshare is used, else classic library parallel is used.
}
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

\code{GlobalPR}: Optional: Default=TRUE, FALSE: estimation of pareto radius for each class individually.

}
}

\value{
\item{Model}{List of model parameters and results.}
\item{c_Kernels_list}{List of matrices, where each matrix represent the kernels of one feature for all classes.}
\item{ListOfLikelihoods}{List of matrices, where each matrix represent the likelihood of one feature for all classes.}
\item{PDFs_funs}{Nested list of depth 1, where the first index assigns the
feature index and the second index assigns the class.
The elements are functions for the density estimation for each feature and each
class.}
\item{ParetoRadiusPerFeauture}{Numeric vector which stores the pareto radius
for each feature.}
\item{Theta}{Parameters mean and standard deviation of the Gaussian
distributions per class and feaures.}
\item{Priors}{Numeric vector which stores the prior probability of each class to appear.}
\item{PlausibleCenters}{[1:k, 1:f] Numeric matrix which stores the centers for
each feature and each class, where the row index assigns features and the column
index assigns classes.}
\item{ClsTrain}{[1:n]  numerical vector with n numbers defining the
classification. It has k unique numbers representing the arbitrary labels of the
classification.}
\item{Posteriors}{[1:n, 1:k] Numeric matrices with posterior probabilities.}
}
\details{
Precomputation of \code{ParetoRadiusPerFeauture} can be usefull to make cross-validation faster although it should be only done on the training data.

If \code{Plausible} is not given, both options are evalauted using shannon information.

\code{c_Kernels_list} and \code{ListOfLikelihoods} have \code{d} elements each storing a matrix \code{[1:m,1:k]}, usually \code{m!=n}. In contrast to \code{DataLikelihoodsPerClass} in which by interpolation the matrix are of size \code{[1:n,1:k]}
}
\author{
Michael Thrun
}
\seealso{
\code{\link{Predict_naiveBayes}}
}
\examples{
if(requireNamespace("FCPS")){
data(Hepta)
Data=Hepta$Data
Cls=Hepta$Cls

#non-parametric
V=Train_naiveBayes_multicore(cl=NULL,Data=Data,Cls=Cls,Gaussian=FALSE,Predict=TRUE)
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

\references{

[Stier et al., 2026]	Stier, Q.,Hoffmann, J. & Thrun, M. C.: Classifying with the Fine Structure of Distributions: Leveraging Distributional Information for Robust and Plausible Naïve Bayes, Machine Learning and Knowledge Extraction (MAKE), Vol. 8(1), 13, doi 10.3390/make8010013, MDPI, 2026. 
}