\name{PlotNaiveBayes}
\alias{PlotNaiveBayes}
\title{PlotNaiveBayes}
\description{
Visualize the class-conditional distributions of the Pareto Density estimated
naive Bayes model (PDENB) [Stier et al., 2026].
}
\usage{
PlotNaiveBayes(Model, FeatureNames, ClassNames, DatasetName = "Data",
nrows = 1, FeatureOrder, NumFeaturesPerRow = 4, Colors,
IndividualFigures = FALSE)
}
\arguments{
\item{Model}{List with elements \code{Priors},\code{c_2List_Train}.}
\item{FeatureNames}{Character vector of names with a name for each feature
contained in the data used to create the naive bayes model.}
\item{ClassNames}{Character vector of class names to present in the legend
of the plots.}
\item{DatasetName}{Character title for each plot.}
\item{nrows}{Number of rows inside one plot.}
\item{FeatureOrder}{Numeric vector representing the order of the features
to be displayed.}
\item{NumFeaturesPerRow}{Maximum number of features to be displayed in one plot.}
\item{Colors}{Character vector of color names. The length of the vector must
be the same as the number of classes within the data modeled by the naive Bayes
classifier.}
\item{IndividualFigures}{Optional boolean: If set to TRUE, it returns a list
of the individual figures for customization.}
}
\value{
\item{Cls}{[1:n]  numerical vector with n numbers defining the
classification. It has k unique numbers representing the arbitrary labels of the
classification.}
\item{Posteriors}{[1:n, 1:l] Numeric matrices with posterior probabilities.}
\item{DataLikelihoodsPerClass}{list of length \code{d}, each element is a matrix \code{[1:n,1:k]} of interpolated class likelihoods per feature \code{d}}

}
\details{
Boundaries are assumed to be zero for plotting.
}
\author{
Quirin Stier
}
\examples{

Data = as.matrix(iris[,1:4])
Cls = as.numeric(iris[,5])
DatasetName = "Iris"

TrainIdx = c(17, 73, 46, 29, 68, 35, 131, 62, 132, 127, 71, 72, 
144, 99, 93, 13, 38, 21, 102, 53, 36, 111, 114, 96, 57, 74, 145, 
86, 3, 16, 52, 59, 140, 40, 122, 109, 6, 91, 79, 15, 108, 139, 
37, 76, 20, 115, 66, 28, 100, 117, 44, 78, 80, 150, 146, 142, 
9, 90, 45, 58, 134, 11, 87, 125, 141, 118, 136, 48, 124, 47, 
8, 27, 33, 92, 130, 54, 65, 104, 23, 98, 129, 123, 34, 128, 135, 
51, 64, 5, 94, 83, 42, 116, 101, 43, 7, 12, 82, 1, 84, 138, 2, 
56, 4, 106, 120)

TestIdx = c(60, 10, 75, 70, 81, 18, 97, 95, 67, 22, 55, 143, 
88, 24, 105, 26, 119, 31, 107, 63, 41, 61, 32, 147, 89, 14, 121, 
19, 113, 49, 126, 112, 25, 77, 137, 103, 50, 30, 149, 110, 39, 
69, 148, 85, 133)

TrainX = Data[TrainIdx, ]
TestX  = Data[TestIdx, ]
TrainY = Cls[TrainIdx]
TestY  = Cls[TestIdx]

VPDENB = Train_naiveBayes(Data = TrainX, Cls = TrainY, Plausible = FALSE)

FeatureNames = colnames(Data)

PlotNaiveBayes(Model = VPDENB$Model, FeatureNames = FeatureNames)
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