\name{PlotBayesianDecision2D}
\alias{PlotBayesianDecision2D}
\title{PlotBayesianDecision2D}
\description{
Plots estimation of decision boundary in a 2D slice of the data using the posteriors}
\usage{
PlotBayesianDecision2D(X, Y, Posteriors, Class = 1, NoBins,
CellColorsOrPallette, Showpoints = TRUE, xlim, ylim, xlab, ylab, main,
PlotIt = TRUE)
}
\arguments{
\item{X}{Numeric vector with point coordinates of first dimension of data selection.}
\item{Y}{Numeric vector with point coordinates of second dimension of data selection.}
\item{Posteriors}{[1:n, 1:Class] matrix of posteriors.}
\item{Class}{Optional,Integer defining which class to look at.}
\item{NoBins}{Optional,Number of bins for class posteriori.}
\item{CellColorsOrPallette}{Optional, Either a function defining the color palette of a
character vector or character vector of length NoBins stating colors.}
\item{Showpoints}{Optional, TRUE, points are displayed.}
\item{xlim}{Optional,Numeric vector of length 2 stating limits of x axis.}
\item{ylim}{Optional,Numeric vector of length 2 stating limits of y axis.}
\item{xlab}{Optional,Character stating name of x axis.}
\item{ylab}{Optional,Character stating name of y axis.}
\item{main}{Optional, Character name of title}
\item{PlotIt}{Optional, TRUE: prints GGPLOT2 object, FALSE: not shown plot.}
}
\value{
List of:
\item{Mapping}{List containing a map for colors, kernels and bin number.}
\item{GGobj}{ggplot2 object containing 2D visualization of Posteriori.}
}
\details{
Boundaries are assumed to be zero for plotting.
}
\author{
Michael Thrun
}
\examples{

Data = as.matrix(iris[,1:4])
Cls = as.numeric(iris[,5])

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

PlotBayesianDecision2D(X = TrainX[, 1], Y = TrainX[, 2],
Posteriors = VPDENB$Posteriors, Class = 1)

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