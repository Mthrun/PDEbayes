\name{PlotPosteriors}
\alias{PlotPosteriors}
\title{PlotPosteriors}
\description{
Plots posteriors either using a panel of plots based on PlotBayesianDecision2D
or in 1D as a line plot [Stier et al., 2026].
}
\usage{
PlotPosteriors(Data, Posteriors, Class = 1,
CellColorsOrPallette,Showpoints=TRUE,NoBins,ShowLegend=TRUE)
}
\arguments{
\item{Data}{Either numeric matrix [1:n, 1:d] with data or one column of data.}
\item{Posteriors}{[1:n, 1:Class] matrix of posteriors.}
\item{Class}{Integer defining which class to look at if numeric matrix is given, for column of data all posteriors are overlayed in line plot.}
\item{CellColorsOrPallette}{Optional, Either a function defining the color palette of a
character vector or character vector of length NoBins stating colors.}
\item{Showpoints}{Optional, TRUE, points are displayed.}
\item{NoBins}{Optional, number of bins for class posteriori}
\item{ShowLegend}{Optional, TRUE, show one posterior legend for all pairwise plots}
}
\value{
\item{GGobj}{ggplot2 object containing 2D visualization of Posteriori.}
}
\details{
Plotting posteriors in one directions only often does not give any insight. The default option using PlotBayesianDecision2D os often more useful.
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
#default option
PlotPosteriors(Data = TrainX, Posteriors = VPDENB$Posteriors, Class = 1)

# alternative option
PlotPosteriors(Data = TrainX[,3], Posteriors = VPDENB$Posteriors)
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