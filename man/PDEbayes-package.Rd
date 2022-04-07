\name{PDEbayes-package}
\alias{PDEbayes-package}
\docType{package}
\title{
\packageTitle{PDEbayes}
}
\description{
\packageDescription{PDEbayes}
}
\details{
Pareto Density Estimated Naïve Bayes Classifier \packageIndices{PDEbayes}
}
\author{
Michal Thrun

Maintainer: Michael Thrun <mthrun@informatik.uni-marburg.de>
}
\references{

[Thrun et al., 2020]  Thrun, M. C., Gehlert, T., & Ultsch, A.: Analyzing the Fine Structure of Distributions, PloS one, Vol. 15(10), pp. e0238835, doi 10.1371/journal.pone.0238835 	 2020.

[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, Vol. 30(C), pp. 105501, doi 10.1016/j.dib.2020.105501, 2020.

[Ultsch et al., 2015]  Ultsch, A., Thrun, M. C., Hansen-Goos, O., & Lötsch, J.: Identification of Molecular Fingerprints in Human Heat Pain Thresholds by Use of an Interactive Mixture Model R Toolbox (AdaptGauss), International journal of molecular sciences, Vol. 16(10), pp. 25897-25911, doi 10.3390/ijms161025897, 2015.
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