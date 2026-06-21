\name{Train_naiveBayes_multicore}
\alias{Train_naiveBayes_multicore}
\title{Train a Multicore Pareto Density Naive Bayes Classifier}
\description{
Trains a naive Bayes classifier using Gaussian likelihoods or nonparametric
Pareto density estimation (PDE), with an optional plausible correction for
low-evidence cases [Stier et al., 2026]. Model training can be distributed across a
parallel cluster. Shared-memory computation can be enabled to prevent memory
duplication among workers [Thrun and Märte, 2026].
}
\usage{
Train_naiveBayes_multicore(
  cl = NULL,
  Data,
  Cls,
  Plausible = TRUE,
  Predict = FALSE,
  Priors,
  UseMemshare = FALSE,
  ...
)
}
\arguments{
\item{cl}{
A cluster object, typically created with
\code{\link[parallel]{makeCluster}}. If \code{NULL}, the function runs in a
single R process; this mode is intended primarily for debugging.
}
\item{Data}{
A numeric \code{n} by \code{d} matrix containing \code{n} observations in
rows and \code{d} features in columns. Objects that are not matrices are
converted with \code{as.matrix()}.
}
\item{Cls}{
A numeric vector of length \code{n} containing the class label for each
observation. The vector must contain at least two distinct finite class labels.
If \code{Cls} is not numeric, the function attempts to convert it using
\pkg{FCPS}; otherwise it falls back to \code{as.numeric()}.
}
\item{Plausible}{
Logical. If \code{TRUE}, use the plausible naive Bayes extension described by
Stier et al. (2026) for low-evidence cases. If \code{FALSE}, use the standard
Bayes decision rule. The plausible adjustment is intended for non-Gaussian
likelihood models and requires \pkg{FCPS}.
}
\item{Predict}{
Logical. If \code{TRUE}, classify the training observations after fitting and
return their predicted classes and posterior probabilities. If \code{FALSE},
only the model is fitted; \code{ClsTrain} and \code{Posteriors} are returned as
\code{NULL}.
}
\item{Priors}{
Optional numeric vector of length \code{k} containing the prior probabilities
of the \code{k} classes. Names should correspond to the class labels. If
missing, priors are estimated from the filtered training labels in \code{Cls}.
}
\item{UseMemshare}{
Logical. If \code{TRUE} and \code{cl} is not \code{NULL}, use
\code{\link[memshare]{memApply}} so that memory can be shared within the
multicore computation. This can reduce duplication of large training objects
among workers and requires the \pkg{memshare} package. If \code{FALSE}, the
\pkg{parallel} backend is used.
}
\item{\dots}{
Additional named arguments passed to the underlying likelihood-estimation and
prediction functions. Common arguments include:

\describe{
\item{\code{Gaussian}}{
Logical; default \code{FALSE}. If \code{TRUE}, estimate a Gaussian likelihood
for every feature and class. If \code{FALSE}, estimate likelihoods
nonparametrically.
}
\item{\code{Type}}{
Integer selecting the nonparametric density estimator. \code{1} uses Pareto
density estimation, \code{2} uses \code{stats::density()}, and \code{3} uses
\code{stats::density()} with the Pareto radius as its bandwidth. The default is
\code{1}.
}
\item{\code{Threshold}}{
Numeric threshold used by the plausible-likelihood adjustment. The default in
\code{Train_naiveBayes()} is \code{1e-4}.
}
\item{\code{SD_Threshold}}{
Positive numeric lower bound used for the standard deviation during Gaussian
parameter estimation. The default is \code{0.001}.
}
\item{\code{ParetoRadiusPerFeauture}}{
Optional precomputed Pareto radius. The spelling of this argument is retained
for API compatibility. The current multicore wrapper supports a scalar value;
a feature-specific vector is not supported. A radius must be estimated using
training data only, for example with
\code{\link[DataVisualizations]{ParetoRadius}} or
\code{\link[DataVisualizations]{ParetoRadius_fast}}.
}
\item{\code{Robust}}{
Logical; default \code{FALSE}. If \code{TRUE} and \code{Gaussian = TRUE}, use
robust estimates of the class-wise means and standard deviations.
}
\item{\code{GlobalPR}}{
Logical; default \code{TRUE}. If \code{TRUE}, estimate one Pareto radius per
feature. If \code{FALSE}, estimate a separate Pareto radius for each class and
feature.
}
\item{\code{PlotIt}}{
Logical; default \code{FALSE}. If \code{TRUE}, plot the estimated likelihoods.
Plotting is intended for the single-process debugging mode
\code{cl = NULL}.
}
\item{\code{PlotCutOff}}{
A positive integer giving the number of leading features to plot, or an integer
vector containing the feature indices to plot. Selecting many features can
produce an excessively large plot.
}
}
}
}
\value{
A list with the following components:
\describe{
\item{\code{Model}}{
An object of class \code{PDEbayes} containing the fitted model. Depending on the
selected options, it contains:
\describe{
\item{\code{c_Kernels_list}}{
A list of length \code{d}. Each element is a matrix containing the density
kernels for one feature across the \code{k} classes.
}
\item{\code{ListOfLikelihoods}}{
A list of length \code{d}. Each element is a matrix containing the estimated
likelihood values for one feature across the \code{k} classes.
}
\item{\code{PDFs_funs}}{
A feature-indexed list of class-specific density functions.
}
\item{\code{ParetoRadiusPerFeauture}}{
For non-Gaussian models, either a numeric vector with one Pareto radius per
feature or, when \code{GlobalPR = FALSE}, a class-by-feature matrix. For
Gaussian models, this component is not used for likelihood estimation.
}
\item{\code{Thetas}}{
For Gaussian models, a list of length \code{d}. Each element is a \code{k} by
\code{2} matrix containing the class-wise means and standard deviations. It is
\code{NULL} for non-Gaussian models.
}
\item{\code{Priors}}{
A named numeric vector containing the class prior probabilities.
}
\item{\code{Plausible}}{
Logical value indicating whether plausible likelihood correction was requested.
}
\item{\code{PlausibleCenters}}{
When plausible correction is used, a \code{d} by \code{k} numeric matrix of
feature-wise class centers; otherwise \code{NULL}.
}
}
}
\item{\code{ClsTrain}}{
If \code{Predict = TRUE}, a numeric vector of length \code{n} containing the
predicted class labels for the training observations; otherwise \code{NULL}.
}
\item{\code{Posteriors}}{
If \code{Predict = TRUE}, an \code{n} by \code{k} matrix of posterior class
probabilities; otherwise \code{NULL}.
}
}
}
\details{
The function trains every column of \code{Data} independently
and combines the resulting components into one multivariate naive Bayes model.
With a non-\code{NULL} cluster, the feature-wise fits are evaluated in
parallel.

When \code{UseMemshare = FALSE}, the standard \pkg{parallel} backend is used
and worker processes can hold separate copies of the required objects. When
\code{UseMemshare = TRUE}, \pkg{memshare} provides shared-memory objects during
the multicore computation [Thrun and Märte, 2026]. The potential memory saving is greatest for large
training matrices and multiple workers. Memory sharing has no effect when
\code{cl = NULL}.

If the number of rows in \code{Data} differs from the length of \code{Cls}, the
inputs are shortened to their common length with a warning. Observations with
non-finite class labels are removed before default priors are estimated.

If \code{Data} has no column names, feature names of the form \code{X1},
\code{X2}, and so on are generated. Feature names are retained in the fitted
model components.

A precomputed Pareto radius can accelerate repeated fitting, such as during
cross-validation. To avoid information leakage, it must be estimated separately
within each training fold and never from the corresponding validation data.
}
\author{
Michael Thrun
}
\seealso{
\code{\link{Train_naiveBayes}},
\code{\link{Predict_naiveBayes}},
\code{\link{predict.PDEbayes}}
}
\examples{
data(Hepta)
Data <- Hepta$Data
Cls <- Hepta$Cls

## Single-process debugging mode
Vdebug <- Train_naiveBayes_multicore(
  cl = NULL,
  Data = Data,
  Cls = Cls,
  Plausible = FALSE,
  Gaussian = FALSE,
  Predict = TRUE
)

    if(requireNamespace("parallel")){
    cl = parallel::makeCluster(1)#set to number of cores >1
    #each core copies the memory
    V=Train_naiveBayes_multicore(cl=cl,Data=Data,Cls=Cls,
    Predict=TRUE,UseMemshare=FALSE)
    ClsTrain=V$ClsTrain
    table(Cls,ClsTrain)
    
    #each core shares the memory
    V=Train_naiveBayes_multicore(cl=cl,Data=Data,Cls=Cls,
    Predict=TRUE,UseMemshare=TRUE)
    ClsTrain=V$ClsTrain
    table(Cls,ClsTrain)
    on.exit(parallel::stopCluster(cl))
    }
}



\references{

[Stier et al., 2026]	Stier, Q.,Hoffmann, J. & Thrun, M. C.: Classifying with the Fine Structure of Distributions: Leveraging Distributional Information for Robust and Plausible Naïve Bayes, Machine Learning and Knowledge Extraction (MAKE), Vol. 8(1), 13, doi 10.3390/make8010013, MDPI, 2026. 

[Thrun and Märte, 2026] Thrun, M.C., Märte, J.: Memshare: Memory Sharing for Multicore Computation in R with an Application to Feature Selection by Mutual Information using PDE, The R Journal, Vol. 17(4), pp. 306 - 322, doi 10.32614/RJ-2025-043, 2026.

}

\keyword{Classification}
\keyword{Bayes}
\keyword{classif}
\keyword{models}
\concept{Pareto Density Estimation}
\concept{Pareto Law}
\concept{Kernel Density Estimation}
\concept{Bayesian Classifier}
\concept{Parallel Computing}
\concept{Shared Memory}
