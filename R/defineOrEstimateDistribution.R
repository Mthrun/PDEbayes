defineOrEstimateDistribution=function(Feature,ClassInd,Gaussian=TRUE,Robust=FALSE,PlotIt=FALSE,na.rm=TRUE,Threshold=0.0001){
  # V=defineOrEstimateDistribution(Feature,ClassInd,Gaussian=TRUE,Robust=FALSE,PlotIt=FALSE,na.rm=TRUE,Threshold=0.0001)
  # 
  # INPUT
  # Feature[1:n]    Numeric Vector
  # ClassInd        Integer Vector with class indices
  # 
  # OPTIONAL
  # Gaussian     (Optional: Default=TRUE). Assume gaussian distribution.         
  # Robust       (Optional: Default=FALSE). Robust computation if set to TRUE.
  # PlotIt       (Optional: Default=FALSE).
  # na.rm        (Optional: Default=TRUE). Remove na.
  # Threshold    (Optional: Default=0.00001). Threshold value for std.
  # 
  # OUTPUT
  # Kernels[1:m]    Numeric vector with domain of a 1D pdf
  # PDF[1:m]        Numeric vector of pdf
  # Theta           Numeric vector with parameters of gaussian - NULL if no gaussian used.
  # 
  if(isTRUE(Gaussian)){#define distibution
    Theta=fitParameters(Feature,ClassInd=ClassInd,Robust=Robust,na.rm=na.rm,Threshold=Threshold)
    pdf=dnorm(Feature,Theta[1],Theta[2])#already likelihood on given data
    Kernels=Feature
  }else{#estimate Distribution
    #Performanz haeng stark davon ab wie hier die kernels gewaehlt werden
    Theta=NULL
    Feature_Class=Feature[ClassInd]
    V=DataVisualizations::ParetoDensityEstimation(Feature_Class,kernels = sort(unique(Feature)),Silent = T)
    paretoRadius=V$paretoRadius
    pdf=V$paretoDensity 
    Kernels=V$kernels
  }
  
  if(isTRUE(PlotIt)){
   plot(Kernels,pdf,xlab="If Feature where in Class",ylab="PDF")
  }
  return(list(Kernels=Kernels,PDF=pdf,Theta=Theta))
}