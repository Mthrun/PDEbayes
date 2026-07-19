densityEstimation4smallNoCases=function(FeatureFull,ClassInd){
  
  FeatureClass=FeatureFull[ClassInd]
  FeatureClass = FeatureClass[is.finite(FeatureClass)]
  
  D = FeatureFull[is.finite(FeatureFull)]
  if(length(D)==0){
    warning("densityEstimation4smallNoCases: no finite values in current data column.")
    return(list("Kernels"       = c(0,1),
                "Density"      = c(0,0)))
  }
 
  MinD = min(D, na.rm = TRUE)
  MaxD = max(D, na.rm = TRUE)
  if(length(FeatureClass)==0){
    warning("densityEstimation4smallNoCases: no finite values in the current class.")
    if(MinD==MaxD)
      MaxD=MinD+1
    return(list("Kernels"       = c(MinD,MaxD),
                "Density"      = c(0,0)))
  }
  if(MinD==MaxD){
    warning("densityEstimation4smallNoCases: only one unique value in current data column. Please check input data")
    #one unique value in data
    #make sure that two kernels are given back
    if(is.finite(MinD)){
      if(MinD<0){
        return(list("Kernels"       = c(MinD,0),
                    "Density"      = c(1,0)))
      }else if(MinD>0){
        return(list("Kernels"       = c(0,MinD),
                    "Density"      = c(0,1)))
      }else{
        return(list("Kernels"       = c(MinD,1),
                    "Density"      = c(1,0)))
      }
    }else{
      return(list("Kernels"       = c(0,1),
                  "Density"      = c(0,0)))
    }
  }
  if(requireNamespace("DataVisualizations",quietly = TRUE))
    optNrOfBins=DataVisualizations::OptimalNoBins(D)
  else
    optNrOfBins=grDevices::nclass.scott(D)
  optNrOfBins = max(c(1,min(c(100,optNrOfBins)))) #
  
  edges = seq(MinD, MaxD, length.out = optNrOfBins+1L)
  bin_width <- diff(edges)[1L]
  
  bin_indices=findInterval(FeatureClass,vec = edges,all.inside = TRUE)
  
  bin_counts <- tabulate(bin_indices, nbins = optNrOfBins)
  
  Kernels=edges[-1]-diff(edges)/2

  Density=bin_counts/(sum(bin_counts) * bin_width)

  if(length(Kernels)==length(Density)){
    return(list("Kernels"       = Kernels,
                "Density"      = Density))
  }else{
    warning("densityEstimation4smallNoCases: unable to count values in current data column. Please check input data")
    Kernels=c(MinD,MaxD)
    return(list("Kernels"       = Kernels,
                "Density"      = c(0,0)))
  }

}