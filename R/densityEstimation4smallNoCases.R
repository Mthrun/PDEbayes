densityEstimation4smallNoCases=function(FeatureFull,ClassInd){
  
  FeatureClass=FeatureFull[ClassInd]
  FeatureClass = FeatureClass[!is.infinite(FeatureClass)]
  
  D = FeatureFull[!is.infinite(FeatureFull)]
 
  MinD = min(D, na.rm = TRUE)
  MaxD = max(D, na.rm = TRUE)
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
  optNrOfBins=DataVisualizations::OptimalNoBins(D)
  optNrOfBins = min(c(100,optNrOfBins)) #
  
  edges <- seq(MinD, MaxD, abs(MinD-MaxD)/optNrOfBins)
  bin_width <- diff(edges)[1L]
  
  bin_indices=findInterval(FeatureClass,vec = edges)
  
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