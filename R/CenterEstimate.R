CenterEstimate=function(Feature,na.rm=TRUE,smallNo=100,...){
  #estimates the center of likelihood based on statistical estimators cited below
  #mode=CenterEstimate(Feature)
  #INPUT 
  #Feature        a feature [1:N]
  #OPTIONAL
  # na.rm    TRUE: filters for finite values
  # smallNo   no of points considered for a sample to be small,default 100, afaik undefined in [Ekblom,1972]
  # ...     further parameters for package modeest
  #OUTPUT
  # Mode  scalar value of Mode
  #Author MCT 05/2025
  if(isTRUE(na.rm)){
    Feature=Feature[is.finite(Feature)]
  }  
  if(!requireNamespace("modeest",quietly = T)){
    warning("CenterEstimate: Please install package modeest for approriate Plausible Center estimation, returning mean")
    return(mean(Feature))
  }
  if(length(Feature)>=100){
    #Bickel, D. R., & Fruehwirth, R.: On a fast, robust estimator of the mode: Comparisons to other robust estimators with applications. Computational Statistics & Data Analysis, 50(12), 3500-3530,2006.
    return(modeest::hsm(x = Feature,...))
  }else if(length(Feature)<smallNo&length(Feature)>3){
    #Ekblom, H.: A Monte Carlo investigation of mode estimators in small samples. Journal of the Royal Statistical Society: Series C (Applied Statistics), 21(2), 177-184,1972
    return(modeest::shorth(x = Feature,...))
  }else if(length(Feature)==3){
    #Bickel, D. R., & Fruehwirth, R.: On a fast, robust estimator of the mode: Comparisons to other robust estimators with applications. Computational Statistics & Data Analysis, 50(12), 3500-3530,2006.
    return(median(Feature))
  }else if(length(Feature)==2){
    return(mean(Feature))
  }else if(length(Feature)==1){
    return(Feature)
  }else if(length(Feature)==0){
    warning("CenterEstimate: returning zero as feature contains no information.")
    return(0)
  }else{
    warning("CenterEstimate: returning zero because something went wrong.")
    return(0)
  }
}