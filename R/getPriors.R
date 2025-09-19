getPriors=function(Cls){
  # V=getPriors(Cls)
  # 
  # INPUT
  # Cls[1:n]    Numeric vector with class labels 
  # 
  # OUTPUT
  # Priors[1:l]    Numeric vector with prior probability for each class.
  # 
  # temp=table(Cls)
  # Priors=as.vector(temp/length(Cls))
  # names(Priors)=paste0("C",names(temp))
  
  V = rle(sort(Cls, method = "radix", decreasing = FALSE))
  countPerCluster = V$lengths
  uniqueClusters = V$values
  Priors=countPerCluster/sum(countPerCluster)
  names(Priors)=uniqueClusters
  
  return(Priors)
}