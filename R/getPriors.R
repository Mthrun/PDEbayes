getPriors=function(Cls){
  # V=getPriors(Cls)
  # 
  # INPUT
  # Cls[1:n]    Numeric vector with class labels 
  # 
  # OUTPUT
  # Priors[1:l]    Numeric vector with prior probability for each class.
  # 
  temp=table(Cls)
  Priors=as.vector(temp/length(Cls))
  names(Priors)=paste0("C",names(temp))
  return(Priors)
}