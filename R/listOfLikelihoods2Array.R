listOfLikelihoods2Array=function(ListOfLikelihoods){
  #to do try:simplify2array
  if(is.list(ListOfLikelihoods)){
  d=length(ListOfLikelihoods)
  class_l=max(unique(sapply(ListOfLikelihoods, ncol)))
  N=max(unique(sapply(ListOfLikelihoods, nrow)))
  
  ArrayOfLikelihoods=array(NaN,c(N,class_l,d))
  for(i in 1:d){
    LL=ListOfLikelihoods[[i]]
    ArrayOfLikelihoods[1:nrow(LL),1:ncol(LL),i]=LL
  }
    return(ArrayOfLikelihoods)
  }else{
    return(ListOfLikelihoods)
  }
}