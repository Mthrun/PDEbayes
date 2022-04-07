Train_naivBayes=function(Data,Cls,Gaussian=TRUE,...){
  # V=Train_naivBayes(Data,Cls,Gaussian=TRUE)
  # 
  # INPUT
  # Data[1:n,1:d]    Numeric matrix for data with n observations and d features.
  # Cls[1:n]         Numeric vector with class labels
  # 
  # OPTIONAL
  # Gaussian    (Optional: Default=TRUE). Assume gaussian distribution.         
  # 
  # OUTPUT
  # ClsTrain[1:n]           Numeric vector with training cls.
  # Posteriors[1:n, 1:l]    Numeric matrices with posterior probabilities
  #                         according to the bayesian Theorem.
  # Priors[1:l]             Numeric vector with prior probability for each class.
  # c_2List_Train
  # Thetas
  # 
  Priors=getPriors(Cls)
  
  c_2List_Train=GetLikelihoods(Data,Cls,Gaussian=Gaussian,...)
  c_Kernels_list=c_2List_Train$c_Kernels_list
  ListOfLikelihoods=c_2List_Train$ListOfLikelihoods
  Thetas=c_2List_Train$Thetas

  if(isFALSE(Gaussian)){
    ListOfLikelihoods=interpolateDistributionOnData(c_Kernels_list,ListOfLikelihoods,Data)
  }else{
    ListOfLikelihoods=ListOfLikelihoods
  }
  Posterioris=ApplyBayesTheorem4Likelihoods(ListOfLikelihoods,Priors = Priors)
  #DefineDecisionBoundaries: TakeOptimalDecision with MAP
  cls_train=apply(Posterioris,1,which.max)
  if(!is.null(rownames(Data))){
    names(cls_train)=rownames(Data)
  }
  return(list(ClsTrain=cls_train,
              Posteriors=Posterioris,
              Priors=Priors,
              c_2List_Train=c_2List_Train,
              ListOfLikelihoods=ListOfLikelihoods,
              Thetas=Thetas))
}