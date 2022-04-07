ApplyBayesTheorem4Likelihoods=function(ListOfLikelihoods,Priors,threshold=0.00001){
  # V=ApplyBayesTheorem4Likelihoods(ListOfLikelihoods,Priors,threshold=0.00001)
  # 
  # INPUT
  # ListOfLikelihoods    List of m numeric matrices with l columns containing 
  #                      the distribution of class i in 1:l
  # Priors[1:l]          Numeric vector with prior probability for each class.
  # 
  # OPTIONAL
  # threshold           (Optional: Default=0.00001).
  # NormalizeWithGMM    (Optional: Default=FALSE).
  # 
  # OUTPUT
  # Posteriors[1:n, 1:l]    Numeric matrices with posterior probabilities
  #                         according to the bayesian Theorem.
  # 
  
  class_l=length(Priors)
  LogPropMat=matrix(NaN,nrow = nrow(ListOfLikelihoods[[1]]),ncol = class_l)
  # if(isFALSE(NormalizeWithGMM)){ #priors werden direkt mit eingerechnet
  #   factor=1
  # }else{
  #   factor=0#priors werden spaeter mit eingerechnet
  # }
  
  for(cc in 1:class_l){

    probability=log(Priors[cc])*0
    for(f in 1:length(ListOfLikelihoods)){
      #bayes theorem ohne normierung, da die fuer MAP ein konstanter faktor
      #muss sieben zahlen pro case geben, falls sieben klassen gegeben
      distribution_current_Class=ListOfLikelihoods[[f]][,cc]
      vec=distribution_current_Class[!is.nan(distribution_current_Class)]
      vec[vec<threshold]=threshold
      probability=probability+log(vec)
    }
    
    LogPropMat[1:length(probability),cc]=probability
  }
  #naiver ansatz
  #Posteriors=exp(LogPropMat)
  
  #trick 17, 1 case
  # CaseOne=LogPropMat[1,]
  # Posteriors=c()
  # for(i in 1:length(CaseOne)){
  #   Posteriors[i]=1/sum(exp(CaseOne - CaseOne[i]))
  # }
  #trick 17, alle cases
  # if(isFALSE(NormalizeWithGMM)){
  #   Posteriors <- apply(LogPropMat, 2, function(x) { 1 / rowSums(exp(LogPropMat - x)) })
  # }else{  #bayes normierung wie bei gmmm
    PropMat=exp(LogPropMat)
    NormalizationFactor = PropMat %*% Priors
    ZeroInd <- which(NormalizationFactor == 0)
    if (length(ZeroInd) > 0) {
      NormalizationFactor[ZeroInd] = 10^(-7)
    }
    Posteriors <- PropMat*0
    for (i in c(1:class_l)) {
      Posteriors[, i] <- PropMat[, i] * Priors[i]/NormalizationFactor
    }
  #}

  
  return(Posteriors)
}