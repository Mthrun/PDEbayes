ApplyBayesTheorem4Likelihoods=function(Likelihoods,Priors,threshold=.Machine$double.eps*1000){
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
  
  #__________________________________________________________________________________________  
  # Step 1: # Clip extremely small probabilities account for non finite
  #__________________________________________________________________________________________
  #beachte reihenfoge a,b,c) im vorgehen
  
  if(!is.list(Likelihoods)){
    V=dim(Likelihoods)
    N=V[1]
    class_l=V[2]
    d=V[3]
    Likelihoods[is.finite(Likelihoods) & Likelihoods>=1]=1-threshold #
    Likelihoods <- pmax(Likelihoods, threshold)#sehr wichtig fuer performanz
    #dann erst b)
    Likelihoods[!is.finite(Likelihoods)]=1
    #Likelihoods[Likelihoods==0]=1 #schwerer fehler
    #compute log probabilities in one simple step
    LogPropMat <- apply(log(Likelihoods), c(1, 2), sum)
    
    #print(LogPropMat)
  }else{#Likelihoods is a list (old legacy code)
    class_l=length(Priors)
    N=nrow(Likelihoods[[1]])#durch interpolateDistributionOnData ist jedes listenelement der gleichen laenge gleich laenge der daten
    d=length(Likelihoods)
    ListOfLikelihoods=Likelihoods
        #variante dauert  1369.24 
    # ListOfLikelihoods=lapply(ListOfLikelihoods,FUN = function(xmat,threshold){#iterate features
    #   xmat=apply(xmat, 2, function(xcol,threshold){#iterate classes
    #     #a)       #stelle sicher das kein log(1)=0 oder log(x)>0 fuer gueltig gemessene dichten exisitert, auch im peak
    #     xcol=pmin(xcol, 1 - threshold,na.rm = F) # distribution_current_Class[distribution_current_Class>=1]=1-threshold#b)
    #    # b)#stelle sicher das kein inf addiert wird
    #     xcol=pmax(xcol, threshold,na.rm = F)  # distribution_current_Class[distribution_current_Class<threshold]=threshold
    #     return(xcol)
    #  },threshold)
    #   #ist anscheinend nicht das gleiche wie das apply
    #   # xmat=Rfast::colPmin(x = xmat, y =  1 - threshold)
    #   # xmat=Rfast::colPmax(x = xmat, y = threshold)
    #    #c) erst danach! : #stelle sicher, das addition immer geht aber nicht beachtet wird, log(1)=0
    #    xmat[!is.finite(xmat)]=1
    #    return(xmat)
    #  },threshold)
    #_________________________________________________________________________
    # Step 2 compute Log (joint) Probabilities for every case ----
    #__________________________________________________________________________________________
    # LogPropMat=matrix(0,nrow =N ,ncol = class_l)
    # 
    # 
    # Loop over features (dimensions), summing log-likelihoods for each class
    # for (f in 1:d) {
    #   LogPropMat = LogPropMat + log(ListOfLikelihoods[[f]])
    # }
    
    LogPropMat=matrix(NaN,nrow =N ,ncol = class_l)
    #LogPropMat_plausible=matrix(NaN,nrow = nrow(ListOfLikelihoods[[1]]),ncol = class_l)
    for(cc in 1:class_l){ #variante dauert 601.02, d.h. nur 4 prozent der oberen zeit
      probability=log(Priors[cc])*0
      # if(!is.null(PlausibleCenters)){
      # probability_plausible=log(Priors[cc])*0
      # }
      for(f in 1:d){
        distribution_current_Class=ListOfLikelihoods[[f]][,cc]
        # #bayes theorem ohne normierung, da die fuer MAP ein konstanter faktor
        # #muss sieben zahlen pro case geben, falls sieben klassen gegeben
        #das ist falsch, nur area=1 reicht aus!
       # distribution_current_Class[distribution_current_Class>=1]=1-threshold #stelle sicher das kein log(1)=0 fuer gueltig gemessene dichten, auch im peak
        # #unwissende stellen setzen wir null, damit die reihenfolge in probability sich nicht aendert
        distribution_current_Class[is.finite(distribution_current_Class) & distribution_current_Class>=1]=1-threshold
        distribution_current_Class[!is.finite(distribution_current_Class)]=1#stelle sicher, das addition immer geht aber nicht beachtet wird, log(1)=0
        distribution_current_Class[distribution_current_Class<threshold]=threshold#stelle sicher das kein inf addiert wird
        
        #achtung zahlen kleiner 1 sind negativ, kleinen groesser 1 positiv
        #da die distribution_current_Class kleiner 1 sind, bis auf peaks
        #ist das hier noch keine wahrscheinlichkeit im eigentlichen sinne
        #dieser schritt ist die multipliktion der wahrscheinlichkeiten, d.h. features sind unabhaengig von einander
        probability=probability+log(distribution_current_Class)#minimal kommt hier raus d*threshold
        # if(!is.null(PlausibleCenters)){
        #   distribution_current_Class_plausible=ListOfLikelihoods_plausible[[f]][,cc]
        #   distribution_current_Class_plausible[distribution_current_Class_plausible>=1]=1-threshold #stelle sicher das kein log(1)=0 bei guetigen dichten
        #   #ungemessene dichten/unbekannte werden null
        #   distribution_current_Class_plausible[!is.finite(distribution_current_Class_plausible)]=1
        #   distribution_current_Class_plausible[distribution_current_Class_plausible<threshold]=threshold
        #
        #   probability_plausible=probability_plausible+log(distribution_current_Class_plausible)
        # }
      }#end for all dims
      LogPropMat[1:length(probability),cc]=probability
      # if(!is.null(PlausibleCenters)){
      #   LogPropMat_plausible[1:length(probability_plausible),cc]=probability_plausible
      # }
    }#end for all classes
    
  }
  
  #transform to probabilities
  PropMat=exp(LogPropMat)
  #Compute Evidence
  NormalizationFactor = PropMat %*% Priors
  

  ##Numerical correction of Evidence ----
  #minimum nach threshold oben ist d*threshold
  ZeroInd <- which(NormalizationFactor < threshold*d)
  
  if (length(ZeroInd) > 0) {
    NormalizationFactor[ZeroInd] = threshold*d
  }
  
  # if(!is.null(PlausibleCenters)){
  #   PropMat_plausible=exp(LogPropMat_plausible)
  #   NormalizationFactor_plausible = PropMat_plausible %*% Priors
  # 
  #   ZeroInd <- which(NormalizationFactor_plausible <2*d*threshold)
  #   
  #   if (length(ZeroInd) > 0) {
  #     NormalizationFactor_plausible[ZeroInd] = 2*d*threshold#10^(-7)
  #   }
  # }#end if(!is.null(PlausibleCenters))
  
  Posteriors <- PropMat*0
  ClassDecisionLogProb <- PropMat*0 #ignores normalization and remains in log for better numerical stability
 #____________________________________________________________________________
 # Step 3: Add class priors ----
 #____________________________________________________________________________

 # For every observation i and class c:
 #
 # ClassDecisionLogProb[i,c] =
 #   log p(x_i | C_c) + log P(C_c)
 #
 # This is the logarithm of the unnormalized posterior numerator.
  ClassDecisionLogProb = sweep(LogPropMat, 2, log(Priors), FUN = "+") 

#____________________________________________________________________________
 # Step 4: Compute the evidence safely on the logarithmic scale ----
 #____________________________________________________________________________
 # The evidence for observation i is
 #
 # p(x_i) =
 #   sum_c p(x_i | C_c) * P(C_c).
 #
 # A direct implementation such as
 #
 #   exp(ClassDecisionLogProb)
 #
 # can underflow to zero when many small feature likelihoods are multiplied.
 # The information is still present in ClassDecisionLogProb, so the evidence
 # is calculated with the log-sum-exp identity:
 #
 # log(sum_c(exp(a_c))) =
 #   m + log(sum_c(exp(a_c-m))),
 #
 # where m=max_c(a_c).
 #
 # Subtracting the row maximum ensures that the largest exponent is exp(0)=1.
 # Consequently, the normalization remains numerically stable even when the
 # original log-probabilities are very negative.
  max_log_prob=apply(ClassDecisionLogProb,1,max)
  log_NormalizationFactor=max_log_prob+
    log(rowSums(exp(sweep(ClassDecisionLogProb,1,max_log_prob,FUN = "-"))))

  #____________________________________________________________________________
 # Step 5: Compute normalized posterior probabilities ----
 #____________________________________________________________________________

 # On the logarithmic scale, division by the evidence becomes subtraction:
 #
 # log P(C_c | x_i) =
 #   log[p(x_i | C_c)P(C_c)] - log p(x_i).

  log_Posteriors = sweep(ClassDecisionLogProb, 1, log_NormalizationFactor, FUN = "-")
  Posteriors=exp(log_Posteriors)
  
  # for (i in c(1:class_l)) {
  #   Posteriors[, i] <- exp(LogPropMat[, i]  + log(Priors[i])-log(NormalizationFactor))
  #   ClassDecisionLogProb[, i] <- LogPropMat[, i] + log(Priors[i])
  #     
  # }#end for (i in c(1:class_l)) {

 #____________________________________________________________________________
 # Step 6: Maximum-a-posteriori class assignment ----
 #____________________________________________________________________________

 # Normalization is not required for the class decision because the evidence
 # is identical for all classes of one observation. The exponential function
 # is also strictly increasing, so the maximum logarithmic score gives the
 # same class as the maximum posterior probability.

  # if x1 > x2, then exp(x1) > exp(x2)
  #max.col resolves ties randomly
  Cls=max.col(ClassDecisionLogProb)#we do not need normalization for defining class
  
  # if(!is.null(PlausibleCenters)){
  #   #renormalisierung der posterioirs
  #   #dreht mir die matrix
  #   #Posteriors=apply(Posteriors,1,function(x) x/sum(x) )
  #   for(i in 1:nrow(Posteriors)){
  #     Posteriors[i,]=Posteriors[i,]/sum(Posteriors[i,])
  #   }
  # }
  return(list(Posteriors=Posteriors,Cls=Cls))
}