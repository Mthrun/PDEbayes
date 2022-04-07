Predict_naivBayes=function(TestData,Priors,c_2List_Train,Thetas){
  # V=Predict_naivBayes(TestData,Priors,c_2List_Train,Thetas)
  # 
  # INPUT
  # TestData[1:n,1:d]         Numeric matrix for testdata with n observations and d features.
  # Priors[1:l]               Numeric vector with prior probability for each class.
  # c_2List_Train
  # Thetas                    List of what?
  # 
  # OUTPUT
  # ClsTest[1:n]         Numeric vector with predicted class labels
  # Posteriors[1:n,1:l]  Numeric matrix with posterior probability according to
  #                      the bayesian theorem.
  # ListOfLikelihoods    List of m numeric matrices with l columns containing 
  #                      the distribution of class i in 1:l
  # 
  if(missing(c_2List_Train)){
    if(missing(Thetas)){
      stop("Predict_naivBayes:Parameters Thetas are missing.")
    }else{
      class_len=length(Priors)
      ListOfLikelihoods=list()
      for(m in 1:ncol(TestData)){
        Feature=TestData[,m]
        Likelihood=list()
        ThetaPerClass=Thetas[[m]]
        for(cc in 1:class_len){
          me = ThetaPerClass[cc,1]
          std = ThetaPerClass[cc,2]
          Likelihood[[cc]]=dnorm(Feature,me,std)
        }
        ListOfLikelihoods[[m]]=do.call(cbind,Likelihood)
      }
    }
    PDFs=ListOfLikelihoods
  }else{
    c_Kernels_list=c_2List_Train$c_Kernels_list
    ListOfLikelihoods=c_2List_Train$ListOfLikelihoods
    PDFs=interpolateDistributionOnData(c_Kernels_list,ListOfLikelihoods,TestData)
  }

  Posterioris=ApplyBayesTheorem4Likelihoods(PDFs,Priors = Priors)
  #DefineDecisionBoundaries: TakeOptimalDecision with MAP
  cls_test=apply(Posterioris,1,which.max)
  if(!is.null(rownames(TestData))){
    names(cls_test)=rownames(TestData)
  }
  return(list(ClsTest=cls_test,Posteriors=Posterioris,ListOfLikelihoods=ListOfLikelihoods))
  
}