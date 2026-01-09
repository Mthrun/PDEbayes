Train_naiveBayes=function(Data,Cls,Predict=TRUE,Priors,...){
  # V=Train_naiveBayes(Data,Cls,Gaussian=TRUE)
  # 
  # INPUT
  # Data[1:n,1:d]    Numeric matrix for data with n observations and d features.
  # Cls[1:n]         Numeric vector with k class labels
  # 
  # OPTIONAL
  # Predict           TRUE: yields ClsTrain and Posteriors, FALSE: yields only Model and Thetas
  #                   Note: Only if Predict TRUE, EvalPlausible can be true
  # Priors            numerical vector [1:k] defining the prior probabilities of k classes, if missing, estimated from Cls
  #                   
  # ... 
  # 
  # OUTPUT
  # Model                   Model that stores naive bayes if Gaussian=F according to the bayesian Theorem.     
  #                         is a list of
                            # Priors[1:l]             Numeric vector with prior probability for each class.
                            # Thetas
                            # PDFs_funs
                            # c_Kernels_list
                            # PlausibleCenters
  # ClsTrain[1:n]           if Predict=T: Numeric vector with training cls, otherwise NULL
  # Posteriors[1:n, 1:l]    if Predict=T: Numeric matrices with posterior probabilities, otherwise NULL
  
  # dots = list("Plausible" = TRUE, "Gaussian" = FALSE, "Type" = 1, "Threshold" = 1e-12, "PlotIt" = FALSE)
  ## memshare addition double to boolean
  if(Predict==1) Predict=TRUE
  if(Predict==0) Predict=FALSE
  
  #DAU catching ----
  if(!is.matrix(Data)){
    warning("Train_naiveBayes: Data is not matrix, calling as.matrix()")
    #otherwise in defineOrEstimateDistribution Error in xtfrm.data.frame(x) : cannot xtfrm data frames when sort is called on a columnd of df
    Data=as.matrix(Data)
  }
  
  if(!is.numeric(Cls)){
    warning("Train_naiveBayes: Cls is not numneric, trying to transform.")
    if(isTRUE(requireNamespace("FCPS",quietly = T))){
      V=FCPS::ClusterCreateClassification(Cls)
      Cls=V$Cls
      message(paste0(V$ClusterNames,collapse = " "))
      message("transformed to")
      message(paste0(names(V$ClusterNames),collapse = " "))
    }else{
      warning("Train_naiveBayes: Please install FCPS package for stable Cls transformation.")
      Cls=as.numeric(Cls)
    }
  }
  
  N=nrow(Data)
  nc=length(Cls)
  if(N!=nc){
    warning("Train_naiveBayes: length of Cls unequal nrow of data, please check input for correct learning. Shortening Input.")
    Data=Data[1:min(c(N,nc)),]
    Cls=Cls[1:min(c(N,nc))]
  }
  
  bool_fin=is.finite(Cls)
  if(sum(bool_fin)!=length(Cls)){
    warning("Train_naiveBayes: Not all elements in Cls are finite, deleting not finite ones.")
    Cls=Cls[bool_fin]
    Data=Data[bool_fin,]
  }


  #Argument catching ----
  dots=list(...)
  
  has_plausible <- "Plausible" %in% names(dots)
  has_Gaussian <- "Gaussian" %in% names(dots)
  has_Type <- "Type" %in% names(dots)
  has_Threshold <- "Threshold" %in% names(dots)
  has_PlotIt <- "PlotIt" %in% names(dots)
  has_PlotCutOff<- "PlotCutOff" %in% names(dots)
  has_globalPR <- "GlobalPR" %in% names(dots)
  
  if(has_globalPR){
    GlobalPR = dots$GlobalPR
  }else{
    GlobalPR = T
  }
  
  if (has_plausible) {
    Plausible= dots$Plausible #kann true oder false sein
    #for memshare
    if(Plausible==1) Plausible=TRUE
    if(Plausible==0) Plausible=FALSE
    
    EvalPlausible=FALSE
  }else{
    #wenn nicht eingestellt wird beides evaluiert
    # Plausible=F
    # PlausibleCenters=NULL
    Plausible=F
    EvalPlausible=TRUE
  }
  if (has_Gaussian) {
    Gaussian= dots$Gaussian
  }else{
    Gaussian=F
  }
  if (has_Threshold) {
    Threshold= dots$Threshold 
  }else{
    Threshold=1e-4
  }
  if (has_PlotIt) {
    PlotIt= dots$PlotIt 
  }else{
    PlotIt=FALSE
  }
  if (has_PlotCutOff) {
    PlotCutOff= dots$PlotCutOff
  }else{
    PlotCutOff=min(c(ncol(Data),4))
  }
  #Getting Priors and Defining Classes internally ----
  unique_classes=sort(unique(Cls),decreasing = F)#class ordering externally can be arbitrary
  if(missing(Priors)){
    Priors=getPriors(Cls)
  }else{
    if(is.null(names(Priors))){
      names(Priors)=unique_classes
      warning("Train_naiveBayes: Input parameter Priors is not named vector, assuming priors are in order 1:k.")
    }else{
      ii=match(as.numeric(names(Priors)),unique_classes)
      if(any(ii!=unique_classes)){
        warning("Train_naiveBayes: Given Priors have to be named numeric vector in order of sort(unique(Cls))")
      }
    }
  }
  

  
  if(any(unique_classes!=as.numeric(names(Priors)))){
    warning("Train_naiveBayes: Given Priors have to be named numeric vector in order of sort(unique(Cls))")
  }

  if(isTRUE(Plausible)&any(1:length(unique_classes)!=unique_classes)){
      if(isTRUE(requireNamespace("FCPS",quietly = T))){
        #make sure that internally Cls is ordered 1:k
        if(utils::packageVersion("FCPS")>"1.3.4"){
          Cls=FCPS::ClusterRedefine(Cls,NewLabels = 1:length(unique_classes),OldLabels =unique_classes,Silent = T)
        }else{
          warning("Train_naiveBayes: Please update FCPS for better stability of algorithm.")
        }
        #otherwise unsure if in PlausibleLikelihoods() FCPS::ClusterApply has to correct ordering
      }else{
        warning("Train_naiveBayes: Please install FCPS package for better stability of algorithm.")
      }
  }
  
  #Start Learning Process----
  c_2List=GetLikelihoods(Data = Data,Cls = Cls,...)
  Model=c_2List
  Model$Priors=Priors
  c_Kernels_list=c_2List$c_Kernels_list
  Thetas=c_2List$Thetas
  PDFs_funs=c_2List$PDFs_funs
  ParetoRadiusPerFeauture=c_2List$ParetoRadiusPerFeauture
  if(isFALSE(Gaussian)){
    #this makes sure that now the ListOfLikelihoods has the length of data
    ArrayOfLikelihoodsOnData=interpolatePDF4Data(Data,PDFs_funs)
    #return(ArrayOfLikelihoods)
    if(isTRUE(Plausible)|isTRUE(EvalPlausible)){
      PDFs_funs_PrePlausible=PDFs_funs
      V=KernelPlausibleLikelihoods(PDFs_funs = PDFs_funs,c_Kernels_list = c_Kernels_list,Data = Data,Cls = Cls,threshold=Threshold)
      PDFs_funs=V$PDFs_funs
      #PlausibleLikelihoods=V$PlausibleLikelihoods
      #Likelihoods_PrePlausible=V$Likelihoods_PrePlausible
      Epsilon=V$Epsilon
      PlausibleCenters=V$PlausibleCenters
      ArrayOfLikelihoodsOnData=interpolatePDF4Data(Data,PDFs_funs)
      Model$PDFs_funs=PDFs_funs
      #PlotLikelihoods(Likelihoods = Likelihoods_PrePlausible,Data,PlausibleLikelihoods = ArrayOfLikelihoodsOnData)
    }else{
      Epsilon=NULL
      PlausibleCenters=NULL
    }
    Model$PlausibleCenters=PlausibleCenters
  }else{
    #override
    EvalPlausible=FALSE
    Plausible=FALSE
    ArrayOfLikelihoodsOnData=listOfLikelihoods2Array(c_2List$ListOfLikelihoods)
  }

  if(isTRUE(PlotIt)){
    if(is.null(Epsilon)){
      PlotLikelihoodFuns(PDFs_funs,Data = Data,PlotCutOff=PlotCutOff)
    }else{
      PlotLikelihoodFuns(PDFs_funs_PrePlausible,Data = Data,PlausibleLikelihoodFuns = PDFs_funs,Epsilon=Epsilon,PlausibleCenters=PlausibleCenters,PlotCutOff=PlotCutOff)
    }
  }
  class(Model)="PDEbayes"
  if(isTRUE(Predict)){
  PosteriorsV1=ApplyBayesTheorem4Likelihoods(Likelihoods = ArrayOfLikelihoodsOnData,
                                             Priors = Priors, threshold = Threshold)
  Posteriors=PosteriorsV1$Posteriors
  cls_train_col=PosteriorsV1$Cls
  
  if(isTRUE(EvalPlausible)){
  #Decide if plausible is better ----
    Likelihoods_PrePlausibleOnData=interpolatePDF4Data(Data,PDFs_funs_PrePlausible)
    PosteriorsV_2=ApplyBayesTheorem4Likelihoods(Likelihoods = Likelihoods_PrePlausibleOnData,
                                              Priors = Priors, threshold = Threshold)
    Posteriors2=PosteriorsV_2$Posteriors
    cls_train_col2=PosteriorsV_2$Cls
    
    #decide based on shannon information
    #measures how diverse and balanced clusters are on average in each solution
    #skewed clusters get a lower entropy (i.e. all points in one cluster)
    #even if one cluster is small and several bigger, the average should be still
    # a good measure
    shan=FCPS::ClusterShannonInfo(cbind(cls_train_col,cls_train_col2))$MeanInfo
    if(shan[1]<shan[2]){
      cls_train_col=cls_train_col2
      Posteriors=Posteriors2
      Plausible=FALSE
      Model$PDFs_funs=PDFs_funs_PrePlausible
    }else{
      Plausible=TRUE
      Model$PDFs_funs=PDFs_funs
    }
  }
  Model$Plausible=Plausible
  
  if(isTRUE(requireNamespace("FCPS",quietly = T))){
    if(utils::packageVersion("FCPS")>"1.3.4"){
      cls_train=FCPS::ClusterRedefine(cls_train_col,NewLabels = unique_classes,OldLabels =1:ncol(Posteriors),Silent = T)
    }else{
      warning("please update FCPS")
      cls_train=cls_train_col
    }
  }else{
    warning("please install FCPS")
    cls_train=cls_train_col
  }

  if(!is.null(rownames(Data))){
    names(cls_train)=rownames(Data)
  }

  return(list(
    Model=Model,
    ClsTrain=cls_train,
    Posteriors=Posteriors
    ))
  }else{
    return(list(
      Model=Model,
      ClsTrain=NULL,
      Posteriors=NULL
    ))
  }
}