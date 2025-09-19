Predict_naiveBayes=function(Data,Model,...){
  # V=Predict_naiveBayes(Data,Priors,c_2List_Train,Thetas)
  # 
  # INPUT
  # Data[1:n,1:d]         Numeric matrix for Data with n observations and d features.
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
  if(!is.matrix(Data)){
    warning("Predict_naiveBayes: Data is not mattrix, calling as.matrix()")
   
    Data=as.matrix(Data)
  }
  
  #Getting Arguments ----
  dots=list(...)
  
  has_cl <- "cl" %in% names(dots)
  if (has_cl) {
    cl= dots$cl#multicore
  }else{#single core
    cl=NULL
  }
  
  
  has_plausible <- "Plausible" %in% names(dots)
  has_Threshold <- "Threshold" %in% names(dots)
  
  has_PlotIt <- "PlotIt" %in% names(dots)
  has_PlotCutOff<- "PlotCutOff" %in% names(dots)
  
  if (has_PlotIt) {
    PlotIt= dots$PlotIt 
  }else{
    PlotIt=FALSE
  }
  if (has_PlotCutOff) {
    PlotCutOff= dots$PlotCutOff 
  }else{
    PlotCutOff=min(c(4,ncol(Data)))
  }
  
  if (has_plausible) {

    Plausible= dots$Plausible 
  }else{
    Plausible=F
    PlausibleCenters=NULL
  }

  if (has_Threshold) {
    Threshold= dots$Type 
  }else{
    Threshold=.Machine$double.eps*1000
  }
  
  if(missing(Model)){
    Model=dots #somewhere here the input has to be stored
  }else{
    has_Model <- "Model" %in% names(Model)
    if (has_Model) {
      Model= Model$Model 
    }
  }
  
  has_Priors <- "Priors" %in% names(Model)
  has_Thetas <- "Thetas" %in% names(Model)
  has_PlausibleCenters <- "PlausibleCenters" %in% names(Model)
  if (has_PlausibleCenters) {
    
    PlausibleCenters= Model$PlausibleCenters 
  }else{
    PlausibleCenters=NULL
    if(isTRUE(Plausible)){
      warning("Predict_naiveBayes: Plausible=TRUE but list element PlausibleCenters is missing in c_2List_Train meaning that model was not train plausible, please correct. Setting Plausible to FALSE.")
      Plausible=FALSE
    }
  }
  if (has_Thetas) {
    Thetas= Model$Thetas 
  }else{
    Thetas=NULL
  }
  if (has_Priors) {
   Priors= Model$Priors 
  }else{
    stop("Priors are missing in input arguments.")
  }
  #legacy code for back compatibility
  has_c_2List_Train <- "c_2List_Train" %in% names(Model)
    if (has_c_2List_Train) {
  
        c_2List_Train= Model$c_2List_Train 
      
        has_Plausible2 <- "Plausible" %in% names(c_2List_Train)
        if (has_Plausible2) {
          #overide user setting
          Plausible= c_2List_Train$Plausible 
        }
        
        has_PlausibleCenters <- "PlausibleCenters" %in% names(c_2List_Train)
      if (has_PlausibleCenters) {
    
        PlausibleCenters= Model$PlausibleCenters 
      }else{
        PlausibleCenters=NULL
        if(isTRUE(Plausible)){
          warning("Predict_naiveBayes: Plausible=TRUE but list element PlausibleCenters is missing in c_2List_Train meaning that model was not train plausible, please correct. Setting Plausible to FALSE.")
          Plausible=FALSE
        }
      }
    }#missing has_c_2List_Train is caught below


  if(!is.null(Thetas)){
      # Likelihood generation for simple Gaussian model if ThetaPerClass given----
      class_len=length(Priors)
      ListOfLikelihoods=list()
      for(m in 1:ncol(Data)){
        Feature=Data[,m]
        Likelihood=list()
        ThetaPerClass=Thetas[[m]]
        for(cc in 1:class_len){
          me = ThetaPerClass[cc,1]
          std = ThetaPerClass[cc,2]
          Likelihood[[cc]]=dnorm(Feature,me,std)
        }
        ListOfLikelihoods[[m]]=do.call(cbind,Likelihood)
      }
      PDFs=ListOfLikelihoods
  }else{ #default case of ListOfLikelihoods given, then ThetaPerClass are ignored
    c_2List_Train=Model
    c_Kernels_list=c_2List_Train$c_Kernels_list
    PlausibleCenters=c_2List_Train$PlausibleCenters
    PDFs_funs=c_2List_Train$PDFs_funs
    #Error Catching ----
    d=ncol(Data)
    Header=colnames(Data)
    d2=length(PDFs_funs)
    d3=length(c_Kernels_list)
    if(d!=d2){
      warning("Predict_naiveBayes: columns of Data do not equal length of the list of PDFs_funs. Trying to match by colname(Data)...")
      Data=Data[,match(table = Header,names(PDFs_funs),nomatch = 0),drop=FALSE]
    }
    d=ncol(Data)
    
    if(d!=d2){
      stop("Predict_naiveBayes: number of data columns still does not equal length of the list PDFs_funs. Please correct the input.")
    }
    if(d!=d3){
      stop("Predict_naiveBayes: columns of data does not equal length of c_Kernels_list. Please correct the input.")
    }
    # case_length1=unique(sapply(ListOfLikelihoods, nrow))
    # case_length2=unique(sapply(c_Kernels_list, nrow))
    # if(length(case_length2)!=1){
    #   warning("Predict_naiveBayes: rows of matrices in c_Kernels_list vary, please check input data")
    # }
    # if(any(case_length1!=case_length2)){
    #   warning("Predict_naiveBayes: rows of matrices in ListOfLikelihoods do not equal rows of matrices in c_Kernels_list, please check input data.")
    # }

    #class_length1=unique(sapply(ListOfLikelihoods, ncol))
     class_length1=unique(sapply(PDFs_funs, length))
     class_length2=unique(sapply(c_Kernels_list, ncol))
    if(length(class_length1)!=1){
      warning("Predict_naiveBayes: columns of matrices in ListOfLikelihoods vary, please check input data")
    }
    if(length(class_length2)!=1){
      warning("Predict_naiveBayes: columns of matrices in c_Kernels_list vary, please check input data")
    }
    class_length=length(Priors)
    if(class_length!=class_length1){
      warning("Predict_naiveBayes: length of of Priors does not equal length of ListOfLikelihoods. Please correct the input")
    }
    
    #Prediction ----
    #PDFs=interpolateDistributionOnData(c_Kernels_list = c_Kernels_list,c_PDFS_List = ListOfLikelihoods,Data = Data,na.rm=TRUE)
    #PDFs=PDFsV$ListOfLikelihoods
    #PDFs  =PDFsV$ArrayOfLikelihoods

    if(is.null(cl)){#for debugging purposes
      PDFs=interpolatePDF4Data(Data,PDFs_funs)
    }else{
      PDFs=interpolatePDF4Data(Data = Data,PDFs_funs,cl)
    }
    

    # if(isTRUE(Plausible)){
    #   #ich muss die korrektur nochmal tun, dass ich PlausibleLikelihoods korrektur der trainingsdaten
    #   #nicht in der ausgabe mitnehme, da ich die PDE dichte schaetzung im original fuer
    #   #eine korrekte interpolateDistributionOnData auf neuen daten benoetige
    #   #ausgabe von dem resultut von PlausibleLikelihoods mit korrekten kernels in Train:naiveBayes
    #   #fuert zur inkorrekten interpolation, warum auch immer
    #   Likelihoods4Plot=PDFs
    #   V2=PlausibleLikelihoods(Likelihoods=PDFs,Data=Data,Cls=NULL,threshold=Threshold,c_PDFS_List=NULL,PlausibleCenters=PlausibleCenters)
    #   Epsilon=V2$Epsilon
    #   PDFs=V2$Likelihoods
    # }else{
      Epsilon=NULL
    # }
  }

  PosteriorsV=ApplyBayesTheorem4Likelihoods(Likelihoods = PDFs,Priors = Priors,threshold = Threshold)
  Posteriors=PosteriorsV$Posteriors
  cls_test_col=PosteriorsV$Cls
  #DefineDecisionBoundaries: TakeOptimalDecision with MAP
  #cls_test_col=max.col(Posteriors)#apply(Posteriors,1,which.max) #breaks ties randomly
  cls_train_lab=as.numeric(gsub("C","",names(Priors)))
  
  #ordering in input classes, which do not necessarly are 1:k
  if(isTRUE(requireNamespace("FCPS",quietly = T))){
    if(utils::packageVersion("FCPS")>"1.3.4")
      cls_test=FCPS::ClusterRedefine(cls_test_col,NewLabels = cls_train_lab,OldLabels =1:ncol(Posteriors),Silent = T)
    else{
      warning("Predict_naiveBayes: Please update FCPS")
      cls_test=cls_test_col
    }
  }else{
    warning("Predict_naiveBayes: Please install FCPS")
    cls_test=cls_test_col
  }
  if(!is.null(rownames(Data))){
    names(cls_test)=rownames(Data)
  }

  # Plotting ----
  if(isTRUE(PlotIt)){
    if(is.null(Epsilon)){
      PlotLikelihoods(PDFs,Data = Data,PlotCutOff=PlotCutOff)
    }else{
      PlotLikelihoods(PDFs, Data = Data, PlausibleLikelihoods = PDFs,
                      Epsilon = Epsilon, PlausibleCenters = PlausibleCenters,
                      PlotCutOff = PlotCutOff)
    }
  }
  
  return(list(Cls=cls_test,Posteriors=Posteriors,DataLikelihoodsPerClass=PDFs,ClsTest=cls_test)) #ClsTest for back compatibility, will be deleted in final version
  
}