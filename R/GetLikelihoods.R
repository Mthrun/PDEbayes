GetLikelihoods=function(Data,Cls,...){
  # V=GetLikelihoods(Data,Cls,Gaussian=TRUE)
  # 
  # INPUT
  # Data[1:n,1:d]    Numeric matrix for data with n observations and d features.
  # Cls              Integer Vector with class lables
  # 
  # OPTIONAL
  # Gaussian     (Optional: Default=TRUE). Assume gaussian distribution.         
  # 
  # OUTPUT
  # c_Kernels_list       List of d numeric matrices, one per feature, each matrix with 1:k columns containing 
  #						 the kernels of class 1:k
  # ListOfLikelihoods    List of d numeric matrices, one per feature, each matrix with 1:k columns containing 
  #                      the distribution of class 1:k per feature, i.e., the Likelihood per class 
  # Thetas: If Gaussian=TRUE:  List of d numeric matrices, one per feauture, each matrix with 1:k rows containing 
  #                      the mean in the first column and the standard deviation in teh seconf columd of class 1:k

  dots=list(...)
  has_plausible <- "Plausible" %in% names(dots)
  has_Gaussian <- "Gaussian" %in% names(dots)
  if (has_Gaussian) {
    Gaussian= dots$Gaussian
  }else{
    Gaussian=F
  }

  unique_classes=sort(unique(Cls),decreasing = F)
  d=ncol(Data)
  
  #bottleneck des classifiers
  if(isFALSE(Gaussian)){
    has_ParetoRadiusVector<- "ParetoRadiusPerFeauture" %in% names(dots)
    if (has_ParetoRadiusVector) {
      ParVec= dots$ParetoRadiusPerFeauture 
    } else{
      has_cl<- "cl" %in% names(dots)
      if(has_cl & isTRUE(requireNamespace("parallel",quietly = T))){

        ParVec=parallel::parApply(cl = dots$cl,X = Data,MARGIN = 2, FUN = function(Feature){
          par=ParetoRadius_faster(Feature)
          return(par)
        })
      }else{
        ParVec=apply(Data,MARGIN = 2, function(Feature){
          par=ParetoRadius_faster(Feature)
          return(par)
        })
      }
    }
  }else{
    #will be ignored for gaussian bayes
    ParVec=rep(0,d)
  }

  #3D array anstatt liste geht nicht, da anzahl an rows sich hier noch aendert. erst in interpolate DistributionOnData koennte man ein 3D array wiedergeben
  # alternatrive ware max no rows abschaetzen und hinten mit NaN auffuellen s.DataVisualizations::CombineRows(), aber das ist vermutlich weniger effizient
  PDFs=list()
  PDFs_funs=list()
  Kernels=list()
  Thetas=list()
  for(i in 1:d){
    c_pdf=list()
    c_kernel=list()
    c_theta=list()
    funs=list()
    for(cc in 1:length(unique_classes)){    #alles ist in reihenfolge der klassen anzuordnen!
      Classind=which(Cls==unique_classes[cc])
      if(length(Classind)>0){
        c_pdf_list=defineOrEstimateDistribution(Feature = Data[,i,drop=F],
                                                ClassInd = Classind,
                                                ParetoRadius = ParVec[i], ...)
        c_pdf[[cc]]=c_pdf_list$PDF
        c_kernel[[cc]]=c_pdf_list$Kernels
        funs[[cc]]=c_pdf_list$PDF_fun
        if(cc==1)
          ParVec[i]=c_pdf_list$ParetoRadius
        if(isTRUE(Gaussian))
          c_theta[[cc]]=c_pdf_list$Theta
      }else{
        c_pdf[[cc]]=c(0,0)
        c_kernel=c(min(Data[,i],na.rm = T),max(Data[,i],na.rm = T))
        
        if(isTRUE(Gaussian))
          c_theta[[cc]]=c(mean(Data[,i],na.rm = T),sd(Data[,i],na.rm = T))
      }
    }#end for each class
    PDFs_funs[[i]]=funs
    if(isTRUE(Gaussian)){
      ThetaPerClass=do.call(rbind,c_theta)
      Thetas[[i]]=ThetaPerClass
    }else{
      ThetaPerClass=NULL
      Thetas=NULL
    }
    
    nr=max(sapply(c_pdf, length))
    mat_pdf=matrix(0,nrow = nr,ncol = length(unique_classes))
    mat_kernels=matrix(NaN,nrow = nr,ncol = length(unique_classes))

    colnames(mat_pdf)=paste0("C",unique_classes)
    colnames(mat_kernels)=paste0("C",unique_classes)

    
    for(cc in 1:length(unique_classes)){
      cur_pdf=c_pdf[[cc]]
      cur_kernel=c_kernel[[cc]]
      
      mat_pdf[1:length(cur_pdf),cc]=cur_pdf
      mat_kernels[1:length(cur_kernel),cc]=cur_kernel

    }#end for each class
    
    PDFs[[i]]=mat_pdf
    Kernels[[i]]=mat_kernels
  }#end for each feature
  
   names(PDFs_funs)=colnames(Data)
   names(ParVec)=colnames(Data)
   names(PDFs)=colnames(Data)
   names(Kernels)=colnames(Data)
  return(list(c_Kernels_list=Kernels,ListOfLikelihoods=PDFs,PDFs_funs=PDFs_funs,ParetoRadiusPerFeauture=ParVec,Thetas=Thetas))
}