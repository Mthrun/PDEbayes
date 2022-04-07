GetLikelihoods=function(Data,Cls,Gaussian=TRUE,...){
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
  #                      the distribution of class 1:k per feature 
  # Thetas: If Gaussian=TRUE:  List of d numeric matrices, one per feauture, each matrix with 1:k rows containing 
  #                      the mean in the first column and the standard deviation in teh seconf columd of class 1:k
  # 
  cl=sort(unique(Cls),decreasing = F)
  d=ncol(Data)
  #ToDo 3D array anstatt liste, da jedes element gleich gross
  PDFs=list()
  Kernels=list()
  Thetas=list()
  for(i in 1:d){
    c_pdf=list()
    c_kernel=list()
    c_theta=list()
    for(cc in cl){
      Classind=which(Cls==cc)
      c_pdf_list=defineOrEstimateDistribution(Data[,i],Classind,Gaussian=Gaussian,...)
      c_pdf[[cc]]=c_pdf_list$PDF
      c_kernel[[cc]]=c_pdf_list$Kernels
      if(isTRUE(Gaussian))
        c_theta[[cc]]=c_pdf_list$Theta
    }#end for each class
    if(isTRUE(Gaussian)){
      ThetaPerClass=do.call(rbind,c_theta)
      Thetas[[i]]=ThetaPerClass
    }else{
      ThetaPerClass=NULL
      Thetas=NULL
    }
    nr=max(sapply(c_pdf, length))
    mat_pdf=matrix(0,nrow = nr,ncol = length(cl))
    mat_kernels=matrix(Inf,nrow = nr,ncol = length(cl))
    colnames(mat_pdf)=paste0("C",cl)
    colnames(mat_kernels)=paste0("C",cl)
    for(cc in cl){
      cur_pdf=c_pdf[[cc]]
      cur_kernel=c_kernel[[cc]]
      mat_pdf[1:length(cur_pdf),cc]=cur_pdf
      mat_kernels[1:length(cur_kernel),cc]=cur_kernel
    }#end for each class
    PDFs[[i]]=mat_pdf
    Kernels[[i]]=mat_kernels
  }#end for each feature
  

   names(PDFs)=colnames(Data)
   names(Kernels)=colnames(Data)
  return(list(c_Kernels_list=Kernels,ListOfLikelihoods=PDFs,Thetas=Thetas))
}