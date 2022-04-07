interpolateDistributionOnData=function(c_Kernels_list,c_PDFS_List,Data){
  # V=interpolateDistributionOnData(c_Kernels_list,c_PDFS_List,Data)
  # 
  # INPUT
  # c_Kernels_list    List of numeric vectors [1:m] with kernels
  # c_PDFS_List       List of numeric vectors [1:m] with pdf
  # Data[1:n,1:d]     Numeric matrix for data with n observations and d features.
  # 
  # OUTPUT
  # 
  # ListOfLikelihoods    List of m numeric matrices with l columns containing 
  #                      the distribution of class i in 1:l  # Thetas
  # 
  Header=colnames(c_PDFS_List[[1]])
  cl=ncol(c_PDFS_List[[1]])
  
  nr=nrow(Data)
  d=ncol(Data)

  PDFs=list()
  for(i in 1:d){
    Distribution_cur=c_PDFS_List[[i]]
    Kernels_cur=c_Kernels_list[[i]]
    
    mat_pdf=matrix(NaN,nrow = nr,ncol = cl)
    colnames(mat_pdf)=Header
    for(cc in 1:cl){
      ind=is.finite(Kernels_cur[,cc])
      c_pdf=stats::approx(Kernels_cur[ind,cc], Distribution_cur[ind,cc], xout =Data[,i],rule = 1, ties = "ordered")$y
      #scheint schlechter zu sein, als lineare approximation und zwar nicht nur an den enden
      #c_pdf=stats::spline(Kernels_cur[ind,cc], Distribution_cur[ind,cc], xout =Data[,i], ties = "ordered",method = "fmm")$y
      c_pdf[!is.finite(c_pdf)]=0#da wo wir in trainingsdaten keine dichte geschaetzt haben, ist die dichte null
      mat_pdf[1:length(c_pdf),cc]=c_pdf
    }
    PDFs[[i]]=mat_pdf
  }#end for 1.d
  names(PDFs)=colnames(Data)
  
  ListOfLikelihoods=PDFs
  return(ListOfLikelihoods)
}