GetLikelihoodFunction=function(Kernels_list,ListOfLikelihoods){

  PDFs_funs = list()
  d = length(Kernels_list)
  class_len = ncol(Kernels_list[[1]])
  for (i in 1:d) {
    c_pdf = list()
    c_kernel = list()
    c_theta = list()
    funs = list()
    KernelsMat = Kernels_list[[i]]
    LL = ListOfLikelihoods[[i]]
    for (cc in 1:class_len) {
      Kernels = KernelsMat[, cc]
      pdf = LL[, cc]
	   if(sum(is.finite(pdf))>2 &sum(is.finite(Kernels))>2){
		  smoothX  = seq(
			from = min(Kernels,na.rm=T),
			to = max(Kernels,na.rm=T),
			length.out = 1000
		  )
        Fun=stats::splinefun(x = Kernels, y = pdf,method="monoH.FC", ties = "ordered")
      }else{
        mm=min(Kernels,na.rm=T)
        nn=max(Kernels,na.rm = T)
        if(!is.finite(mm)) mm=0
        if(!is.finite(nn)) nn=1
        Fun=stats::approxfun(x = c(mm,nn), y = c(0,0),rule = 2, ties = "ordered")
      }
      funs[[cc]] = Fun
      #alles ist in reihenfolge der klassen anzuordnen!
    }
    PDFs_funs[[i]] = funs
  }#end for each class
  PDFs_funs[[i]] = funs
  return(PDFs_funs)
  
}#GetLikelihoodFunction