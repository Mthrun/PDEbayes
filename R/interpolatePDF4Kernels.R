interpolatePDF4Kernels=function(KernelList,PDFfunList){
  
  matList <- Map(
    function(funs_i, kern_i) {
      # split into a list of column‐vector, each column repesents a class likelihood
      xs <- asplit(kern_i, 2)
      # apply each f to its x, mapply will give you a N×class_length matrix
      mapply(function(f, x) f(x),
             funs_i, xs,
             SIMPLIFY = TRUE, USE.NAMES = FALSE)
    },
    PDFfunList,
    KernelList
  )
  arr=simplify2array(matList)
  arr[arr<0]=0 #auch kleine schwankungen unter null nicht erlaubt
  # 2) stack those matrices along the 3rd dimension which is 1:d
  return(arr)
  
}