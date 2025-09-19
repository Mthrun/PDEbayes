interpolatePDF4Data=function(Data,PDFfunList,cl){
  if(!is.matrix(Data)){
    Data=as.matrix(Data)
  }
  cl_length=length(PDFfunList[[1]])
  N=nrow(Data)
  d=ncol(Data)
  Array=array(NaN,c(N,cl_length,d))

  # if(missing(cl)){
   for(i in 1:d){
    for(cc in 1:cl_length){
        x=Data[,i]
        bb=!is.finite(x)
        if(sum(bb)==0){
          cpdf=PDFfunList[[i]][[cc]](x)
        }else{
          cpdf=rep(0,length(x))
          cpdf[!bb]=PDFfunList[[i]][[cc]](x[!bb])
        }
        cpdf[cpdf<0]=0 #auch kleine schwankungen unter null nicht erlaubt
        #plot(x,cpdf)
        Array[1:length(cpdf),cc,i]=cpdf
      }
    }
  # }else{
  # 
  #   Matrix=array(NaN,c(N,cl_length))
  #   #das lohnt sich nur bei sehr groessen N und D
  #   parallel::clusterExport(cl, c("Data","PDFfunList","cl_length","Matrix"))
  #   
  #   List=parallel::parLapply(cl,1:d,function(i,Matrix){
  #     for(cc in 1:cl_length){
  #       x=Data[,i]
  #       bb=!is.finite(x)
  #       if(sum(bb)==0){
  #         cpdf=PDFfunList[[i]][[cc]](x)
  #       }else{
  #         cpdf=rep(0,length(x))
  #         cpdf[!bb]=PDFfunList[[i]][[cc]](x[!bb])
  #       }
  #       cpdf[cpdf<0]=0 #auch kleine schwankungen unter null nicht erlaubt
  #       #plot(x,cpdf)
  #       Matrix[1:length(cpdf),cc]=cpdf
  #     }
  #     return(Matrix)
  #   },Matrix)
  #   
  #   for (i in seq_along(List)) {
  #     Array[ , , i] <- List[[i]]
  #   }
  # }

  return(Array)
}