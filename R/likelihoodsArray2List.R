likelihoodsArray2List=function(Likelihoods,ClassLength=NULL){

  #to do try: asplit
  if(is.array(Likelihoods)){
    if(is.null(ClassLength)){
      #we have an 3d array
      V=dim(Likelihoods)
      N=V[1]
      class_l=V[2]
      d=V[3]
    }else{
      #we have an 2 d array, i.e. the training data to define the kernels
      N=V[1]
      d=V[2]
      class_l=ClassLength
      Likelihoods=array(Likelihoods,dim = c(N,class_l,d))
    }

    ListOfLikelihoods=list()
    for(i in 1:d){
      cur=Likelihoods[,,i]
      if(N>1)
        ListOfLikelihoods[[i]]=cur
      else
        ListOfLikelihoods[[i]]=matrix(cur,nrow=1)
    }
    return(ListOfLikelihoods)
  }else{
    return(Likelihoods)
  }

}