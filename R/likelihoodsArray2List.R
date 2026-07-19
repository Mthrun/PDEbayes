likelihoodsArray2List=function(Likelihoods,ClassLength=NULL){

  #to do try: asplit
  if(is.array(Likelihoods)){
    V=dim(Likelihoods)
    if(is.null(ClassLength)){
      #we have an 3d array
      N=V[1]
      class_l=V[2]
      d=V[3]
    }else{
      #we have an 2 d array, i.e. the training data to define the kernels
      N=V[1]
      class_l=ClassLength
      #nochmal spaeter dreuber nachdenken
	  #if(V[2]%%class_l!=0)
      #  stop("likelihoodsArray2List: number of columns is not divisible by ClassLength.")
      #d=V[2]/class_l
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