Train_naiveBayes_multicore=function(cl=NULL,Data,Cls,Predict=FALSE,Priors, UseMemshare=FALSE,...){
  
  if(!is.matrix(Data)){
    warning("Train_naiveBayes_multicore: Data is not matrix, calling as.matrix()")
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
  
  
  if(is.null(cl)){#for debugging purposes
    List=apply(Data,MARGIN = 2,function(x,Cls,...){
      col=as.matrix(x)
      return(Train_naiveBayes(Data = col,Cls = Cls,...)$Model)
    },Cls,...)
  }else if (UseMemshare) {
    # TODO: Make this handle "..."
    extra_args <- list(...)
    if (length(extra_args) > 1) {
      warning("When using memshare using the ellipsis operator is currently not supported! A workaround is given by pushing your parameters into the cluster as local variables.")
    }
    has_plausible <- "Plausible" %in% names(extra_args)
    if (has_plausible) {
      Plausible= dots$Plausible #kann true oder false sein
      #for memshare
      if(Plausible==TRUE) Plausible=1
      if(Plausible==FALSE) Plausible=0
    }else{
      Plausible=0
    }
    if(missing(Priors))
      Priors=getPriors(Cls)
    
    if(Predict==TRUE) Predict=1
    if(Predict==FALSE) Predict=0
    
    List=memshare::memApply(X = Data, MARGIN = 2, FUN = function(x,Cls,Priors,Predict,Plausible) PDEnaiveBayes::Train_naiveBayes(Data = x,Cls = Cls,Priors = Priors,Predict = Predict,Plausible=Plausible), NAMESPACE = "namespace", VARS = list(Cls=Cls,Priors=Priors,Predict=Predict,Plausible=Plausible))
  } else {
    List=parallel::parApply(cl = cl,X = Data,MARGIN = 2,FUN = function(x,Cls,...){
      col=as.matrix(x)
      m=NULL
      try({
        m=PDEnaiveBayes::Train_naiveBayes(Data = col,Cls = Cls,...)$Model
      })
      return(m)
    },Cls,...)
  }

  Varnames=names(List[[1]])
 
  model=new.env()
  for(i in 1:length(Varnames)){
    nestedlist_cur=lapply(List, "[[",i)
    if(length(nestedlist_cur[[1]][[1]])>1){
      flattened_list_cur <- lapply(nestedlist_cur, function(x) x[[1]])
      names(flattened_list_cur) <- names(nestedlist_cur)
    }else{
      flattened_list_cur=nestedlist_cur
    }
    assign(Varnames[i],flattened_list_cur,envir = model)
  }
  model=as.list(model)

  dots=list(...)
  has_plausible <- "Plausible" %in% names(dots)
  if (has_plausible) {
    model$Plausible= dots$Plausible #kann true oder false sein
  }else{
    model$Plausible=F
  }

  model$ParetoRadiusPerFeauture=unlist(model$ParetoRadiusPerFeauture)
  
  if(isTRUE(model$Plausible)){
    if(is.list(model$PlausibleCenters)){
      model$PlausibleCenters=do.call(rbind,model$PlausibleCenters)
      rownames(model$PlausibleCenters)=names(List)
    }
  }

  model$Priors=model$Priors[[1]]
  model$Thetas=unlist(model$Thetas)
  
  if(isTRUE(Predict)){
    res=Predict_naiveBayes(Data=Data,Model=model,...)
    return(list(
      Model=model,
      ClsTrain=res$Cls,
      Posteriors=res$Posteriors
    ))
  }else{
    return(list(
      Model=model,
      ClsTrain=NULL,
      Posteriors=NULL
    ))
  }
}
