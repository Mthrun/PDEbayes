Train_naiveBayes_multicore=function(cl=NULL,Data,Cls,Plausible=TRUE,Predict=FALSE,Priors, UseMemshare=FALSE,...){
  
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
  extra_args <- list(...)
  
  if (
    length(extra_args) > 0L &&
    (is.null(names(extra_args)) || any(!nzchar(names(extra_args))))
  ) {
    stop(
      "Train_naiveBayes_multicore: all arguments in ... must be named."
    )
  }
  if (
    "ParetoRadiusPerFeauture" %in% names(extra_args) &&
    length(extra_args$ParetoRadiusPerFeauture) > 1L
  ) {
    stop(
      "Train_naiveBayes_multicore: a feature-specific ",
      "ParetoRadiusPerFeauture is currently unsupported."
    )
  }
  N  <- nrow(Data)
  nc <- length(Cls)
  
  if (N != nc) {
    warning(
      "Train_naiveBayes_multicore: length of Cls unequal to ",
      "nrow(Data); shortening input."
    )
    
    n_keep <- min(N, nc)
    
    if (n_keep == 0L) {
      stop("Train_naiveBayes_multicore: no observations available.")
    }
    
    ind  <- seq_len(n_keep)
    Data <- Data[ind, , drop = FALSE]
    Cls  <- Cls[ind]
  }
  
  bool_fin <- is.finite(Cls)
  
  if (!all(bool_fin)) {
    warning(
      "Train_naiveBayes_multicore: deleting observations ",
      "with non-finite class labels."
    )
    
    Cls  <- Cls[bool_fin]
    Data <- Data[bool_fin, , drop = FALSE]
  }
  
  if (length(Cls) == 0L) {
    stop("Train_naiveBayes_multicore: no finite class labels remain.")
  }
  
  if (length(unique(Cls)) < 2L) {
    stop("Train_naiveBayes_multicore: at least two classes are required.")
  }
  
  if(missing(Priors))
    Priors=getPriors(Cls)
  
  feature_names <- colnames(Data)
  
  if (is.null(feature_names)) {
    feature_names <- paste0("X", seq_len(ncol(Data)))
    colnames(Data) <- feature_names
  }
  
  if(is.null(cl)){#for debugging purposes
    message("Train_naiveBayes_multicore: is called with a single core without memshare. 'cl=NULL' is only for debugging purposes. ")
    List=apply(Data,MARGIN = 2,function(x,Cls,Priors,Plausible,...){
      col=as.matrix(x)
      return(Train_naiveBayes(Data = col,Cls = Cls,Priors=Priors,Plausible=Plausible,Predict = FALSE,...)$Model)
    },Cls,Priors,Plausible,...)
  }else if (UseMemshare) {
    #memshare accepts only double in Vars
    if(isTRUE(Plausible)){
      Plausible_mem=1
    }else{
      Plausible_mem=0
    }
    stamp <- gsub(
      "[^0-9]",
      "",
      format(Sys.time(),"M%OS6")
    )
    mem_namespace <- sprintf(
      "PB_%x_%s",
      as.integer(Sys.getpid()),
      stamp
    )
    on.exit({
      memshare::memshare_gc(namespace = mem_namespace,cluster = cl)
    })
    
    List=memshare::memApply(X = Data, MARGIN = 2, FUN = function(x,Cls,Priors,Plausible,...) 
      PDEnaiveBayes::Train_naiveBayes(Data = x,Cls = Cls,Priors=Priors,Plausible=Plausible,Predict = FALSE,...)$Model, 
      NAMESPACE = mem_namespace, CLUSTER = cl,VARS = list(Cls=Cls,Priors=Priors,Plausible=Plausible_mem,...))


  } else {
    List=parallel::parApply(cl = cl,X = Data,MARGIN = 2,FUN = function(x,Cls,Priors,Plausible,...){
      col=as.matrix(x)
      m=PDEnaiveBayes::Train_naiveBayes(Data = col,Cls = Cls,Priors=Priors,Plausible=Plausible,Predict = FALSE,...)$Model
      return(m)
    },Cls,Priors,Plausible,...)
  }
  
  if (!is.list(List) || length(List) != ncol(Data)) {
    stop("Train_naiveBayes_multicore: A training backend did not return the correct model, use 'cl=NULL' for debugging.")
  }
  
  if(is.null(names(List))){
    #minor bur memshare: returned list ist without names sometimes
    names(List)=feature_names
  }
  valid_models <- vapply(
    List,
    function(x) is.list(x) && !is.null(names(x)),
    logical(1)
  )
  
  if (!all(valid_models)) {
    stop(
      "Train_naiveBayes_multicore: Feature training failed for: ",
      paste(feature_names[!valid_models], collapse = ", ")
    )
  }
  
  model_component_names <- names(List[[1L]])

  same_structure <- vapply(
    List,
    function(x) identical(names(x), model_component_names),
    logical(1)
  )
  
  if (!all(same_structure)) {
    stop("Train_naiveBayes_multicore: Per-feature models do not have identical components, use 'cl=NULL' for debugging.")
  }
  
  model=new.env()
  for(i in seq_along(model_component_names)){
    nestedlist_cur=lapply(List, "[[",i)
    if(length(nestedlist_cur[[1]][[1]])>1){
      flattened_list_cur <- lapply(nestedlist_cur, function(x) x[[1]])
      names(flattened_list_cur) <- names(nestedlist_cur)
    }else{
      flattened_list_cur=nestedlist_cur
    }
    assign(model_component_names[i],flattened_list_cur,envir = model)
  }
  model=as.list(model)
  
  Gaussian <- if ("Gaussian" %in% names(extra_args)) {
    isTRUE(extra_args$Gaussian)
  } else {
    FALSE
  }
  model$Plausible=Plausible && !Gaussian


  radii <- lapply(
    List,
    `[[`,
    "ParetoRadiusPerFeauture"
  )
  
  scalar_radii <- vapply(
    radii,
    function(x) {
      is.atomic(x) &&
        is.null(dim(x)) &&
        length(x) == 1L
    },
    logical(1)
  )
  
  matrix_radii <- vapply(
    radii,
    function(x) {
      is.matrix(x) && ncol(x) == 1L
    },
    logical(1)
  )
  
  if (all(scalar_radii)) {
    
    model$ParetoRadiusPerFeauture <- setNames(
      vapply(
        radii,
        function(x) as.numeric(x)[1L],
        numeric(1)
      ),
      feature_names
    )
    
  } else if (all(matrix_radii)) {
    
    model$ParetoRadiusPerFeauture <- do.call(
      cbind,
      radii
    )
    
    colnames(model$ParetoRadiusPerFeauture) <-
      feature_names
    
  } else {
    
    stop(
      "Train_naiveBayes_multicore: inconsistent ",
      "Pareto-radius structures between features."
    )
  }
  
  if(isTRUE(model$Plausible)){
    if(is.list(model$PlausibleCenters)){
      model$PlausibleCenters=do.call(rbind,model$PlausibleCenters)
      rownames(model$PlausibleCenters)=names(List)
    }
  }
  
  model$Priors <- List[[1L]]$Priors
  
  if ("Thetas" %in% names(model)) {
    theta_is_null <- vapply(
      model$Thetas,
      is.null,
      logical(1)
    )
    
    if (all(theta_is_null)) {
      model$Thetas <- NULL
    } else if (any(theta_is_null)) {
      stop(
        "Train_naiveBayes_multicore: Gaussian theta matrices ",
        "are missing for some features."
      )
    } else {
      names(model$Thetas) <- feature_names
    }
  }
  
  class(model) <- "PDEbayes"
  
  if(isTRUE(Predict)){
    res=Predict_naiveBayes(Data=Data,Model=model,  cl = cl,...)
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