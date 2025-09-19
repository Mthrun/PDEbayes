PlotLikelihoodFuns=function(LikelihoodFuns, Data, PlausibleLikelihoodFuns = NULL,
                            Epsilon = NULL, PlausibleCenters = NULL,
                            PlotCutOff = 4, xlim){
  # PlotLikelihoodFuns(Likelihoods,Data)
  # Input
  # Likelihoods            List of d numeric matrices, one per feature, each matrix with 1:k columns containing 
  #                             the distribution of class 1:k per feature, i.e., the Likelihood per class
  # Data                         numerical data matrix [1:n,1:k]
  # OPTIONAL
  # PlausibleLikelihoods     List of d numeric matrices of plausible Likelihood, one per feature, each matrix with [1:n,1:k] with k columns containing 
  #                             the distribution of class 1:k per feature 
  # Epsilon                      scalar defining epsiolon fo plausible likelihoods
  # PlausibleCenters             [d:k] plausible centers used to compute plausible likelihoods
  # PlotCutOff                   scalar defining the how many feature starting from 1 should ne plottet or numerical vector defining the index of features to be plottet
  #                             in second case should not be too many otherwise plot yields an error
  # OUTPUT
  # native plots in grid
  
  #author MCT 05/2025
  #reset par after plotting
  if(!is.matrix(Data)){
    Data=as.matrix(Data)
  }
  
  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  
  Likelihoods=interpolatePDF4Data(Data,LikelihoodFuns)
  
  if(!is.null(PlausibleLikelihoodFuns))
    PlausibleLikelihoods=interpolatePDF4Data(Data,PlausibleLikelihoodFuns)
  else
    PlausibleLikelihoods=NULL
  
    V=dim(Likelihoods)
    N=V[1]
    class_l=V[2]
    dinit=V[3]
  

  
  if(!is.null(PlotCutOff)){
    if(is.numeric(PlotCutOff)){
      if(length(PlotCutOff)==1){
        d=min(c(dinit,PlotCutOff),na.rm = T)
        rangePlots=1:d
      }else{
        rangePlots=PlotCutOff
        rangePlots[rangePlots>dinit]=dinit
        rangePlots=rangePlots[rangePlots>0]
        d=max(rangePlots,na.rm = T)-min(rangePlots,na.rm = T)+1
      }
    }else{
      rangePlots=1:dinit
      d=dinit
    }
  }else{
    rangePlots=1:dinit
    d=dinit
  }
  
  if(is.null(PlausibleLikelihoods)){
    par(mfrow = c(d, 1))  # d rows, 1 columns
  }else{
    par(mfrow = c(d, 2))  # d rows, 2 columns
  }
  
  if(isTRUE(requireNamespace("DataVisualizations",quietly = T))){
    Colors=DataVisualizations::DefaultColorSequence[1:class_l]
  }else{
    warning("PlotLikelihoods: Please install DataVisualizations package for color setting.")
    Colors=rep(c("black","blue","green","gold","magenta","red","grey","orange"),class_l)
    Colors=Colors[1:class_l]
  }
  
  #plot all dimensions
  
  for(f in rangePlots){
    x=Data[,f]
    ind=order(x,decreasing = F)
    fnam=colnames(Data)[f]
    if(is.list(Likelihoods)){
      LL=Likelihoods[[f]]
    }else{
      LL=Likelihoods[,,f]
    }
  
    maxY1=apply(LL,2,function(x) max(x[is.finite(x)]))
    
    if(!is.null(PlausibleLikelihoods)){
      if(is.list(Likelihoods)){
        LL2=PlausibleLikelihoods[[f]]
      }else{
        LL2=PlausibleLikelihoods[,,f]
      }
      maxY2=apply(LL2,2,function(x) max(x[is.finite(x)]))
      ylim=c(0,max(c(maxY1,maxY2)))
    }else{
      ylim=c(0,c(max(maxY1)))
    }
    
    if(missing(xlim)){
      plot(x[ind],LL[ind,1],type="l",xlab=paste("Feature",fnam),col=Colors[1],main="Bayesian",
           ylim=ylim,lwd=2,ylab="Class Likelihoods")
    }else{
      plot(x[ind],LL[ind,1],type="l",xlab=paste("Feature",fnam),col=Colors[1],main="Bayesian",
           ylim=ylim,lwd=2,ylab="Class Likelihoods",xlim=xlim)
    }
    for(i in 2:class_l){
      points(x[ind],LL[ind,i],type="l",col=Colors[i],ylim=ylim,lwd=2)
    }
    if(!is.null(PlausibleLikelihoods)){
      if(missing(xlim)){
        plot(x[ind],LL2[ind,1],type="l",xlab=paste("Feature",fnam),col=Colors[1],ylab="Class Likelihoods",
             main=paste0("Plausible Bayes with approx. Limit ",round(Epsilon[f]/class_l,3)),ylim=ylim,lwd=2)
      }else{
        plot(x[ind],LL2[ind,1],type="l",xlab=paste("Feature",fnam),col=Colors[1],ylab="Class Likelihoods",
             main=paste0("Plausible Bayes with approx. Limit ",round(Epsilon[f]/class_l,3)),ylim=ylim,lwd=2,xlim=xlim)
      }
  
      if(!is.null(PlausibleCenters)){
        PlausibleCenters_cur=PlausibleCenters[f,]
        abline(v=PlausibleCenters_cur[1],col=Colors[1])
      }
      for(i in 2:class_l){
        points(x[ind],LL2[ind,i],type="l",col=Colors[i],ylim=ylim,lwd=2)
        if(!is.null(PlausibleCenters)){
          abline(v=PlausibleCenters_cur[i],col=Colors[i])
        }
      }
    }#end in case of plausible
  }#end for rangePlots
}