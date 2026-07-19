PlotPosteriors=function(Data,Posteriors,Class=1,CellColorsOrPallette,Showpoints=TRUE,NoBins,ShowLegend=TRUE){
# PlotPosteriors(Data,Posteriors)
#Input
#Data                         numerical data matrix [1:n,1:k]
# Posteriors[1:n, 1:l]    Numeric matrices with posterior probabilities
#                         according to the bayesian Theorem.
#OPTIONAL
# Class             scalar defining which class to look at
# CellColorsOrPallette function defining the color palette or a character vector of colors
# Showpoints         TRUE, Punkte werden angezeigt
# NoBins             number of bins for class posteriori
# ShowLegend         TRUE, show one posterior legend for all pairwise plots
  
#OUTPUT
# ggplot2 object
#Note: gridExtra is required only for arranging multidimensional plots
#author MCT 05/2025
  
  if(!is.matrix(Posteriors)){
    Posteriors=as.matrix(Posteriors)
  }
  if(!is.matrix(Data)){
    xlab = deparse1(substitute(Data)) #has to come before vectorization in next line
    Data=as.vector(Data)
    ind=order(Data)
    # plot(Data[ind],Posteriors[ind,1],col=DatabionicSwarm::DefaultColorSequence[3],type="l",lwd=2,xlab=xlab,ylab="Posteriors")
    # for(i in 2:ncol(Posteriors)){
    #   points(Data[ind],Posteriors[ind,i],col=DatabionicSwarm::DefaultColorSequence[3+i],type="l",lwd=2)
    # }
    X = Data[ind]
    P = Posteriors[ind, , drop = FALSE]
    if (is.null(colnames(P))) colnames(P) = paste0("Class ", seq_len(ncol(P)))
    
    # Make a "long" data.frame using base R only
    long = stack(as.data.frame(P))                  # columns -> rows; gives 'values' and 'ind'
    long$x    = rep(X, times = ncol(P))             # replicate x for each class
    long$ord  = rep(seq_along(X), times = ncol(P))  # order index to preserve original sequence
    long      = long[order(long$ord), ]
    names(long)[names(long) == "values"] = "posterior"
    names(long)[names(long) == "ind"]    = "class"
    
    # Colors: indices 3, 4, 5, ... as in your base plot
    if(requireNamespace("DataVisualizations",quietly = TRUE)){
      pal = DataVisualizations::DefaultColorSequence[3 + 0:(ncol(P) - 1L)]
    }else{
      pal = rep(NA_character_,ncol(P))
    }
    if(length(pal)!=ncol(P) || anyNA(pal)){
      pal = grDevices::hcl.colors(ncol(P),palette = "Dark 3")
    }
    
    # ggplot2 issues of finding variables by names in datastructures
    # CMD check requires the variable to be known (initiated)
    # Naming the variables in a list/data.frame/... is not enough (for CMD check)
    x = NULL
    y = NULL
    posterior = NULL
    class = NULL
    
    p=ggplot(long, aes(x = x, y = posterior, color = class, group = class)) +
      geom_path(linewidth = 1) +
      scale_color_manual(values = pal, name = "Class") +
      labs(x = xlab, y = "Posterior") +
      coord_cartesian(ylim = c(0,1)) +
      theme_minimal()
    
    
    return(p)
  }else{
    d=ncol(Data)
    if(d<=1){
      warning("PlotPosteriors: Data has to have more than one dimension. Cannot plot.")
      return(invisible(NULL))
    }
    if(!requireNamespace("gridExtra",quietly = TRUE)){
      warning("PlotPosteriors: Please install package gridExtra")
      return(NULL)
    }
    feature_names=colnames(Data)
    if(is.null(feature_names)){
      feature_names=paste0("V",seq_len(d))
    }
    ggobjList=list()
    LegendMapping=NULL
    k=1
    for(x in 1:d){
      if(x<d)
        for(y in (x+1):d){
          Arguments=list(X = Data[,x], Y = Data[,y],
                         Posteriors = Posteriors,
                         Class = Class,
                         Showpoints = Showpoints, PlotIt = FALSE,
                         xlab=feature_names[x],
                         ylab=feature_names[y])
          if(!missing(NoBins)){
            Arguments$NoBins=NoBins
          }
          if(!missing(CellColorsOrPallette)){
            Arguments$CellColorsOrPallette=CellColorsOrPallette
          }
          Result=tryCatch(
            do.call(PlotBayesianDecision2D,Arguments),
            error=function(e){
              warning(paste0("PlotPosteriors: Could not create plot for ",
                             feature_names[x]," and ",feature_names[y],": ",
                             conditionMessage(e)),call. = FALSE)
              NULL
            }
          )
          if(isTRUE(ShowLegend) && is.null(LegendMapping) &&
             is.list(Result) && !is.null(Result$Mapping) &&
             "Color" %in% colnames(Result$Mapping) &&
             length(Result$Mapping$Color)>=2L){
            LegendMapping=Result$Mapping
          }
          if(is.list(Result) && !is.null(Result$GGobj)){
            obj=Result$GGobj
          }else{
            obj=NULL
          }
          if(!is.null(obj)){
            ggobjList[[k]]=obj
          }else{
            ggobjList[[k]]=ggplot2::ggplot()
          }

          k=k+1
        }
    }
    numPlots = length(ggobjList)
    ColNo=max(1L,ceiling(sqrt(numPlots)))

    LegendGrob=NULL
    if(isTRUE(ShowLegend) && !is.null(LegendMapping)){
      PDEnaiveBayes.legend.posterior=NULL
      LegendPlot=ggplot2::ggplot(
        data.frame(PDEnaiveBayes.legend.posterior=c(0,1)),
        ggplot2::aes(x=1,
                     y=PDEnaiveBayes.legend.posterior,
                     fill=PDEnaiveBayes.legend.posterior)
      ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradientn(
          colours=as.character(LegendMapping$Color),
          limits=c(0,1),
          breaks=c(0,0.25,0.5,0.75,1),
          name="Posterior"
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position="right")

      LegendTable=ggplot2::ggplotGrob(LegendPlot)
      LegendIndex=which(grepl("^guide-box",LegendTable$layout$name))
      LegendIndex=LegendIndex[
        !vapply(LegendTable$grobs[LegendIndex],
                inherits,logical(1),"zeroGrob")
      ]
      if(length(LegendIndex)>0L){
        LegendGrob=LegendTable$grobs[[LegendIndex[1L]]]
      }
    }

    ArrangeArguments=c(ggobjList,list(ncol=ColNo))
    if(!is.null(LegendGrob)){
      ArrangeArguments$right=LegendGrob
    }
    p=do.call(gridExtra::grid.arrange,ArrangeArguments)
    
    return(p)
  }
}
