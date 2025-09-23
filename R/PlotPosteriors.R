PlotPosteriors=function(Data,Posteriors,Class=1,CellColorsOrPallette,Showpoints=TRUE){
# PlotPosteriors(Data,Posteriors)
#Input
#Data                         numerical data matrix [1:n,1:k]
# Posteriors[1:n, 1:l]    Numeric matrices with posterior probabilities
#                         according to the bayesian Theorem.
#OPTIONAL
# Class             scalar defining which class to look at
# Cellcolors        [1:unique(X,Y)] colors
# Showpoints         TRUE, Punkte werden angezeigt
  
#OUTPUT
# ggplot2 object
#Note: gridExtra has to be installed otherwise returns null with warning
#author MCT 05/2025
  
  if(!requireNamespace("gridExtra",quietly = T)){
    warning("PlotPosteriors: Please install package gridExtra")
    return(NULL)
  }
  if(!is.matrix(Data)){
    xlab = deparse1(substitute(Data)) #has to come before vectorization in next line
    Data=as.vector(Data)
    ind=order(Data)
    # plot(Data[ind],Posteriors[ind,1],col=DatabionicSwarm::DefaultColorSequence[3],type="l",lwd=2,xlab=xlab,ylab="Posteriors")
    # for(i in 2:ncol(Posteriors)){
    #   points(Data[ind],Posteriors[ind,i],col=DatabionicSwarm::DefaultColorSequence[3+i],type="l",lwd=2)
    # }
    X <- Data[ind]
    P <- Posteriors[ind, , drop = FALSE]
    if (is.null(colnames(P))) colnames(P) <- paste0("Class ", seq_len(ncol(P)))
    
    # Make a "long" data.frame using base R only
    long <- stack(as.data.frame(P))                  # columns -> rows; gives 'values' and 'ind'
    long$x    <- rep(X, times = ncol(P))             # replicate x for each class
    long$ord  <- rep(seq_along(X), times = ncol(P))  # order index to preserve original sequence
    long      <- long[order(long$ord), ]
    names(long)[names(long) == "values"] <- "posterior"
    names(long)[names(long) == "ind"]    <- "class"
    
    # Colors: indices 3, 4, 5, ... as in your base plot
    pal <- DataVisualizations::DefaultColorSequence[3 + 0:(ncol(P) - 1)]
    
    # ggplot2 issues of finding variables by names in datastructures
    # CMD check requires the variable to be known (initiated)
    # Naming the variables in a list/data.frame/... is not enough (for CMD check)
    x = NULL
    y = NULL
    posterior = NULL
    
    p=ggplot(long, aes(x = x, y = posterior, color = class, group = class)) +
      geom_path(linewidth = 1) +
      scale_color_manual(values = pal, name = "Class") +
      labs(x = xlab, y = "Posteriors") +
      theme_minimal()
    
    
    return(p)
  }else{
    ggobjList=c()
    d=ncol(Data)
    if(d==1){
      warning("PlotPosteriors: Data has to have more than one dimension. Cannot plot.")
      return(invisible(NULL))
    }
    k=1
    for(x in 1:d){
      if(x<d)
        for(y in (x+1):d){
          obj=PlotBayesianDecision2D(X = Data[,x], Y = Data[,y],
                                     Posteriors = Posteriors,
                                     Class = Class,
                                     CellColorsOrPallette = CellColorsOrPallette,
                                     Showpoints = Showpoints, PlotIt = F,
                                     xlab=colnames(Data)[x],
                                     ylab=colnames(Data)[y])$GGobj
          if(!is.null(obj)){
            ggobjList[k]=list(obj)
          }else{
            ggobjList[k]=list(ggplot2::ggplot()  )
          }

          k=k+1
        }
    }
    numPlots = length(ggobjList)
    ColNo=round(d/3,0)
    p <- do.call(gridExtra::grid.arrange, c(ggobjList, list(ncol = ColNo)))
    
    return(p)
  }
}