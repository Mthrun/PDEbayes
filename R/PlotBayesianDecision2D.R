PlotBayesianDecision2D=function(X, Y, Posteriors, Class = 1, NoBins,
                                CellColorsOrPallette, Showpoints = TRUE,
                                xlim, ylim, xlab, ylab, main, PlotIt = TRUE){
  #ggobj=PlotBayesianDecision2D(X,Y,Posteriors)
  # Zeichnet der Voronoizellen eingefaerbt mit den Posteriors
  #yellow ist sicher klasse 1 und rot sicher klasse 2
  #orange toene sind dazwischen
  #bei multiplen klassen ist es geringer posterior vs hoher posterior
  # ohne Border correction, so dass offene ungefaebten Randzellen zu sehen sind
  # INPUT
  # [X(1:n),Y(1:n)]      Punktkoordinaten
  # Posteriors(1:n,1:Class) matrix of posteriors
  #           
  # OPTIONAL
  # Class             scalar defining which class to look at
  # NoBins            number of bins for class posteriori
  # CellColorsOrPallette either a function defining the color palette of a character vector of colors of length NoBins
  # Showpoints         TRUE, Punkte werden angezeigt
  #xlim,yim
  #PlotIt             TRUE: prints GGPLOT2 object, FALSE: not shown plot
  # OUTPUT
  # GGobj             OBJECT OF GGPLOT2
  # Mapping           [1:m,1:3] matrix that maps colors to Posterior to range of Posteriors values (center of bin=Kernels) for which a color is defined
  #author mct 05/2025
  
  if(length(X)!=length(Y)){
    stop("PlotBayesianDecision2D: length of X does not equal length of Y.")
  }
  if(!is.matrix(Posteriors)){
    warning("PlotBayesianDecision2D: Posteriors is not a matrix, calling as.matrix(Posteriors)")
    Posteriors=as.matrix(Posteriors)
  }
  D=Posteriors[,Class]
  
  if(length(X)!=length(D)){
    stop("PlotBayesianDecision2D: length of X does not equal number of rows of Posteriors.")
  }
  if(sum(abs(X-Y))==0){
    warning("PlotBayesianDecision2D: X and Y are identical, cannot polt.")
    return(invisible(NULL))
  }
  if (missing(xlab)) 
    xlab = deparse1(substitute(X))
  if (missing(ylab)) 
    ylab = deparse1(substitute(Y))
  

  
  
  MinD = 0#min(D, na.rm = TRUE)
  MaxD = 1#max(D, na.rm = TRUE)
  if(missing(NoBins)){
    optNrOfBins=DataVisualizations::OptimalNoBins(D)
    optNrOfBins = min(c(100,optNrOfBins)) #
  }else{
    optNrOfBins=NoBins
  }
  #if(isTRUE(Simple)){
    edges = seq(from=MinD, to=MaxD, by=abs(MinD-MaxD)/optNrOfBins)
    bin_indices=findInterval(D,vec = edges,all.inside = T)
    Cls = bin_indices
  # }else{
  #  #dauert sehr lang
  #   Cls=ScatterDensity::DDCAL(D,nClusters = optNrOfBins)
  # }
  #Cls = FCPS::ClusterRenameDescendingSize(Cls)
  BinNo = sort(unique(Cls),decreasing = F)
  NumberColors=max(BinNo)
  if(!requireNamespace("deldir",quietly = T)){
    stop("PlotBayesianDecision2D: Please install package deldir")
  }
  
  if(!requireNamespace("deldir",quietly = T)){
    stop("PlotBayesianDecision2D: Please install package deldir")
  }

  up <- DatabionicSwarm::UniquePoints(cbind(X, Y))

  if(!missing(CellColorsOrPallette)){
    if(is.function(CellColorsOrPallette)){
          Cols      = CellColorsOrPallette(NumberColors)
          CellColor = Cols[Cls[up$UniqueInd]]
    }else{
      Cols      = CellColorsOrPallette[1:NumberColors]
      CellColor = Cols[Cls[up$UniqueInd]]
    }
  }else{
    CellColorPalette = colorRampPalette(c("white", "pink", "yellow", "orange", "red", "firebrick", "darkred"))
    #Cols = heat.colors(NumberColors)
    Cols = CellColorPalette(NumberColors)
    CellColor = Cols[Cls[up$UniqueInd]]#DataVisualizations::DefaultColorSequence[Cls[up$UniqueInd]]; # remove the cls entries for the removed duplicates and calculate colors
  }
  #if(isTRUE(Simple)){
  Kernels=edges[-1]-diff(edges)/2
  #should be of equal size but sometimes is not, to be checked
  Output=DataVisualizations::CombineCols(Color=Cols,Kernels=Kernels,BinNo=BinNo)
  colnames(Output)=c("Color","Kernels","BinNo")
  # }else{
  #   Output=NULL
  # }
  if(missing(xlim)){
    xlim=c(min(X),max(X))
  }
  if(missing(ylim)){
    ylim=c(min(Y),max(Y))
  }
  
  if(length(up$UniqueInd)<3){
    warning("PlotBayesianDecision2D: Not enough unique points in data. Doing nothing.")
    return(invisible(NULL))
  }

  out = try({
    DelTri <- deldir::deldir(X[up$UniqueInd],Y[up$UniqueInd])
    VoronoiCells <- deldir::tile.list(DelTri)
  })

  if (inherits(out, "try-error")) {
    return(invisible(list(GGobj=NULL,Mapping=Output)))
  }

#  labels <- factor(CellColor)
  
  # ggplot2 issues of finding variables by names in datastructures
  # CMD check requires the variable to be known (initiated)
  # Naming the variables in a list/data.frame/... is not enough (for CMD check)
  x = NULL
  y = NULL
  PDEnaiveBayes.Voronoi.x     = NULL
  PDEnaiveBayes.Voronoi.y     = NULL
  PDEnaiveBayes.Voronoi.x2    = NULL
  PDEnaiveBayes.Voronoi.y2    = NULL
  PDEnaiveBayes.Voronoi.group = NULL
  PDEnaiveBayes.Voronoi.fill  = NULL
  
  voronoi_df <- do.call(rbind, lapply(seq_along(VoronoiCells), function(i) {
    cell <- VoronoiCells[[i]]
    data.frame(PDEnaiveBayes.Voronoi.x = cell$x,
               PDEnaiveBayes.Voronoi.y = cell$y,
               PDEnaiveBayes.Voronoi.group = i, #color_label = labels[i],
               PDEnaiveBayes.Voronoi.fill = if (!is.null(CellColor)) CellColor[i] else NA  # Fill color if provided
               )
  }))
  
  #globalVariables(c("x", "y", "group", "fill"))
  
  ggobj=ggplot2::ggplot(voronoi_df, ggplot2::aes(x = PDEnaiveBayes.Voronoi.x,
                                                 y = PDEnaiveBayes.Voronoi.y,
                                                 group = PDEnaiveBayes.Voronoi.group,
                                                 fill = PDEnaiveBayes.Voronoi.fill)) +
    ggplot2::geom_polygon(color = "black", show.legend = FALSE) +
    ggplot2::scale_fill_identity() + # +  scale_fill_manual(values = levels(labels), name = "Region Color")
    ggplot2::coord_equal(xlim = xlim, ylim = ylim) +
    ggplot2::theme_minimal() + 
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  if (Showpoints){
    ggobj = ggobj + ggplot2::geom_point(data = data.frame(PDEnaiveBayes.Voronoi.x2 = X[up$UniqueInd],
                                                          PDEnaiveBayes.Voronoi.y2 = Y[up$UniqueInd]),
                                        ggplot2::aes(PDEnaiveBayes.Voronoi.x2,
                                                            PDEnaiveBayes.Voronoi.y2),
                                        inherit.aes = FALSE)
  }
  if(!missing(main)){
    ggobj=ggobj+ggplot2::ggtitle(main)
  }
  if(isTRUE(PlotIt)){
    print(ggobj)
  }
  return(invisible(list(GGobj=ggobj,Mapping=Output)))
  
}
#i see no sense in this
#globalVariables(c("PDEnaiveBayes.Voronoi.x", "PDEnaiveBayes.Voronoi.y",
#                  "PDEnaiveBayes.Voronoi.group", "PDEnaiveBayes.Voronoi.fill",
#                  "PDEnaiveBayes.Voronoi.x2", "PDEnaiveBayes.Voronoi.y2"))


