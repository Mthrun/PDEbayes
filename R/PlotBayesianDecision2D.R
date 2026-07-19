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
  #1.editor mct 07/2026: bugfix in posterior color mapping, better unique point computation for d>2 cia median.
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
  
  # Validate and constrain posterior probabilities
  if (any(!is.finite(D))) {
    stop("PlotBayesianDecision2D: Posteriors contain non-finite values.")
  }
  
  if (any(D < 0 | D > 1)) {
    warning("Posterior values outside [0, 1] are being clamped.")
    D <- pmin(1, pmax(0, D))
  }
  
  if(missing(NoBins)){
    optNrOfBins=DataVisualizations::OptimalNoBins(D)
    optNrOfBins = min(c(100,optNrOfBins)) #
  }else{
    optNrOfBins=NoBins
  }
  # Use length.out rather than by to guarantee exactly NoBins + 1 edges
  edges <- seq(
    from = 0,
    to = 1,
    length.out = optNrOfBins + 1L
  )
  
  Cls <- findInterval(
    D,
    vec = edges,
    all.inside = TRUE
  )
  
  # Important: construct the palette for the complete probability range
  NumberColors <- optNrOfBins
  
  if(!requireNamespace("deldir",quietly = T)){
    stop("PlotBayesianDecision2D: Please install package deldir")
  }
  
  if(!requireNamespace("deldir",quietly = T)){
    stop("PlotBayesianDecision2D: Please install package deldir")
  }

  up = DatabionicSwarm::UniquePoints(cbind(X, Y))
  # Aggregate posteriors of observations sharing the same two-dimensional coordinates.
  Dunique = vapply(up$UniqueInd, function(i) {
    stats::median(D[up$Uniq2DatapointsInd == i])
  }, numeric(1))
  # CellColorPalette = colorRampPalette(c("white", "pink", "yellow", "orange", "red", "firebrick", "darkred"))
  
  if (missing(CellColorsOrPallette)) {
    anchors <- c(
      "#FFFFFF", "#FFF7BC", "#F7E225", "#FDAE61", "#F46D43", "#D73027", "#7F0000" )
    
    Cols <- grDevices::colorRampPalette(anchors, space = "Lab" )(NumberColors)
    
  } else if (is.function(CellColorsOrPallette)) {
    Cols <- CellColorsOrPallette(NumberColors)
    
  } else {
    supplied <- as.character(CellColorsOrPallette)
    
    if (length(supplied) == NumberColors) {
      # Interpret as one explicitly supplied color per bin
      Cols <- supplied
    } else if (length(supplied) >= 2L) {
      # Interpret a shorter vector as palette anchors
      Cols <- grDevices::colorRampPalette(supplied,space = "Lab" )(NumberColors)
    } else {
      stop("At least two palette colors are required.",.Call=TRUE)
    }
  }
  if (length(Cols) != NumberColors) {
    stop("Palette function did not return the requested number of colors.",.Call=TRUE)
  }
  #if(isTRUE(Simple)){
  Kernels <- head(edges, -1L) + diff(edges) / 2
  
  Output <- data.frame(
    Color    = Cols,
    Kernels  = Kernels,
    BinNo    = seq_len(NumberColors),
    Lower    = head(edges, -1L),
    Upper    = tail(edges, -1L),
    Observed = seq_len(NumberColors) %in% unique(Cls),
    stringsAsFactors = FALSE
  )  
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
    DelTri = deldir::deldir(X[up$UniqueInd],Y[up$UniqueInd],z = Dunique)
    VoronoiCells = deldir::tile.list(DelTri)
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
  
  voronoi_df = do.call(rbind, lapply(seq_along(VoronoiCells), function(i) {
    cell = VoronoiCells[[i]]
    cell_posterior = if (!is.null(cell$z)) cell$z else Dunique[cell$ptNum]
    cell_class = findInterval(cell_posterior, vec = edges, all.inside = TRUE)
    data.frame(PDEnaiveBayes.Voronoi.x = cell$x,
               PDEnaiveBayes.Voronoi.y = cell$y,
               PDEnaiveBayes.Voronoi.group = i, #color_label = labels[i],
               PDEnaiveBayes.Voronoi.fill = Cols[cell_class]  # Fill color associated with this cell's posterior
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


