PlotNaiveBayes = function(Model,FeatureNames,ClassNames,DatasetName = "Data",nrows = 1,FeatureOrderOrSubset,NumFeaturesPerRow = 4,Colors, IndividualFigures = FALSE){
  
  Kernels     = Model$c_Kernels_list
  Likelihoods = Model$ListOfLikelihoods
  Priors      = Model$Priors
  
  if(missing(Colors)){
    Colors = c("blue",         "gold", "firebrick",  "limegreen",
               "dodgerblue", "orange",   "magenta",      "green",
               "cyan",          "red",      "plum",  "darkgreen",
               "turquoise",  "bisque",  "lavender", "lightgreen")
    
    Colors = c("black", #a
               "gold", #b
               "firebrick", #c
               "plum", #d
               "blue", "steelblue", # e1 e2
               "cyan", #f
               "purple", "magenta", #g1 g2
               "green", "limegreen",  "darkgreen", # h1 h2 h3
               "bisque",  # i
               "turquoise",  "orange", "lightgreen") # Remainder
  }
  
  if(length(Kernels) == 0L){
    stop("PlotNaiveBayes: Model$c_Kernels_list contains no features.")
  }
  
  NumClasses  = dim(Kernels[[1]])[2]
  NumFeatures = length(Kernels)
  
  if(missing(FeatureNames)){
    FeatureNames = paste0("X", seq_len(NumFeatures))
  }
  
  if(length(FeatureNames) != NumFeatures){
    stop(
      "PlotNaiveBayes:`FeatureNames` must have one name for every model feature: ",
      NumFeatures, " names are required."
    )
  }
  
  if(missing(FeatureOrderOrSubset)){
    FeatureOrderOrSubset = seq_len(NumFeatures)
  }
  
  if(!is.numeric(FeatureOrderOrSubset) || anyNA(FeatureOrderOrSubset) || any(FeatureOrderOrSubset %% 1 != 0)){
    stop("PlotNaiveBayes:`FeatureOrderOrSubset` must contain integer feature indices.")
  }
  
  FeatureOrderOrSubset = as.integer(FeatureOrderOrSubset)
  
  if(length(FeatureOrderOrSubset) == 0L){
    stop("PlotNaiveBayes:`FeatureOrderOrSubset` must select at least one feature.")
  }
  
  if(any(FeatureOrderOrSubset < 1L | FeatureOrderOrSubset > NumFeatures)){
    stop(
      "PlotNaiveBayes: Every value in `FeatureOrderOrSubset` must be between 1 and ",
      NumFeatures, "."
    )
  }
  
  if(anyDuplicated(FeatureOrderOrSubset)){
    stop("PlotNaiveBayes:`FeatureOrderOrSubset` must not contain duplicate indices.")
  }
  
  if(missing(ClassNames)){
    ClassNames = paste0("C", seq_len(NumClasses))
  }
  
  if(length(ClassNames) != NumClasses){
    stop(
      "PlotNaiveBayes:`ClassNames` must have one name for every class: ",
      NumClasses, " names are required."
    )
  }
  
  if(length(Priors) != NumClasses){
    stop("PlotNaiveBayes: The number of priors does not match the number of classes.")
  }
  
  if(length(Colors) < NumClasses){
    stop(
      "PlotNaiveBayes: At least ", NumClasses,
      " colors are required, but only ", length(Colors),
      " were supplied."
    )
  }
  
  if(length(NumFeaturesPerRow) != 1L || is.na(NumFeaturesPerRow) || NumFeaturesPerRow < 1L || NumFeaturesPerRow %% 1 != 0) {
    stop("PlotNaiveBayes:`NumFeaturesPerRow` must be a positive integer.")
  }
  
  NumFeaturesPerRow = as.integer(NumFeaturesPerRow)
  
  # Apply the class priors only to the selected likelihoods.
  for(FeatureIndex in FeatureOrderOrSubset){
    for(ClassIndex in seq_len(NumClasses)){
      Likelihoods[[FeatureIndex]][, ClassIndex] =
        Priors[ClassIndex] *
        Likelihoods[[FeatureIndex]][, ClassIndex]
    }
  }
  
  NumSelectedFeatures = length(FeatureOrderOrSubset)
  ListFigs            = vector("list", NumSelectedFeatures)
  
  # PlotPosition is the position within the requested subset/order.
  # FeatureIndex is the original feature index in the model.
  for(PlotPosition in seq_along(FeatureOrderOrSubset)){
    
    FeatureIndex = FeatureOrderOrSubset[PlotPosition]
    
    CurrKernel = Kernels[[FeatureIndex]]
    CurrLikeli = Likelihoods[[FeatureIndex]]
    
    if(nrow(CurrKernel) < 2L){
      stop("PlotNaiveBayes: Kernel for feature ", FeatureIndex,
        " must contain at least two rows."
      )
    }
    
    if(ncol(CurrKernel) != NumClasses || ncol(CurrLikeli) != NumClasses) {
      stop("PlotNaiveBayes: Kernel and likelihood matrices for feature ",
        FeatureIndex,
        " must have one column per class."
      )
    }
    
    if (nrow(CurrKernel) != nrow(CurrLikeli)) {
      stop(
        "Kernel and likelihood matrices for feature ",
        FeatureIndex,
        " have different numbers of rows."
      )
    }
    
    # Extend the curves so the filled regions return to zero.
    Diff = CurrKernel[2, 1] - CurrKernel[1, 1]
    
    CurrKernel = rbind(
      CurrKernel[1, ] - Diff,
      CurrKernel,
      CurrKernel[nrow(CurrKernel), ] + Diff
    )
    
    CurrLikeli = rbind(
      rep(0, NumClasses),
      CurrLikeli,
      rep(0, NumClasses)
    )
    
    # Show the legend on the final figure of each row.
    IsEndOfRow = PlotPosition %% NumFeaturesPerRow == 0L
    IsLastPlot = PlotPosition == NumSelectedFeatures
    ShowLegend = IsEndOfRow || IsLastPlot
    
    fig = plotly::plot_ly(fill = "tozeroy")
    
    for(ClassIndex in seq_len(NumClasses)){
      
      RgbaLine = paste0(
        "rgba(",
        paste0(grDevices::col2rgb(Colors[ClassIndex]), collapse = ", "),
        ", 1)"
      )
      
      RgbaFill = paste0(
        "rgba(",
        paste0(grDevices::col2rgb(Colors[ClassIndex]), collapse = ", "),
        ", 0.2)"
      )
      
      fig = plotly::add_trace(
        p           = fig,
        y           = CurrKernel[, ClassIndex],
        x           = CurrLikeli[, ClassIndex],
        type        = "scatter",
        mode        = "lines",
        name        = ClassNames[ClassIndex],
        legendgroup = ClassNames[ClassIndex],
        line        = list(color = RgbaLine),
        fillcolor   = RgbaFill,
        showlegend  = FALSE
      )
      
      fig = plotly::add_trace(
        p           = fig,
        y           = CurrKernel[, ClassIndex],
        x           = -CurrLikeli[, ClassIndex],
        type        = "scatter",
        mode        = "lines",
        name        = ClassNames[ClassIndex],
        legendgroup = ClassNames[ClassIndex],
        line        = list(color = RgbaLine),
        fillcolor   = RgbaFill,
        showlegend  = ShowLegend
      )
    }
    
    fig = plotly::layout(
      p = fig,
      xaxis = list(
        title     = FeatureNames[FeatureIndex],
        titlefont = list(size = 20, face = "bold"),
        tickfont  = list(size = 12)
      ),
      yaxis = list(
        title     = "Class-cond. PDE",
        titlefont = list(size = 18, face = "bold"),
        tickfont  = list(size = 12)
      )
    )
    
    # The list follows FeatureOrderOrSubset rather than the original order.
    ListFigs[[PlotPosition]] = fig
  }
  
  names(ListFigs) = FeatureNames[FeatureOrderOrSubset]
  
  if(isTRUE(IndividualFigures)){
    return(ListFigs)
  }
  
  CreateSubplot = function(Figures,SelectedFeatureIndices,DatasetName){
    
    NumFigures = length(Figures)
    
    if(NumFigures < 1L || NumFigures > NumFeaturesPerRow){
      stop("PlotNaiveBayes`CreateSubplot` received an unsupported number of figures: ",
        NumFigures, "."
      )
    }
    
    # Preserve the original quarter-width behavior for rows of up to four.
    # For larger NumFeaturesPerRow values, use equal widths.
    if(NumFeaturesPerRow == 4L){
      Widths = rep(1 / 4, NumFigures)
    }else{
      Widths = rep(1 / NumFigures, NumFigures)
    }
    
    TmpMargin = c(0.05, 0.05, 0, 0)
    
    TmpPlot = plotly::subplot(
      Figures,
      nrows  = 1,
      titleX = TRUE,
      titleY = TRUE,
      margin = TmpMargin,
      widths = Widths,
      heights = 1
    )
    
    PlotMargin = list(
      l   = 0,
      r   = 0,
      b   = 0,
      t   = 50,
      pad = 1
    )
    
    FeatureLabel = paste(SelectedFeatureIndices, collapse = ", ")
    
    if(NumFigures == 1L){
      PlotTitle = paste0(
        DatasetName,
        " Feature ",
        FeatureLabel
      )
    }else{
      PlotTitle = paste0(
        DatasetName,
        " Features ",
        FeatureLabel
      )
    }
    
    plotly::layout(
      p      = TmpPlot,
      title  = PlotTitle,
      font   = list(size = 18),
      margin = PlotMargin
    )
  }
  
  PlotMe = list()
  
  RowStarts = seq.int(
    from = 1L,
    to   = NumSelectedFeatures,
    by   = NumFeaturesPerRow
  )
  
  for(RowIndex in seq_along(RowStarts)){
    
    Start = RowStarts[RowIndex]
    End   = min(
      Start + NumFeaturesPerRow - 1L,
      NumSelectedFeatures
    )
    
    PlotMe[[RowIndex]] = CreateSubplot(
      Figures = ListFigs[Start:End],
      SelectedFeatureIndices =
        FeatureOrderOrSubset[Start:End],
      DatasetName = DatasetName
    )
  }
  
  return(PlotMe)
}