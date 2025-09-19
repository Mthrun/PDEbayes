PlotNaiveBayes = function(Model, FeatureNames, DatasetName = "Data", nrows = 1,
                          FeatureOrder, NumFeaturesPerRow = 4){
  
  Kernels     = Model$c_Kernels_list
  Likelihoods = Model$ListOfLikelihoods
  Priors      = Model$Priors
  
  Colors = c("blue",         "gold", "firebrick",  "limegreen",
             "dodgerblue", "orange",   "magenta",      "green",
             "cyan",          "red",      "plum",  "darkgreen",
             "turquoise",  "bisque",  "lavender", "lightgreen")
  
  #UniqueCls = unique(Cls)
  #NumClasses = length(UniqueCls)
  
  NumClasses  = dim(Kernels[[1]])[2]
  UniqueCls   = 1:NumClasses
  NumFeatures = length(Kernels)
  
  if(missing(FeatureNames)){
    FeatureNames = paste0("X", 1:NumFeatures)
  }
  
  if((missing(FeatureOrder))){
    FeatureOrder = 1:NumFeatures
  }
  
  for(i in 1:NumFeatures){
    for(j in 1:NumClasses){
      Likelihoods[[i]][,j] = Priors[j] * Likelihoods[[i]][,j]
    }
  }
  
  # TODO
  # PDEs bis über 1er Grenzen (-1, 1) hinausschätzen, damit die Plots
  # vertikal bis zur Nullinie (gedrehte x-achse) gehen?
  # FIX: Variablennamen pro Subplot!
  
  ListFigs = list()
  j = 1
  for(j in 1:NumFeatures){
    CurrKernel = Kernels[[j]]
    CurrLikeli = Likelihoods[[j]]
    # Make the visualization go to zero:
    # Get one step of the Kernel sequence
    Diff       = CurrKernel[2,1] - CurrKernel[1,1]
    # Add a step before and after
    CurrKernel = rbind(CurrKernel[1,] - Diff, CurrKernel)
    CurrKernel = rbind(CurrKernel, CurrKernel[dim(CurrKernel)[1],] + Diff)
    # Add 0 values for the Likelihood at the start and end
    CurrLikeli = rbind(rep(0, dim(CurrLikeli)[2]), CurrLikeli)
    CurrLikeli = rbind(CurrLikeli, rep(0, dim(CurrLikeli)[2]))
    
    
    #fig = plot_ly(type = 'scatter', mode = 'lines', fill = "tozeroy")
    #for(i in 1:NumClasses){
    #  fig = add_lines(p = fig, x = CurrKernel[,i], y = CurrLikeli[,i],
    #                  name = UniqueCls[i], legendgroup = UniqueCls[i],
    #                  line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
    #                  fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
    #                  showlegend = FALSE, yaxis = "y2")
    #  fig = add_lines(p = fig, x = CurrKernel[,i], y = -CurrLikeli[,i],
    #                  line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
    #                  fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
    #                  showlegend = FALSE, yaxis = "y2")
    #}
    
    fig = plot_ly(fill = "tozeroy")
    for(i in 1:NumClasses){
      fig = add_trace(p = fig, y = CurrKernel[,i], x = CurrLikeli[,i],
                      type = "scatter", mode = "lines",
                      name = UniqueCls[i], legendgroup = UniqueCls[i],
                      line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
                      fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
                      showlegend = FALSE)
      fig = add_trace(p = fig, y = CurrKernel[,i], x = -CurrLikeli[,i],
                      type = "scatter", mode = "lines",
                      line = list(color = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 1)")),
                      fillcolor = paste0("rgba(", paste0(col2rgb(Colors[i]), collapse = ", "), ", 0.2)"),
                      showlegend = FALSE)
    }
    
    fig = layout(p = fig,
                 xaxis = list(title = FeatureNames[j], titlefont = list(size = 20, face = "bold"), tickfont = list(size = 12)),
                 yaxis = list(title = "Class-cond. PDE", titlefont = list(size = 18, face = "bold"), tickfont = list(size = 12)))
    
    ListFigs[[j]] = fig
  }
  
  #testSubPlot = subplot(ListFigs[1:4], nrows = 1, titleX = TRUE, titleY = TRUE)
  #testSubPlot = layout(p = testSubPlot, title = "Titlino")
  
  
  CreateSubplot = function(ListFigs, Start, End, DatasetName, FeatureOrder){
    
    if((End-Start) == 0){
      #tmpMargin = c(0.5, 0.5, 0, 0)
      width = 1/4
      height = 1
    }else if(((End-Start) == 1)){
      #tmpMargin = c(0.05, 0.05, 0, 0)
      width = rep(1/4, 2)
      height = 1
    }else if(((End-Start) == 2)){
      #tmpMargin = c(0.05, 0.05, 0, 0)
      width = rep(1/4, 3)
      height = 1
    }else if(((End-Start) == 3)){
      #tmpMargin = c(0.05, 0.05, 0, 0)
      width = rep(1/4, 4)
      height = 1
    }else{
      warning("Unrecognized correct numbers of figures in subplot.")
    }
    
    tmpMargin = c(0.05, 0.05, 0, 0)
    tmpP      = subplot(ListFigs, nrows = 1, titleX = TRUE, titleY = TRUE, margin = tmpMargin,
                        widths = width, heights = height)
    
    if((End == 0) | ((End-Start) == 0)){
      if(missing(FeatureOrder)){
        tmpP = layout(p = tmpP, title = paste0(DatasetName, " Feature ", Start),
                      font = list(size = 18), margin = list(l = 0, r = 0, b = 0, t = 50, pad = 1))
      }else{
        tmpP = layout(p = tmpP, title = paste0(DatasetName, " Feature ", paste0(FeatureOrder[Start:End], collapse = ", ")),
                      font = list(size = 18), margin = list(l = 0, r = 0, b = 0, t = 50, pad = 1))
      }
    }else{
      if(missing(FeatureOrder)){
        tmpP = layout(p = tmpP, title = paste0(DatasetName, " Features ", Start, "-", End),
                      font = list(size = 18), margin = list(l = 0, r = 0, b = 0, t = 50, pad = 1))
      }else{
        tmpP = layout(p = tmpP, title = paste0(DatasetName, " Features ", paste0(FeatureOrder[Start:End], collapse = ", ")),
                      font = list(size = 18), margin = list(l = 0, r = 0, b = 0, t = 50, pad = 1))
      }
    }
    return(tmpP)
  }
  
  PlotMe = list()
  Idx    = 1
  NumFeatsTBP = length(ListFigs)
  Start = 1
  End   = 0
  while(NumFeatsTBP > 0){
    if(NumFeatsTBP >= NumFeaturesPerRow){
      End           = End + NumFeaturesPerRow
      tmpP          = CreateSubplot(ListFigs    = ListFigs[Start:End],
                                    Start       = Start,
                                    End         = End,
                                    DatasetName = DatasetName, FeatureOrder = FeatureOrder)
      PlotMe[[Idx]] = tmpP
      Start         = Start + NumFeaturesPerRow
      NumFeatsTBP   = NumFeatsTBP - NumFeaturesPerRow
    }else{
      End           = End + NumFeatsTBP
      tmpP          = CreateSubplot(ListFigs    = ListFigs[Start:End],
                                    Start       = Start,
                                    End         = End,
                                    DatasetName = DatasetName, FeatureOrder = FeatureOrder)
      PlotMe[[Idx]] = tmpP
      NumFeatsTBP   = 0
    }
    Idx = Idx + 1
  }
  
  #FinFig = subplot(ListFigs, nrows = 1)
  
  return(PlotMe)
}
