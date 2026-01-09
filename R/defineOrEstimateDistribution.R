defineOrEstimateDistribution=function(Feature,ClassInd,Gaussian=FALSE,
                                      ParetoRadius=NULL,
                                      InternalPlotIt=FALSE,
                                      SD_Threshold=0.001,
                                      ...){
  # INPUT
  # Feature[1:n]    Numeric Vector
  # ClassInd        Integer Vector with class indices
  # 
  # OPTIONAL
  # Gaussian     (Optional: Default=TRUE). Assume gaussian distribution.         
  # Robust       (Optional: Default=FALSE). Robust computation if set to TRUE.
  # PlotItInternal       (Optional: Default=FALSE).
  # na.rm        (Optional: Default=TRUE). Remove na.
  # SD_Threshold    (Optional: Default=0.00001). Threshold value for std.
  # Type          1: MCT PDE, 2: QS PDE
  # OUTPUT
  # Kernels[1:m]    Numeric vector with domain of a 1D pdf
  # PDF[1:m]        Numeric vector of pdf
  # Theta           Numeric vector with parameters of gaussian - NULL if no gaussian used.
  # 
  dots=list(...)
  has_Robust <- "Robust" %in% names(dots)
  if (has_Robust) {
    Robust= dots$Robust
  }else{
    Robust=F
  }
  has_Type <- "Type" %in% names(dots)
  if (has_Type) {
    Type= dots$Type
  }else{
    Type=1
  }
  
  has_narm <- "na.rm" %in% names(dots)
  if (has_narm) {
    na.rm= dots$na.rm
  }else{
    na.rm=TRUE
  }
  Fun=NULL
  QuantityThreshold = 30
  UniqueValuesThreshold = 12
  MinMax = c(min(Feature,na.rm = T), max(Feature,na.rm = T))

  if(sum(is.finite(MinMax))!=2){
	#uniqueFeature=1
    if(!is.finite(MinMax[1])) MinMax[1]=0#dummy value
    if(!is.finite(MinMax[2])) MinMax[2]=1#dummy value
  }else{
    #uniqueFeature=unique(Feature)
	#uniqueFeature=uniqueFeature[is.finite(uniqueFeature)]#no missing or inf
  }

  #kernels_p=sort(uniqueFeature) #automatische setzung der kernels (kernels_p=NULL) verschlechtert das ergebnis deutlich
  #n=min(c(length(uniqueFeature),200))
  # breaks = pretty(c(min(Feature,na.rm = T), max(Feature,na.rm = T)), n = n, min.n = 1)
  # nB = length(breaks)
  # kernels_p = 0.5 * (breaks[-1L] + breaks[-nB])
  #kernels_p=seq(from=min(Feature,na.rm = T),to= max(Feature,na.rm = T),length.out=n)
  if(isTRUE(Gaussian)){#define distibution
    Theta=fitParameters(Feature,ClassInd=ClassInd,Robust=Robust,na.rm=na.rm,SD_Threshold=SD_Threshold)
    pdf=dnorm(Feature,Theta[1],Theta[2])#already likelihood on given data
    Kernels=Feature
  }else{#estimate Distribution
    #Performanz haeng stark davon ab wie hier die kernels gewaehlt werden
    if(isFALSE(requireNamespace("DataVisualizations"))){
      Type=2
      N_UniqueFC=20
      warning("Please install DataVisualizations. setting Type of PDE to 2")
    }
    Theta=NULL
    Feature_Class=Feature[ClassInd]
    N_UniqueFC=length(unique(Feature_Class))
    if(is.null(ParetoRadius)){
	  if(!requireNamespace("DataVisualizations",quietly = T)){
		warning("Please install package DataVisualizations.")
		return(NULL)
	  }else{
		if(packageVersion("DataVisualizations")<"1.1.5"){
		  warning("Please update package DataVisualizations to version 1.1.5 or higher.")
		  return(NULL)
		}
	  }
      ParetoRadius=if(packageVersion("DataVisualizations")>="1.4.0")DataVisualizations::ParetoRadius_fast(Feature) else DataVisualizations::ParetoRadius(Feature)

      
      #bayes is sensitiv gegenueber radius ->volles feature nehmen ist besser als uber class feature
    #in spezialaellen ist die interne berechnung nicht durchfuerbar
    }#ind dem fall wird NULL zurueckgegeben
    if(!is.finite(ParetoRadius)){
      #in dem fall koennen wir keine dichte schätzen
      N_UniqueFC=0#fuert zu dem fall von densityEstimation4smallNoCases()
	  ParetoRadius=0
    }
    
   if(N_UniqueFC>UniqueValuesThreshold & length(ClassInd)>QuantityThreshold){#density estimation by PDE is possible, see MDplot paper
      #if(Type==1 | N_UniqueFC<UniqueValuesThreshold){#Type=2 scheint dort nicht alle daten bei dichte messung zu beruecksichten
    if(Type==1 ){
      #KernelVec has to be equally spaced
      KernelVec=seq(from=MinMax[1]-ParetoRadius,to=MinMax[2]+ParetoRadius,length.out=512)
	    if(!requireNamespace("DataVisualizations",quietly = T)){
			warning("Please install package DataVisualizations.")
			return(NULL)
		  }else{
			if(packageVersion("DataVisualizations")<"1.1.5"){
			  warning("Please update package DataVisualizations to version 1.1.5 or higher.")
			  return(NULL)
			}
		  }
      V=DataVisualizations::ParetoDensityEstimation(Feature_Class, Silent = T,kernels = KernelVec,
                                                      Compute = "cpp", paretoRadius = ParetoRadius)
        paretoRadius=V$paretoRadius
        Kernels=V$kernels#[-c(1,length(V$kernels))]
        paretoDensity=V$paretoDensity#[-c(1,length(V$kernels))]
        # FFT-based smoothing step 
        # assume KernelVec is equally spaced
        #but there are always numerical instabilities, hence mean
        dx = mean(diff(Kernels))
        m  = length(Kernels)
        #midpoint
        mid = ceiling(m/2)
        #computes the physical distances of each grid point from the center of your kernel grid
        rel = (seq_len(m) - mid) * dx
        # Gaussian smoothing kernel: use paretoRadius as bandwidth
        kFFT = dnorm(rel / paretoRadius) / paretoRadius * dx
        #rectangle kernel
        # kFFT = ifelse(abs(rel) <= paretoRadius,
        #               dx / (2 * paretoRadius),
        #               0)
        # Zero-pad to length L
        #choose L as the next power of two at least m + m - 1 so 
        # that when we multiply FFTs we perform a linear convolution
        L = 2^ceiling(log2(2*m - 1))
        #Zero-padding both vectors to length L prevents wrap-around artifacts
        #from the FFT and leverages FFT speedups for power-of-two lengths
        w_pad = c(paretoDensity, rep(0, L - m))# original (normalized) Pareto density vector extended with zeros up to length L
        k_pad = c(kFFT, rep(0, L - m))#discrete smoothing kernel vector (kFFT) similarly zero-padded to length L.
        # Convolution via FFT
        #adding the kernel to the same length as w_pad is required so their FFTs can be multiplied element-wise to compute the convolution.
        conv = fft(fft(w_pad) * fft(k_pad), inverse = TRUE) / L
        #similar to conv2 <- convolve(paretoDensity, kFFT, type = "open") #but with controlling the FFT length and power-of-two padding
        # Extract smoothed density
        #ignore zero padding which is symmetrically distributed to both sides
        pdf = Re(conv[((L-m)/2):(ceiling((2*m+L-m)/2)-1)])#Re(conv[1:(2*m)])
        # #centered non-causal moving average
        # n=round(0.05*length(Kernels),0)
        # pdf= stats::filter(V$paretoDensity, rep(1/n, n), sides = 2)
        # #left side moving average, causal
        # #pdf=TSAT::MovingAverage(V$paretoDensity,25)
        # #kein smoothing  an den enden
        # pdf[1:n]=V$paretoDensity[1:n]
        # pdf[(length(pdf)-n):length(pdf)]=V$paretoDensity[(length(pdf)-n):length(pdf)]
        # # pdf=V$paretoDensity
    
        # if(length(Feature_Class)>3&length(Feature_Class)<1000){
        #   Feature_Class2=sampleByPDE(Kernels,pdf,1000)
        #     V2=DataVisualizations::ParetoDensityEstimation(c(Feature_Class,Feature_Class2),MinAnzKernels = 800,KernelRange = MinMax,Silent = T,Compute = "cpp_exp",paretoRadius = ParetoRadius)
        #     paretoRadius=V2$paretoRadius
        #     pdf=V2$paretoDensity
        #     Kernels=V2$kernels
        # }
    }else if(Type==2){##type=2, Rs density estimation
        V=density(Feature_Class,na.rm=T)
        pdf=V$y
        Kernels=V$x
    }else if(Type==3){
      V=density(Feature_Class,na.rm=T,bw=ParetoRadius)
      pdf=V$y
      Kernels=V$x
    }else{
      stop("defineOrEstimateDistribution: Select type Type=1 or Type=2 or Type=3")
    }
  
        #stats::spline scheint nicht so gut zu sein

         #pdf=pdf/quantile(pdf,c(0.999)) #sonst sind feature mit dichte >1 hoeher gewichtet
         #quantile ignoriert peaks
        # c_pdfV=stats::spline(Kernels, pdf, xout =smoothX, ties = "ordered",method = "fmm")
        # pdf=c_pdfV$y
        # Kernels=c_pdfV$x
      # }else{# if(Type==2){
      #   V=   ParetoDensityEstimation2(Feature_Class,Kernel = kernels_p,paretoRadius = ParetoRadius)
      #   pdf=V$Density  
      #   Kernels=V$Kernel
      #   # if(length(Feature_Class)>3&length(Feature_Class)<1000){
      #   #   Feature_Class2=sampleByPDE(Kernels,pdf,1000)
      #   #   V2=ParetoDensityEstimation2(c(Feature_Class,Feature_Class2),Kernel = kernels_p,paretoRadius = ParetoRadius)
      #   #   pdf=V2$Density  
      #   #   Kernels=V2$Kernel
      #   # }
      # }#end density estimation of type 2
      }else{#fall back to hist-based density estimation
        V=densityEstimation4smallNoCases(Feature,ClassInd)
        pdf=V$Density  
        Kernels=V$Kernels
      }#end if QuantityThreshold and UniqueValuesThreshold is appropriate
    
    ##no error catching if something went wrong
    # if(length(Kernels)!=length(pdf)){
    #   warning("defineOrEstimateDistribution: Unable to estimate PDE correctly, please check input data, fallback to densityEstimation4smallNoCases()")
    #   V=densityEstimation4smallNoCases(Feature,ClassInd)
    #   pdf=V$Density  
    #   Kernels=V$Kernels
    # }
    if(sum(is.finite(pdf))>1&sum(is.finite(Kernels))>1){#at least two values are required
      #ist ein fehler, weil ich moeglicherweise die eine klasse dann niedriger als
      # die andere klasse gewichte
      # if(sum(pdf>1,na.rm = T)/length(ClassInd)>0.05){#more than five percent of pdf values is above one
      #   #we have peaks in density which will result in bad liklihoods in log
      #   pdf=pdf/max(pdf,na.rm=TRUE)#now pdf is not normalized to area of one
      # }
    }else{
      warning("defineOrEstimateDistribution: Unable to estimate PDE correctly, please check input data, fallback to densityEstimation4smallNoCases()")
      V=densityEstimation4smallNoCases(Feature,ClassInd)
      pdf=V$Density  
      Kernels=V$Kernels
    }

  }# end if(isTRUE(Gaussian))
  if(isFALSE(Gaussian)){
    if(N_UniqueFC>UniqueValuesThreshold & length(ClassInd)>QuantityThreshold){
      if(sum(is.finite(pdf))>2 &sum(is.finite(Kernels))>2){
        smoothX  = seq(from=min(Kernels,na.rm = T), to=max(Kernels,na.rm = T),length.out=1000)
        Fun=stats::splinefun(x = Kernels, y = pdf,method="monoH.FC", ties = "ordered")
        pdf=Fun(smoothX)
        Kernels=smoothX
      }else{

        Kernels= c(MinMax[1],MinMax[2])
        pdf=c(0,0)
        Fun=stats::approxfun(x = Kernels, y = pdf,rule = 2, ties = "ordered")
        pdf=Fun(Kernels)
      }
    }else{
      if(sum(is.finite(pdf))>2 &sum(is.finite(Kernels))>2){
        smoothX  = seq(from=min(Kernels,na.rm = T), to=max(Kernels,na.rm = T),length.out=1000)
        Fun=stats::approxfun(x = Kernels, y = pdf,rule = 2, ties = "ordered")
        pdf=Fun(smoothX)
        Kernels=smoothX
      }else{
        Kernels= c(MinMax[1],MinMax[2])
        pdf=c(0,0)
        Fun=stats::approxfun(x = Kernels, y = pdf,rule = 2, ties = "ordered")
        pdf=Fun(Kernels)
      }
    }
    # V=get_pdf_fun(Kernels,pdf,length.out=1000,N_UniqueFC=N_UniqueFC,UniqueValuesThreshold=UniqueValuesThreshold,N_class=length(ClassInd),QuantityThreshold=QuantityThreshold)
    # Fun=V$PDFfun
    # pdf=V$pdf
    # Kernels=V$Kernels
  }
  if(isTRUE(InternalPlotIt)){
   plot(Kernels,pdf,xlab="If Feature where in Class",ylab="PDF")
  }
  return(list(Kernels=Kernels,PDF=pdf,Theta=Theta,PDF_fun=Fun,ParetoRadius=ParetoRadius))
}