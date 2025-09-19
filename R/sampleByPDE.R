sampleByPDE <- function(Kernels, pdf, n) {
  #ind=which(!duplicated(Kernels))
  #Kernels=Kernels[ind]
  #pdf=pdf[ind]
  if(sum(is.finite(Kernels))>3&sum(is.finite(pdf))>3){
    
    if(length(unique(Kernels))>3&length(unique(pdf))>3){
    dx      <- diff(Kernels)
    mass    <- c(0, cumsum(0.5 * (pdf[-1] + pdf[-length(pdf)]) * dx))
    cdf     <- mass / tail(mass, 1)
    
    u       <- runif(n)
    #u=unique(u)
    feature=approx(cdf, Kernels, xout = u,rule = 2, ties = "ordered")$y #rule = 1, ties = "ordered"
    }else{
      feature=NULL
    }
  }else{
    feature=NULL
  }
  return(feature)
}