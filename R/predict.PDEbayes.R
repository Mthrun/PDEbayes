predict.PDEbayes= function(object, newdata, type = c("class", "response","prob"), ...) {
  if (missing(newdata)) {
    stop("Please supply `newdata` to predict()", call. = FALSE)
  }
  type <- match.arg(type)

  res=Predict_naiveBayes(Data=newdata,Model=object,...)
  
  switch (type,
          prob = {
            out=res$Posteriors
          },
          class={
            out=factor(res$Cls, levels = sort(unique(res$Cls)))
          },
          response={
            out=res$Cls
          }
  )
  return(out)
}