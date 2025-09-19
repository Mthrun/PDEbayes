fitParameters=function(Feature,ClassInd,Robust=FALSE,na.rm=TRUE,SD_Threshold=0.0001){
  # V=fitParameters(Feature,ClassInd,Robust=FALSE,na.rm=TRUE,SD_Threshold=0.0001)
  # 
  # INPUT
  # Feature[1:n]    Numeric Vector
  # ClassInd        Integer Vector with class indices
  # 
  # OPTIONAL
  # Robust       (Optional: Default=FALSE). Robust computation if set to TRUE.
  # na.rm        (Optional: Default=TRUE). Remove na.
  # SD_Threshold    (Optional: Default=0.00001). Threshold value for std.
  # 
  # OUTPUT
  # Parameters[1:2]    Numeric vector with Mean and Std.
  # 
  Feature_Class=Feature[ClassInd]
   if(!requireNamespace("DataVisualizations")){
   Robust=FALSE
   warning("fitParameters: requires internal package dbt.Statistic to work which is not installed, setting Robust=FALSE")
   }
  if(isTRUE(Robust)){
 
    me=DataVisualizations::Meanrobust(Feature_Class,na.rm = na.rm)
    std=DataVisualizations::Stdrobust(Feature_Class,na.rm = na.rm)

  }else{
    me=mean(Feature_Class,na.rm=na.rm)
    std=sd(Feature_Class,na.rm=na.rm)
  }

  std[std<SD_Threshold]=SD_Threshold

  return(c(Mean=me,Std=std))
}