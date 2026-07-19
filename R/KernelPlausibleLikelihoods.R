KernelPlausibleLikelihoods=function(PDFs_funs,c_Kernels_list,Data,Cls,PlausibleCenters=NULL,threshold=1e-12){
  
  #umweg ueber data
  if(is.null(PlausibleCenters)){
    if(isTRUE(requireNamespace("FCPS",quietly = T))){
      CenterPerClass=FCPS::ClusterApply(DataOrDistances = Data,FUN = CenterEstimate,Cls = Cls,Simple = T)
      PlausibleCenters=t(CenterPerClass)
    }else{
      stop("KernelPlausibleLikelihoods: For Plausible=T FCPS package needs to be installed")
    }
  }
  c_Kernels_list_stand=c_Kernels_list
  N=max(vapply(c_Kernels_list,nrow,integer(1)))
  d=length(c_Kernels_list)
  class_l=ncol(c_Kernels_list[[1]])
  for(i in 1:d){
    x=c_Kernels_list[[i]]
    vec=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),length.out=N)
    xy=vec
    for(k in 2:class_l){
      xy=cbind(xy,vec)
    }
    c_Kernels_list_stand[[i]]=xy
  }
  Likelihoods_PrePlausible=interpolatePDF4Kernels(KernelList = c_Kernels_list_stand,PDFfunList =PDFs_funs )
  PDFs_funs_PrePlausible=PDFs_funs
  KernelData=c_Kernels_list_stand[[1]][,1,drop=F]
  if(d>1){
    for(f in 2:d){
      KernelData = cbind(KernelData,c_Kernels_list_stand[[f]][,1])
    }
  }
  #ListOfLikelihoodsV=KernelPlausibleLikelihoods(ListOfLikelihoods = Likelihoods_PrePlausible,KernelsList=c_Kernels_list,PlausibleCenters=PlausibleCenters,threshold=Threshold)
  ListOfLikelihoodsV=PlausibleLikelihoods(Likelihoods = Likelihoods_PrePlausible,Data = KernelData,Cls = Cls,threshold = threshold,PlausibleCenters = PlausibleCenters)
  Epsilon=ListOfLikelihoodsV$Epsilon
  ArrayOfLikelihoods=ListOfLikelihoodsV$Likelihoods
  #PlotLikelihoods(ArrayOfLikelihoods,KernelData)
  PDFs_funs=GetLikelihoodFunction(c_Kernels_list_stand,likelihoodsArray2List(ArrayOfLikelihoods))
  
  names(PDFs_funs)=colnames(Data)
  
  return(list(PDFs_funs=PDFs_funs,PlausibleCenters=PlausibleCenters,Epsilon=Epsilon,PlausibleLikelihoods=ArrayOfLikelihoods,Likelihoods_PrePlausible=Likelihoods_PrePlausible))
}  
#Diese Variante geht nicht, weil fixed_kmeans auf den kernel nicht gut funktioniert, wieso weis ich nicht  
  # #Likelihoods=likelihoodsArray2List(ListOfLikelihoods)#die array variante schein fehlerhaft zu sein
  # 
  # Likelihoods=ListOfLikelihoods
  # #Kernels=likelihoodsArray2List(KernelsList)#die array variante schein fehlerhaft zu sein
  # 
  # Kernels=listOfLikelihoods2Array(KernelsList)
  # #__________________________________________________________________________________________
  # #  Make Likelihoods plausible  ----
  # #__________________________________________________________________________________________
  # #Cases that have a probability of belonging to a particular class below 
  # #the threshold 𝜀 calculated above can either be left “unclassified”, 
  # #which is called “reasonable Bayes” classification, or their classification 
  # #can be based on the distance to the class center of neighborhood classified cases, 
  # #which is called “plausible Bayes” classification.
  # 
  # if(!is.list(Likelihoods)){
  #     V=dim(Likelihoods)
  #     N=V[1]
  #     class_l=V[2]
  #     d=V[3]
  #     BC_limit_vec=rep(NaN,d)
  #     #The threshold 𝜀 below which the evidence is considered as low is estimated 
  #     #for each pdf of the posteriors using a computed ABC analysis. Subset “C” of
  #     #this item categorization contains the nonprofitable values, i.e., the lowest 
  #     #probabilities (“the trivial many”). 
  #     for( f in 1:d){
  #       #skip variable if any centers lie very near to each other
  #       #nearer than 5%
  #       Kernels_cur=Kernels[,1,f]#as.vector(Kernels[,,f])#second dimension is per class, we take all classes as this dim is irrelevant
  #       #vermutlich sinnvoller nur mode weitesten links mit weitesten rechts zu vergleichen
  #       # diffs=unlist(sapply(1:(class_l-1),function(lag,x) diff(x,lag),PlausibleCenters[f,]))
  #       diffs=abs(max(PlausibleCenters[f,])-min(PlausibleCenters[f,]))
  #       check=min(abs(diffs))<diff(quantile4LargeVectors(Kernels_cur,c(0.5,0.6)),lag=1)
  #       if(isTRUE(check)){
  #         BC_limit_vec[f]=0
  #         next;
  #       }
  # 
  #       Prop_cur=apply(Likelihoods[,,f,drop=F],1,function(x) sum(x[is.finite(x)]))
  #    
  #       if(isTRUE(requireNamespace("ABCanalysis",quietly=TRUE))){
  #         ## identify critical points below threshold ----
  #         #The threshold 𝜀 for a class assignment probability that is considered 
  #         #too low for the assignment decision is defined as the BC limit calculated 
  #         #in the ABC analysis. In this way, the “trivial many” probabilities are 
  #         #removed from the Bayesian class assignment decision
  #         
  #           V=ABCanalysis::ABCanalysis(Prop_cur)
  #           BClimit=V$BCLimit
  # 
  #         
  #         #gibts erst ab 1.2.2
  #         # ABCcls=ABCanalysis::ABCclassification(exp(ClassDecisionLogProb[,i]))
  #         # UnReasonableInd=which(ABCcls>2)#klasse drei is plausible oder reasonable
  #         BC_limit_vec[f]=BClimit
  #         UnReasonableInd <- which(Prop_cur < class_l*BClimit)
  #       }else{#stupid fallback
  #         warning("Please install ABCanalysis")
  #         UnReasonableInd <- which(Prop_cur < 0.1)
  #         
  #       }#end check if package installed
  #       #ab hier ist noch ein bug drin, da falsch korrigiert wird
  #       #cases that are unreasonable are made plausible
  #       #alternative would be to return class assignment of zero (unknown)
  #       #if any liklihood has and unreasonable row for a specific case
  #       #this would be too strong
  #       if(length(UnReasonableInd)>0){
  #         #Kernels_cur=seq(from=min(Kernels_cur),to=max(Kernels_cur),length.out=N)#sort(sample(unique(Kernels_cur),N))#size has to be the same as Likelihoods and in ascending rank
  #         #funktioniert nur, da PlausibleCenters in reihenfolge der klassen ist!
  #         Cls=fixed_kmeans(Kernels_cur,Centers = PlausibleCenters[f,]) #klasse k=1:length(PlausibleCenters)
  #         #likelihoods sind in der reihenfolge der datenpunkte
  #         #funktioniert nur, da likelihood spalten in reihenfolge der klassen sind!
  #         LikelihoodCls=max.col(Likelihoods[,,f,drop=F],ties.method = "first")#apply(LL, 1, which.max)#klasse k=1:nrow(LikelihoodCls)
  #         #identify ALL rows that are not plausible at the moment EVEN if above threshold
  #         diff_ind_x_all=which(LikelihoodCls!=Cls)
  #         #select only rows that are below threshold
  #         diff_ind_x=intersect(UnReasonableInd,diff_ind_x_all)
  #         # #epsilon change makes sure that the correct ones are now really maximal, i.e.,
  #         # #the likelihood that the class with the closest distance is assigned to x is maximal and infuences in the right direction
  #         # #the Posteriors
  #         eps    <- threshold#^(1/4)#set epsilon
  #         if(length(diff_ind_x)>0){#switch them to plaubile ones
  #           for(p in 1:length(diff_ind_x)){
  #             #which the inplausubile lilihood has a current maximum
  #             colcur_max=LikelihoodCls[diff_ind_x[p]]
  #             #the current maximum value of the implausible class 
  #             Max_cur=Likelihoods[diff_ind_x[p],colcur_max,f,drop=F]
  #             #what is the class likelihood that should have be maximal
  #             col_should_max=Cls[diff_ind_x[p]]
  #             #lower the maximum value of the inplausible class with eps
  #             Likelihoods[diff_ind_x[p],colcur_max,f]=Max_cur-eps
  #             #replace with the value that is currently maximal and add eps
  #             Likelihoods[diff_ind_x[p],col_should_max,f]=Max_cur+eps
  #             #do not change likelihoods for other classes
  #           }
  #         }
  #       }#end length(UnReasonableInd)>0)
  #     }#end for( f in 1:d)
  # }else{# Likelihoods is a list - (old legacy code)
  #   ListOfLikelihoods=Likelihoods
  #   ListOfLikelihoods_plausible=ListOfLikelihoods
  #   d=length(ListOfLikelihoods)#fuer jedes features gibt es ein listenelement
  #   N=nrow(ListOfLikelihoods[[1]])#durch interpolateDistributionOnData ist jedes listenelement der gleichen laenge gleich laenge der daten
  #   #If the “plausible Bayes” calculation results
  #   #in an undefined value, the class with the closest distance is assigned to x
  #   #: class(x) = argmin({d(x,m1), …, d(x,m𝑐)}).
  #   class_l=ncol(ListOfLikelihoods[[1]])
  #   #print(class_l)
  #   BC_limit_vec=rep(NaN,d)
  #   for( f in 1:d){
  #     #skip variable if any centers lie very near to each other
  #     #nearer than 5%
  #     LL=ListOfLikelihoods[[f]]
  #     Kernels_cur=Kernels[[f]][,1]#as.vector(Kernels[[f]])
  # 
  #     diffs=abs(max(PlausibleCenters[f,])-min(PlausibleCenters[f,]))
  #     check=min(abs(diffs))<diff(quantile(Kernels_cur,c(0.5,0.6)),lag=1)
  #     if(isTRUE(check)){
  #       BC_limit_vec[f]=0
  #       next;
  #     }
  #     #Kernels_cur=sort(sample(Kernels_cur,nrow(LL)))#but they have to be in the righ ranks to fit Likelihoods!
  # 
  #     #funktioniert nur, da PlausibleCenters in reihenfolge der klassen ist!
  #     Cls=fixed_kmeans(Kernels_cur,Centers = PlausibleCenters[f,]) #klasse k=1:length(PlausibleCenters)
  #     #likelihoods sind in der reihenfolge der datenpunkte
  #     #funktioniert nur, da likelihood spalten in reihenfolge der klassen sind!
  #     LikelihoodCls=max.col(LL,ties.method = "first")#apply(LL, 1, which.max)#klasse k=1:nrow(LikelihoodCls)
  #     #select wich likeliehoods are not plausbile
  #     #for debugging
  #     # x=Data[,f]
  #     # ind=order(x,decreasing = F)
  #     # plot(x[ind],LL[ind,1],type="l",col=DataVisualizations::DefaultColorSequence[1],main="Bayesian",lwd=2)
  #     # for(i in 2:ncol(LL)){
  #     #   points(x[ind],LL[ind,i],type="l",col=DataVisualizations::DefaultColorSequence[i],lwd=2)
  #     # }
  #     # DataVisualizations::DefaultColorSequence[1:3]
  #     # points(x,rep(0.2,length(x)),col=DataVisualizations::DefaultColorSequence[Cls])
  #     #
  #     #kein plan wieso das falsch ist
  #     # diff_ind_x <- which(LikelihoodCls != Cls)
  #     # if(length(diff_ind_x) > 0){
  #     #   col_from <- LikelihoodCls[diff_ind_x]
  #     #   col_to   <- Cls[diff_ind_x]
  #     #   eps    <- threshold^(1/4)
  #     #   Max_cur <- LL[diff_ind_x,col_from]
  #     #   LL[diff_ind_x,col_to] <- Max_cur + eps
  #     #   LL[diff_ind_x,col_from] <- Max_cur - eps
  #     # }
  #     
  #     diff_ind_x=which(LikelihoodCls!=Cls)#identify the rows that are not plausible at the moment
  #     # #epsilon change makes sure that the correct ones are now really maximal, i.e.,
  #     # #the likelihood that the class with the closest distance is assigned to x is maximal and infuences in the right direction
  #     # #the Posteriors
  #     eps    <- threshold#^(1/4)#set epsilon
  #     if(length(diff_ind_x)>0){#switch them to plaubile ones
  #       for(p in 1:length(diff_ind_x)){
  #         #which the inplausubile lilihood has a current maximum
  #         colcur_max=LikelihoodCls[diff_ind_x[p]]
  #         #the current maximum value of the implausible class 
  #         Max_cur=LL[diff_ind_x[p],colcur_max]
  #         #what is the class likelihood that should have be maximal
  #         col_should_max=Cls[diff_ind_x[p]]
  #         #lower the maximum value of the inplausible class with eps
  #         LL[diff_ind_x[p],colcur_max]=Max_cur-eps
  #         #replace with the value that is currently maximal and add eps
  #         LL[diff_ind_x[p],col_should_max]=Max_cur+eps
  #         #do not change likelihoods for other classes
  #       }
  #     }
  #     
  #     # #store them
  #     ListOfLikelihoods_plausible[[f]]=LL
  #     #fore debugging
  #     # plot(x[ind],LL[ind,1],type="l",col=DataVisualizations::DefaultColorSequence[1],main="Bayesian",lwd=2)
  #     # for(i in 2:ncol(LL)){
  #     #   points(x[ind],LL[ind,i],type="l",col=DataVisualizations::DefaultColorSequence[i],lwd=2)
  #     # }
  #     # DataVisualizations::DefaultColorSequence[1:3]
  #     # points(x,rep(0.2,length(x)),col=DataVisualizations::DefaultColorSequence[Cls])
  #   }
  #   
  #   #The threshold 𝜀 below which the evidence is considered as low is estimated 
  #   #for each pdf of the posteriors using a computed ABC analysis. Subset “C” of
  #   #this item categorization contains the nonprofitable values, i.e., the lowest 
  #   #probabilities (“the trivial many”). 
  #   for( f in 1:d){
  #     LL=ListOfLikelihoods[[f]]
  #     Prop_cur=apply(LL,1,function(x) sum(x[is.finite(x)]))
  #     
  #     
  #     if(isTRUE(requireNamespace("ABCanalysis",quietly=TRUE))){
  #       ## identify critical points below threshold ----
  #       #The threshold 𝜀 for a class assignment probability that is considered 
  #       #too low for the assignment decision is defined as the BC limit calculated 
  #       #in the ABC analysis. In this way, the “trivial many” probabilities are 
  #       #removed from the Bayesian class assignment decision
  # 
  #         V=ABCanalysis::ABCanalysis(Prop_cur)
  #         BClimit=V$BCLimit
  #  
  #       #gibts erst ab 1.2.2
  #       # ABCcls=ABCanalysis::ABCclassification(exp(ClassDecisionLogProb[,i]))
  #       # UnReasonableInd=which(ABCcls>2)#klasse drei is plausible oder reasonable
  #       if(is.nan(BC_limit_vec[f])){
  #         BC_limit_vec[f]=BClimit
  #         UnReasonableInd <- which(Prop_cur < class_l*BClimit)
  #       }else{
  #         #centers sind zu nahe beieiander
  #         UnReasonableInd=NULL
  #       }
  # 
  #     }else{#stupid fallback
  #       warning("Please install ABCanalysis")
  #       UnReasonableInd <- which(Prop_cur < 0.1)
  #       
  #     }#end check if package installed
  #     #cases that are unreasonable are made plausible
  #     #alternative would be to return class assignment of zero (unknown)
  #     #if any liklihood has and unreasonable row for a specific case
  #     #this would be too strong
  #     if(length(UnReasonableInd)>0){
  #       #Cases that have a probability of belonging to a particular class below 
  #       #the threshold 𝜀 ... have to be in plausible categorization
  #       #reset likelihoods on these UnReasonableInd for prediction
  #       LL=ListOfLikelihoods[[f]]
  #       LL2=ListOfLikelihoods_plausible[[f]]
  #       LL[UnReasonableInd,]=LL2[UnReasonableInd,]
  #       ListOfLikelihoods[[f]]=LL
  #       # print(f)
  #       # print(LL[UnReasonableInd,])
  #       # print(LL2[UnReasonableInd,])
  #     }#end length(UnReasonableInd)>0)
  #   }#end for( f in 1:d)
  #   Likelihoods=listOfLikelihoods2Array(ListOfLikelihoods)
  #   
  # }#end if ! is list Likelihoods 
  #   return(list(Likelihoods=Likelihoods,Epsilon=BC_limit_vec))
#}