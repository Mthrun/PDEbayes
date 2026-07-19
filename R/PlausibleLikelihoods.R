PlausibleLikelihoods=function(Likelihoods,Data,Cls,threshold=1e-4,c_PDFS_List=NULL,PlausibleCenters=NULL){
   #__________________________________________________________________________________________
  #  Make Likelihoods plausible  ----
  #__________________________________________________________________________________________
  #Cases that have a probability of belonging to a particular class below 
  #the threshold 𝜀 calculated above can either be left “unclassified”, 
  #which is called “reasonable Bayes” classification, or their classification 
  #can be based on the distance to the class center of neighborhood classified cases, 
  #which is called “plausible Bayes” classification.
  
  if(is.null(PlausibleCenters)){
    # In the one-dimensional case, a “plausible Bayes” classification is computed using
    #the distances from x to the modes (=maxima) or mean values {m1, ⋯, m𝑐} of the pdfs
    #of the class likelihoods. 
    if(!requireNamespace("FCPS",quietly = T)){
      stop("PlausibleLikelihoods: Please install package FCPS for Plausible Center estimation")
    }
    CenterPerClass=FCPS::ClusterApply(DataOrDistances = Data,FUN = CenterEstimate,Cls = Cls,Simple = T)
    
    #CenterPerClass=FCPS::ClusterApply(Data,modeest::mlv,Cls,Simple = T)
    PlausibleCenters=t(CenterPerClass)
    #cl=sort(unique(Cls),decreasing = F)
    #colnames(PlausibleCenters)=paste0("Class",cl)
  }

  if(!is.list(Likelihoods)){
      V=dim(Likelihoods)
      N=V[1]
      class_l=V[2]
      d=V[3]
      BC_limit_vec=rep(NaN,d)
      #The threshold 𝜀 below which the evidence is considered as low is estimated 
      #for each pdf of the posteriors using a computed ABC analysis. Subset “C” of
      #this item categorization contains the nonprofitable values, i.e., the lowest 
      #probabilities (“the trivial many”). 
      for( f in 1:d){
        if(N>1)
          LL=Likelihoods[,,f]#drop=F belaesst ein array
        else
          LL=as.matrix(Likelihoods[,,f,drop=F])
        #skip variable if any centers lie very near to each other
        #nearer than10%
        #vermutlich sinnvoller nur mode weitesten links mit weitesten rechts zu vergleichen
        # diffs=unlist(sapply(1:(class_l-1),function(lag,x) diff(x,lag),PlausibleCenters[f,]))
        #diffs=abs(max(PlausibleCenters[f,])-min(PlausibleCenters[f,]))
        pc_len=length(PlausibleCenters[f,])
        diffs <- as.matrix(dist(PlausibleCenters[f,],method="manhattan"))
        threshold_diff=diff(quantile4LargeVectors(Data[,f],c(0.5,0.6)),lag=1)
        
        check_ind=which(diffs<threshold_diff & upper.tri(diffs),arr.ind = T)#identify any diff only once
        if(nrow(check_ind)==(pc_len*(pc_len-1))/2){#all diffs are smaller
          BC_limit_vec[f]=0
          next;
        }
        #approximation posterior, priors are irrelevant here
        Prop_cur=apply(LL,1,function(x) prod(pmin(pmax(x[is.finite(x)],threshold),1-threshold)))
        # print(f)
        # print(Prop_cur)
        if(isTRUE(requireNamespace("ABCanalysis",quietly=TRUE))){
          ## identify critical points below threshold ----
          #The threshold 𝜀 for a class assignment probability that is considered 
          #too low for the assignment decision is defined as the BC limit calculated 
          #in the ABC analysis. In this way, the “trivial many” probabilities are 
          #removed from the Bayesian class assignment decision
          
          #dieser teil haengt davon ab, wie lang der vector und mit welchen werten ist
          #das unterscheidet sich in trainings und test daten
          #alu's paper definiert nicht, ueber welche daten die abc analyse getaetigt werden soll
          if(is.null(c_PDFS_List)){#wir haben nur trainingsdaten
            V=ABCanalysis::ABCanalysis(Prop_cur)
            BClimit=V$BCLimit
          }else{#es gibt predicton und training
            LL_train=c_PDFS_List[[f]]
            x_train=apply(LL_train,1,function(x) prod(pmin(pmax(x[is.finite(x)],threshold),1-threshold)))
            insert=c(Prop_cur,x_train)#use test and train for abc analysis                                        
            V=ABCanalysis::ABCanalysis(insert)
            BClimit=V$BCLimit
          }
          
          #gibts erst ab 1.2.2
          # ABCcls=ABCanalysis::ABCclassification(exp(ClassDecisionLogProb[,i]))
          # UnReasonableInd=which(ABCcls>2)#klasse drei is plausible oder reasonable
          BC_limit_vec[f]=BClimit
          UnReasonableInd <- which(Prop_cur < BClimit)
        }else{#stupid fallback
          warning("Please install ABCanalysis")
          UnReasonableInd <- which(Prop_cur < 0.1)
          
        }#end check if package installed
        #ab hier ist noch ein bug drin, da falsch korrigiert wird
        #cases that are unreasonable are made plausible
        #alternative would be to return class assignment of zero (unknown)
        #if any liklihood has and unreasonable row for a specific case
        #this would be too strong
        if(length(UnReasonableInd)>0){
          #funktioniert nur, da PlausibleCenters in reihenfolge der klassen ist!
          Cls=fixed_kmeans(Feature = Data[,f],Centers = PlausibleCenters[f,]) #klasse k=1:length(PlausibleCenters)
          #likelihoods sind in der reihenfolge der datenpunkte
          #funktioniert nur, da likelihood spalten in reihenfolge der klassen sind!
          LikelihoodCls=max.col(LL,ties.method = "first")#apply(LL, 1, which.max)#klasse k=1:nrow(LikelihoodCls)
          #identify ALL rows that are not plausible at the moment EVEN if above threshold
          diff_ind_x_all=which(LikelihoodCls!=Cls)
          #clear cols that should not make changes
          #check_ind has entries defining col1->col2 and reverese i.e., col2->col1 type of change
          if(nrow(check_ind)>0&length(diff_ind_x_all)>0){
            diff_ind_x_all_pre=diff_ind_x_all
            mat=cbind(LikelihoodCls,Cls)[diff_ind_x_all,,drop=FALSE]
            #defines proposed plausible changes of likelihoods
            #in wich the difference in centers is to small
            #i.e., centers were not approximated correctly
            for(h in 1:nrow(check_ind)){
              boolvec=apply(mat,1,function(x) sum(x==check_ind[h,])==2)|apply(mat,1,function(x) sum(x==rev(check_ind[h,]))==2)
              #View(mat[!boolvec,])
              #only plausible changes remain that not fillfill above condition
              diff_ind_x_all_cur=diff_ind_x_all_pre[!boolvec]
              diff_ind_x_all=intersect(diff_ind_x_all,diff_ind_x_all_cur)
            }
            
          }
          #select only rows that are below threshold
          diff_ind_x=intersect(UnReasonableInd,diff_ind_x_all)
          # #epsilon change makes sure that the correct ones are now really maximal, i.e.,
          # #the likelihood that the class with the closest distance is assigned to x is maximal and infuences in the right direction
          # #the Posteriors
          eps    <- min(c(LL[LL>threshold],threshold^(1/4)))#set epsilon
          if(length(diff_ind_x)>0){#switch them to plaubile ones
            for(p in 1:length(diff_ind_x)){
              #which the inplausubile lilihood has a current maximum
              colcur_max=LikelihoodCls[diff_ind_x[p]]
              #the current maximum value of the implausible class 
              Max_cur=Likelihoods[diff_ind_x[p],colcur_max,f,drop=F]
              #what is the class likelihood that should have be maximal
              col_should_max=Cls[diff_ind_x[p]]
              #lower the maximum value of the inplausible class with eps
              Likelihoods[diff_ind_x[p],colcur_max,f]=max(Max_cur-eps,0)
              #replace with the value that is currently maximal and add eps
              Likelihoods[diff_ind_x[p],col_should_max,f]=Max_cur+eps
              #do not change likelihoods for other classes
            }
          }
        }#end length(UnReasonableInd)>0)
      }#end for( f in 1:d)
  }else{# Likelihoods is a list - (old legacy code)
    warning("Plausible Likelihoods: legacy code is used!")
    ListOfLikelihoods=Likelihoods
    ListOfLikelihoods_plausible=ListOfLikelihoods
    d=length(ListOfLikelihoods)#fuer jedes features gibt es ein listenelement
    N=nrow(ListOfLikelihoods[[1]])#durch interpolateDistributionOnData ist jedes listenelement der gleichen laenge gleich laenge der daten
    #If the “plausible Bayes” calculation results
    #in an undefined value, the class with the closest distance is assigned to x
    #: class(x) = argmin({d(x,m1), …, d(x,m𝑐)}).
    class_l=ncol(ListOfLikelihoods[[1]])
    BC_limit_vec=rep(NaN,d)
    for( f in 1:d){
      #skip variable if any centers lie very near to each other
      #nearer than 5%
      diffs=abs(max(PlausibleCenters[f,])-min(PlausibleCenters[f,]))
      check=min(abs(diffs))<diff(quantile(Data[,f],c(0.5,0.6)),lag=1)
      if(isTRUE(check)){
        BC_limit_vec[f]=0
        next;
      }
      LL=ListOfLikelihoods[[f]]
      #funktioniert nur, da PlausibleCenters in reihenfolge der klassen ist!
      Cls=fixed_kmeans(Data[,f],Centers = PlausibleCenters[f,]) #klasse k=1:length(PlausibleCenters)
      #likelihoods sind in der reihenfolge der datenpunkte
      #funktioniert nur, da likelihood spalten in reihenfolge der klassen sind!
      LikelihoodCls=max.col(LL,ties.method = "first")#apply(LL, 1, which.max)#klasse k=1:nrow(LikelihoodCls)
      #select wich likeliehoods are not plausbile
      #for debugging
      # x=Data[,f]
      # ind=order(x,decreasing = F)
      # plot(x[ind],LL[ind,1],type="l",col=DataVisualizations::DefaultColorSequence[1],main="Bayesian",lwd=2)
      # for(i in 2:ncol(LL)){
      #   points(x[ind],LL[ind,i],type="l",col=DataVisualizations::DefaultColorSequence[i],lwd=2)
      # }
      # DataVisualizations::DefaultColorSequence[1:3]
      # points(x,rep(0.2,length(x)),col=DataVisualizations::DefaultColorSequence[Cls])
      #
      #kein plan wieso das falsch ist
      # diff_ind_x <- which(LikelihoodCls != Cls)
      # if(length(diff_ind_x) > 0){
      #   col_from <- LikelihoodCls[diff_ind_x]
      #   col_to   <- Cls[diff_ind_x]
      #   eps    <- threshold^(1/4)
      #   Max_cur <- LL[diff_ind_x,col_from]
      #   LL[diff_ind_x,col_to] <- Max_cur + eps
      #   LL[diff_ind_x,col_from] <- Max_cur - eps
      # }
      
      diff_ind_x=which(LikelihoodCls!=Cls)#identify the rows that are not plausible at the moment
      # #epsilon change makes sure that the correct ones are now really maximal, i.e.,
      # #the likelihood that the class with the closest distance is assigned to x is maximal and infuences in the right direction
      # #the Posteriors
      eps    <- threshold^(1/4)#set epsilon
      if(length(diff_ind_x)>0){#switch them to plaubile ones
        for(p in 1:length(diff_ind_x)){
          #which the inplausubile lilihood has a current maximum
          colcur_max=LikelihoodCls[diff_ind_x[p]]
          #the current maximum value of the implausible class 
          Max_cur=LL[diff_ind_x[p],colcur_max]
          #what is the class likelihood that should have be maximal
          col_should_max=Cls[diff_ind_x[p]]
          #lower the maximum value of the inplausible class with eps
          LL[diff_ind_x[p],colcur_max]=max(Max_cur-eps,0)
          #replace with the value that is currently maximal and add eps
          LL[diff_ind_x[p],col_should_max]=Max_cur+eps
          #do not change likelihoods for other classes
        }
      }
      
      # #store them
      ListOfLikelihoods_plausible[[f]]=LL
      #fore debugging
      # plot(x[ind],LL[ind,1],type="l",col=DataVisualizations::DefaultColorSequence[1],main="Bayesian",lwd=2)
      # for(i in 2:ncol(LL)){
      #   points(x[ind],LL[ind,i],type="l",col=DataVisualizations::DefaultColorSequence[i],lwd=2)
      # }
      # DataVisualizations::DefaultColorSequence[1:3]
      # points(x,rep(0.2,length(x)),col=DataVisualizations::DefaultColorSequence[Cls])
    }
    
    #The threshold 𝜀 below which the evidence is considered as low is estimated 
    #for each pdf of the posteriors using a computed ABC analysis. Subset “C” of
    #this item categorization contains the nonprofitable values, i.e., the lowest 
    #probabilities (“the trivial many”). 
    for( f in 1:d){
      LL=ListOfLikelihoods[[f]]
      #approximation posterior, priors are irrelevant here
      Prop_cur=apply(LL,1,function(x) prod(pmin(pmax(x[is.finite(x)],threshold),1-threshold)))
      
      
      if(isTRUE(requireNamespace("ABCanalysis",quietly=TRUE))){
        ## identify critical points below threshold ----
        #The threshold 𝜀 for a class assignment probability that is considered 
        #too low for the assignment decision is defined as the BC limit calculated 
        #in the ABC analysis. In this way, the “trivial many” probabilities are 
        #removed from the Bayesian class assignment decision
        
        #dieser teil haengt davon ab, wie lang der vector und mit welchen werten ist
        #das unterscheidet sich in trainings und test daten
        #alu's paper definiert nicht, ueber welche daten die abc analyse getaetigt werden soll
        if(is.null(c_PDFS_List)){#wir haben nur trainingsdaten
          V=ABCanalysis::ABCanalysis(Prop_cur)
          BClimit=V$BCLimit
        }else{#es gibt predicton und training
          LL_train=c_PDFS_List[[f]]
          x_train=apply(LL_train,1,function(x) prod(pmin(pmax(x[is.finite(x)],threshold),1-threshold)))
          insert=c(Prop_cur,x_train)#use test and train for abc analysis                                        
          # if(length(Prop_cur)<3){
          #   x_train=exp(LogPropMatTrain[,i])
          #   insert=c(x_train,Prop_cur)
          # }else{
          #   insert=Prop_cur
          # }
          V=ABCanalysis::ABCanalysis(insert)
          BClimit=V$BCLimit
        }
        #gibts erst ab 1.2.2
        # ABCcls=ABCanalysis::ABCclassification(exp(ClassDecisionLogProb[,i]))
        # UnReasonableInd=which(ABCcls>2)#klasse drei is plausible oder reasonable
        if(is.nan(BC_limit_vec[f])){
          BC_limit_vec[f]=BClimit
          UnReasonableInd <- which(Prop_cur < BClimit)
        }else{
          #centers sind zu nahe beieiander
          UnReasonableInd=NULL
        }

      }else{#stupid fallback
        warning("Please install ABCanalysis")
        UnReasonableInd <- which(Prop_cur < 0.1)
        
      }#end check if package installed
      #cases that are unreasonable are made plausible
      #alternative would be to return class assignment of zero (unknown)
      #if any liklihood has and unreasonable row for a specific case
      #this would be too strong
      if(length(UnReasonableInd)>0){
        #Cases that have a probability of belonging to a particular class below 
        #the threshold 𝜀 ... have to be in plausible categorization
        #reset likelihoods on these UnReasonableInd for prediction
        LL=ListOfLikelihoods[[f]]
        LL2=ListOfLikelihoods_plausible[[f]]
        LL[UnReasonableInd,]=LL2[UnReasonableInd,]
        ListOfLikelihoods[[f]]=LL
        # print(f)
        # print(LL[UnReasonableInd,])
        # print(LL2[UnReasonableInd,])
      }#end length(UnReasonableInd)>0)
    }#end for( f in 1:d)
    Likelihoods=listOfLikelihoods2Array(ListOfLikelihoods)
    
  }#end if ! is list Likelihoods 
    return(list(Likelihoods=Likelihoods,PlausibleCenters=PlausibleCenters,Epsilon=BC_limit_vec))
}