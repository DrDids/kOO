#################
#   ................... in working progress gathering of utilities for the koo packages
# WARNING WARNING: beware that some functions are experiments and are not necessary working properly
#         in the mean time you could notify me at c3s2i@free.fr or contact@c3s2i.co.uk
#         when you find an error
#collaborations to improve this and to achieve kOO are wellcome
#   ...................       
#whilst waiting for the R-package kOO
#refer to this as: 
# 1) 
# Leibovici DG (2011) Rfunc_colloc.R: Unpacked R-functions towards the kOO R-package. Online: http://c3s2i.free.fr/UnpackedR-#
# functions/
# 2) 
# Leibovici DG, . Bastin L,. and Jackson M. (2011) " Higher-Order Co-occurrences for Exploratory Point Pattern Analysis and Decision 
# Tree Clustering on Spatial Data." Computers & Geosciences: 37(3): 382-389
#
#################
     ###########################
      library(spatstat)
      library(tensor)
      library(tensorA)
        #functions to do
  #####################     source("Rfunc_colloc.R")                                                                                                                                                                                                                                                         
  #dcircle(i,j,k,d) counting n of occ i j k in circles of diameter d
  #dCircle (si,j,k,d) counting n of occ j k in a d-circle centered on si
  #dIntCircle(si,sj,sk,d') 0 or 1 if they are in a d'-circle
  # (equiv to  distances 2 by 2 are less than d with d'=1/(sqrt(3)/2) d~1.15d
  #####################
  #versions local or on containers
  cOO3d1SCircle.ppp =function(S,d,cont=FALSE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark if S is a concateantion of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime=proc.time()
        matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number
        if(length(d)>1){
          Res=array(0,c(dim(matD)[1],Ip,Ip,length(d)),list(S$marks,as.character(d),Ipm,Ipm))
          for(i in 1:Ds){
          for (dd in d){
            inJ= (1:Ds)[-i]
            inJ=inJ[matD[i,inJ]<dd]
             for(j in inJ) {        #dij <d    j<>i
               inJk=inJ[inJ!=j]
               inJk=inJk[matD[j,inJk]<dd]
                  wk=summary(S$marks[inJk])
              for(kk in 1:length(wk)){      #dik <d djk <d    k<>j k<>i
                k=nIpm(as.factor(names(wk[kk])))
                 Res[i,nIpm(S$marks[j]),k,nIpm(dd,d)]=Res[i,nIpm(S$marks[j]),k,nIpm(dd,d)]+wk[kk]
              }#end of k
             }#end of j
          }#end of d
        }#end of i
        }
        else{
          Res=array(0,c(Ds,Ip,Ip),list(S$marks,Ipm,Ipm))
          for(i in 1:Ds){
            inJ= (1:Ds)[-i]
            inJ=inJ[matD[i,inJ]<d]
             for(j in inJ) {        #dij <d    j<>i
               inJk=inJ[inJ!=j]
               inJk=inJk[matD[j,inJk]<d]
               #calcul from inJk des poids et des classes
               #obsklev=as.factor(as.character(S$marks[inJk]))
               #klev=levels(obsklev)
               wk=summary(S$marks[inJk])
              for(kk in 1:length(wk)){      #dik <d djk <d    k<>j k<>i
                k=nIpm(as.factor(names(wk[kk])))
                              Res[i,nIpm(S$marks[j]),k]=Res[i,nIpm(S$marks[j]),k]+wk[kk]
              }#end of kk
             }#end of j
          }#end of i
        }#end of else
    cat("\n", "-----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(Res)
  }
  #end of cOO3d1SCircle.ppp

#essai2=cOO3d1SCircle.ppp(ll,0.1)
#      plan=plot(ll,cols=1:6,lwd=2)
#     legend(locator(1), pch=plan, legend=names(plan),col=1:6,pt.lwd=2, box.lty=0)

  dSCircle.ppp =function(Si,Sj,Sk,d,all=FALSE){
      #Ss are a ppp with Ss$marks as the marks
      # I J and K are marks in Si Sj Sk
      #output is an array os dim Si J K lengths
      #      an array of colloc of j k around si
      # or Si Sj Sk if all=TRUE      a 0 or 1 array


  }
  dCircle.ppp =function(S,J,K,d,Isubset=FALSE){
      #Sm is a ppp with S$marks as the marks
      # I J and K are marks in S
      # if Isubset=TRUE the counts are only done for I points (not J not K)
      #output is an array os dim S (or I) J K lengths
  }


  cOO3d1Stest=function(S,d,old=FALSE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark if S is a concateantion of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime=proc.time()
        matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number
        if(old){
          Res=array(0,c(Ds,Ip,Ip),list(S$marks,Ipm,Ipm))

          for(i in 1:Ds){

            inJ= (1:Ds)[-i]
            inJ=inJ[matD[i,inJ]<d]
             for(j in inJ) {        #dij <d    j<>i
               inJk=inJ[inJ!=j]
               inJk=inJk[matD[j,inJk]<d]
              for(k in inJk){      #dik <d djk <d    k<>j k<>i
                 Res[i,nIpm(S$marks[j]),nIpm(S$marks[k])]=Res[i,nIpm(S$marks[j]),nIpm(S$marks[k])]+1
              }#end of k
             }#end of j

        }#end of i
        }
        else{
          Res=array(0,c(Ds,Ip,Ip),list(S$marks,Ipm,Ipm))
          for(i in 1:Ds){
            inJ= (1:Ds)[-i]
            inJ=inJ[matD[i,inJ]<d]
             for(j in inJ) {        #dij <d    j<>i
               inJk=inJ[inJ!=j]
               inJk=inJk[matD[j,inJk]<d]
               #calcul from inJk des poids et des classes
               #obsklev=as.factor(as.character(S$marks[inJk]))
               #klev=levels(obsklev)
               wk=summary(S$marks[inJk])
              for(kk in 1:length(wk)){      #dik <d djk <d    k<>j k<>i
                k=nIpm(as.factor(names(wk[kk])))
                              Res[i,nIpm(S$marks[j]),k]=Res[i,nIpm(S$marks[j]),k]+wk[kk]
              }#end of kk
             }#end of j
          }#end of i
        }#end of else
    cat("\n", "-----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(Res)
  }
     ### the one I used ###########
     
 cOO3d1S <- function(S, d, matD = pairdist(S), inclu = FALSE, silent=TRUE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont = TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime = proc.time()
        if(is.null(matD)) matD = pairdist(S)
        Ds = dim(matD)[1]
         colnames(matD) = S$marks
         rownames(matD) = S$marks
        Ip = nlevels(S$marks)
        Ipm = levels(S$marks)
        nIpm = function(m, lIpm = Ipm){(1:length(lIpm))[lIpm == m]} #get position number
        if(inclu){
          Res = array(0, c(Ds, Ip, Ip), list(S$marks, Ipm, Ipm))
          for(i in 1:Ds){
            inJ =  (1:Ds)
            inJ = inJ[matD[i, inJ]<d]
             for(j in inJ) {        #dij <d
               inJk = inJ[matD[j, inJ]<d]       #inJk = inJ[matD[j, inJk]<d]
               #calcul from inJk des poids et des classes
               #obsklev = as.factor(as.character(S$marks[inJk]))
               #klev = levels(obsklev)
               wk = summary(S$marks[inJk])
               if(length(wk)!=0){
              for(kk in 1:length(wk)){      #dik <d djk <d
                k = nIpm(as.factor(names(wk[kk])))
                              Res[i, nIpm(S$marks[j]), k] = Res[i, nIpm(S$marks[j]), k] + wk[kk]
              }#end of kk
              }
             }#end of j
          }#end of i
          }
        else{
          Res = array(0, c(Ds, Ip, Ip), list(S$marks, Ipm, Ipm))
          for(i in 1:Ds){
            inJ =  (1:Ds)[-i]
            inJ = inJ[matD[i, inJ] < d]
             for(j in inJ) {        #dij <d    j<>i
               inJk = inJ[inJ != j]
               inJk = inJk[matD[j, inJk]<d]
               #calcul from inJk des poids et des classes
               #obsklev = as.factor(as.character(S$marks[inJk]))
               #klev = levels(obsklev)
               wk = summary(S$marks[inJk])
               if(length(wk)!=0){
              for(kk in 1:length(wk)){      #dik <d djk <d    k<>j k<>i
                k = nIpm(as.factor(names(wk[kk])))
                Res[i, nIpm(S$marks[j]), k] = Res[i, nIpm(S$marks[j]), k] + wk[kk]
              }#end of kk
              }
             }#end of j
          }#end of i
        }#end of else
    if(!silent)cat("\n",  "-----Execution Time-----",  (proc.time() - debtime)[3], "\n")
   return(Res)
  }   ####
      ### the one I used as well########
     ##### or the one
  cOO3d1Sel=function(S,d,matD=pairdist(S),inclu=FALSE,Iselmarks=NULL, Jselmarks=NULL, Kselmarks=NULL,silent=TRUE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      # if inclu=TRUE one has 3rd 2nd 1st ... but with spurius counts
         
      debtime=proc.time()
      
        if(is.null(matD)) matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number
        TFin=function(v, m){out=sapply(v,FUN=function(val){any(val ==m)});ifelse(length(out)==0,return(FALSE),return(out))}
       
            dDs=1:Ds
            if(!is.null(Iselmarks)){
              ifelse(class(Iselmarks)=="numeric",dDs <- Iselmarks, dDs <-(1:Ds)[TFin(S$marks,Iselmarks)] )
            }
       
       
          IpmJ=Ipm
           if(!is.null(Jselmarks)) IpmJ=Jselmarks
          IpmK=Ipm 
          if(!is.null(Kselmarks)) IpmK=Kselmarks
          
          if(length(d)==1){Res=array(0,c(length(dDs),length(IpmJ),length(IpmK)),list(S$marks[dDs],IpmJ,IpmK))}
          else{Res=array(0,c(length(dDs),length(IpmJ),length(IpmK),length(d)),list(S$marks[dDs],IpmJ,IpmK,as.character(d)))}
          #Res=array(0,c(Ds,Ip,Ip),list(S$marks,Ipm,Ipm))
          
        if(inclu){
         for (ld in 1:length(d)){ 
          for(i in dDs){
             ii= nIpm(i,dDs)
             # if(!is.null(Iselmarks) && !any(S$marks[i]==Iselmarks)){  next }
              inJ=1:Ds
              inJ=inJ[matD[i,inJ]<d[ld]]
            if(!is.null(Jselmarks)){inJsel=inJ[TFin(S$marks[inJ],Jselmarks)]} else{inJsel=inJ}        
             for(j in inJsel) {        #dij <d 
               inJk=inJ[matD[j,inJ]<d[ld]]       #inJk=inJ[matD[j,inJk]<d]
               if(!is.null(Kselmarks)){inJk=inJk[TFin(S$marks[inJk],Kselmarks)]}
               #calcul from inJk des poids et des classes
               #obsklev=as.factor(as.character(S$marks[inJk]))
               #klev=levels(obsklev)
               wk=summary(S$marks[inJk])
               if(length(wk)!=0){
              for(kk in 1:length(wk)){      #dik <d djk <d
                 k=nIpm(as.factor(names(wk[kk])),IpmK)
                 if(length(d)==1){Res[ii,nIpm(S$marks[j],IpmJ),k]=Res[ii,nIpm(S$marks[j],IpmJ),k]+wk[kk]}
                 else{Res[ii,nIpm(S$marks[j],IpmJ),k,ld]=Res[ii,nIpm(S$marks[j],IpmJ),k,ld]+wk[kk]}
              }#end of kk
              }
             }#end of j
           }#end of i
          }   
          }
        else{
        for (ld in 1:length(d)){ 
          for(i in dDs){
          ii= nIpm(i,dDs)
            #if(!is.null(Iselmarks) && !any(S$marks[i]==Iselmarks)){next}     
            inJ= (1:Ds)[-i]
            inJ=inJ[matD[i,inJ]<d[ld]]
            if(!is.null(Jselmarks)){inJsel=inJ[TFin(S$marks[inJ],Jselmarks)]} else {inJsel=inJ}
             for(j in inJsel) {        #dij <d    j<>i
               inJk=inJ[inJ!=j]
               inJk=inJk[matD[j,inJk]<d[ld]]
               if(!is.null(Kselmarks)){inJk=inJk[TFin(S$marks[inJk],Kselmarks)]}
               #calcul from inJk des poids et des classes
               #obsklev=as.factor(as.character(S$marks[inJk]))
               #klev=levels(obsklev)
               wk=summary(S$marks[inJk])
               if(length(wk)!=0){
              for(kk in 1:length(wk)){      #dik <d djk <d    k<>j k<>i
                k=nIpm(as.factor(names(wk[kk])),IpmK)
                 if(length(d)==1){Res[ii,nIpm(S$marks[j],IpmJ),k]=Res[ii,nIpm(S$marks[j],IpmJ),k]+wk[kk]}
                 else{Res[ii,nIpm(S$marks[j],IpmJ),k,ld]=Res[ii,nIpm(S$marks[j],IpmJ),k,ld]+wk[kk]}
              }#end of kk
              }
             }#end of j
          }#end of i
          }
        }#end of else
   if(!silent)  cat("\n", "-----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(Res)
  }   ####
  SelfcOO3d1S=function(S,d,silent=TRUE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime=proc.time()
            Ip=nlevels(S$marks)
            Ipm=levels(S$marks)
            Res=rep(0,Ip)
            names(Res)=Ipm
        for (i in 1:Ip){
        temp=S[S$marks==Ipm[i]]
        matD=pairdist(temp)
            inJ= (1:dim(matD)[1])
             for(j in inJ) {        #dij <d
               inJk=inJ[matD[j,inJ[inJ!=j]]<d]
                for (k in inJk){
                   inJk3=inJk[matD[k,inJk[inJk!=k]]<d]             
                   Res[i]=Res[i]+length(inJk3)                     
              }#end of kk
             }#end of j
          }#end of i
         
   if(!silent)  cat("\n", "-----Execution Time-----", (proc.time() - debtim)[3],"\n")
   return(Res/3)
  }   ####
  AcOO3d1S=function(S,d,matD=pairdist(S),inclu=FALSE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime=proc.time()
        if(is.null(matD)) matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number
        if(inclu){

          #for(i in 1:Ds){ }
              RRes=as.list(1:Ds)
          applist=function(i,matD=matD,S=S, d=d, Ip=Ip,Ipm=Ipm,nIpm=nIpm){
               Res=matrix(rep(0,Ip*Ip),c(Ip,Ip),dimnames=list(Ipm,Ipm)) # of the i S$marks[i]
                 inJ= 1:(dim(matD)[1])
                   inJ=inJ[matD[i,inJ]<d]
                   for(j in inJ) {        #dij <d
                     inJk=inJ[matD[j,inJ]<d]       #inJk=inJ[matD[j,inJk]<d]
                      wk=summary(S$marks[inJk])
                    for(kk in 1:length(wk)){      #dik <d djk <d
                        k=nIpm(as.factor(names(wk[kk])))
                              Res[nIpm(S$marks[j]),k]=Res[nIpm(S$marks[j]),k]+wk[kk]
                    }
                 }
               return(Res)
            }# end function  applist

              RRes=lapply(RRes, applist)
          }
        else{
        #for(i in 1:Ds){ }
              RRes=as.list(1:Ds)

          applist=function(i,mD=matD,pS=S, dis=d, IIp=Ip,IIpm=Ipm,InIpm=nIpm){
              Res=matrix(rep(0,IIp*IIp),c(IIp,IIp),dimnames=list(IIpm,IIpm)) # of the i S$marks[i]
                 inJ= (1:(dim(mD)[1]))[-i]
                   inJ=inJ[mD[i,inJ]<dis]
                   for(j in inJ) {        #dij <d
                     inJk=inJ[inJ!=j]
                     inJk=inJ[mD[j,inJ]<dis]       #inJk=inJ[matD[j,inJk]<d]
                      wk=summary(pS$marks[inJk])
                    for(kk in 1:length(wk)){      #dik <d djk <d
                        k=InIpm(as.factor(names(wk[kk])))
                              Res[InIpm(pS$marks[j]),k]=Res[InIpm(pS$marks[j]),k]+wk[kk]
                    }
                 }
               return(Res)
            }# end function  applist
              
              RRes=lapply(RRes, applist)

        }#end of else

    cat("\n", "-----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(RRes)
  }
  BcOO3d1S=function(S,d,matD=pairdist(S),CLusters=rep("localhost",4),inclu=FALSE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime=proc.time()
        if(is.null(matD)) matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number
        if(inclu){

          #for(i in 1:Ds){ }
          Res=array(0,c(Ds,Ip,Ip),list(S$marks,Ipm,Ipm))
              RRes=as.list(1:Ds)
          applist=function(i,mD=matD,pS=S, dis=d, IIp=Ip,IIpm=Ipm,InIpm=nIpm){
               Res=matrix(rep(0,IIp*IIp),c(IIp,IIp),dimnames=list(IIpm,IIpm)) # of the i S$marks[i]
                 inJ= 1:(dim(mD)[1])
                   inJ=inJ[mD[i,inJ]<dis]
                   for(j in inJ) {        #dij <d
                     inJk=inJ[mD[j,inJ]<dis]       #inJk=inJ[matD[j,inJk]<d]
                      wk=summary(pS$marks[inJk])
                    for(kk in 1:length(wk)){      #dik <d djk <d
                        k=InIpm(as.factor(names(wk[kk])))
                              Res[InIpm(pS$marks[j]),k]=Res[InIpm(pS$marks[j]),k]+wk[kk]
                    }
                 }
               return(Res)
            }# end function  applist

           library(snow)
              cl <- makeSOCKcluster(CLusters)
               clusterExport(cl, "d")
                clusterExport(cl, "S")
                 clusterExport(cl, "matD")
              RRes=parLapply(cl,RRes, applist)
          }
        else{
        #for(i in 1:Ds){ }
              RRes=as.list(1:Ds)

          applist=function(i,mD=matD,pS=S, dis=d, IIp=Ip,IIpm=Ipm,InIpm=nIpm){
              Res=matrix(rep(0,IIp*IIp),c(IIp,IIp),dimnames=list(IIpm,IIpm)) # of the i S$marks[i]
                 inJ= (1:(dim(mD)[1]))[-i]
                   inJ=inJ[mD[i,inJ]<dis]
                   for(j in inJ) {        #dij <d
                     inJk=inJ[inJ!=j]
                     inJk=inJ[mD[j,inJ]<dis]       #inJk=inJ[matD[j,inJk]<d]
                      wk=summary(pS$marks[inJk])
                    for(kk in 1:length(wk)){      #dik <d djk <d
                        k=InIpm(as.factor(names(wk[kk])))
                              Res[InIpm(pS$marks[j]),k]=Res[InIpm(pS$marks[j]),k]+wk[kk]
                    }
                 }
               return(Res)
            }# end function  applist

             library(snow)

              cl <- makeSOCKcluster(CLusters)
              clusterExport(cl, "d")
                clusterExport(cl, "S")
                 clusterExport(cl, "matD")
              RRes=parLapply(cl,RRes, applist)

        }#end of else

    cat("\n", "-----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(RRes)
  }# end of BcOO3d1S
  #### same as cOO3d1S but with more marks
   dIJKS=function(including=TRUE,d,S,mI,mJ,mK=NULL){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      # mI mJ mK are marks
         # this will be used for different Ss in DIJKSs with crossdist
      # including is to not bother about the points i.e. to consider marks as events
      # if not it is like in cOO3d1S the colloc as to be with different points
      debtime=proc.time()
        matDIJ=pairdist(S)
        Ds=dim(matDIJ)[1]
         colnames(matDIJ)= mJ
         rownames(matDIJ)= S$marks
        IpI=nlevels(mI)
        IpmI=levels(mI)
        IpJ=nlevels(mJ)
        IpmJ=levels(mJ)


        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number

        #3D or 4D
          if(is.null(mK)){Res=array(0,c(Ds,IpI,IpJ),list(S$marks,IpmI,IpmJ))}
          else {
           IpK=nlevels(mK)
           IpmK=levels(mK)
          Res=array(0,c(Ds,IpI,IpJ,IpK),list(S$marks,IpmI,IpmJ,IpmK))
          }
        #
          for(i in 1:Ds){
            inJ= (1:Ds)
            inJ=inJ[matDIJ[i,inJ]<d]
             for(j in inJ) {        #dij <d
               inJk=inJ[matDIJ[j,inJ]<d]
                  #wkI=summary(SI$marks[inJk])
               wkJ=summary(mJ[inJk])
             if(is.null(mK)){
                for(kk in 1:length(wkJ)){      #dik <d djk <d
                   kJ=nIpm(as.factor(names(wkJ[kk])),IpmJ)
                              Res[i,nIpm(mI[j],IpmI),kJ]=Res[i,nIpm(mI[j],IpmI),kJ]+wkJ[kk]
               }#end of kk
             }
             else {
               wkK=summary(mK[inJk])
              for(kk in 1:length(wkJ)){      #dik <d djk <d
                   kJ=nIpm(as.factor(names(wkJ[kk])),IpmJ)
                 for(kkk in 1:length(wkK)){
                     kK=nIpm(as.factor(names(wkK[kkk])),IpmK)
                              Res[i,nIpm(mI[j],IpmI),kJ,kK]=Res[i,nIpm(mI[j],IpmI),kJ,kK]+wkJ[kk]*wkK[kkk]
                              # if not including we need summary(factor matrix J K)
                 }#end of kkk
              }#end of kk

              }
             }#end of j
          }#end of i

    cat( " -----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(Res)
  }

  #S I J to K I J
  #
  
  cOO2d1S=function(S,d,matD=pairdist(S),inclu=TRUE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime=proc.time()
        if(is.null(matD)) matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number
        if(inclu){
          Res=array(0,c(Ds,Ip),list(S$marks,Ipm))
          for(i in 1:Ds){
            inJ= (1:Ds)
            inJ=inJ[matD[i,inJ]<d]
               wj=summary(S$marks[inJ])
              for(jj in 1:length(wj)){      #dik <d djk <d
                j=nIpm(as.factor(names(wj[jj])))
                              Res[i,j]=Res[i,j]+wj[jj]
              
             }#end of jj
          }#end of i
          }
        else{
          Res=array(0,c(Ds,Ip),list(S$marks,Ipm))
          for(i in 1:Ds){
            inJ= (1:Ds)[-i]
            inJ=inJ[matD[i,inJ]<d]
               wj=summary(S$marks[inJ])
              for(jj in 1:length(wj)){      #dik <d djk <d    k<>j k<>i
                j=nIpm(as.factor(names(wj[jj])))
                Res[i,j]=Res[i,j]+wj[jj]
             }#end of j
          }#end of i
        }#end of else
    cat("\n", "-----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(Res)
  }   ####

  cOO4d1S=function(S,d,matD=pairdist(S),inclu=TRUE){
      #S is a ppp with S$marks as the marks
      # I   (could be multimark or k processes if S is a concatenation of Ss
      #output is an array os dim Si I I lengths
      #      an array of colloc of i' i'' around si
      # or I I I if  cont=TRUE
      # d could be a vector of distances then another dimension is in the output
      debtime=proc.time()
        if(is.null(matD)) matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        nIpm=function(m,lIpm=Ipm){(1:length(lIpm))[lIpm==m]} #get position number
        if(inclu){
          Res=array(0,c(Ds,Ip,Ip,Ip),list(S$marks,Ipm,Ipm,Ipm))
          for(i in 1:Ds){
            inJ= (1:Ds)
            inJ=inJ[matD[i,inJ]<d]
             for(j in inJ) {        #dij <d
               inJk=inJ[matD[j,inJ]<d]       #inJk=inJ[matD[j,inJk]<d]
               #calcul from inJk des poids et des classes
               #obsklev=as.factor(as.character(S$marks[inJk]))
               #klev=levels(obsklev)
               for (k in inJk){
                   inJkl=inJk[matD[k,inJk]<d] 
               wkl=summary(S$marks[inJkl])
                 for(ll in 1:length(wkl)){      #dik <d djk <d
                           l=nIpm(as.factor(names(wkl[ll])))
                           Res[i,nIpm(S$marks[j]),nIpm(S$marks[k]),l]=Res[i,nIpm(S$marks[j]),nIpm(S$marks[j]),l]+wkl[ll]
                }#end of ll
              }#end of k
             }#end of j
          }#end of i
          }
        else{
          Res=array(0,c(Ds,Ip,Ip,Ip),list(S$marks,Ipm,Ipm,Ipm))
          for(i in 1:Ds){
            inJ= (1:Ds)[-i]
            inJ=inJ[matD[i,inJ]<d]
             for(j in inJ) {        #dij <d    j<>i
               inJk=inJ[inJ!=j]
               inJk=inJk[matD[j,inJk]<d]
               #calcul from inJk des poids et des classes
               #obsklev=as.factor(as.character(S$marks[inJk]))
               #klev=levels(obsklev)
              for (k in inJk){
                   inJkl=inJk[inJk!=k]  
                   inJkl=inJkl[matD[k,inJkl]<d] 
                   wkl=summary(S$marks[inJkl])
                 for(ll in 1:length(wkl)){      #dik <d djk <d
                           l=nIpm(as.factor(names(wkl[ll])))
                           Res[i,nIpm(S$marks[j]),nIpm(S$marks[k]),l]=Res[i,nIpm(S$marks[j]),nIpm(S$marks[j]),l]+wkl[ll]
                }#end of ll
                }#end of k
             }#end of j
          }#end of i
        }#end of else
    cat("\n", "-----Execution Time-----", (proc.time() - debtime)[3],"\n")
   return(Res)
  }   ####

  aggreg.list<-function(L,fact){
     #list of matrix or array of the same dim
     #      [[1]] etc ...
     # fact and [[ ]] are in the same order
      factlev=levels(fact)
      Rout=to.tensor(0, c(list(factlev), dimnames(L[[1]])))
        names(Rout)=paste("I",1:length(dim(Rout)),sep="")
       nIpm=function(m,lIpm=factlev){(1:length(lIpm))[lIpm==m]} #get position number
    for(i in 1:dim(as.array(L))){
    temp=Rout[["I1"=as.character(fact[i]) ]]
      Rout[["I1"=as.character(fact[i]) ]]<-array(as.vector(temp),dim(temp)) + L[[i]]
    }
    return(Rout)
  } #
 ######################################################
  aggreg.table<-function(A,dimagg=1,fact=NULL,factlev=NULL,FUN=sum){
           # dim is the dimension to aggregate according to factor in fac
           #fact is same length as dim-length if NULL dimnames(A)[[dim]] is used
      if(dimagg>5)cat("\n","up to 5 ...edit the function","\n")
      if(is.null(fact)) fact=as.factor(dimnames(A)[[dimagg]])
      if(is.null(factlev))factlev=levels(fact)
      if (dimagg==1){
          Rout=to.tensor(0, c(list(factlev), dimnames(A)[-dimagg]))
      }
      else{
          if(dimagg==length(dim(A))){
                 Rout=to.tensor(0, c(dimnames(A)[-dimagg],list(factlev)))
          }
          else{
                 Rout=to.tensor(0, c(dimnames(A)[1:(dimagg-1)],list(factlev),dimnames(A)[(dimagg+1):length(dim(A))]))  
          }
      }
        
        names(Rout)=paste("I",1:length(dim(Rout)),sep="")
      if(!is.tensor(A))A=to.tensor(as.vector(A),dimnames(A))
      for (e in factlev){
        sel=(1:dim(A)[dimagg])[fact==e]
        if(length(sel)==0)next
        if(length(sel)==1){
            if(dimagg==1) Rout[["I1"=e]]<- A[[I1 = sel]]
            if(dimagg==2) Rout[["I2"=e]]<- A[[I2 = sel]]
            if(dimagg==3) Rout[["I3"=e]]<- A[[I3 = sel]]
            if(dimagg==4) Rout[["I4"=e]]<- A[[I4 = sel]]
            if(dimagg==5) Rout[["I5"=e]]<- A[[I5 = sel]]
            }
        else{
       if(dimagg==1) Rout[["I1"=e]]<-apply(A[[I1 = sel]], (1:length(dim(A)))[-dimagg], FUN = FUN)
       if(dimagg==2) Rout[["I2"=e]]<-apply(A[[I2 = sel]], (1:length(dim(A)))[-dimagg], FUN = FUN)
       if(dimagg==3) Rout[["I3"=e]]<-apply(A[[I3 = sel]], (1:length(dim(A)))[-dimagg], FUN = FUN)
       if(dimagg==4) Rout[["I4"=e]]<-apply(A[[I4 = sel]], (1:length(dim(A)))[-dimagg], FUN = FUN)
       if(dimagg==5) Rout[["I5"=e]]<-apply(A[[I5 = sel]], (1:length(dim(A)))[-dimagg], FUN = FUN)
        }
      }
   return(Rout)
  }#end of aggreg.table

  CollScale <-function(ppp,d){
                   ###
                  d3=cOO3d1S(ppp,d[1])
                  d4=to.tensor(0,c(dimnames(d3),list(paste("d",d,sep=""))))
                  evalCh.f<-function(st){
                      #st is a expression quoted e.g."x=2"
                        tmp <- tempfile()
                         writeLines(st, tmp)
                             return(eval.parent(parse(tmp)))
                    } #end of evalCh.f
                    evalCh.f(paste("d4[[",names(d4)[length(dim(d4))],"=1]]=d3",sep=""))
                   for(di in 2:length(d)){
                      gc()
                    d3=cOO3d1S(ppp,d[di])
                    evalCh.f(paste("d4[[",names(d4)[length(dim(d4))],"=di]]=d3",sep=""))
                   }
                 return(d4)
                } #end Collscale

   ########################################## entropy
     Entropyd<- function(A,base=2,RatioUniform=TRUE,Before=TRUE, ALL=FALSE,SYMM=TRUE, Prob=FALSE,Self=FALSE){
              U=1
     if(sum(as.vector(A))==0){
          Res=A; Hsu=0}
     else{
              #self=T A is a "square" multiway table and we work on the hyoerdiagonal
             if(Self){    AA=rep(0,dim(A)[1])
                  if (length(dim(A))==2) {A=diag(A)}
                   if (length(dim(A))==3) {for(i in 1:dim(A)[1]){AA[i]=A[i,i,i]}; A=AA}
                     if (length(dim(A))==4) {for(i in 1:dim(A)[1]){AA[i]=A[i,i,i,i]}; A=AA}
                      if (length(dim(A))==5) {for(i in 1:dim(A)[1]){AA[i]=A[i,i,i,i,i]}; A=AA}
               }
               ##       
            if(Before){ if(RatioUniform)U=log(length(A),base=base)}
            
             if(!Prob) {A=A/sum(A) }
             A=as.vector(A)
             A=A[A!=0]
             if(!Before) {if(RatioUniform)U=log(length(A),base=base) }
        
              res=-A*log(A,base=base)/U
              Hsu= sum(res[res!=Inf])
     } 
             if(ALL){return(list("HSu"=Hsu,"ALL"=res,"Prob"=A))}
             else {return(Hsu)}
       }
  ########################################## Kappa
  KappaO.f<-function(A,B,reord=FALSE){
          # A and B are two vector of classes of the same length
          # but possibly not ordered in the same way
          # the function computes a Kappa of cooccurences
          # by reordering the most probable matching
          # i.e. on the diagonal of crosstab one have the highest values

          # first reorder the columns of Tab as the order is biasing the results but less if on favorise the highers frequencies to match

           K=0
          Tab=table(A,B)
           Tab=Tab[,order(apply(Tab,2,FUN=sum),decreasing=TRUE)]
          rTab=dim(Tab)[1]
          ord= order(Tab[,1],decreasing=TRUE)
          Tab=Tab[ord,]
          for(i in 2:rTab){
           pto=order(Tab[i:rTab,i],decreasing=TRUE)
           Tab[i:rTab,]=Tab[(i:rTab)[pto],]
           ord[i:rTab]= ord[(i:rTab)[pto]]
          }
          rownames(Tab)=ord
           Tabp=Tab/sum(Tab)
           pc=sum(apply(Tabp,1,FUN=sum)*apply(Tabp,2,FUN=sum)) ##independence
           reclassrate=sum(Tabp- diag(diag(Tabp)))
          K=(sum(diag(Tabp))-pc)/(1-pc)
          testK=0
     return(list("Kapp"=K,"Tabo"=table(A,B),"Tab"=Tab,"reclassrate"=reclassrate))
  }#end of KappaO.f
  ###############################
 Hcd=function(ppp,dvect,matD=pairdist(ppp),dagg=1,Keep=FALSE,Self=F,...){
          #entropy order=3 collocations at dvect
        rout=0
        if(Keep){
        Ds=dim(matD)[1]
         colnames(matD)= ppp$marks
         rownames(matD)= ppp$marks
        Ip=nlevels(ppp$marks)
        Ipm=levels(ppp$marks)
        KeepCOO=array(0,c(Ds,Ip,Ip,length(dvect)),list(ppp$marks,Ipm,Ipm,as.character(dvect)))
        }
        i=1
        for (d in dvect){
         cat("\n",i,"th out of ",length(dvect)," d:",d)
          if(Self){ KCOO=SelfcOO3d1S(ppp,d,matD=matD)} else {KCOO=cOO3d1S(ppp,d,matD=matD)}
           if (Keep){KeepCOO[,,,i]=KCOO}
           rout=c(rout,Entropyd(aggreg.table(KCOO,dagg),Self=Self,...))

        i=i+1
        }
        if(!Keep) return(rout[-1])
        if(Keep) return(list("KCOO"=KeepCOO,"HSu"=rout))
  }#Hcd
  #############################"""""""
  BHcd=function(ppp,dvect,matD=pairdist(ppp),fact=ppp$marks,parall=TRUE,CLusters=rep("localhost",4)){
          #entropy order=3 collocations at dvect

        if(parall){
                      appL=function(d,ppp=ppp,matD=matD,fact=fact){
                        
                      return(Entropyd(aggreg.list(BcOO3d1S(ppp,d,matD=matD),fact)))
                         }
          library(snow)
            cl <- makeSOCKcluster(CLusters)
              
            res=parSapply(cl,dvect,appL)
         return(res)
        }
        else{
             rout=0
          i=1
          for (d in dvect){
          cat("\n",i,"th out of ",length(dvect)," d:",d)
           rout=c(rout,Entropyd(aggreg.list(BcOO3d1S(ppp,d,matD=matD),fact)))

          i=i+1
          }

        return(rout[-1])
        }
  }#BHcd
  
   ###envelope sample
   BHcdenvelope=function(ppp,dvect,matD=pairdist(ppp),nsample=19,parall1=TRUE,parall2=FALSE,CLusters=rep("localhost",5)){
              # us parallel here and in cOO

          outH=as.list(1:nsample)

          appL=function(i,pSp=ppp,dv=dvect,mD=matD,nsa=nsample){
              pSp$marks=sample(pSp$marks)
              pSp$marks[1:10]
              cat("\n"," sample:",i,"  /",nsa)
              if (parall2){
                  out=BHcd(pSp,dv,mD)}
              else{
              out=Hcd(pSp,dv,mD)
              }
             return(out)
          }
          if(parall1){
            library(snow)
            cl <- makeSOCKcluster(CLusters)
             clusterExport(cl, "dvect")
             clusterExport(cl, "ppp")
             clusterExport(cl, "matD")
             
            res=parLapply(cl,outH,appL)
            }
          else{
             res=sapply(outH,appL)
             }
       
       return(res)
     }#Hcdenvelope

     Hcdenvelope <-function(ppp,dvect,matD=pairdist(ppp),nsample=19,...){
              #
            outH=matrix(rep(0,nsample*length(dvect)),c(nsample,length(dvect)))
            colnames(outH)=as.character(dvect)
          for(i in 1:nsample){
          ppp$marks=sample(ppp$marks)
          outH[i,]=Hcd(ppp,dvect,matD,...)
          cat("\n"," sample:",i,"  /",nsample)
          }
     return(outH)
     }#Hcdenvelope


      ###envelope sample   all
      
   togettheinside=function(){   
      
              # use parallel here and in cOO
     #################  
     
     
          library(snow)
          library(spatstat)
          library(tensorA)    
          CLusters=rep("localhost",11)
          cl= makeSOCKcluster(CLusters)
ppp=  RSA.ppp          ;clusterExport(cl, "ppp")
dvect=MRSA.dd   ;clusterExport(cl, "dvect")
matD= pairdist(ppp)    ; clusterExport(cl, "matD")
nsample= 20              ; clusterExport(cl, "nsample")

             clusterEvalQ(cl, library(spatstat))
         clusterEvalQ(cl, library(tensorA))   
    clusterExport(cl,"Hcd")
    clusterExport(cl, "cOO3d1S")
     clusterExport(cl,"Entropyd")
      clusterExport(cl, "aggreg.table")
        

              
        appL=function(i,pSp=ppp,dv=dvect,mD=matD){
              if(i>1) pSp$marks=sample(pSp$marks)        
              out=Hcd(pSp,dv,mD)
             return(out)
          }
         clusterExport(cl, "appL")      
            MRSAnotabHc.envelope.res=parSapply(cl,as.list(1:nsample),appL)
    save.image()        
     #################       
        
}    #end of togettheinside



##################
#entropy Li and Claramunt 2006
LiClaraEnt=function(S,matD=pairdist(S),details=FALSE) {
    if(is.null(matD)) matD=pairdist(S)
        Ds=dim(matD)[1]
         colnames(matD)= S$marks
         rownames(matD)= S$marks
        Ip=nlevels(S$marks)
        Ipm=levels(S$marks)
        pro=summary(S$marks)
         Rout=0
        dratio=0
      for (i in 1:length(pro)){  
        dint= sum(matD[S$marks==Ipm[i],S$marks==Ipm[i]])/(pro[i]*(pro[i]-1))
        dext=  sum(matD[S$marks==Ipm[i],S$marks!=Ipm[i]])/(pro[i]*(sum(pro)-pro[i]))
        dratio=c(dratio,dint/dext)
        Rout=c(Rout ,- dint/dext*pro[i]/sum(pro)*log(pro[i]/sum(pro),2))
      }
     if(details) return(list("dratio"=dratio[-1],"Diversity"=Rout[-1],"DivTot"=sum(Rout)))
     else return(sum(Rout))     
}#end of LiClaraEnt
 envLi=function(S,n){
 Rout=0
 for(i in 1:n){
 S$marks=S$marks[sample(1:length(S$marks))]
 Rout=c(Rout,LiClaraEnt(S))
 }
 return(Rout[-1])
 } ###########
 ######################################### 
 FBo=function(v,x,decreas=FALSE,l=1){
    #novar for the xth val
     
    xth=sort(v,decreas)[x]
	 
	 out=(1:length(v))[v==xth][l]
     return(out)
 }
###############
FBo2=function(v,x,prev=NULL,decreas=FALSE,l=1){
    #novar for the xth val
	
    xth=sort(v,decreas)[x]
	
	out=(1:length(v))[v==xth]
	if(length(out)!=1){
		outo=out
		i=1
		out=outo[1]
		if(!is.null(prev)){
		while(out %in% prev){
			i=i+1
			if(i> length(outo)){
				out=FBo2(v,x+1,prev)
			}
			else {
				out=outo[i]
			}
			
		}
		}
	}
	
	return(out)
}
 #############
 FBoV=function(A,x,sansV,decreas=FALSE){
         #nnover in A by row  removing sansV ligne
         vb=1:(dim(A)[1])
    for(i in 1:(dim(A)[1])){
      if(is.vector(sansV)){sansVi=sansV[i]}
      else{sansVi=as.vector(sansV[i,])}
     ord=(1:(dim(A)[2]))[-sansVi]
     xth=order(A[i,-sansVi], decreasing=decreas)
      vb[i]=ord[xth[x]]
    }     
  return(vb)
 }
 ######################################## FBoV(H1,1,ccY)
 
  choi.f=function(colld,choi){
  #aggregating along three dim with factors in choi
           if(!is.list(choi)) choi=list(choi,choi,choi)
          colld.1= aggreg.table(colld,dimagg=1,fact=choi[[1]],FUN=sum)
          colld.1= aggreg.table(colld.1,dimagg=2,fact=choi[[2]],FUN=sum)
          colld.1= aggreg.table(colld.1,dimagg=3,fact=choi[[3]],FUN=sum)
   return(c("HSu"=Entropyd(colld.1), "HSuA"=Entropyd(colld.1,Before=FALSE),"HS"=Entropyd(colld.1,RatioUniform=FALSE)))
  }#f
   ####
concat.f=function(A,sep=""){
	if(!is.null(dim(A))){
  	out=rep(A[1,1],dim(A)[1])
  	 for (i in 1:dim(A)[1]){
     	 out[i]=paste(A[i,1])
   		 for (j in 2:dim(A)[2]){
     		 out[i]=paste(out[i],A[i,j],sep=sep)
    	}
   	 }
   }
   else{
  		out=paste(A[1],sep=sep)
   for (i in 2:length(A)){
      out=paste(out,A[i],sep=sep)
   }
   }
  return(out) 
} #
###################################
getDistr <- function(T3way){
                 # T3way is a tensor
                 # cross includes dia
                 
                 if(length(dim(T3way))!=3 || !(dimnames(T3way)[[1]]==dimnames(T3way)[[2]]) || !(dimnames(T3way)[[1]]==dimnames(T3way)[[3]])){
                      #print(T3way)
                      #cat("not symmetric 3way","\n")
                      return(list("diagonly"=1,"crossdia"=T3way))
                      }
                dd= dim(T3way)[1]
                 for (i in 1:dd){
                  if(i==1){
                    dia <- T3way[i,i,i] 
                    cross <-T3way[i,,][lower.tri(T3way[i,,], diag=TRUE )]
                    namescross <-  matrix(paste(substr(dimnames(T3way)[[1]][1],1,2),outer(substr
                    (dimnames(T3way)[[1]],1,2),substr(dimnames(T3way)[[1]],1,2),FUN=paste)),c(dd,dd) )
                    namescross <-namescross[lower.tri(namescross,diag=TRUE)]
                    rim <-i
                    }
                  else {
                    dia<-c(dia,T3way[i,i,i])
                      cross <- c(cross, T3way[i,-rim,-rim][lower.tri(T3way[i,-rim,-rim], diag=TRUE)])
                      temp <-matrix(paste(substr(dimnames(T3way)[[1]][i],1,2),outer(substr
                    (dimnames(T3way)[[1]][-rim],1,2),substr(dimnames(T3way)[[1]][-rim],1,2),FUN=paste)), c(dd-length(rim),dd-length(rim)) )
                       temp <-temp[lower.tri(temp,diag=TRUE)]
                       namescross <-  c(namescross,temp)
                      rim <- c(rim,i)
                    }
                  } 
                                   
                  names(dia)=dimnames(T3way)[[1]]
                  names(cross)=namescross
                 return(list("diagonly"=dia,"crossdia"=cross))
              }
       ###
 Chi2k <-function(X,chi2Print=FALSE,E=NULL,No0margins=TRUE, verbose=FALSE){
         # done for FCAk         
         #temp <- FCAmet(T3way, chi2 = chi2, E = E) 
         #(X, chi2 = FALSE, E = NULL,No0margins=TRUE)

    if (!is.array(X)) {
        stop(paste("--- X must be an array  ! ---"))
    }
    
    datanam <- substitute(X)
    ord <- length(dim(X)) 
     N <- sum(X)
      metafc <- rep(list(NULL), ord)
       
    if(No0margins){
    evalCh.f<-function(st){
      #st is a expression quoted e.g."x=2"
      tmp <- tempfile()
       writeLines(st, tmp)
       return(eval.parent(parse(tmp)))      
         } #end of evalCh.f          
     library(tensorA)
      dnam=dimnames(X)
      X=to.tensor(as.vector(X),dim(X))
       for(t in 1:ord){
          metafc[[t]] <- apply(X, t, sum)
          if (any(metafc[[t]]==0)){
           if(verbose){
             cat("-------Zeros for margin:",t,"\n")
             cat("-------zero values replaced by min margin on dim:",t,"\n")
             cat("--------old $count N:",N,"\n")
             }
               the0=(1:length(metafc[[t]]))[metafc[[t]]==0]
              if(verbose)  cat(the0,"\n")
             amin=min(metafc[[t]][metafc[[t]]!=0])/prod(dim(X)[-t])
            
           evalCh.f(paste("X[[",names(X)[t],"=the0]]=amin",sep=""))

            #X[[(names(X)[t])=the0]]=min(metafc[[t]][metafc[[t]]!=0])/(N^2) doesn't work
            #  if(t=1)  X[["I1"=the0]]=min(metafc[[t]][metafc[[t]]!=0])/(N^2)
            #  if(t=2)  X[["I2"=the0]]=min(metafc[[t]][metafc[[t]]!=0])/(N^2)
            #  if(t=3)  X[["I3"=the0]]=min(metafc[[t]][metafc[[t]]!=0])/(N^2)
            #  if(t=4)  X[["I4"=the0]]=min(metafc[[t]][metafc[[t]]!=0])/(N^2)
          }
       }
       X=array(as.vector(X),dim(X))
       dimnames(X)=dnam 
     }
      
      N <- sum(X)
      
     metafc[[1]] <- apply(X, 1, sum)/N
        
     Indep <-  metafc[[1]]
    for (t in 2:ord) {
        metafc[[t]] <- apply(X, t, sum)/N
        Indep <- Indep %o% metafc[[t]]
    }
    
        Indep <- array(Indep, dim(X))
        Chi2 <- N * sum((X/N - Indep)^2/Indep)
        pchi <- pchisq(Chi2, df = prod(dim(X) -1), lower.tail = FALSE)
        
       if (chi2Print) {
        cat("\n", " --")
        cat("\n", "++++ Data is            ", deparse(datanam),
            "        +++++++")
        cat("\n", "-------------- Multiple contingency Table of dimensions ",
            dim(X), "  ----", "\n")
        cat("\n", "-------------- Chi2 = ", Chi2, " with ddl = ",
            prod(dim(X) - 1))
        cat("\n", " ------------- p(>Chi2)= ", pchi, "\n")
        cat("\n", " --", "\n")
    }
          dimnames(Indep)=dnam
    if (!is.null(E)) 
        invisible(list("E"=Indep*N, "data" = (X/N - E)/Indep, "met" = metafc,"count" = N, "Chi2"=list("Chi2"=Chi2,"pChi2"=pchi)))  
    else invisible(list("E"=Indep*N, "data" = (X/N)/Indep, "met" = metafc, "count" = N,"Chi2"=list("Chi2"=Chi2,"pChi2"=pchi))) 
}   #end of chi2k
#############################################   
 CrossEntr<- function(A, B=A,base=2, ALL=FALSE){
              U=1
               A=as.vector(A)
                B=as.vector(B)
          if(length(A)!=length(B))stop("A and B not same length")    
     if(sum(A)*sum(B)==0){
          Res=A; Hc=0}
     else{   
         A=A[A*B!=0]
         B=B[A*B!=0]             
             A=A/sum(A)
             B=B/sum(B)
        
              res=-A*log(B,base=base)/U
              Hsu= sum(res[res!=Inf])
     } 
             if(ALL){return(list("HcA"=Hsu, "ALL"=res))}
             else {return(Hsu)}
 } 
 #############
                 #cOO3d1Sel(S, d, matD = pairdist(S), inclu = FALSE, Iselmarks = NULL, 
                 # Jselmarks = NULL, Kselmarks = NULL) 
 
 #######################
 color.ramp <-function (nclass, color = "red", nvec = NULL, type = "q", colrmp=NULL){
	# colrmp=colorRampPalette(Yl)(31)
	#
	# is to define the colors breaks q is fro quantile
  eq.split <- function(ncl){
    mult <- rep((1 / ncl), (ncl - 1))
    mult * seq(1, (ncl - 1))
    }
  color.list <- list(cname = c("blue", "green", "yellow", "red"), hsvcol = c(0.7, 0.375, 0.19, 0))
  cind <- match(color, color.list$cname)
### change from "if(nvec)" to "if(!is.null(nvec))"
  if(!is.null(nvec)){
    if(type == "q"){
      pr <- eq.split(nclass)
### changes in min, quantile and max
      brks <- c(min(nvec, na.rm = TRUE),
                quantile(nvec, pr, names = FALSE, na.rm = TRUE),
                max(nvec, na.rm = TRUE))
      brks <- unique(brks)
      classvec <- cut(nvec, brks, labels = FALSE, include.lowest = TRUE)
      
      }
  else
    if(type == "e"){
      pr <- eq.split(nclass)
### changes in min, range and max
      brks <- c(min(nvec, na.rm = TRUE),
                pr * diff(range(nvec, na.rm = TRUE)),
                max(nvec, na.rm = TRUE))
      brks <- unique(brks)
      classvec <- cut(nvec, brks, labels = FALSE, include.lowest = TRUE)
      }
       ramp <- colrmp
      if(is.null(colrmp)) ramp <- hsv(rep(color.list$hsvcol[cind], nclass), c(pr, 1))
      return(list(ramp = ramp, col.class = classvec, breaks=brks))
    }
  return(NULL)
}
 #######
 plotMAP <-function(Poly=OAmap, colrmp=col.rmp,...){
 	########OAmap is of class SpatialPolygonsDataFrame from sp
 	### altogether library(maptools) requiring foreign sp and lattice ... also library(RColorBrewer)
 	#classic call:
 	  	# Yl <- brewer.pal(5, "PuOr")
		# colrmp = colorRampPalette(Yl)(10) 
 		# col.rmp <- color.ramp(nclass=10,  nvec = Ward.NSSEC2.FCAk[[1]]$v[28, ],color="red", colrmp=colrmp,type="q")
		# plotMAP(Poly=Wmap,colrmp=col.rmp)
		# title("2.72%  n°28: 37-7 vs111 33 77")
 	
 	
 	
 mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    par(las = 1)
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1
    par(mar = mar)
    plot.new()
    n=length(colrmp$ramp)
    if(FALSE){
    levels= quantile(colrmp$breaks,probs=seq(0,1,1/n))
      Ilevels=findInterval(levels,colrmp$breaks)
      }
      else{
      levels= col.rmp$breaks
             Ilevels=1:length(levels)
             }
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i", ...)
    rect(0, levels[-length(levels)], 1, levels[-1], col = colrmp$ramp[Ilevels])
    #rect(0, Ilevels[-length(Ilevels)], 1, Ilevels[-1], col = col.rmp$ramp[Ilevels])
   ##labels
 
       levels=pretty(levels,n=n/3)
       axis(4,at=levels,labels=as.character(levels))
    mar <- mar.orig
    mar[4] <- 1
    par(mar = mar)
    plot.new()
    xylims=summary(Poly)$bbox
     plot.window(xylims[1,], xylims[2,], "", asp = 1)
     colo=colrmp$ramp[colrmp$col.class]
     plot(Poly, border="white",col=colrmp$ramp[colrmp$col.class],lty=0,add=TRUE)
  }
  ################
 