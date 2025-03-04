#' @encoding UTF-8
#' @keywords internal
"_PACKAGE"

#' @noRd
structure.fun <- function(design) {
  design.colnames <- colnames(design)
  novar <- length(colnames(design))
  effects.table <- matrix(" ",nrow=novar,ncol=novar)
  rownames(effects.table)<- design.colnames
  colnames(effects.table)<- design.colnames
  for (r.effect in 2:novar) effects.table[r.effect,1] <- 1  #Setting entries for mean column
  for (c.effect in 2:novar) effects.table[1,c.effect] <- 0  #Setting entries for mean row           
  for (r.effect in 2:novar) {
    for (c.effect in 2:novar) {
      if (r.effect != c.effect) {
        freq.table <- table(design[ ,r.effect],design[ ,c.effect])
        # freq.table contains number of times mth value of factor alpha occurs with kth value of factor betai
        # First condition checks whether there is a combination of alphath factor and kth value of betaith factor
        # which has a freq of 0 i.e. doesn't occur
        # If first condition is satisfied then table(freq.table[ ,k])[1] is number of times 0 occurs
        # Second condition checks whether all but one row has a value 0 for kth value of betaith factor
        # If both conditions are satisfied then the kth value of betaith factor is nested within alphath factor
        nestk <- rep(0, length=nrow(freq.table))   #Set up factor for all k levels of factorj to indicate whether nested within factor i
        for (k in 1:nrow(freq.table)) {
          if (min(freq.table[k,])==0 && table(freq.table[k, ])[1]+1==ncol(freq.table)) { 
            nestk[k] <- 1 
          } else {
            if (min(freq.table[k, ])==0) {
              nestk[k] <- 0.5 
            } else {
              nestk[k] <- 0
            }
          }
        }
        effects.table[r.effect,c.effect] <- if (all(nestk==1)) "1" else if (all(nestk==0)) "0" else "(0)"
      }
    }
  }
  effects.table
}

#' @noRd
extract.factor.fun <- function(effect) {
  pos_<-gregexpr("\\^",effect)
  posbo<-gregexpr("\\(",effect)
  posbc<-gregexpr("\\)",effect)
  posi_ <- -1
  #This loop finds position of ^ which separate effects but are not contained within a nested effect
  for (k in 1:length(pos_[[1]])) {
    if (length(posbo[[1]][posbo[[1]]< pos_[[1]][k]]) == length(posbc[[1]][posbc[[1]]< pos_[[1]][k]])) {
      posi_ <- c(posi_,pos_[[1]][k])
    }
  }
  if (length(posi_)>1) posi_ <- posi_[-1]
  if (posi_[1] ==-1) {
    vec.factor<- effect
  } else {
    no.factors <- 1+length(posi_)
    vec.factor <- rep("",no.factors)
    if (no.factors==2) {
      vec.factor[1] <- substring(effect,1,posi_[1]-1)
      vec.factor[2] <- substring(effect,posi_[1]+1,nchar(effect))
    } else {
      for (i in 1:(no.factors-2)) {
        if (i==1) vec.factor[1] <- substring(effect,1,posi_[i]-1)
        vec.factor[i+1] <- substring(effect,posi_[i]+1,posi_[i+1]-1)
        if (i==(no.factors-2)) vec.factor[i+2] <- substring(effect,posi_[i+1]+1,nchar(effect))
      }
    }
    vec.factor
  }
}

#' @noRd
extract.nestfactor.fun <- function(effect) {
  posbo<-gregexpr("\\(",effect)[[1]]      #opening brackets
  posbc<-gregexpr("\\)",effect)[[1]]      #closing brackets      
  if (posbo[1] == -1) {
    vec.factor<- effect
  } else {    
    posbci.lt <- posbc[c((posbc[-length(posbc)]<posbo[-1]),T)]
    posboi.lt <- posbo[c(T,(posbc[-length(posbc)]<posbo[-1]))]
    vec.factor <- substring(effect,1,posboi.lt[1]-1)    #Includes effect up to first opening bracket
    if (length(posbci.lt)>1) {
      for (lt in 1:(length(posbci.lt)-1)) {
        vec.factor <- paste(vec.factor,substring(effect,posbci.lt[lt]+1,posboi.lt[lt+1]-1),sep="")  #Includes effect after closed bracket corresponding first opening bracket up to next opneing bracket    
      }
    }
    if (posbci.lt[length(posbci.lt)] != nchar(effect)) {
      vec.factor <- paste(vec.factor,substring(effect,posbci.lt[length(posbci.lt)]+1,nchar(effect)),sep="")
    }
  }
  vec.factor
}

#' @noRd
brief.effect.fun <- function(effect, abbrev.length) {
  pos_<-gregexpr("\\^",effect)[[1]]
  if (pos_[1] ==-1) {
    brief.effect<- effect
  } else {
    no.factors <- 1+length(pos_)
    if (no.factors==2) {
      brief.effect <- paste(substring(effect,1,min(pos_[1]-1,abbrev.length[substring(effect,1,pos_[1]-1)])),"^",substring(effect,pos_[1]+1,min(pos_[1]+abbrev.length[substring(effect,pos_[1]+1,nchar(effect))],nchar(effect))),sep="")
    } else {
      for (i in 1:(no.factors-2)) {
        if (i==1) brief.effect <- substring(effect,1,min(pos_[1]-1,abbrev.length[substring(effect,1,pos_[1]-1)]))
        brief.effect <- paste(brief.effect,"^",substring(effect,pos_[i]+1,min(pos_[i]+abbrev.length[substring(effect,pos_[i]+1,pos_[i+1]-1)],pos_[i+1]-1)),sep="")
        if (i==(no.factors-2)) brief.effect <- paste(brief.effect,"^",substring(effect,pos_[i+1]+1,min(pos_[i+1]+abbrev.length[substring(effect,pos_[i+1]+1,nchar(effect))],nchar(effect))),sep="") 
      }
    }
  }
  brief.effect
}     


#' @noRd
sortaov.term.fun <- function(x, noall) {
  splitmatrixformat <- suppressWarnings(do.call(rbind, strsplit(x, ":")))
  sort.splitmatrix <- t(apply(as.matrix(splitmatrixformat),1,sort))
  sorted.effects <- matrix(NA, nrow=dim(sort.splitmatrix)[1], ncol=dim(sort.splitmatrix)[2] )
  for (k in 1:dim(sort.splitmatrix)[1] ) {
    for (j in 1:length(unique(sort.splitmatrix[k, ]) )) {
      sorted.effects[k,j] <- unique(sort.splitmatrix[k, ])[j]
    }
  }
  sortedx <- rep("",length(x))
  for (k in 1:length(x)) {
    for (j in 1:dim(sorted.effects)[2]) {
      if (j==1) sortedx[k] <- paste(sorted.effects[k,j])
      if (j>1 && sortedx[k]!="" && !is.na(sorted.effects[k,j])) sortedx[k] <- paste(sortedx[k],":",sorted.effects[k,j],sep="")
    }
  }
  sortedx     
}


#' @noRd
confound.tab.fun <- function(no.confounded, noall, all.nestedin.conf.table, conf.nestedin.all.table,
                             confounded.main, outputlistip1) {
  confound.main <- "" 
  confound.effect <- "" 
  if (no.confounded > 0) {
    for (confoundi in 1:no.confounded) {
      for (effectalli in 1:noall) {
        if (all.nestedin.conf.table[effectalli,confoundi]=="1" & conf.nestedin.all.table[confoundi,effectalli]=="1") {
          confound.main <- c(confound.main,confounded.main[confoundi])
          confound.effect <- c(confound.effect,colnames(outputlistip1$designi)[effectalli])
        }
      }
    }
  }
  confound.tab <- cbind(confound.main[-1],confound.effect[-1])
}

confound.print.fun <- function(confound.tab) {
  if (nrow(confound.tab) > 0) {
    for (confoundi in 1:nrow(confound.tab)) {
      cat("\nEffect ",confound.tab[confoundi,1]," is confounded with ",confound.tab[confoundi,2])
    }
  }
}


#' @noRd
designorder <- function (level, orderdsi, designi_1, order.effectsi_1, level.order.objectsi_1, 
                         keep.nested.objectsi_1, keep.nested.interactionsi_1, keep.order.objectsiuniqi_1, 
                         main.effects.table.nestintnames, main.effects.table, prelim.effect.order, 
                         maxfacs, datadesign) {
  
  #Add interactions
  #putting all interactions between effects of order i-1 into order.effectsi
  order.effectsi <- ""
  keep.order.objectsiuniqi <- ""
  if (length(order.effectsi_1)>1) {
    for (i in 1:length(order.effectsi_1)) {
      for (j in 1:length(order.effectsi_1)) {
        if (j > i) {
          int.name <- paste(order.effectsi_1[i],"^",order.effectsi_1[j],sep="")
          if (length(order.effectsi) > 1 || order.effectsi !="") {               
            order.effectsi <- c(order.effectsi,int.name)
          } else {
            order.effectsi <- int.name
          }
        }
      }
    }
  }
  if (length(keep.nested.objectsi_1) >1 || keep.nested.objectsi_1 != "") {
    if (length(order.effectsi) > 1 || order.effectsi !="") {
      order.effectsi <- c(order.effectsi,keep.nested.objectsi_1)
    } else {
      order.effectsi <- keep.nested.objectsi_1
    }
  }
  if (length(keep.nested.interactionsi_1) >1 || keep.nested.interactionsi_1 != "") {
    if (length(order.effectsi) > 1 || order.effectsi !="") {
      order.effectsi <- c(order.effectsi,keep.nested.interactionsi_1)
    } else {
      order.effectsi <- keep.nested.interactionsi_1
    }
  } 
  if (length(keep.order.objectsiuniqi_1) >1 || keep.order.objectsiuniqi_1 != "") {
    if (length(order.effectsi) > 1 || order.effectsi !="") {
      order.effectsi <- c(order.effectsi,keep.order.objectsiuniqi_1)
    } else {
      order.effectsi <- keep.order.objectsiuniqi_1
    }
  } 
  
  #-----------------------------------------------------------------------------------------------------------
  #Eliminating terms in order.effectsi which are repeats
  # First split terms into their components and eliminate repeats
  if (length(order.effectsi) >1 || order.effectsi != "") {
    factor.effectsi <- array("",dim=c(length(order.effectsi),maxfacs))
    for (k in 1:length(order.effectsi))  {
      factor.veci <- sort(unique(extract.factor.fun(order.effectsi[k])))
      factor.veci <- c(factor.veci,rep("",(maxfacs-length(factor.veci))))
      factor.effectsi[k, ] <- factor.veci
    }
    factor.effectsiuniq <- unique(factor.effectsi)
    
    #This now sorts factor.effectsiuniq so that effects with less terms come first - needed for nested algorithm
    for (j in 1: ncol(factor.effectsiuniq)) {
      factor.effectsiuniq<-rbind(factor.effectsiuniq[factor.effectsiuniq[ ,j]=="", ],factor.effectsiuniq[factor.effectsiuniq[ ,j]!="", ])
    }
    now.factor.effectsiuniq <- factor.effectsiuniq
    keep.factor.effectsiuniq <- factor.effectsiuniq
    #This restricts interactions to those with number of terms of level or less e.g. at level 4 interactions involve no more than 4 terms  
    now.factor.effectsiuniq<-now.factor.effectsiuniq[now.factor.effectsiuniq[ ,eval(level+1)]=="", ,drop=F]
    keep.factor.effectsiuniq<-keep.factor.effectsiuniq[keep.factor.effectsiuniq[ ,eval(level+1)]!="", ,drop=F]
    
    # Then merge components to give model interaction terms to be included at this level
    for (i in 1:nrow(now.factor.effectsiuniq)) {
      for (j in 1:maxfacs)  {
        if (now.factor.effectsiuniq[i,j] !="") {
          if (j==1) {
            int.val <- paste(datadesign[ ,paste(now.factor.effectsiuniq[i,j])],sep="")
            int.name <- now.factor.effectsiuniq[i,j]
          } else {
            int.val <- paste(int.val,"_",datadesign[ ,paste(now.factor.effectsiuniq[i,j])],sep="")
            int.name <- paste(int.name,"^",now.factor.effectsiuniq[i,j],sep="")
          }
        }
      }
      if (i==1) {
        designiints <- array(int.val,dim=c(length(int.val),1))
        order.effectsiuniq <- int.name
      } else {
        designiints <- cbind(designiints,int.val)
        order.effectsiuniq <- c(order.effectsiuniq,int.name)
      }
    }
    colnames(designiints)<- order.effectsiuniq
    # Then merge components to give model interaction terms to be included at lower levels
    
    if (nrow(keep.factor.effectsiuniq)>0) {
      for (i in 1:nrow(keep.factor.effectsiuniq)) {
        for (j in 1:maxfacs) {
          if (keep.factor.effectsiuniq[i,j] !="")  {
            if (j==1) {
              int.name <- keep.factor.effectsiuniq[i,j]
            } else {
              int.name <- paste(int.name,"^",keep.factor.effectsiuniq[i,j],sep="")
            }
          }
        }
        if (i==1) {
          keep.order.effectsiuniq <- int.name
        } else {
          keep.order.effectsiuniq <- c(keep.order.effectsiuniq,int.name)
        }
      }
      if (length(keep.order.effectsiuniq)>1) {
        keep.order.objectsiuniqi <- unique(keep.order.effectsiuniq) #Not sure about this bit
      } else {
        keep.order.objectsiuniqi <- ""
      }
    }
  }
  
  #Next look for effects identified in preliminary effects order as level i
  order.effectsxint <- main.effects.table.nestintnames[prelim.effect.order==orderdsi]
  order.effectsx <-rownames(main.effects.table)[prelim.effect.order==orderdsi]
  
  if (length(order.effectsx)>0) {
    designi <- cbind(designi_1,datadesign[ ,paste(order.effectsx)])
    colnames(designi) <- c(colnames(designi_1),order.effectsxint) 
  } else {
    designi<-designi_1                       
  }
  
  if (length(order.effectsi) >1 || order.effectsi != "") designi <- cbind(designi,designiints)
  
  effects.table.orderi <- structure.fun(designi)
  
  #-----------------------------------------------------------------------------------------------------------
  #Remove any effects which are confounded
  rm.effect <- 0
  no.effectsi <- length(colnames(effects.table.orderi))
  for (i in 1:(no.effectsi-1)) {
    for (j in (i+1):no.effectsi) {
      if (effects.table.orderi[i,j]=="1" & effects.table.orderi[j,i]=="1") rm.effect <- c(rm.effect,j)
    }
  }
  
  if (length(rm.effect)>1) {
    rm.effects <- unique(rm.effect[-1])
    designi <- designi[ ,-rm.effects]
    effects.table.orderi <- effects.table.orderi[-rm.effects,-rm.effects]
  }
  
  order.effects <-  colnames(designi)
  level.order.objectsi <- c(level.order.objectsi_1,rep(orderdsi,(length(colnames(designi))-length(level.order.objectsi_1))))
  
  rm.nested.effect <- 0
  keepa.nested.effect <- ""
  keep.nested.interaction <- ""
  no.effectsi <- length(colnames(effects.table.orderi))
  for (i in 1:(no.effectsi-1)) {
    for (j in (i+1):no.effectsi) {
      keep.nested.effectk <- ""
      if (level.order.objectsi[i] <= level.order.objectsi[j]) {
        i.factors <- extract.factor.fun(colnames(effects.table.orderi)[i])
        j.factors <- extract.factor.fun(colnames(effects.table.orderi)[j])
        if ((all(i.factors %in% j.factors))& (level.order.objectsi[i] != level.order.objectsi[j])) {
          ignore <- "y" 
        } else {
          ignore <- "n"
        }
        if (ignore=="n" & effects.table.orderi[i,j]=="(0)" & effects.table.orderi[j,i]=="1" & (length(i.factors)>1 | length(j.factors)>1)) {
          rm.nested.effect <- c(rm.nested.effect,j)  #Keeping col numbers of effects to be removed at this level
          
          #Keeping note of effects to be incorporated lower down
          if ((all(i.factors %in% j.factors)) & (level.order.objectsi[i] == level.order.objectsi[j]))  {
            keep.nested.interaction <- c(keep.nested.interaction,colnames(effects.table.orderi)[j])   #Keeping name of interaction effect which needs moving to a lower level
          } else {
            #Keeping name of interaction effect which needs moving to a lower level
            keep.nested.factors <- unique(c(i.factors,j.factors))
            for (k in 1:length(keep.nested.factors)) {
              if (k==1) {
                keep.nested.effectk <- keep.nested.factors[k]
              } else {
                keep.nested.effectk <- paste(keep.nested.effectk,"^",keep.nested.factors[k],sep="")
              }
            }
            keepa.nested.effect <- c(keepa.nested.effect,keep.nested.effectk)
          }
        }
      }
    }
  }
  
  #-----------------------------------------------------------------------------------------------------------
  
  if (length(rm.nested.effect)>1) {
    rm.nested.effects <- unique(rm.nested.effect[-1])
    designi <- designi[ ,-rm.nested.effects]
    effects.table.orderi <- effects.table.orderi[-rm.nested.effects,-rm.nested.effects]
  }
  
  if (length(keepa.nested.effect)>1) {
    keepa.nested.effects <- unique(keepa.nested.effect[-1])
  }
  
  if (length(keep.nested.interaction)>1) {
    keep.nested.interactionsi <- unique(keep.nested.interaction[-1])
  } else {
    keep.nested.interactionsi <- ""
  }
  
  if (length(keepa.nested.effect)>1) {
    factor.nested.effectsi <- array("",dim=c(length(keepa.nested.effects),maxfacs))
    for (k in 1:length(keepa.nested.effects)) {
      nested.veci <- sort(unique(extract.factor.fun(keepa.nested.effects[k])))
      nested.veci <- c(nested.veci,rep("",(maxfacs-length(nested.veci))))
      factor.nested.effectsi[k, ] <- nested.veci
    }
    nested.effectsiuniq <- unique(factor.nested.effectsi)
    order.nested.effectsiuniq <- ""
    
    # Then merge components to give model interaction terms
    for (i in 1:nrow(nested.effectsiuniq)) {
      for (j in 1:maxfacs)  {
        if (nested.effectsiuniq[i,j] !="") {
          if (j==1) {  
            int.name <- nested.effectsiuniq[i,j]  
          } else { 
            int.name <- paste(int.name,"^",nested.effectsiuniq[i,j],sep="") 
          }
        }
      }
      if (i==1) {  
        order.nested.effectsiuniq <- int.name
      } else {
        order.nested.effectsiuniq <- c(order.nested.effectsiuniq,int.name)
      }
    }
    keep.nested.objectsi<- order.nested.effectsiuniq
    
  } else {
    keep.nested.objectsi <- ""
  }
  
  #-----------------------------------------------------------------------------------------------------------
  order.effects <-  colnames(designi)
  level.order.objectsi <- c(level.order.objectsi_1,rep(orderdsi,(length(colnames(designi))-length(level.order.objectsi_1))))
  order.effectsi <- order.effects[level.order.objectsi==orderdsi]
  # outputlist <- list(designi, order.effectsi, level.order.objectsi)
  outputlist <- list(designi=designi,level.order.objectsi=level.order.objectsi)
  outputlist$keep.nested.objectsi <- keep.nested.objectsi
  outputlist$keep.nested.interactionsi <- keep.nested.interactionsi
  outputlist$keep.order.objectsiuniqi <- keep.order.objectsiuniqi
  outputlist
}

#' @noRd
model.effects.fun <- function (x) {
  #The function assumes the first term is the mean which is removed
  model.effects.facfn <- x[-1]
  for (i in 1:length(model.effects.facfn)) {
    model.effects.facfn[i] <- gsub("\\^",":",model.effects.facfn[i])
    model.effects.facfn[i] <- gsub("\\(",":",model.effects.facfn[i])
    model.effects.facfn[i] <- gsub("\\)","",model.effects.facfn[i])
    model.effects.facfn[i] <- gsub("\\:","\\):as.factor\\(",model.effects.facfn[i])
    model.effects.facfn[i] <- paste(" as.factor(", model.effects.facfn[i],")", sep="")
  }
  model.effects.facfn
}

#' @noRd
model.equation.fun <- function (x) {
  #creating the equation together with dummy response
  model.equation <- paste("DummyResponse ~ ", x[1],sep=" ")
  if(length(x)>1) {
    for (i in 2:(length(x))) model.equation<- paste(model.equation , x[i] , sep=" + ")
  }
  model.equation
}


#' @noRd
model.rhsequation.fun <- function (x) {
  #creating the equation without dummy response
  model.rhsequation <- paste("~ ", x[1],sep=" ")
  if(length(x)>1) {
    for (i in 2:(length(x))) model.rhsequation<- paste(model.rhsequation , x[i] , sep=" + ")
  }
  model.rhsequation
}


#' @noRd
strip.order.fun <- function (x) {
  x <- gsub(" ","",x)
  x <- gsub("as.factor\\(","",x)
  x <- gsub("\\)","",x)
  x
}


#' @noRd
strip.order.biglm.fun <- function (x) {
  x <- gsub(" ","",x)
  x <- gsub("as.factor","",x)
  x <- gsub("\\(","",x)
  x <- gsub("\\:","\\^",x)
  x <- gsub("\\)","",x)
  x <- gsub("Intercept","Mean",x)
  x <- gsub("[0123456789]","",x)
  x
}


#' @noRd
sort.effectorder.fun <- function(x) {
  #zzz^change
  splitmatrixformat <- suppressWarnings(do.call(rbind, strsplit(x, "\\^")))
  sorted.effects <- apply(t(apply(as.matrix(splitmatrixformat),1,sort)),1,unique)
  sortedx <- rep("",length(x))
  for (k in 1:length(x)) {
    for (i in 1:length(sorted.effects[[k]]))  {
      if (i==1) sortedx[k] <- paste(sorted.effects[[k]][i])
      if (i>1) sortedx[k] <- paste(sortedx[k],"^",sorted.effects[[k]][i],sep="")
    }
  }
  sortedx     
}


#' @noRd
dscoords.fun <- function(DStype, feffects, ceffects.table.fb, larger.fontlabelmultiplier,
                         smaller.fontlabelmultiplier, middle.fontlabelmultiplier) {
  if (DStype=="LS"){
    xfinaleffects <- feffects
    xceffects.table.final.brief <- ceffects.table.fb
  }
  if (DStype=="RLS"){
    xfinaleffects <- feffects
    xceffects.table.final.brief <- ceffects.table.fb
  }
  
  finaleffects.reverse <- c(xfinaleffects[length(xfinaleffects):2],0,xfinaleffects[1],0)
  
  coordsy <- 95 - 90* finaleffects.reverse / finaleffects.reverse[1]
  no.perlevel.reverse <- c(table(xfinaleffects)[length(table(xfinaleffects)):2],3)  #How many effects per level plus 3 at mean level
  max.no.perlevel <- max(no.perlevel.reverse)
  index.x <- 0
  coordsx <- rep(NA,length(coordsy))
  textlabel.size <- rep(larger.fontlabelmultiplier*1,length(coordsy))
  textlabel.size.df <- rep(larger.fontlabelmultiplier*0.85,length(coordsy))
  
  #Identifies whether terms in levels where there is only one term are nested in the term above -> single.nested
  #If so the term will be placed directly below the one above - else it will be moved
  single.nested <- rep(NA,length(no.perlevel.reverse))
  names(single.nested) <- names(no.perlevel.reverse)
  oneperlev <- which(xfinaleffects %in% names(no.perlevel.reverse[no.perlevel.reverse==1]))   #selects terms with one per level
  levs.with.one <- no.perlevel.reverse[no.perlevel.reverse==1]
  single.nested[names(levs.with.one)[length(oneperlev)]] <- "y"
  if (length(oneperlev) !=1) {
    for (i in (length(oneperlev)-1):1) {
      if (xceffects.table.final.brief[oneperlev,oneperlev][length(oneperlev)-i+1,length(oneperlev)-i]=="1") {
        single.nested[names(no.perlevel.reverse)== names(levs.with.one)[i]] <- "y" } else {
          single.nested[names(no.perlevel.reverse)== names(levs.with.one)[i]] <- "n" }
    }
  }
  
  single.coord <- 50
  for (m in 1:length(no.perlevel.reverse))  {
    upper <- no.perlevel.reverse[m]-1
    for (k in upper:0)  {
      index.x <- index.x + 1
      if (max.no.perlevel > 2) {
        leftd <- (30*max.no.perlevel-28*no.perlevel.reverse[m])/(max.no.perlevel-2)
        rightd <- 100-leftd
      } else {
        leftd<-30
        rightd<-70}
      if (m==length(no.perlevel.reverse)) {
        leftd<-0
        rightd<-100
      }
      if (upper != 0) coordsx[index.x] <- leftd + (100-2*leftd)* k / upper else {
        if (single.nested[m]=="n" && single.coord==30) single.coord <- 50 else if (single.nested[m]=="n" && single.coord==50) single.coord <- 30
        coordsx[index.x] <- single.coord
      }
      if (upper > 4) textlabel.size[index.x] <- smaller.fontlabelmultiplier*0.5
      if (upper > 4) textlabel.size.df[index.x] <- smaller.fontlabelmultiplier*0.5
      if ((length(grep("=",colnames(xceffects.table.final.brief)[length(colnames(xceffects.table.final.brief)):1][index.x]))>0) && (textlabel.size[index.x]==smaller.fontlabelmultiplier*0.5)) {
        textlabel.size[index.x] <- middle.fontlabelmultiplier*0.75  
      }
    }
  }
  
  coords.output <- list(coords=cbind(coordsx,coordsy),textlabel.size=textlabel.size,textlabel.size.df=textlabel.size.df )
  coords.output
}


#' @noRd
dfs.fun <- function(DStype, noall, feffects, ceffects.table.fb, adjm, outputlistip1, maxfacs,
                    maxlevels.df, check.confound.df, datadesign, finalnames.effects=NULL) {
  if (DStype=="LS") {
    xfinaleffects <- feffects
    xfinaldesign.effects <- names(feffects)
    xadjm <- adjm
    xceffects.table.final.brief <- ceffects.table.fb
  }
  
  if (DStype=="RLS") {
    xfinaleffects <- feffects
    xfinaldesign.effects <- names(feffects)
    xadjm <- adjm
    xceffects.table.final.brief <- ceffects.table.fb
  }
  
  #set up matrix for degrees of freedom, 1st col = Tier, 2nd col=max number of levels, 3rd col = actual number of levels, 4th col = dfs
  xdfs <- matrix(NA,nrow=length(xfinaleffects),ncol=4)
  rownames(xdfs) <- names(xfinaleffects)
  colnames(xdfs) <- c("Tier","Maxlev","Actlev","DFs")
  xdfs[ ,1] <- xfinaleffects
  
  #Provides number of levels present in the design - all effects and individual factors - restricts this to final effects for RLS
  numberlevs <- apply(outputlistip1$designi,2, function (x) {length(unique(x))})
  
  if (DStype=="LS") {
    xdfs[ ,3] <- numberlevs
  } else {
    if (DStype=="RLS") {
      xdfs[ ,3] <- apply(outputlistip1$designi,2, function (x) {length(unique(x))})[finalnames.effects %in% xfinaldesign.effects]
    }
  }
  #Set 1 degree of freedom for Mean
  xdfs[1,2] <- 1
  xdfs[1,4] <- 1
  
  #Find number of levels and dfs for first tier effects
  for (i in 1: length(xfinaleffects)) {
    if (xfinaleffects[i]==1) {
      xdfs[i,2] <- xdfs[i,3]
      xdfs[i,4] <- xdfs[i,3]-1
    }
  }
  
  #--------------------------------------------------------------------------------------------------------
  #Find max levels
  #name effects in final format e.g.nested names rather than as interactions
  if (DStype=="RLS") {
    names(xfinaleffects)<- xfinaldesign.effects
    names(numberlevs) <- finalnames.effects
    
  }
  
  if (DStype=="LS") {
    names(numberlevs) <- names(xfinaleffects)
  }
  
  numberlevs <- c("-"=1,numberlevs)
  
  #First split finaleffects in DS into individual terms
  factor.finaldfeffectsi <- array("-",dim=c(length(xfinaleffects),maxfacs))
  
  for (k in 1:length(xfinaleffects)) {
    factor.finaldfveci <- sort(unique(extract.factor.fun(names(xfinaleffects)[k])))
    factor.finaldfveci <- c(factor.finaldfveci,rep("-",(maxfacs-length(factor.finaldfveci))))
    factor.finaldfeffectsi[k, ] <- factor.finaldfveci
  }
  
  #Now multiply levels in each independent term to get max number of levels
  maxlevels <- rep(1,nrow(factor.finaldfeffectsi))
  maxlevelsf <- rep("",nrow(factor.finaldfeffectsi))
  
  for (i in 1:nrow(factor.finaldfeffectsi)) {
    for (j in 1:ncol(factor.finaldfeffectsi)) {
      maxlevels[i] <- maxlevels[i]*numberlevs[factor.finaldfeffectsi[i,j]]
      if (factor.finaldfeffectsi[i,2] != "-" && maxlevels.df == "Y")  maxlevelsf[i] <- paste(sep="","(",maxlevels[i],")")
    }
  }
  
  xdfs[ ,2] <- maxlevels
  
  #For effects below tier 1 sum the degrees of freedom for effects above and substract from actual number of levels
  for (m in eval(length(xfinaleffects[xfinaleffects<2])+1):length(xfinaleffects)) {
    xdfs[m,4] <- xdfs[m,3]-sum(xadjm[m,1:length(xfinaleffects[xfinaleffects<xfinaleffects[m]])]*xdfs[1:length(xfinaleffects[xfinaleffects<xfinaleffects[m]]),4])
    
    
  }
  
  #################################################################################################################
  #Looking for confounded degrees of freedom
  #############################################################################################################
  
  #call function to construct the equation terms with as.factor in front and replacing nested and * parts of effects with :
  model.effects.fac <- model.effects.fun(xfinaldesign.effects)
  
  #--------------------------------------------------------------------------------------------------------------------------
  #Compare degrees of freedom from program with rank of model terms except last, try to indicate where they may be
  if (check.confound.df=="Y") {
    #Remove bottom/residual term
    test.terms <- xceffects.table.final.brief[-nrow(xceffects.table.final.brief),-nrow(xceffects.table.final.brief)]
    #Removes rows and columns nested within higher terms, except the mean
    for (i in nrow(test.terms):2) {
      if (any(test.terms[ ,i]==1)) test.terms <- test.terms[-i,-i]
    }
    test.terms2 <- rep("NA",length(rownames(test.terms)))
    
    #Takes out terms which nest others
    for (k in 1:length(rownames(test.terms))) {
      test.terms2[k] <- extract.nestfactor.fun(rownames(test.terms)[k])
    }
    
    #Puts as.factor in front of each term
    model.terms.fac.test <- model.effects.fun(test.terms2)
    
    #call function to construct the righthand side of equation terms with as.factor in front and replacing nested and * parts of effects with :
    model.rhsequation  <- model.rhsequation.fun(model.terms.fac.test)
    
    RankCalculationError <- NULL  # Declare the variable before use
    
    #Calculates rank of model terms ignoring the last term (residual)
    tryCatch(
      Rank_Notresid<-qr(model.matrix.default(as.formula(model.rhsequation) , data=datadesign))$rank, error = function(err) {
        # warning handler picks up where error was generated
        print(paste("CALCULATING_RANK_ERROR:  ", err))
        RankCalculationError<-err
      }, finally = { Rank_Notresid <- NA})
    
    if (!is.null(RankCalculationError)) {
      testmess0<-"An error has occurred"
      testmess1<-"If the Error refers to too large a size for vector it is likely that the number of model terms and number of levels is too large."
      testmess2<-"This means that the degrees of freedom could not be checked for confounding between terms and model fitting is likely to be a problem."
      
      print(testmess0)
      print(RankCalculationError)
      print(testmess1)
      print(testmess2)
     } else {
      Rank_Notresid<-qr(model.matrix.default(as.formula(model.rhsequation) , data=datadesign))$rank
      
      #Compares rank with number of degrees of freedom calculated by method
      nonneg.xdfs <- xdfs[-nrow(xdfs), ]
      nonneg.xdfs[xdfs[-nrow(xdfs),4]<0,4]<-0 #If dfs <0 (except for residual) then set to 0
      nonneg.DFs_Notresid <- sum(nonneg.xdfs[ ,4])
      DFs_Notresid <- sum(xdfs[-nrow(xdfs),4])
      negdfs <- 0
      if (nonneg.DFs_Notresid != DFs_Notresid) {
        negdfs<-1     #Indicates if there are neg dfs above the resid level
      }
      xDFsDiff <- DFs_Notresid - Rank_Notresid
     }
    
    #If the degrees of freedom differ then it tries to find out where using aov
    
    #This now attempts to re-calculate the degrees of freedom and confounded degrees of freedom
    if (exists("xDFsDiff")) {
      if (negdfs == 0) {
        print.xDFsDiff <- paste(" ",xDFsDiff," ",sep="")
      } else {
        print.xDFsDiff <- NULL
      }  #The confounded dfs will only be printed if there are no negative dfs above the bottom object
      
      if (xDFsDiff > 0 & !is.null(print.xDFsDiff)) {
        cat("\nThere are",print.xDFsDiff, "confounded degrees of freedom\n")
        
        tiers <- as.numeric(names(table(xdfs[ ,1])))
        
        # For each tier, take model of all terms in that tier and below, drop each term and record dfs
        # For each term the max dfs is the number of dfs related to the term, diff dfs=max dfs-min dfs is the number of confounded dfs
        #Set up matrix to contain df results
        
        colnames.tier.dfs <- rep("",length(tiers))
        for (k in 1:length(tiers)-1) {
          colnames.tier.dfs[k] <- paste("dropdfs",k,sep="")
        }
        colnames.tier.dfs[length(tiers)] <- "confoundedf"
        colnames.tier.dfs[length(tiers)+1] <- "confoundedfs"
        colnames.tier.dfs[length(tiers)+2] <- "reviseddfs"
        tier.dfs <- matrix(NA,nrow=nrow(xceffects.table.final.brief)-1,ncol=length(tiers)+2,dimnames=list(rownames(xceffects.table.final.brief)[-1],colnames.tier.dfs))
        
        for (i in 2:length(tiers)) {
          test.termsi <- rownames(xceffects.table.final.brief[xdfs[ ,1]<=tiers[i],xdfs[ ,1]<=tiers[i]])  #select terms <= tier[i]
          #Puts as.factor in front of each term
          model.terms.fac.testi <- model.effects.fun(test.termsi)
          #call function to construct the right hand side of equation terms with as.factor in front and replacing nested and * parts of effects with :
          model.equation  <- model.equation.fun(model.terms.fac.testi)
          datadesign$DummyResponse = sample(1:100,dim(datadesign)[1],replace=T)
          #fit the aov model
          suppressWarnings(droptermi <- dropterm(aov(as.formula(model.equation), data=datadesign),test="F")[1])
          dropi <- droptermi[-1, ]
          if (any(grepl(":",rownames(droptermi)[-1]))) {        
            #checks of any terms are interactions
            namedropi <- gsub("\\:","\\^",sortaov.term.fun(as.vector(strip.order.fun(rownames(droptermi)[-1])), noall))
          } else {
            namedropi <- gsub("\\:","\\^",as.vector(strip.order.fun(rownames(droptermi)[-1]))) 
          }
          names(dropi) <- namedropi
          dropi
          
          #This finds no nested names and sorts interactions in alphabetical order    
          nonnested.rownames.tier.dfs <- rownames(tier.dfs)
          
          for (j in 1:length(rownames(tier.dfs))) {
            nonnested.rownames.tier.dfs[j] <- gsub("\\(","^",nonnested.rownames.tier.dfs[j])
            nonnested.rownames.tier.dfs[j] <- gsub("\\)","",nonnested.rownames.tier.dfs[j])
          }
          tier.dfs[rownames(tier.dfs)[which(sort.effectorder.fun(nonnested.rownames.tier.dfs)%in% names(dropi))],i-1] <- dropi
        }
        
        #Puts column confoundedf = 0 if no confounded dfs, 1 if confounded dfs
        tier.dfs[(apply(tier.dfs[ ,-length(tiers)],1,min,na.rm=T)==xdfs[-1,4]),length(tiers)] <- 0
        tier.dfs[(apply(tier.dfs[ ,-length(tiers) ],1,min,na.rm=T)!=xdfs[-1,4]),length(tiers)] <- 1
        mindrop.dfs <- apply(tier.dfs[ ,-length(tiers)],1,min,na.rm=T)
        tier.dfs <- cbind(tier=xdfs[-1,1],tier.dfs)
        
        #Finds first tier with confounded degrees of freedom
        sharetier <- 0
        
        for (i in 1:length(tiers)) {
          if (sharetier ==0 && any(tier.dfs[tier.dfs[ ,1]==i ,length(tiers)+1]==1)) sharetier <- i
        }
        #Puts dfs into column reviseddfs in tier.dfs up to and including the confounded tier
        tier.dfs[tier.dfs[  ,1]<=sharetier,length(tiers)+3]<-xdfs[-1, ][tier.dfs[  ,1]<=sharetier,4]
        
        for (i in 1:nrow(tier.dfs)) {
          if (tier.dfs[i,1] > sharetier) {
            coliname <- paste("term",i,sep="")
            tier.dfs <- cbind(tier.dfs,rep(0,nrow(tier.dfs)))
            colnames(tier.dfs)[ncol(tier.dfs)]<-coliname
            posterms <- sort.effectorder.fun(nonnested.rownames.tier.dfs[1:i])
            modeli <- c("mean",posterms[c(xadjm[i+1,2:length(feffects[feffects<feffects[i+1]])]==1,T)])
            model.testi <- model.effects.fun(modeli)
            
            #call function to construct the righthand side of equation terms with as.factor in front and replacing nested and * parts of effects with :
            model.equationi  <- model.equation.fun(model.testi)
            datadesign$DummyResponse = sample(1:100,dim(datadesign)[1],replace=T)
            suppressWarnings(anovai <- anova(aov(as.formula(model.equationi), data=datadesign),test="F")[1])
            anovai.df <- anovai[-nrow(anovai),1]#need to sort and put : to *
            names(anovai.df) <- rownames(anovai)[-nrow(anovai)]
            
            if (any(grepl(":",names(anovai.df)))) {        
              #checks of any terms are interactions
              nameanovai <- gsub("\\:","\\^",sortaov.term.fun(as.vector(strip.order.fun(names(anovai.df))), noall))
            } else {
              nameanovai <- gsub("\\:","\\^",as.vector(strip.order.fun(names(anovai.df)))) 
            }
            names(anovai.df) <- nameanovai
            
            anovai.dfs <- rep(0,length(modeli)-1)
            names(anovai.dfs) <- modeli[-1]
            anovai.dfs[names(anovai.dfs) %in% names(anovai.df)] <- anovai.df
            anovai.dfs
            
            tier.dfs[rownames(tier.dfs)[which(sort.effectorder.fun(nonnested.rownames.tier.dfs)%in% names(anovai.dfs))],ncol(tier.dfs)] <- anovai.dfs
            tier.dfs[i,length(tiers)+3] <- tier.dfs[i,ncol(tier.dfs)]
          }
        }
        
        tier.dfs[ ,length(tiers)+2]<-tier.dfs[  ,length(tiers)+3]-mindrop.dfs
        print.dfs <- cbind(actual.levs=xdfs[-1,3],dfs.by.subtract=xdfs[-1,4],confounded.dfs=tier.dfs[ ,length(tiers)+2])
        
        print.dfs <- as.data.frame(print.dfs)
        
        # Replace 0 with "No" and 1 with "Yes" in the confounded.dfs column
        print.dfs[, 3] <- ifelse(print.dfs[, 3] == 0, "No", "Yes")
        
        colnames(print.dfs) <- c("Actual levels", "DF by Subtraction", "Potential confounded DF")
        
        # View the updated matrix
        print(print.dfs)
        
      }	
    }
    }
  
  xdfs.reverse <- rbind(xdfs[nrow(xdfs):2, ],c(0,0,0,0),Mean=xdfs[1, ],c(0,0,0,0))  #Need to add in dummydfs for dummy nodes
  maxlevelsf.reverse <- c(maxlevelsf[nrow(xdfs):2],"","","")
  dfs.fun.output <- list(xdfs=xdfs,xdfs.reverse=xdfs.reverse,maxlevelsf.reverse=maxlevelsf.reverse)
  dfs.fun.output
  }


#' @noRd
anyna <- function(x) {any(is.na(x))}

