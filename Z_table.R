#mod is a list of lm objects
table.lm<-function(mod,se=FALSE,pv=TRUE) {
  lapply(mod,function(x) names(coef(x)))->nms
  unique(do.call("c",nms))->nms
  length(nms)->nr
  length(mod)->nc
  mat.est<-mat.tstat<-mat.pv<-matrix(NA,nr,nc)
  for (j in 1:nc) {
    summary(mod[[j]])$coef->foo
    for (i in 1:nr) {
      match(nms[i],rownames(foo))->index
      if (length(index)>0) {
        foo[index,1]->mat.est[i,j]
        if (!se) foo[index,3]->mat.tstat[i,j] else foo[index,2]->mat.tstat[i,j]
        if (pv) foo[index,4]->mat.pv[i,j] 
      }
    }
  }
  sapply(mod,function(x) length(residuals(x)))->N
  out<-list()
  new.nms<-list()
  for (i in 1:nr) {
    if (pv) rbind(mat.est[i,],mat.tstat[i,],mat.pv[i,])->out[[i]] else rbind(mat.est[i,],mat.tstat[i,])->out[[i]]
    if (pv) {
        new.nms[[i]]<-c(nms[i],paste(nms[i],ifelse(!se,".tstat",".se"),sep=""),paste(nms[i],".pv",sep=""))
    } else {
        new.nms[[i]]<-c(nms[i],paste(nms[i],ifelse(!se,".tstat",".se"),sep=""))
        }
  }
  do.call("rbind",out)->out
  rbind(out,N)->out
  c(do.call("c",new.nms),"N")->rownames(out)
  out
}


