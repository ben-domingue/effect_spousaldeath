## ##age-independent

## ###############################################################health and age
## pred<-function(zz,fm,nb=1000,proto.vars) {
##     library(lfe)
##     felm(fm,zz)->m
##     print(m)
##     print(length(m$resid))
##     for (iii in 1:length(proto.vars)) assign(names(proto.vars)[iii],proto.vars[[iii]])
##     expand.grid(proto.vars)->y
##     coef(m)->co
##     names(co)->nms
##     grep(":",nms)->ii
##     for (i in ii) {
##         nms[i]->nm
##         strsplit(nm,":")[[1]]->tmp
##         gsub(":",".",nm,fixed=TRUE)->nm
##         y[[tmp[1] ]]*y[[tmp[2] ]]->y[[nm]]
##         nm->nms[i]
##     }
##     nms->names(co)
##     y[,nms]->y
##     as.numeric(as.matrix(y) %*% matrix(co,ncol=1)) -> resp
##     ##
##     out<-list()
##     parfun<-function(i,zz,y,fm) {
##         sample(1:nrow(zz),nrow(zz),replace=TRUE)->index
##         zz[index,]->zzb
##         library(lfe)
##         felm(fm,zzb)->m
##         coef(m)->co
##         as.numeric(as.matrix(y) %*% matrix(co,ncol=1))
##     }
##     library(parallel)
##     makeCluster(10)->cl
##     clusterApply(cl,1:nb,parfun,zz=zz,y=y,fm=fm)->out
##     stopCluster(cl)
##     do.call("cbind",out)->out
##     apply(out,1,function(x) quantile(x,.025))->ql
##     apply(out,1,function(x) quantile(x,.975))->qh
##     resp->y$resp
##     ql->y$ql
##     qh->y$qh
##     y
## }

## load("lrd.Rdata")
## nb<-3
## ##
## formula("cond~dead.spouse+age+cond.lasttime|hhidpn|0|hhidpn")->fm
## vars<-c(cesd.std="CESD",iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## proto.vars<-list(
##     dead.spouse=0:1,
##     age=0,#seq(-1,2,length.out=50),
##     cond.lasttime=0
## )
## y<-list()
## for (var in names(vars)) {
##     tmp<-list()
##     for (mod.no in c(1,3,5)) {
##         eval(L.rd[[var]][[mod.no]]$model)->zz
##         pred(zz,fm=fm,nb=nb,proto.vars=proto.vars)->tmp[[as.character(mod.no)]]
##     }
##     tmp->y[[var]]
## }

## #save(y,file="boot_n.Rdata")


## vars<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## load("boot_n.Rdata")
## par(mfrow=c(2,2),mgp=c(2,1,0),oma=c(2,2,1,1),mar=c(3,3,1,1))
## pf<-function(z,...) {
##     plot(NULL,xlim=c(-1,2),ylim=c(-.2,0.8),xlab="",ylab="",bty="n")
##     z[z$dead.spouse==0,]->tmp
##     lines(tmp$age,tmp$resp,col="blue")
##     col2rgb("blue")->cc
##     polygon(c(tmp$age,rev(tmp$age)),c(tmp$ql,rev(tmp$qh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55)) 
##     z[z$dead.spouse==1,]->tmp
##     lines(tmp$age,tmp$resp,col="red")
##     col2rgb("red")->cc
##     polygon(c(tmp$age,rev(tmp$age)),c(tmp$ql,rev(tmp$qh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55)) 
## }
## formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
## for (var in names(vars)) {
##     y[[var]]->tmp
##     pf(z=tmp)
##     legend("topleft",bty="n",title=formal[var],legend="",cex=1.2)
##     mtext(side=2,line=2.3,"Expected Outcome",cex=1)
##     mtext(side=1,line=2.3,"Age (in decades relative to mean)",cex=1)
## }
## legend("bottomright",bty="n",fill=c("blue","red"),c("Observations prior to spousal death","Observations post-spousal death"),cex=.8)
## #mtext(side=3,adj=0,outer=TRUE,"A",line=-.8)
##                                         #mtext(side=1,line=2,"Age (in decades) relative to mean age at bereavement obs")

## ###############################################################health and depression
## L.cesd->L1
## #quantile(L1[[1]][[2]]$model$fp.cesd.res,c(.1,.9))->qu
## del<-se<-numeric()
## for (i in 1:length(L1)) {
##     plot(NULL,xlab="CESD (Relative to expectation at bereavement observation)",ylab="Standardized Outcome (at bereavement observation)",type="l",ylim=c(-.3,.35),xlim=c(qu[1]-.7,qu[2]+.7),bty="n",xaxt="n")
##     axis(side=1,at=seq(-2,3))
##     L1[[i]][['1']]->m
##     #seq(qu[1],qu[2],length.out=100)->xx
##     c(0,2)->xx
##     yy<-predict(m,data.frame(fp.cesd.res=xx,age=0,male=0,cesd.pre=mean(m$model$cesd.pre),cond.lasttime=0),se.fit=TRUE)
##     yy$fit[2]-yy$fit[1] -> del[i]
##     sqrt(yy$se.fit[1]^2+yy$se.fit[2]^2)->se[i]
## }
## library(gplots)
## names(L1)->names(del)
## par(mar=c(3,8,2,2),mgp=c(2,1,0),oma=rep(1,4))
## barplot2(del,horiz=TRUE,plot.ci=TRUE,ci.u=del+1.96*se,ci.l=del-1.96*se,col="blue",las=2,xaxt="n",xlab="Standardized Difference in health associated with CESD two units higher at bereavement")
## axis(side=1)

## ## L.cesd->L1
## ## quantile(L1[[1]][[2]]$model$fp.cesd.res,c(.1,.9))->qu
## ## par(mfrow=c(4,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## ## cols<-heat.colors(length(vars)) #cols<-c("orange","red","green","blue")
## ## for (i in 1:length(L1)) {
## ##     plot(NULL,xlab="CESD (Relative to expectation at bereavement observation)",ylab="Standardized Outcome (at bereavement observation)",type="l",ylim=c(-.3,.35),xlim=c(qu[1]-.7,qu[2]+.7),bty="n",xaxt="n")
## ##     axis(side=1,at=seq(-2,3))
## ##     L1[[i]][['1']]->m
## ##     seq(qu[1],qu[2],length.out=100)->xx
## ##     yy<-predict(m,data.frame(fp.cesd.res=xx,age=0,male=0,cesd.pre=mean(m$model$cesd.pre),cond.lasttime=0),se.fit=TRUE)
## ##     col2rgb(cols[i])->cc
## ##     polygon(c(xx,rev(xx)),c(yy$fit-1.96*yy$se.fit,rev(yy$fit+1.96*yy$se.fit)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55))
## ##     lines(xx,yy$fit)
## ##     length(xx)->ii
## ##     text(xx[ii],yy$fit[ii],vars[i],col=cols[i],pos=4)
## ##     text(xx[1],yy$fit[1],vars[i],col=cols[i],pos=2)
## ## }
## ## mtext(side=3,adj=.52,outer=TRUE,"B",line=-.8)
