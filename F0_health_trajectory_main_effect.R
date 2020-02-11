## ##separately by age and with splines


## ##############################################################################################
## vars<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## names(vars)->vars
## source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/E0_precursor.R")
## ##linear
## df[!is.na(df$delta.death),]->df
## df[!is.na(df$fp.cesd.res),]->df
## ifelse(df$delta.death<0,abs(df$delta.death),0)->df$tl
## ifelse(df$delta.death>=0,abs(df$delta.death),0)->df$th

## ##adding age at spousal death
## df[df$fpd==1,]->tmp
## tmp$delta.death/(12*10) -> del
## tmp$age-del -> tmp$age.death
## tmp[,c("hhidpn","age.death")]->tmp
## merge(df,tmp)->df


## ## df[df$fpd==1,]->tmp
## ## #cut(tmp$age.raw,c(-Inf,quantile(tmp$age.raw,c(1/3,2/3)),Inf),labels=1:3)->tmp$gr
## ## mean(tmp$age.raw,na.rm=TRUE)->m
## ## ifelse(tmp$age.raw<m,1,2)->tmp$gr
## ## tmp[,c("hhidpn","gr")]->tmp
## ## merge(df,tmp)->df

## ## dim(df)
## ## df[abs(df$delta.death)<60,]->df
## ## dim(df)

## df[df$hhidpn %in% df$hhidpn[df$fpd==1] & df$delta.death<=60,]->df






## #####################################################################################
## ci<-FALSE
## outfun<-function(df,fm,fm.cesd,ci=ci,nb=1000) {
##     pred<-function(zz,fm,nb,pm,ci) {
##         library(lfe)
##         felm(fm,zz)->m
##         #print(m)
##         #print(length(m$resid))
##         ## for (iii in 1:length(proto.vars)) assign(names(proto.vars)[iii],proto.vars[[iii]])
##         ## expand.grid(proto.vars)->y
##         pm->y
##         coef(m)->co
##         names(co)->nms
##         grep(":",nms)->ii
##         if (length(ii)>0) {
##             for (i in ii) {
##                 nms[i]->nm
##                 strsplit(nm,":")[[1]]->tmp
##                 gsub(":",".",nm,fixed=TRUE)->nm
##                 y[[tmp[1] ]]*y[[tmp[2] ]]->y[[nm]]
##                 nm->nms[i]
##             }
##         }
##         nms->names(co)
##         y[,nms,drop=FALSE]->y
##         as.numeric(as.matrix(y) %*% matrix(co,ncol=1)) -> resp
##         ##
##         if (!ci) {
##             resp->y$resp
##             resp->y$ql
##             resp->y$qh
##             0->y$s
##         } else {
##             out<-list()
##             parfun<-function(i,zz,y,fm) {
##                 sample(1:nrow(zz),nrow(zz),replace=TRUE)->index
##                 zz[index,]->zzb
##                 library(lfe)
##                 felm(fm,zzb)->m
##                 coef(m)->co
##                 as.numeric(as.matrix(y) %*% matrix(co,ncol=1))
##             }
##             library(parallel)
##             makeCluster(20)->cl
##             clusterApply(cl,1:nb,parfun,zz=zz,y=y,fm=fm)->out
##             stopCluster(cl)
##             do.call("cbind",out)->out
##             apply(out,1,function(x) quantile(x,.025))->ql
##             apply(out,1,function(x) quantile(x,.975))->qh
##             apply(out,1,function(x) sd(x))->s
##             resp->y$resp
##             ql->y$ql
##             qh->y$qh
##             s->y$s
##         }
##         y
##     }
##     df[df$fpd==1,]->tmp
##     #paste("Age at death\nrange: ",min(tmp$age.raw),"-",max(tmp$age.raw),sep="")->tr.txt
##     ##
##     #source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/make_lm_table.R")
##     ff<-function(var,zz) {
##         zz[[var]]->zz$cond
##         library(lfe)
##         m<-list()
##         ##v3
##         #felm(cond~age|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['0']]
##         felm(fm,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['a']]
##         felm(fm.cesd,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['b']]
##         m
##     }
##     L1<-list()
##     for (var in vars) ff(var,df)->L1[[var]]
##     L1
##     ## do.call("c",L1)->m
##     ## #table.lm(m,se=FALSE,pv=FALSE)
##     ## pL1<-pL2<-list() 
##     ## for (i in 1:length(L1)) {
##     ##     tl<- seq(-24,-2,by=.01) #this time is in months. 
##     ##     th<- seq(2,36,by=.01)
##     ##     dd<-c(tl,th)
##     ##     df[df$fpd==1,]->tmp
##     ##     mean(tmp$age.death)->ad
##     ##     quantile(tmp$fp.cesd.res,c(.75,.25))->qu
##     ##     ##ad-dd/120 -> age
##     ##     data.frame(delta.death=dd,age.death=ad)->xx
##     ##     ifelse(xx$delta.death>0,1,0)->xx$dead.spouse
##     ##     print(i)
##     ##     ##
##     ##     to.pred<-list()
##     ##     1->xx$gr
##     ##     to.pred[[1]]<-xx
##     ##     xx->xx2
##     ##     0->xx2$dead.spouse
##     ##     2->xx2$gr
##     ##     xx2->to.pred[[2]]
##     ##     ##
##     ##     data.frame(do.call("rbind",to.pred))->xx.use
##     ##     pred(L1[[i]]$a$model,fm=fm,nb=nb,pm=xx.use,ci=ci)->y
##     ##     data.frame(t=y$delta.death,resp=y$resp,ql=y$ql,qh=y$qh,s=y$s,gr=xx.use$gr)->pL1[[names(L1)[i] ]]
##     ##     ##
##     ##     xx->xx2a
##     ##     qu[1]->xx2a$fp.cesd.res 
##     ##     1->xx2a$gr
##     ##     xx->xx2b
##     ##     qu[2]->xx2b$fp.cesd.res 
##     ##     2->xx2b$gr
##     ##     data.frame(rbind(xx2a,xx2b))->xx.use
##     ##     ifelse(xx.use$delta.death<0,0,xx.use$fp.cesd.res)->xx.use$fp.cesd.res #this is only non-zero at obs post-death
##     ##     pred(L1[[i]]$b$model,fm=fm.cesd,nb=nb,pm=xx.use,ci=ci)->y
##     ##     data.frame(t=y$delta.death,resp=y$resp,ql=y$ql,qh=y$qh,s=y$s,gr=xx.use$gr,fp.cesd.res=xx.use$fp.cesd.res)->pL2[[names(L1)[i] ]]
##     ## }
##     ## list(tr.txt=tr.txt,pL1=pL1,pL2=pL2)
## }
## ##linear age effect
## fm<-formula(cond~delta.death+dead.spouse+delta.death:dead.spouse|hhidpn|0|hhidpn)
## fm.cesd<-formula(cond~delta.death+fp.cesd.res+delta.death:fp.cesd.res|hhidpn|0|hhidpn)
##                                         #fm.cesd<-formula(cond~delta.death+fp.cesd.res+delta.death:fp.cesd.res+delta.death:age.death+fp.cesd.res:age.death+delta.death:fp.cesd.res:age.death|hhidpn|0|hhidpn)
## outfun(df,fm=fm,fm.cesd=fm.cesd,ci=ci)->LL2

## f<-function(x) summary(x)$coef
## lapply(LL2,function(x) f(x[[1]]))->t1
## lapply(LL2,function(x) f(x[[2]]))->t2

## #####################################################################################
## pf1<-function(pL,col,to.plot=FALSE,time.out=24) {
##     #formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",mobila="Mobility Problems",lgmusa="Large muscle",grossa="Gross motor problems",finea="Fine motor problems",)#,cesd="CESD")
##     formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
##     par(mar=c(3,3,1.5,3.5))
##     par(mgp=c(2,1,0))
##     lf<-function(xx,col,...) {
##         lines(xx$t,xx$resp,col=col,...,lwd=3)
##         col2rgb(col)->cc
##         polygon(c(xx$t,rev(xx$t)),c(xx$ql,rev(xx$qh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
##     }
##     out<-out.s<-list()
##     for (i in 1:length(pL)) {
##         pL[[i]]->tmp
##         ##
##         tmp[tmp$gr==2,]->tmp2
##         tmp2[tmp2$t>0,]->z2a
##         ##
##         tmp[tmp$gr==1,]->tmp
##         tmp[tmp$t<0,]->z1
##         tmp[tmp$t>0,]->z2
##         ##
##         z2a$resp[z2a$t== time.out]->yl
##         z2a$s[z2a$t== time.out]->sl
##         z2$resp[z2$t== time.out]->yh
##         z2$s[z2$t== time.out]->sh
##         if (to.plot & names(pL[i])=="iadlza") {
##             plot(NULL,xlim=range(pL[[1]]$t),ylim=c(-.15,.33),xlab="Time from spousal death (months)",ylab=formal[names(pL)[i] ])
##             abline(v=0,lwd=.5,col="gray")
##             ##
##                                         #lines(tmp$t,tmp$resp0,col="gray",lty=2)
##             lf(z1,type="l",col=col[1])
##                                         #segments(-12,yl,100,yl,lty=2,col="gray")
##             lf(z2,type="l",col=col[2])
##             segments(time.out,yh,100,yh,lwd=2,col="gray")
##             lf(z2a,type="l",col=col[3])
##             segments(time.out,yl,100,yl,lwd=2,col="gray")
##             mtext(side=4,at=(yl+yh)/2,text=format(yh-yl,digits=2),las=2,line=.2)
##             segments(37.5,yl,37.5,yh,col="red",lwd=4)
##             ##
##             col2rgb("pink")->cc
##                                         #polygon(c(-13,-11,-11,-13),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
##             polygon(c(25,23,23,25),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
##                                         #legend("bottomright",bty="n",fill=c("red","blue"),c("Older","Younger"))
##             #text(0,.35,formal[names(pL)[i] ],pos=4)
##         }
##         yh-yl->out[[names(pL)[i] ]]
##         sqrt(sl^2+sh^2)->out.s[[i]]
##     }
##     list(out=out,out.s=out.s)
## }
## pf2<-function(ll,yl,cols) { #horiz bars
##     #formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",mobila="Mobility Problems",lgmusa="Large muscle",grossa="Gross motor problems",finea="Fine motor problems")#,cesd="CESD")
##     formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
##     ll[[1]]->out
##     ll[[2]]->out.s
##     library(gplots)
##     par(mar=c(3.5,7,1,.5))
##     par(mgp=c(2.5,1,0))
##     barplot2(out,beside=TRUE,horiz=TRUE,
##              plot.ci=TRUE,ci.l=out-1.96*out.s,ci.u=out+1.96*out.s,
##              col=cols,
##              names.arg=formal[colnames(out)],
##              las=2,xlim=yl,xlab="Standardized Effect")->zz
##     for (i in 1:length(out)) {
##         out[[i]]+1.96*as.numeric(out.s[[i]])->yp
##         if (yp<0) 0->yp 
##         text(yp,zz[i],round(out[[i]],2),cex=.95,pos=4)
##     }
## }

## ##graphics
## c("pink","darkred")->cols
## time.out<-24
## #layout(matrix(c(1,1,1,2,2,3,3,3,4,4,5,5,5,6,6,7,7,7,8,8),4,5,byrow=TRUE))
## layout(matrix(c(1,1,2,2,2,3,3,4,4,4),2,5,byrow=TRUE))
## par(oma=rep(1.5,4),lwd=1)
## ##
## mat<-list()
## for (i in 1:length(LL2)) pf1(LL2[[i]]$pL1,to.plot=FALSE,time.out=time.out)->mat[[i]]
## pf1(LL2[[2]]$pL1,to.plot=TRUE,col=c("blue","red","blue"),time.out=time.out)->foo
## legend(-23,.27,bty="n",title=LL2[[2]]$tr.txt,legend=c("Pre-death trend","Post-death"),fill=c("blue","red"),cex=.75)
## mtext(side=3,adj=0,"A")
## ##
## mat2<-list()
## for (i in 1:2) {
##     lapply(mat,"[[",i)->tmp
##     do.call("rbind",lapply(tmp,unlist))->mat2[[i]]
## }
## pf2(mat2,yl=c(-.05,.35),cols=cols)
## lapply(LL2,"[[",1)->txt
## gsub("\n"," ",txt)->txt
## legend("bottomright",bty="n",fill=rev(cols),legend=rev(txt),cex=.95)
## ##
## mat<-list()
## for (i in 1:length(LL2)) pf1(LL2[[i]]$pL2,to.plot=FALSE,time.out=time.out)->mat[[i]]
## pf1(LL2[[2]]$pL2,to.plot=TRUE,col=c("gray","darkblue","lightblue"),time.out=time.out)
## legend(-23,.27,bty="n",title=LL2[[2]]$tr.txt,legend=c("Pre-death","25th percentile adjusted CESD","75th percentile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.75)
## mtext(side=3,adj=0,"B")
## ##
## mat2<-list()
## for (i in 1:2) {
##     lapply(mat,"[[",i)->tmp
##     do.call("rbind",lapply(tmp,unlist))->mat2[[i]]
## }
## pf2(mat2,yl=c(-.05,.35),cols=cols)
## #legend("topleft",bty="n",fill=heat.colors(4),legend=txt,cex=.7)







