##separately by age and with splines


##############################################################################################
vars<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
names(vars)->vars
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/E0_precursor.R")


##linear
df[!is.na(df$delta.death),]->df
df[!is.na(df$fp.cesd.res),]->df
ifelse(df$delta.death<0,abs(df$delta.death),0)->df$tl
ifelse(df$delta.death>=0,abs(df$delta.death),0)->df$th

##adding age at spousal death
df[df$fpd==1,]->tmp
tmp$delta.death/(12*10) -> del
tmp$age-del -> tmp$age.death
tmp[,c("hhidpn","age.death")]->tmp
merge(df,tmp)->df


df[df$fpd==1,]->tmp
#cut(tmp$age.raw,c(-Inf,quantile(tmp$age.raw,c(1/3,2/3)),Inf),labels=1:3)->tmp$gr
mean(tmp$age.raw,na.rm=TRUE)->m
ifelse(tmp$age.raw<m,1,2)->tmp$gr
tmp[,c("hhidpn","gr")]->tmp
merge(df,tmp)->df

## dim(df)
## df[abs(df$delta.death)<60,]->df
## dim(df)

df[df$hhidpn %in% df$hhidpn[df$fpd==1] & df$delta.death<=60,]->df

df$delta.death/60->df$delta.death
## ifelse(!is.na(df$del.interview) & df$del.interview<5,1,0)->df$dead.5year
## df[df$dead.5year==1,]->tmp
## df[!(df$hhidpn %in% tmp$hhidpn),]->df

split(df,df$gr)->LL

f<-function(x) length(unique(x$hhidpn))
lapply(LL,f)


save.image(file="LL.Rdata")



#####################################################################################

load("LL.Rdata")
outfun<-function(df,fm,fm.cesd,ci=ci,nb=1000) {
    pred<-function(zz,fm,nb,pm,ci) {
        library(lfe)
        felm(fm,zz)->m
        print(summary(m))
        #print(m)
        #print(length(m$resid))
        ## for (iii in 1:length(proto.vars)) assign(names(proto.vars)[iii],proto.vars[[iii]])
        ## expand.grid(proto.vars)->y
        pm->y
        coef(m)->co
        names(co)->nms
        grep(":",nms)->ii
        if (length(ii)>0) {
            for (i in ii) {
                nms[i]->nm
                strsplit(nm,":")[[1]]->tmp
                gsub(":",".",nm,fixed=TRUE)->nm
                y[[tmp[1] ]]*y[[tmp[2] ]]->y[[nm]]
                nm->nms[i]
            }
        }
        nms->names(co)
        y[,nms,drop=FALSE]->y
        as.numeric(as.matrix(y) %*% matrix(co,ncol=1)) -> resp
        ##
        if (!ci) {
            return(m)
        } else {
            out<-list()
            parfun<-function(i,zz,y,fm) {
                sample(1:nrow(zz),nrow(zz),replace=TRUE)->index
                zz[index,]->zzb
                library(lfe)
                felm(fm,zzb)->m
                coef(m)->co
                as.numeric(as.matrix(y) %*% matrix(co,ncol=1))
            }
            library(parallel)
            RNGkind("L'Ecuyer-CMRG")
            ## makeCluster(20)->cl
            ## clusterApply(cl,1:nb,parfun,zz=zz,y=y,fm=fm)->out
            ## stopCluster(cl)
            mclapply(1:nb,parfun,zz=zz,y=y,fm=fm,mc.cores=10)->out
            do.call("cbind",out)->out
            apply(out,1,function(x) quantile(x,.025))->ql
            apply(out,1,function(x) quantile(x,.975))->qh
            apply(out,1,function(x) sd(x))->s
            resp->y$resp
            ql->y$ql
            qh->y$qh
            s->y$s
            return(y)
        }
    }
    df[df$fpd==1,]->tmp
    paste("Age at death\nrange: ",min(tmp$age.raw),"-",max(tmp$age.raw),sep="")->tr.txt
    ##
    #source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/make_lm_table.R")
    ff<-function(var,zz) {
        zz[[var]]->zz$cond
        library(lfe)
        m<-list()
        ##v3
        #felm(cond~age|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['0']]
        felm(fm,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['a']]
        felm(fm.cesd,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['b']]
        ##get numbers for figure captions
        numfun<-function(m) {
            print(nrow(m$model))
            print(length(unique(m$model$hhidpn)))
        }
        print(var)
        lapply(m,numfun)
        m
    }
    L1<-list()
    for (var in vars) ff(var,df)->L1[[var]]
    do.call("c",L1)->m
    if (!ci) {
        source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
        table.lm(m,se=TRUE,pv=TRUE)
    } else {
        pL1<-pL2<-list() 
        for (i in 1:length(L1)) {
            tl<- seq(-12,-2,by=.01) #this time is in months. 
            th<- seq(2,60,by=.01)
            tl/60 -> tl
            th/60 -> th
            dd<-c(tl,th)
            df[df$fpd==1,]->tmp
            mean(tmp$age.death)->ad
            quantile(tmp$fp.cesd.res,c(.75,.25))->qu
            ##ad-dd/120 -> age
            data.frame(delta.death=dd,age.death=ad)->xx
            ifelse(xx$delta.death>0,1,0)->xx$dead.spouse
            print(i)
            ##
            to.pred<-list()
            1->xx$gr
            to.pred[[1]]<-xx
            xx->xx2
            0->xx2$dead.spouse
            2->xx2$gr
            xx2->to.pred[[2]]
            ##
            data.frame(do.call("rbind",to.pred))->xx.use
            pred(L1[[i]]$a$model,fm=fm,nb=nb,pm=xx.use,ci=ci)->y
            data.frame(t=y$delta.death,resp=y$resp,ql=y$ql,qh=y$qh,s=y$s,gr=xx.use$gr)->pL1[[names(L1)[i] ]]
            ##
            xx->xx2a
            qu[1]->xx2a$fp.cesd.res 
            1->xx2a$gr
            xx->xx2b
            qu[2]->xx2b$fp.cesd.res 
            2->xx2b$gr
            data.frame(rbind(xx2a,xx2b))->xx.use
            ifelse(xx.use$delta.death<0,0,xx.use$fp.cesd.res)->xx.use$fp.cesd.res #this is only non-zero at obs post-death
            pred(L1[[i]]$b$model,fm=fm.cesd,nb=nb,pm=xx.use,ci=ci)->y
            data.frame(t=y$delta.death,resp=y$resp,ql=y$ql,qh=y$qh,s=y$s,gr=xx.use$gr,fp.cesd.res=xx.use$fp.cesd.res)->pL2[[names(L1)[i] ]]
        }
        list(tr.txt=tr.txt,pL1=pL1,pL2=pL2)
    }
}

##linear age effect
fm<-formula(cond~delta.death+dead.spouse+delta.death:dead.spouse+delta.death:age.death+dead.spouse:age.death+delta.death:dead.spouse:age.death|hhidpn|0|hhidpn)
#fm.cesd<-formula(cond~dead.spouse*delta.death+fp.cesd.res+delta.death:fp.cesd.res|hhidpn|0|hhidpn)
#fm.cesd<-formula(cond~dead.spouse*delta.death+fp.cesd.res+delta.death:fp.cesd.res+delta.death:age.death+fp.cesd.res:age.death+delta.death:fp.cesd.res:age.death|hhidpn|0|hhidpn)
fm.cesd<-formula(cond~dead.spouse*delta.death+delta.death*fp.cesd.res|hhidpn|0|hhidpn)
LL2<-list()



ci<-TRUE
for (i in 1:length(LL)) outfun(LL[[i]],fm=fm,fm.cesd=fm.cesd,ci=ci)->LL2[[i]]
save(LL2,file="LL_boot.Rdata")

##for table in si
LL2<-list()
for (i in 1:length(LL)) outfun(LL[[i]],fm=fm,fm.cesd=fm.cesd,ci=FALSE)->LL2[[i]]
LL2[[2]]->tab
##cesd
grep("b$",colnames(tab))->index
write.csv(tab[,index],"")
##health
grep("a$",colnames(tab))->index
write.csv(tab[,index],"")






########################################################################################
##barplots
##main text figure 3
load("LL_boot.Rdata")
f<-function(x) {
    x[x$t==1,]->tmp ##1=5 years
    tmp[order(tmp$fp.cesd.res),]->tmp
    tmp[,c("resp","s")]
}
lapply(rev(LL2[[2]]$pL2),f)->L
lapply(L,function(x) x[,1])->est
do.call("cbind",est)->est
lapply(L,function(x) x[,2])->se
do.call("cbind",se)->se
library(gplots)
vars<-c(hsptim="Hospital Visits",conde="Chronic Health Conditions",adla="Disability - ADLs",iadlza="Disability - IADLs",cesd.std="Depressive Symtoms")
#tiff("/tmp/fig3.tiff", width = 3.2, height = 3, units = 'in', res = 300,pointsize=8)
par(mgp=c(2,1,0),mar=c(4,3,1,1),oma=c(1,8,1,1))
barplot2(est,horiz=TRUE,beside=TRUE,col=c("lightblue","blue"),
         names=vars[colnames(est)],las=2,xaxt="n",xlim=c(0,.65),
         plot.ci=TRUE,ci.u=est+1.96*se,ci.l=est-1.96*se,cex.axis=.7,
         xlab="")->vals
mtext(side=1,line=3,"Projection (SD units) 5 years\nfollowing spousal death",cex=1)
axis(side=1)
legend("bottomright",bty="n",title="Depression",c("75th %-ile","25th %-ile"),cex=.6,fill=c("blue","lightblue"))
for (i in 1:ncol(est)) text(est[,i]+1.96*se[,i],vals[,i],round(est[,i],2),cex=.7,pos=4)
#dev.off()

##for si, younger respondents
lapply(rev(LL2[[1]]$pL2),f)->L
lapply(L,function(x) x[,1])->est
do.call("cbind",est)->est
lapply(L,function(x) x[,2])->se
do.call("cbind",se)->se
library(gplots)
vars<-c(hsptim="Hospital Visits",conde="Chronic Health Conditions",adla="Disability - ADLs",iadlza="Disability - IADLs",cesd.std="Depressive Symtoms")
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=c(1.5,10,1,1))
barplot2(est,horiz=TRUE,beside=TRUE,col=c("lightblue","blue"),
         names=vars[colnames(est)],las=2,xaxt="n",xlim=c(-.2,.55),
         plot.ci=TRUE,ci.u=est+1.96*se,ci.l=est-1.96*se,
         xlab="")->vals
mtext(side=1,line=3,"Projection (SD units)  5 years\nfollowing spousal death",cex=1)
axis(side=1)
legend("bottomright",bty="n",title="Depression",c("75th %-ile","25th %-ile"),cex=.75,fill=c("blue","lightblue"))
    for (i in 1:ncol(est)) text(est[,i]+1.96*se[,i],vals[,i],round(est[,i],2),cex=.75,pos=4)

    
## #####################################################################################
## ##differences
## pf1<-function(pL,col,to.plot=FALSE,time.out=48,...) { #version focusing on difference
##     formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
##     lf<-function(xx,col,...) {
##         lines(xx$t,xx$resp,col=col,...,lwd=3)
##         col2rgb(col)->cc
##         #polygon(c(xx$t,rev(xx$t)),c(xx$ql,rev(xx$qh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75),border=NA)
##         polygon(c(xx$t,rev(xx$t)),c(xx$resp-1.96*xx$s,rev(xx$resp+1.96*xx$s)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75),border=NA)
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
##         z2$resp-z2a$resp -> z2$resp
##         sqrt(z2$s^2+z2a$s^2) -> z2$s
##         if (to.plot) {
##             plot(NULL,xlim=c(0,60),xlab="Time from spousal death (months)",ylab="Difference",...,bty="n")
##             legend("topleft",bty="n",formal[names(pL)[i] ])
##             abline(v=0,lwd=.5,col="gray")
##             abline(h=0,lwd=.5,col="gray")
##             lf(z2,type="l",col=col)
##             mtext(side=3,at=time.out,text=prettyNum(round(100*(yh-yl))/100,width=3,digits=2,nsmall=2),line=.2,cex=.75)
##             col2rgb("pink")->cc
##             polygon(c(time.out+1,time.out-1,time.out-1,time.out+1),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
##             mtext(side=3,line=.2,adj=0,LETTERS[i])
##         }
##         yh-yl->out[[names(pL)[i] ]]
##         sqrt(sl^2+sh^2)->out.s[[i]]
##     }
##     list(out=out,out.s=out.s)
## }
## ##graphics
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[2]]$pL2,to.plot=TRUE,col=c("blue"),time.out=time.out,ylim=c(-.05,.4))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## ## legend(5,-.1,bty="n",
## ##        #title=LL2[[2]]$tr.txt,
## ##        legend=c("Pre-death","25th %ile adjusted CESD","75th %ile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.95)

## ##for si, younger
## ##graphics
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[1]]$pL2,to.plot=TRUE,col=c("blue"),time.out=time.out,ylim=c(-.25,.4))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## ## legend(5,-.1,bty="n",
## ##        #title=LL2[[2]]$tr.txt,
## ##        legend=c("Pre-death","25th %ile adjusted CESD","75th %ile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.95)

## ##for si, health
## ##graphics
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[2]]$pL1,to.plot=TRUE,col=c("blue","red","blue"),time.out=time.out,ylim=c(-.1,.4))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## lapply(LL2,"[[",1)->txt
## gsub("\n"," ",txt)->txt
## legend(5,.35,bty="n",
##        legend=c("Pre-death trend","Post-death"),
##        fill=c("blue","red"),cex=1)

#####################################################################################
#original showing both
pf1<-function(pL,col,to.plot=FALSE,time.out=1,...) {
    formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
    lf<-function(xx,col,...) {
        lines(xx$t,xx$resp,col=col,...,lwd=3)
        col2rgb(col)->cc
        polygon(c(xx$t,rev(xx$t)),c(xx$ql,rev(xx$qh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75),border=NA)
    }
    out<-out.s<-list()
    for (i in 1:length(pL)) {
        pL[[i]]->tmp
        ##
        tmp[tmp$gr==2,]->tmp2
        tmp2[tmp2$t>0,]->z2a
        ##
        tmp[tmp$gr==1,]->tmp
        tmp[tmp$t<0,]->z1
        tmp[tmp$t>0,]->z2
        ##
        z2a$resp[z2a$t== time.out]->yl
        z2a$s[z2a$t== time.out]->sl
        z2$resp[z2$t== time.out]->yh
        z2$s[z2$t== time.out]->sh
        if (to.plot) {
            plot(NULL,xlim=range(pL[[1]]$t),xlab="Time from spousal death (years)",ylab=formal[names(pL)[i] ],...,xaxt="n")
            axis(side=1,at=seq(-.2,1,by=.2),c(-1:5))
            abline(v=0,lwd=.5,col="gray")
            ##
                                        #lines(tmp$t,tmp$resp0,col="gray",lty=2)
            lf(z1,type="l",col=col[1])
                                        #segments(-12,yl,100,yl,lty=2,col="gray")
            lf(z2,type="l",col=col[2])
            #segments(time.out,yh,100,yh,lwd=2,col="gray")
            lf(z2a,type="l",col=col[3])
            #segments(time.out,yl,100,yl,lwd=2,col="gray")
            #mtext(side=4,at=(yl+yh)/2,text=format(yh-yl,digits=2),las=2,line=.2)
            #mtext(side=3,at=time.out,text=format(yh-yl,digits=2,nsmall=2),line=.2)
            mtext(side=3,at=time.out,text=prettyNum(round(100*(yh-yl))/100,width=3,digits=2,nsmall=2),line=.2,cex=.75)
            #segments(60,yl,60,yh,col="red",lwd=4)
            ##
            col2rgb("pink")->cc
                                        #polygon(c(-13,-11,-11,-13),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
            polygon(c(time.out+.025,time.out-.025,time.out-.025,time.out+.025),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
                                        #legend("bottomright",bty="n",fill=c("red","blue"),c("Older","Younger"))
            #text(0,.35,formal[names(pL)[i] ],pos=4)
            mtext(side=3,line=.2,adj=0,LETTERS[i])
        }
        yh-yl->out[[names(pL)[i] ]]
        sqrt(sl^2+sh^2)->out.s[[i]]
    }
    list(out=out,out.s=out.s)
}

## ##original call
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-1
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[2]]$pL2,to.plot=TRUE,col=c("gray","darkblue","lightblue"),time.out=time.out,ylim=c(-.1,.45))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## legend(5,-.1,bty="n",
##        #title=LL2[[2]]$tr.txt,
##        legend=c("Pre-death","25th %ile adjusted CESD","75th %ile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.95)

## ##for si, younger
## #graphics
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[1]]$pL2,to.plot=TRUE,col=c("gray","darkblue","lightblue"),time.out=time.out,ylim=c(-.2,.25))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## legend(5,-.1,bty="n",
##        #title=LL2[[2]]$tr.txt,
##        legend=c("Pre-death","25th %ile adjusted CESD","75th %ile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.95)

##for si, health
##graphics
c("pink","darkred")->cols
load("LL_boot.Rdata")
time.out<-1#48
par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
##
pf1(LL2[[2]]$pL1,to.plot=TRUE,col=c("blue","red","blue"),time.out=time.out,ylim=c(-.1,.4))->tmp
as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
lapply(LL2,"[[",1)->txt
gsub("\n"," ",txt)->txt
legend(5,.35,bty="n",
       legend=c("Pre-death trend","Post-death"),
       fill=c("blue","red"),cex=1)

#####################################################################################
##based on old timing (months rather than years)
## #original showing both
## pf1<-function(pL,col,to.plot=FALSE,time.out=48,...) {
##     formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
##     lf<-function(xx,col,...) {
##         lines(xx$t,xx$resp,col=col,...,lwd=3)
##         col2rgb(col)->cc
##         polygon(c(xx$t,rev(xx$t)),c(xx$ql,rev(xx$qh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75),border=NA)
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
##         if (to.plot) {
##             plot(NULL,xlim=range(pL[[1]]$t),xlab="Time from spousal death (months)",ylab=formal[names(pL)[i] ],...)
##             abline(v=0,lwd=.5,col="gray")
##             ##
##                                         #lines(tmp$t,tmp$resp0,col="gray",lty=2)
##             lf(z1,type="l",col=col[1])
##                                         #segments(-12,yl,100,yl,lty=2,col="gray")
##             lf(z2,type="l",col=col[2])
##             #segments(time.out,yh,100,yh,lwd=2,col="gray")
##             lf(z2a,type="l",col=col[3])
##             #segments(time.out,yl,100,yl,lwd=2,col="gray")
##             #mtext(side=4,at=(yl+yh)/2,text=format(yh-yl,digits=2),las=2,line=.2)
##             #mtext(side=3,at=time.out,text=format(yh-yl,digits=2,nsmall=2),line=.2)
##             mtext(side=3,at=time.out,text=prettyNum(round(100*(yh-yl))/100,width=3,digits=2,nsmall=2),line=.2,cex=.75)
##             #segments(60,yl,60,yh,col="red",lwd=4)
##             ##
##             col2rgb("pink")->cc
##                                         #polygon(c(-13,-11,-11,-13),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
##             polygon(c(time.out+1,time.out-1,time.out-1,time.out+1),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
##                                         #legend("bottomright",bty="n",fill=c("red","blue"),c("Older","Younger"))
##             #text(0,.35,formal[names(pL)[i] ],pos=4)
##             mtext(side=3,line=.2,adj=0,LETTERS[i])
##         }
##         yh-yl->out[[names(pL)[i] ]]
##         sqrt(sl^2+sh^2)->out.s[[i]]
##     }
##     list(out=out,out.s=out.s)
## }
## ##original call
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[2]]$pL2,to.plot=TRUE,col=c("gray","darkblue","lightblue"),time.out=time.out,ylim=c(-.2,.25))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## legend(5,-.1,bty="n",
##        #title=LL2[[2]]$tr.txt,
##        legend=c("Pre-death","25th %ile adjusted CESD","75th %ile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.95)

## ##for si, younger
## graphics
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[1]]$pL2,to.plot=TRUE,col=c("gray","darkblue","lightblue"),time.out=time.out,ylim=c(-.2,.25))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## legend(5,-.1,bty="n",
##        #title=LL2[[2]]$tr.txt,
##        legend=c("Pre-death","25th %ile adjusted CESD","75th %ile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.95)

## ##for si, health
## ##graphics
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
## par(mgp=c(2,1,0),mar=c(3,3,2,1),mfrow=c(2,2),oma=rep(1.5,4),lwd=1)
## ##
## pf1(LL2[[2]]$pL1,to.plot=TRUE,col=c("blue","red","blue"),time.out=time.out,ylim=c(-.1,.4))->tmp
## as.numeric(tmp[[1]])-1.96*as.numeric(tmp[[2]])
## as.numeric(tmp[[1]])+1.96*as.numeric(tmp[[2]])
## lapply(LL2,"[[",1)->txt
## gsub("\n"," ",txt)->txt
## legend(5,.35,bty="n",
##        legend=c("Pre-death trend","Post-death"),
##        fill=c("blue","red"),cex=1)



## #####################################################################################
## ##original graphics with two different plots side by side
## pf1<-function(pL,col,to.plot=FALSE,time.out=48) {
##     #formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",mobila="Mobility Problems",lgmusa="Large muscle",grossa="Gross motor problems",finea="Fine motor problems",)#,cesd="CESD")
##     formal<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
##     par(mar=c(3,3,1.5,3.5))
##     par(mgp=c(2,1,0))
##     lf<-function(xx,col,...) {
##         lines(xx$t,xx$resp,col=col,...,lwd=3)
##         col2rgb(col)->cc
##         polygon(c(xx$t,rev(xx$t)),c(xx$ql,rev(xx$qh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75),border=NA)
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
##             segments(60,yl,60,yh,col="red",lwd=4)
##             ##
##             col2rgb("pink")->cc
##                                         #polygon(c(-13,-11,-11,-13),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
##             polygon(c(time.out+1,time.out-1,time.out-1,time.out+1),c(-1,-1,1,1),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))
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
## ##
## c("pink","darkred")->cols
## load("LL_boot.Rdata")
## time.out<-48
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
## pf2(mat2,yl=c(-.05,.4),cols=cols)
## lapply(LL2,"[[",1)->txt
## gsub("\n"," ",txt)->txt
## legend("bottomright",bty="n",fill=rev(cols),legend=rev(txt),cex=.95)
## ##
## mat<-list()
## for (i in 1:length(LL2)) pf1(LL2[[i]]$pL2,to.plot=FALSE,time.out=time.out)->mat[[i]]
## pf1(LL2[[2]]$pL2,to.plot=TRUE,col=c("gray","darkblue","lightblue"),time.out=time.out)
## legend(-13,.27,bty="n",title=LL2[[2]]$tr.txt,legend=c("Pre-death","25th percentile adjusted CESD","75th percentile adjusted CESD"),fill=c("gray","lightblue","darkblue"),cex=.75)
## mtext(side=3,adj=0,"B")
## ##
## mat2<-list()
## for (i in 1:2) {
##     lapply(mat,"[[",i)->tmp
##     do.call("rbind",lapply(tmp,unlist))->mat2[[i]]
## }
## pf2(mat2,yl=c(-.2,.7),cols=cols)
## #legend("topleft",bty="n",fill=heat.colors(4),legend=txt,cex=.7)







