load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
library(minpack.lm)
df$age->df$age.raw
df$age/10->df$age
df$age-mean(df$age[df$fpd],na.rm=TRUE) -> df$age
df->df.all
df[df$fpd,]->df

mm<-list()

start<-list(
    b0=mean(df$cesd[df$delta.death>20],na.rm=TRUE),
    b2=.4,
    lambda=20
)
fm<-as.formula(cesd~b0+b2*exp(-1*delta.death/(lambda)))
library(minpack.lm)
mm[[1]]<-nlsLM(fm,data=df,
           start=start
           )    

mm[[2]]<-nlsLM(fm,data=df[df$delta.death>2,],
           start=start
           )    
mm[[3]]<-nlsLM(fm,data=df.all[df.all$delta.death>0,],
           start=start
           )    


## start<-list(
##     b0=mean(df$cesd[df$delta.death>20],na.rm=TRUE),
##     b.no= -.1,
##     b2=.4,
##     lambda=20
## )
## fm<-as.formula(cesd~b0+b.no*live.spouse+b2*exp(-1*delta.death/(lambda)))
## df.all[df.all$hhidpn %in% df.all$hhidpn[df$fpd==1],]->tmp
## ifelse(tmp$delta.death>0,1,0)->tmp$live.spouse
## m<-nlsLM(fm,data=tmp,
##            start=start
##            )    


start<-list(
    b0=mean(df$cesd[df$delta.death>20],na.rm=TRUE),
    b.male=0,
    b.age=0,
    b2=.4,
    lambda=20
)
fm1<-as.formula(cesd~b0+
                   b.male*male+b.age*age+
                   (b2)*exp(-1*delta.death/(lambda)))
mm[[4]]<-nlsLM(fm1,data=df,start=start)

start<-list(
    b0=mean(df$cesd[df$delta.death>20],na.rm=TRUE),
    b.male=0,
    b.age=0,
    b2.inter=0,
    b2=.4,
    lambda=20,
    lambda.inter=0
)
fm1<-as.formula(cesd~b0+
                   b.male*male+b.age*age+
                   (b2+b2.inter*age)*exp(-1*delta.death/(lambda+lambda.inter*age)))
mm[[5]]<-nlsLM(fm1,data=df,start=start)

fm1<-as.formula(cesd~b0+
                   b.male*male+b.age*age+
                   (b2+b2.inter*male)*exp(-1*delta.death/(lambda+lambda.inter*male)))
mm[[6]]<-nlsLM(fm1,data=df,start=start)

source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
table.lm(mm,se=TRUE)->tab
write.csv(tab,"")

##race/ethnicity
load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
library(minpack.lm)
df$age->df$age.raw
df$age/10->df$age
df$age-mean(df$age[df$fpd],na.rm=TRUE) -> df$age
df->df.all
df[df$fpd,]->df
ff<-function(df) {
    start<-list(
        b0=mean(df$cesd[df$delta.death>20],na.rm=TRUE),
        b2=.4,
        lambda=20
    )
    fm<-as.formula(cesd~b0+b2*exp(-1*delta.death/(lambda)))
    library(minpack.lm)
    mm<-nlsLM(fm,data=df,
                   start=start
                   )    
    summary(mm)$coef
}
make.race(df)->L
split(df,df$ragender)->tmp
c(L,tmp)->L
lapply(L,ff)















##                                         #dev.new(width=12,height=8)
## load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")

## library(minpack.lm)
## c(flone="lonely",fsad="sad",depres="depress",nhappy="not happy",nenlife="not enjoylife",going="motivation",sleepr="sleep",effort="effort")->vars

## ifelse(is.na(df$delta.death),-100,df$delta.death)->df$delta.death
## df$rabyear->df$rabyear.raw
## df$rabyear-mean(df$rabyear,na.rm=TRUE)->df$rabyear

## df[df$fpd,]->df



## make.tab<-function(m) {
##     summary(m)->s
##     s$parameters->pars
##     grep("b2",rownames(pars))->index
##     pars[index,]->sh
##     c(sh[1],sh[1]-1.96*sh[2],sh[1]+1.96*sh[2])->tr1
##     grep("lambda",rownames(pars))->index
##     pars[index,]->sh
##     c(sh[1],sh[1]-1.96*sh[2],sh[1]+1.96*sh[2])->tr2
##     pars[index,1]->ret
##     c(tr1,tr2,-1*log(0.2)*ret)
## }
## pf.fitted.baseline<-function(dat,start,fm,ni=10) {
##     #baseline mod
##     mod<-nlsLM(fm,data=dat,
##              start=start
##              )
##     summary(mod)$coef->S
##     #paste(rownames(S),round(S[,1],digits=2),format(S[,4],3,sci=TRUE))->leg
##     #
##     seq(0.1,30,length.out=5000)->xv
##                                         #predict(mod,data.frame(ragender=factor("2.female"),age=73,rabyear=0,delta.death=xv))->yp1
##     predict(mod,data.frame(ragender=factor("2.female"),age=73,delta.death=xv))->yp1
##     ##adding stuff
##     if (ni>0) {
##         make.tab(mod)->vals
##         abs(xv-vals[7]) -> del.tmp
##         which.min(del.tmp)->iii
##         segments(vals[7],0,vals[7],yp1[iii],col="red",lty=3,lwd=2)
##         #text(xv[iii],.95,paste(round(vals[4],1),"Months"),col="red",cex=1.5)
##         text(18,.83,paste(round(vals[7],1),"Months"),col="red",cex=1.4,adj=0)
##         ##
##         abs(xv-2) -> del.tmp
##         which.min(del.tmp)->iii
##         segments(2,0,2,yp1[iii],lty=3,lwd=2)
##         ##
##         coef(mod)->co
##         #segments(0,yp1[1],0,rev(yp1)[1],col="blue",xpd=NA,lwd=2)
##         #abline(h=rev(yp1)[1],col="blue",lty=2,lwd=2)
##         segments(0,co[1]+co[4]+co[3]*73,0,co[1]+co[3]*73,col="blue",xpd=NA,lwd=2)
##         abline(h=co[1]+co[3]*73,col="blue",lty=1,lwd=2)
##         print(range(yp1))
##         print(S)
##                                         #mtext(side=4,line=.5,at=mean(yp1),round(rev(yp1)[1]-yp1[1],2),col="blue",las=2)
##     }
##     ##
##     lines(xv,yp1,lwd=2,col="lightgray")
##     if (ni>0) {
##         lines(xv,yp1,lwd=3,col="red")
##         hi<-lo<-list()
##         for (i in 1:ni) {
##             err<-TRUE
##             while(err) {
##                 sample(1:nrow(dat),nrow(dat),replace=TRUE)->index
##                 dat[index,]->zz
##                 try(
##                     mod.boot<-nlsLM(fm,data=zz,
##                                     start=start,
##                                     control=list(maxiter=150)
##                                     )
##                 )->test
##                 grepl("error",class(test))->err
##             }
##             predict(mod.boot,data.frame(ragender=factor("2.female"),age=73,rabyear=0,delta.death=xv))->hi[[i]]
##         }
##         do.call("cbind",hi)->hi
##         apply(hi,1,quantile,.025)->hl
##         apply(hi,1,quantile,.975)->hh
##                                         #
##         col2rgb("pink")->cc
##         polygon(c(xv,rev(xv)),c(hl,rev(hh)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=105))
##         predict(mod,data.frame(ragender=factor("2.female"),age=73,rabyear=0,delta.death=2))->y2
##         text(2+.5,y2,round(y2,2),pos=4,cex=1.5)
##         predict(mod,data.frame(ragender=factor("2.female"),age=73,rabyear=0,delta.death=30))->y30
##         text(29,y30+.03,round(y30,2),pos=3,cex=1.5)
##     }
##     mod
## }

## out<-list()
## dev.new(width=12,height=8)
## par(mfrow=c(2,4),mgp=c(2,1,0),mar=c(3.5,2,1.5,2),oma=rep(.8,4))
## for (ii in 1:length(vars)) {
##     plot(NULL,xlim=c(0,30),ylim=c(0,1),xlab="Months after Spouse’s Death",ylab="",bty="n")
##     names(vars)[ii]->var
##     for (small.var in names(vars)[-ii]) {
##         df[[small.var]]->df$var
##         start<-list(
##             b0=mean(df$var,na.rm=TRUE),
##             b.male=0,
##             b.age=0,
##             b2=.3,
##             lambda=5
##         )
##         fm<-as.formula(var~b0+
##                            b.male*(ragender=="1.male")+b.age*age+
##                            (b2)*exp(-1*delta.death/(lambda)))
##         pf.fitted.baseline(df,start=start,fm=fm,ni=0)->out[[var]]
##     }
##     df[[var]]->df$var
##     start<-list(
##         b0=mean(df$var,na.rm=TRUE),
##         b.male=0,
##         b.age=0,
##         b2=.3,
##         lambda=5
##     )
##     fm<-as.formula(var~b0+
##                        b.male*(ragender=="1.male")+b.age*age+
##                        (b2)*exp(-1*delta.death/(lambda)))
##     pf.fitted.baseline(df,start=start,fm=fm,ni=100)->out[[var]]    
##                                         #legend("bottomright",bty="n",vars[var],cex=1.5)
##     text(18,.9,cex=1.7,vars[var],adj=0)
## }

## ##this part ruins the picture but adds cesd to table
## fm<-as.formula(cesd~b0+
##                    b.male*(ragender=="1.male")+b.age*age+
##                    (b2)*exp(-1*delta.death/(lambda)))
## pf.fitted.baseline(df,start=start,fm=fm,ni=100)->out$cesd
## ##
## lapply(out,make.tab)->tab
## do.call("rbind",tab)->tab
## write.csv(tab,"")

## ##get p-values for coefficients
## (tab[,3]-tab[,2])/(1.96*4)-> se
## pnorm(tab[,1]/se,lower.tail=FALSE)*2


## ###############################################


    
    


