load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")

#merge(df,ws,all.x=TRUE)->df

df[df$fpd,]->df.first
df.first$hhidpn ->ids
df[df$hhidpn %in% ids,]->df.sub
nrow(df.sub)
length(unique(df.sub$hhidpn))
df.sub->df
df$delta.death->df$delta
df[!is.na(df$delta),]->df
"cesd"->nm



##################################################################################
##main text, figure 1 [fitted cesd in months following spousal death]
tiff("/tmp/fig1.tiff", width = 3.2, height = 3, units = 'in', res = 300,pointsize=8)
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-1,36),ylim=c(1,5))
abline(v=0,lwd=.5,col="gray")
df[df$fpd,]->fp
fp[fp$delta.death<36,]->fp
fp[!is.na(fp$cesd),]->fp
pf(fp,col.band="red",points=TRUE,nobs.per.point=45,nm=nm,std=FALSE,return.loess=TRUE,col.points="gray")->foo
##
start<-list(
    b0=mean(fp$cesd[fp$delta.death>20]),
    b2=.4,
    lambda=20
)
fm<-as.formula(cesd~b0+b2*exp(-1*delta.death/(lambda)))
library(minpack.lm)
mod<-nlsLM(fm,data=fp,
           start=start
           )    
coef(mod)->co
seq(0.1,30,length.out=5000)->xv
lines(xv,co[1]+co[2]*exp(-1*xv/co[3]),lwd=3,col="black")
mod
##
mod<-nlsLM(fm,data=fp[fp$delta.death>2,],
           start=start
           )    
seq(.1,30,length.out=5000)->xv
coef(mod)->co
#lines(xv,co[1]+co[2]*exp(-1*xv/co[3]),lwd=2,col="blue")
mod
#legend("topright",bty="n",lty=c(1,1,1,1),title="Bereavement Observations",col=c("red","black","blue"),c("LOESS","Exp Decay","Exp Decay (months>2)"),lwd=1)
legend("topright",bty="n",lty=c(1,1,1,1),title="Bereavement Observations",col=c("red","black"),c("LOESS","Exp Decay"),lwd=1)
dev.off()

##################################################################################
##SI
par(mgp=c(2,1,0),mar=c(3.5,3.5,1,1))
plot(NULL,xlab="Months after Spousal Death",ylab="",xlim=c(-40,40),ylim=c(0,4.5),bty="n")
abline(v=0)
pf(df,points=TRUE,nobs=60,nm=nm,std=FALSE)
legend("topright",bty="n","CESD",cex=1.7)



###########################################################################
##older versions
#layout(matrix(c(1,1,1,2,3,4),2,3,byrow=TRUE))
layout(matrix(c(1,1,1,2,4,4,3,4,4),3,3,byrow=TRUE))
par(mgp=c(2,1,0),mar=c(3.5,3.5,1,1),oma=rep(1.4,4))
##1-different waves
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-60,120),ylim=c(0,4.2))
abline(v=0,lwd=.5,col="gray")
heat.colors(7)->cols
seq(-2,3,by=1)->vals
for (ii in 1:length(vals)) {
    vals[ii]->i
    df[!is.na(df$waves.since) & df$waves.since==i,]->tmp
    if(i<0) ifelse(tmp$delta.death>0,NA,tmp$delta.death)->tmp$delta.death
    if(i>=0) ifelse(tmp$delta.death<0,NA,tmp$delta.death)->tmp$delta.death
    pf(tmp,col.band=cols[ii],std=FALSE,nm="cesd",points=FALSE)->xx
    ifelse(i%%2==1,.4,0)->offset
    arrows(xx[1],.05+offset,xx[2],.05+offset,angle=90,length=.1)
    arrows(xx[2],.05+offset,xx[1],.05+offset,angle=90,length=.1)
    text(mean(xx),.05+offset,pos=3,paste(i))
}
mtext(side=3,adj=0,"A",line=.2)
legend("topright",bty="n",title="Waves of data collection before/after spousal death",legend="")

##2-old/young
mean(df$age[df$fpd],na.rm=TRUE)->M
"cesd"->nm
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-40,60),ylim=c(0,4.2))
abline(v=0,lwd=.5,col="gray")
pf(df[df$age<M,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(df[df$age>M,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
legend("topright",bty="n",fill=c("blue","red"),c("Bereavement while younger","Bereavement while older"))
mtext(side=3,adj=0,"B",line=.2)

##3-first versus all
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-40,60),ylim=c(0,4.2))
abline(v=0,lwd=.5,col="gray")
pf(df,col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(df[df$fpd,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
legend("topright",bty="n",fill=c("blue","red"),c("All observations","Bereavement observation"))
mtext(side=3,adj=0,"C",line=.2)

##4-first, parametric versus loesss
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-1,36),ylim=c(1,4.5))
abline(v=0,lwd=.5,col="gray")
df[df$fpd,]->fp
fp[fp$delta.death<36,]->fp
fp[!is.na(fp$cesd),]->fp
pf(fp,col.band="red",points=TRUE,nobs=36,nm=nm,std=FALSE,return.loess=TRUE,col.points="gray")->foo
##
start<-list(
    b0=mean(fp$cesd[fp$delta.death>20]),
    b2=.4,
    lambda=20
)
fm<-as.formula(cesd~b0+b2*exp(-1*delta.death/(lambda)))
library(minpack.lm)
mod<-nlsLM(fm,data=fp,
           start=start
           )    
coef(mod)->co
seq(0.1,30,length.out=5000)->xv
lines(xv,co[1]+co[2]*exp(-1*xv/co[3]),lwd=3,col="black")
mod
##
mod<-nlsLM(fm,data=fp[fp$delta.death>2,],
           start=start
           )    
seq(.1,30,length.out=5000)->xv
coef(mod)->co
lines(xv,co[1]+co[2]*exp(-1*xv/co[3]),lwd=2,col="blue")
mod
legend("topright",bty="n",lty=c(1,1,1,1),title="Bereavement Observations",col=c("red","black","blue"),c("LOESS","Exp Decay","Exp Decay (months>2)"),lwd=1)
mtext(side=3,adj=0,"D",line=.2)





## ##fits
## load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
## ##merge(df,ws,all.x=TRUE)->df
## df[df$fpd,]->df.first
## df.first$hhidpn ->ids
## df[df$hhidpn %in% ids,]->df.sub
## nrow(df.sub)
## length(unique(df.sub$hhidpn))
## df.sub->df
## df$delta.death->df$delta
## df[!is.na(df$delta),]->df
## "cesd"->nm

## plot(NULL,xlab="Time from Spousal Death (months)",ylab="Standardized CESD",xlim=c(-1,36),ylim=c(-.7,1))
## abline(v=0,lwd=.5,col="gray")
## df[df$fpd,]->fp
## fp[fp$delta.death<36,]->fp
## (fp$cesd-mean(fp$cesd,na.rm=TRUE))->fp$cesd
## fp[!is.na(fp$cesd),]->fp
## ##
## pf(fp,col.band="red",points=TRUE,nobs=36,nm=nm,std=TRUE)
## ##
## library(minpack.lm)
## ## start<-list(
## ##     b0=mean(fp$cesd[fp$delta.death>20]),
## ##     b2=.4,
## ##     lambda=20
## ## )
## ## fm<-as.formula(cesd~b0+b2*exp(-1*delta.death/(lambda)))
## ## mod<-nlsLM(fm,data=fp,
## ##            start=start
## ##            )    
## ## coef(mod)->co
## ## seq(0.1,30,length.out=5000)->xv
## ## lines(xv,co[1]+co[2]*exp(-1*xv/co[3]),lwd=2,col="gray")
## ## ##
## ## mod<-nlsLM(fm,data=fp[fp$delta.death>2,],
## ##            start=start
## ##            )    
## ## seq(2,30,length.out=5000)->xv
## ## coef(mod)->co
## ## lines(xv,co[1]+co[2]*exp(-1*xv/co[3]),lwd=2,col="blue")
## ## ##
## ## start<-list(
## ##     b0=mean(fp$cesd[fp$delta.death>20]),
## ##     b2=.4
## ## )
## ## fm<-as.formula(cesd~b0+b2/delta.death)
## ## mod<-nlsLM(fm,data=fp,
## ##            start=start
## ##            )    
## ## coef(mod)->co
## ## lines(xv,co[1]+co[2]/xv,lwd=2,col="green")
## ## ##
## ## legend("topright",bty="n",lty=c(1,1,1,1),title="Bereavement Observations",col=c("black","red","gray","blue","green"),c("Linear","LOESS","Exp Decay","Exp Decay (months>2)","Inverse (months>2)"),lwd=2)
## ## mtext(side=3,adj=0,"D",line=.2)
## ## abline(lm(cesd~delta.death,fp[fp$delta.death<24,]),col="black",lwd=2)
## ##
## start<-list(
##     b0=mean(fp$cesd[fp$delta.death>20]),
##     b2=.3,
##                                         #bb=exp(1),
##     lambda=10
## )
## fm<-as.formula(cesd~b0+b2*1.3^(-1*(log(delta.death))/(lambda)))
## mod<-nlsLM(fm,data=fp[fp$delta.death<12 & fp$delta.death>2,],
##                start=start
##            )    
## seq(2,12,length.out=5000)->xv
## coef(mod)->co
## lines(xv,co[1]+co[2]*1.3^(-1*(log(xv))/co[3]),lwd=2,col="darkblue")


## ##modeling foo object from pf()
## foo[,1]->x
## foo[,2]+rnorm(nrow(foo))->y

## library(minpack.lm)
## start<-list(
##     b0=1,
##     b2=0,
##     lambda=20
## )
## data.frame(cesd=y,delta.death=x)->fp
## fm<-as.formula(cesd~b0+b2*exp(-1*delta.death/(lambda)))
## mod<-nlsLM(fm,data=fp,
##            start=start
##            )    
## plot(x,foo[,2])
## coef(mod)->co
## seq(0.1,30,length.out=5000)->xv
## lines(xv,co[1]+co[2]*exp(-1*xv/co[3]),lwd=2,col="red")

## start<-list(
##     b0=1,
##     b2=.1
## )
## fm<-as.formula(cesd~b0+b2/delta.death)
## mod<-nlsLM(fm,data=fp,
##            start=start
##            )    
## coef(mod)->co
## seq(0.1,30,length.out=5000)->xv
## lines(xv,co[1]+co[2]/xv,lwd=2,col="blue")
