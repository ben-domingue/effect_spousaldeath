load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")

df[df$dead.spouse==0 & df$delta.death< -6,]->tmp
by(tmp$cesd,tmp$hhidpn,mean,na.rm=TRUE)->m
data.frame(hhidpn=names(m),cesd.pre=as.numeric(m))->tmp
merge(df,tmp,all.x=TRUE)->df
df$age->df$age.raw
df$age/10->df$age
df$age-mean(df$age[df$fpd],na.rm=TRUE) -> df$age


df[df$fpd,]->df.first
df.first$hhidpn ->ids
df[df$hhidpn %in% ids,]->df.sub
nrow(df.sub)
length(unique(df.sub$hhidpn))
df.sub->df
df$delta.death->df$delta
df[!is.na(df$delta),]->df
"cesd"->nm

L<-list()
df[df$cesd.pre==0,]->L[['cesd=0']]
df[df$cesd.pre>0 & df$cesd.pre<=1,]->L[['cesd>= & cesd<=1']]
df[df$cesd.pre>1 & df$cesd.pre<=2,]->L[['cesd>1 & cesd<=2']]
df[df$cesd.pre>2,]->L[['cesd>2']]

library(minpack.lm)
ff<-function(df) {
    df[df$fpd,]->df
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
lapply(L,ff)->tab

##depressed versus not
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
par(mgp=c(2,1,0),mar=c(3.5,3.5,1,1),oma=rep(1.4,4))
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-40,60),ylim=c(0,6))
abline(v=0,lwd=.5,col="gray")
pf(L[[1]],col.band="black",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(L[[2]],col.band="royalblue4",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(L[[3]],col.band="royalblue3",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(L[[4]],col.band="royalblue1",points=FALSE,nobs=60,nm=nm,std=FALSE)
legend("topright",bty="n",fill=rev(c("black","royalblue4","royalblue3","royalblue1")),c("CESD>2","CESD>1 & CESD<=2","CESD>0 & CESD<=1","CESD=0"),title="Mean CESD prior to spousal death")

##race and sex
make.race(df)->L
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-40,60),ylim=c(0,6))
abline(v=0,lwd=.5,col="gray")
pf(L[[1]],col.band="black",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(L[[2]],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(L[[3]],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
legend("topright",bty="n",fill=c("black","blue","red"),names(L))

split(df,df$ragender)->L
plot(NULL,xlab="Time from Spousal Death (months)",ylab="Expected CESD",xlim=c(-40,60),ylim=c(0,6))
abline(v=0,lwd=.5,col="gray")
pf(L[[1]],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
pf(L[[2]],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
legend("topright",bty="n",fill=c("blue","red"),names(L))
