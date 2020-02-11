load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
df[df$fpd==1,]->tmp
table(is.na(tmp$raddate))



ifelse(df$ragender=="1.male",1,0)->df$male
table(df$hhidpn)->tab
data.frame(names(tab),as.numeric(tab))->tmp
names(tmp)<-c("hhidpn","n.waves")
merge(df,tmp)->df

##table
df[!duplicated(df$hhidpn),]->tmp
desc<-function(x) {
    vars<-c("male","rabyear","raedyrs","n.waves","cesd.mean")
    out<-list()
    for (var in vars) {
        mean(x[[var]],na.rm=TRUE)->m
        sd(x[[var]],na.rm=TRUE)->s
        c(m,s)->out[[var]]
    }
    c(nrow(x),unlist(out))
}
desc(tmp)->tab.all
df[df$fpd,]->tmp
desc(tmp)->tab.sd

rbind(tab.all,tab.sd)->tab
write.csv(tab,"")

vars<-c(cesd="CESD",conde.raw="# Conditions",iadlza.raw="IADLs",adla.raw="ADLs",hsptim.raw="# Hospital Visits")
f<-function(x) {
    x[!is.na(x)]->x
    min(x)->m1
    mean(x)->m2
    sd(x)->m3
    max(x)->m4
    length(x)->m5
    c(m1,m2,m3,m4,m5)
}
tab<-list()
for (var in names(vars)) f(df[[var]])->tab[[vars[var] ]]
do.call("rbind",tab)->tab1
tab<-list()
for (var in names(vars)) f(tmp[[var]])->tab[[vars[var] ]]
do.call("rbind",tab)->tab2
cbind(tab1,tab2)->tab
write.csv(tab,"")


##role of age
#tiff("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/docs/psych_med/fig1.tif",units="in",height=4,width=4,res=1200)
#layout(matrix(c(1,2,2,3,3,3),2,3,byrow=TRUE))
load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
##
by(df$age,df$hhidpn,min,na.rm=TRUE)->z
density(as.numeric(z),na.rm=TRUE)->den
col2rgb("gray")->cc
plot(den,xlim=c(45,100),xlab="age",lwd=2,main="",col="gray")
mtext(side=3,adj=0,"A")
polygon(c(den$x,rev(den$x)),c(rep(0,length(den$x)),rev(den$y)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55)) 
df[df$fpd,]->tmp
density(tmp$age,na.rm=TRUE)->den
lines(den,col="red",lwd=2)
col2rgb("red")->cc
polygon(c(den$x,rev(den$x)),c(rep(0,length(den$x)),rev(den$y)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55))
legend("topright",fill=c("gray","red"),bty="n",c("age at first observation, all respondents","age at bereavement observation"))
##
vars<-c(cesd="CESD",conde.raw="# Conditions",iadlza.raw="IADLs",adla.raw="ADLs",hsptim.raw="# Hospital\nVisits")
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
for (var in names(vars)) {
    std(df[[var]])->df[[var]]
}
#
df[df$age %in% 50:85,]->tmp
plot(NULL,xlim=c(43,92),ylim=c(-.7,.9),xlab="Age",ylab="Standardized mean",bty="n",xaxt="n")
mtext(side=3,adj=0,"B")
axis(side=1,seq(50,85,by=5))
library(fBasics)
rampPalette(length(vars),"blue2red")->cols
for (i in 1:length(vars)) {
    names(vars)[[i]]->nm
    tmp[[nm]]->tmp$x
    aggregate(tmp$x,list(tmp$age),mean,na.rm=TRUE)->z
    lines(z,type="l",pch=19,cex=.7,col=cols[i],lwd=2)
    text(85,z[nrow(z),2],vars[nm],pos=4,cex=.7,col=cols[i])
    text(50,z[1,2],vars[nm],pos=2,cex=.7,col=cols[i])
}


##distributions of outcomes
load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
layout(matrix(c(1,1,2,3,4,5),byrow=TRUE,3,2))
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
vars<-c(cesd="CESD",conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
for (nm in c("cesd","conde","iadlza","adla","hsptim")) {
    df[[nm]]->z
    density(as.numeric(z),na.rm=TRUE)->den
    col2rgb("gray")->cc
    plot(den,xlab=vars[nm],lwd=2,main="",col="gray")
    polygon(c(den$x,rev(den$x)),c(rep(0,length(den$x)),rev(den$y)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55)) 
    df[df$fpd,]->tmp
    density(tmp[[nm]],na.rm=TRUE)->den
    lines(den,col="red",lwd=2)
    col2rgb("red")->cc
    polygon(c(den$x,rev(den$x)),c(rep(0,length(den$x)),rev(den$y)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55))
    if (nm=="adla") legend("topright",fill=c("gray","red"),bty="n",c("all observations","bereavement observations"))
}

##timing figure
layout(matrix(c(1,1,1,2,3,4),2,3,byrow=TRUE))
par(mgp=c(2,1,0),mar=c(4,3,1,1),oma=rep(.5,4))
hist(df$iwend.raw,col="blue",breaks="months",xaxt="n",xlab="Interview Date",main="")
mtext(side=3,adj=0,"A")
axis.POSIXct(side=1,at=strptime(paste(1990:2018,"-1-1",sep=""),format="%Y-%m-%d"),format="%Y")
                                        #
ifelse(df$delta.death>48,48,df$delta.death)->df$delta.death
hist(df$delta.death[df$fpd],xlab="Months between spousal death and interview",breaks=35,col="red",xlim=c(0,36),xaxt="n",main="",freq=FALSE)
mtext(side=3,adj=0,"B")
axis(side=1,at=c(0,12,24,36,48))
##looking at distribution of time following spousal death and time since last interview
df[df$fpd,]->tmp
tmp[,c("hhidpn","wave","delta.death","iwend")]->tmp
paste(tmp[,1],tmp[,2]-1)->id
paste(df$hhidpn,df$wave)->tmp.id
df[tmp.id %in% id,]->z
z[,c("hhidpn","iwend")]->z
names(z)[2]<-"last.iwend"
merge(tmp,z)->tmp
tmp$iwend-tmp$last.iwend -> tmp$delta.iwend
plot(tmp$delta.death,jitter(tmp$delta.iwend),col=ifelse(tmp$delta.death<2,"red","black"),pch=1,cex=.5,xlab="Time since death",ylab="Time since last interview")
mtext(side=3,adj=0,"C")
loess(delta.iwend~delta.death,tmp)->m
cbind(m$x,m$fitted)->z
z[order(z[,1]),]->z
lines(z,lwd=2)
##
df[df$fpd,]->tmp
tmp<-df[df$hhidpn %in% tmp$hhidpn,]
tmp<-tmp[tmp$delta.death>0,]
tab<-table(table(tmp$hhidpn))
library(gplots)
vals<-barplot2(tab,col="red",xlab="# observations post-death for bereaved",ylab="N bereaved",ylim=c(0,1300))
for (i in 1:length(names(tab))) text(vals[i],tab[i],tab[i],cex=.8,pos=3)
mtext(side=3,adj=0,"D")

##mortality
df[df$fpd==1,]->tmp
by(df$iwend,df$hhidpn,max,na.rm=TRUE)->foo
data.frame(hhidpn=names(foo),max=as.numeric(foo))->foo
merge(tmp,foo)->tmp
(tmp$max-tmp$iwend)/365 -> del
ifelse(is.na(tmp$del.interview),0,1)->tmp$dead
ifelse(is.na(tmp$del.interview),del,tmp$del.interview)->tmp$time
##
library(survival)
par(mgp=c(2,1,0),mar=c(4,3,1,1),oma=rep(.5,4))
plot(NULL,ylim=c(0,1),xlim=c(0,10.5),xlab="Time since bereavement observation (years)",ylab="Proportion Surviving")
f<-function(tmp,col) {
    survfit(Surv(time=time,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod$time,mod$surv,lty=1,lwd=3,col=col)
    c(mod$surv+1.96*mod$std.err,rev(mod$surv-1.96*mod$std.err))->y.ci
    ifelse(!is.finite(y.ci),2,y.ci)->y.ci
    col2rgb(col)->cc
    polygon(c(mod$time,rev(mod$time)),y.ci,col=rgb(cc[1],cc[2],cc[3],max=255,alpha=90))
}
f(tmp,col="blue")

