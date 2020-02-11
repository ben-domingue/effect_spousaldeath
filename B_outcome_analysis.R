
load("df.Rdata")
formal.nm<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
##formal.nm<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits",grip="Grip",gait="Gait")
#formal.nm<-c(gait="gait",grip="grip")
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
#par(mfrow=c(4,3),mgp=c(2,1,0),mar=c(3,3,3,2),oma=rep(1,4))
par(mfrow=c(4,2),mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.7,4))
for (nm in names(formal.nm)) {
    ##
    ff<-function(x,nm,col,lty=1) {
        loess(paste(nm,"~age"),x)->m
        seq(50,100,length.out=100)->xv
        predict(m,data.frame(age=xv))->tmp
        lines(xv,tmp,col=col,lwd=3,lty=lty)
    }
    std(df[[paste(nm,".raw",sep="")]])->df[[paste(nm,".raw",sep="")]]
    ##
    sample(unique(df$hhidpn),5000,replace=FALSE)->index
    df[df$hhidpn %in% index,]->zz
    plot(NULL,xlim=c(50,100),ylim=c(-1,3),xlab="age",ylab="standardized"
         #,main="all respondents"
         )
    abline(h=0,col="gray")
    ff(zz[zz$male==0,],nm,col="red")
    ff(zz[zz$male==1,],nm,col="blue")
    ff(zz[zz$male==0,],paste(nm,".raw",sep=""),col="red",lty=2)
    ff(zz[zz$male==1,],paste(nm,".raw",sep=""),col="blue",lty=2)
    legend("topleft",bty="n",lty=1,col=c("blue","red"),lwd=3,c("male","female"),title=formal.nm[nm])
    ##
    ## df[df$fpd==1,]->tmp
    ## df[df$hhidpn %in% tmp$hhidpn,]->zz
    ## plot(NULL,xlim=c(50,100),ylim=c(-1,3),xlab="age",ylab="standardized",main="those who experience bereavement")
    ## abline(h=0,col="gray")
    ## ff(zz[zz$male==0,],nm,col="red")
    ## ff(zz[zz$male==1,],nm,col="blue")
    ## ff(zz[zz$male==0,],paste(nm,".raw",sep=""),col="red",lty=2)
    ## ff(zz[zz$male==1,],paste(nm,".raw",sep=""),col="blue",lty=2)
    ## legend("topleft",bty="n",lty=c(1,2),lwd=3,c("residualized","raw"),title=formal.nm[nm])
    ##
    df[[nm]]->z
    density(z,na.rm=TRUE)->den
    col2rgb("green")->cc
    plot(den,xlab="",main="",lwd=2,col="green")
    polygon(c(den$x,rev(den$x)),c(rep(0,length(den$x)),rev(den$y)),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55))
    legend("topright",bty="n",legend="",title=formal.nm[nm])
}


## ##################################################################################################
## #health at different ages amongst bereaved & non
## load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
## unique(df$hhidpn[df$dead.spouse==1])->id
## df[df$hhidpn %in% id,]->df.dead
## df[!(df$hhidpn %in% id),]->df.others

## formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## par(mfcol=c(4,4),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
## for (nm in names(formal.nm)) {
##     ##age
##     plot(NULL,xlim=c(50,95),ylim=c(-1,1),ylab="",xlab="")
##     legend("bottomleft",bty="n",legend=nm)
##     if (nm=="iadlza") {
##         mtext(side=3,line=.3,"age-based comparison",cex=.7)
##         legend("topleft",bty="n",c("those who experience spousal death","others"),fill=c("red","black"))
##     }
##     aggregate(df.dead[[nm]],list(df.dead$age),mean,na.rm=TRUE)->m1
##     lines(m1,col="red",lwd=3)
##     aggregate(df.others[[nm]],list(df.others$age),mean,na.rm=TRUE)->m1
##     lines(m1,col="black",lwd=3)
##     ##
##     plot(NULL,xlim=c(50,95),ylim=c(-1,1),ylab="",xlab="")
##     legend("bottomleft",bty="n",legend=nm)
##     if (nm=="iadlza") {
##         mtext(side=3,line=.3,"restricted to obs 5y < spousal death",cex=.7)
##         legend("topleft",bty="n",c("those who experience spousal death","others"),fill=c("red","black"))
##     }
##     df.dead[df.dead$delta.death< -60,]->tmp
##     aggregate(tmp[[nm]],list(tmp$age),mean,na.rm=TRUE)->m1
##     lines(m1,col="red",lwd=3)
##     aggregate(df.others[[nm]],list(df.others$age),mean,na.rm=TRUE)->m1
##     lines(m1,col="black",lwd=3)
##     ##birthyear
##     plot(NULL,xlim=c(1910,1950),ylim=c(-1,1),ylab="",xlab="")
##     legend("bottomleft",bty="n",legend=nm)
##     if (nm=="iadlza") {
##         mtext(side=3,line=.3,"birthyear-based comparison",cex=.7)
##         legend("topleft",bty="n",c("those who experience spousal death","others"),fill=c("red","black"))
##     }
##     aggregate(df.dead[[nm]],list(df.dead$rabyear),mean,na.rm=TRUE)->m1
##     lines(m1,col="red",lwd=3)
##     aggregate(df.others[[nm]],list(df.others$rabyear),mean,na.rm=TRUE)->m1
##     lines(m1,col="black",lwd=3)
##     ##
##     plot(NULL,xlim=c(1910,1950),ylim=c(-1,1),ylab="",xlab="")
##     legend("bottomleft",bty="n",legend=nm)
##     if (nm=="iadlza") {
##         mtext(side=3,line=.3,"restricted to obs 5y < spousal death",cex=.7)
##         legend("topleft",bty="n",c("those who experience spousal death","others"),fill=c("red","black"))
##     }
##     df.dead[df.dead$delta.death< -60,]->tmp
##     aggregate(tmp[[nm]],list(tmp$rabyear),mean,na.rm=TRUE)->m1
##     lines(m1,col="red",lwd=3)
##     aggregate(df.others[[nm]],list(df.others$rabyear),mean,na.rm=TRUE)->m1
##     lines(m1,col="black",lwd=3)
## }


## ##################################################################################################
## load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
## formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## plot(NULL,xlim=c(1,11),ylim=c(-.15,.1))
## for (nm in names(formal.nm)) lines(by(df[[nm]],df$wave,mean,na.rm=TRUE))


## df[df$fpd,]->z
## df[df$hhidpn %in% z$hhidpn,]->df


## library(lme4)
## lm(iadlza~age+male+rabyear,df)
## library(lfe)
## felm(iadlza~age|hhidpn,df)
## #lmer(iadlza~(1|hhidpn)+age,df)


## f<-function(df) if (nrow(df)>5) cor(df$iadlza,df$age,use='p') else NA
## f(df)
## split(df,df$hhidpn)->tmp
## f<-function(x) {
##     x[order(x$wave),]->x
##     nrow(x)->n
##     if (n>3) x$iadlza[c(1,n)] else c(NA,NA)
## }
## sapply(tmp,f)->z

