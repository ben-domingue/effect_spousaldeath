load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")

df[df$fpd,]->df.first
df.first$hhidpn ->ids
df[!(df$hhidpn %in% df$hhidpn[df$dead.spouse==1]),]->df.others
df[df$hhidpn %in% ids,]->df.sub
nrow(df.sub)
length(unique(df.sub$hhidpn))
df.sub->df
df$delta.death->df$delta
df[!is.na(df$delta),]->df



df[df$fpd,]->tmp
tmp[,c("hhidpn","age")]->tmp
names(tmp)[2]<-"age.fpd"
merge(df,tmp,all.x=TRUE)->df


##############################################################
##main result figure showing just 2 years following spousal death
formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
par(mfcol=c(2,4),mgp=c(2.5,1,0),oma=rep(3,4),mar=c(2,2,1,.5))
ifelse(df$del.spdeath<5,1,NA)->df$gr
ifelse(df$del.spdeath>=5 | is.na(df$raddate),2,df$gr)->df$gr
for (nm in names(formal.nm)) {
    plot(NULL,xlab="",ylab="",xlim=c(-24,24),ylim=c(-.5,1.5))
    if (nm==names(formal.nm)[1]) mtext(side=2,line=2.5,"Average value (Standardized)")
    abline(v=0,lwd=.5,col="gray")
    pf(df,col.band="green",points=FALSE,nobs=60,nm=nm,std=FALSE)
    legend('topleft',bty='n',title=formal.nm[nm],legend="",cex=1.5)
    ##
    plot(NULL,xlab="",ylab="",xlim=c(-24,24),ylim=c(-.5,1.5))
    if (nm==names(formal.nm)[1]) mtext(side=2,line=2.5,"Average value (Standardized)")
    abline(v=0,lwd=.5,col="gray")
    pf(df[df$gr==1,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
    pf(df[df$gr>1,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
    legend("topleft",bty="n",c("Survived<5y","Survived>5y"),fill=c("red","blue"))
}
mtext(side=1,outer=TRUE,line=1,"Time from Spousal Death (months)")

##############################################################
##stratified by length of post-spousal death obs window
ifelse(df$del.spdeath<5,1,NA)->df$gr
ifelse(df$del.spdeath>=5 | is.na(df$raddate),2,df$gr)->df$gr
mean(df$age[df$fpd],na.rm=TRUE)->M
#formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits",grip="Grip",gait="Gait")
formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
par(mfcol=c(2,length(formal.nm)),mgp=c(2,1,0),oma=rep(2,4))
for (nm in names(formal.nm)) {
    par(mar=c(2.5,2.5,1.5,.5))
    plot(NULL,xlab="",ylab="",xlim=c(-50,50),ylim=c(-.5,1.5))
    if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
    abline(v=0,lwd=.5,col="gray")
    pf(df[df$age.fpd<M & df$gr==1,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
    pf(df[df$age.fpd<M & df$gr>1,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
    ## pf(df[df$age.fpd<M & df$gr==2,],col.band="green",points=FALSE,nobs=60,nm=nm,std=FALSE)
    ## pf(df[df$age.fpd<M & df$gr==3,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
    mtext(line=.5,formal.nm[nm])
    legend("bottomright","Younger",bty="n")
    ##legend("bottomright",bty="n",formal.nm[nm],cex=1)
    ##
    par(mar=c(2.5,2.5,.5,.5))
    plot(NULL,xlab="",ylab="",xlim=c(-50,50),ylim=c(-.5,1.5))
    if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
    abline(v=0,lwd=.5,col="gray")
    pf(df[df$age.fpd>M & df$gr==1,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
    pf(df[df$age.fpd>M & df$gr>1,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
    legend("bottomright","Older",bty="n")
    #legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
    #if (nm==names(formal.nm)[1]) {
    #    legend("bottomleft",bty="n",c("Older","Younger"),fill=c("red","blue"))
    #}
}
mtext(side=1,outer=TRUE,line=0,"Time from Spousal Death (months)")



#need to make use age.fpd

## ##young v old, separate panels
## mean(df$age[df$fpd],na.rm=TRUE)->M
## par(mfcol=c(2,4),mgp=c(2,1,0),oma=rep(2,4))
## formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## for (nm in names(formal.nm)) {
##     par(mar=c(2.5,2.5,1.5,.5))
##     plot(NULL,xlab="",ylab="",xlim=c(-24,24),ylim=c(-.45,.25))
##     if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
##     abline(v=0,lwd=.5,col="gray")
##     pf(df[df$age<M,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     mtext(line=.5,formal.nm[nm])
##     legend("bottomright","Younger",bty="n")
##     ##legend("bottomright",bty="n",formal.nm[nm],cex=1)
##     ##
##     par(mar=c(2.5,2.5,.5,.5))
##     plot(NULL,xlab="",ylab="",xlim=c(-24,24),ylim=c(-.45,.25))
##     if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
##     abline(v=0,lwd=.5,col="gray")
##     pf(df[df$age>M,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     legend("bottomright","Older",bty="n")
##     #legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
##     #if (nm==names(formal.nm)[1]) {
##     #    legend("bottomleft",bty="n",c("Older","Younger"),fill=c("red","blue"))
##     #}
## }
## mtext(side=1,outer=TRUE,line=0,"Time from Spousal Death (months)")


## #dev.new(width=8,height=8)
## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.5,3.5,1,1))
## formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## for (nm in names(formal.nm)) {
##     plot(NULL,xlab="Time from Spousal Death (months)",ylab="Mean",xlim=c(-40,40),ylim=c(-.5,.5))
##     abline(v=0)
##     pf(df,points=TRUE,nobs=60,nm=nm,std=FALSE)
##     legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
## }

################################################################################################
##% of people there as a function of when they die

## by(df$dead.spouse,df$hhidpn,sum)->N
## data.frame(hhidpn=names(N),nobs=as.numeric(N))->tmp
## merge(df,tmp)->df

## ################################################################################################
## ##young v old, separate panels, with age-matched controls
## mean(df$age[df$fpd],na.rm=TRUE)->M
## par(mfcol=c(2,4),mgp=c(2,1,0),oma=rep(2,4))
## formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## age.matched.controls<-function(df2,df.others,nm) {
##     range(df2$delta.death)->ran
##     seq(ran[1],ran[2],length.out=100)->xv
##     hold<-yv<-list()
##     for (y in xv) {
##         df2[abs(df2$delta.death-y)<1,]->tmp
##         mean(tmp$age,na.rm=TRUE)->ma->hold[[as.character(y)]]
##         df.others[abs(df.others$age-ma)<1,]->tmp
##         mean(tmp[[nm]],na.rm=TRUE)->yv[[as.character(y)]]
##     }
##     cbind(xv,unlist(yv))->tmp
##     print(cbind(unlist(xv),unlist(hold)))
##     lines(tmp,lwd=2.5)
## }
## for (nm in names(formal.nm)) {
##     par(mar=c(2.5,2.5,1.5,.5))
##     plot(NULL,xlab="",ylab="",xlim=c(-200,200),ylim=c(-.45,.25))
##     if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
##     abline(v=0,lwd=.5,col="gray")
##     df[df$age<M,]->df2
##     pf(df2,col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     age.matched.controls(df2,df.others,nm=nm)
##     mtext(line=.5,formal.nm[nm])
##     legend("bottomright","Younger",bty="n")
##     ##
##     par(mar=c(2.5,2.5,.5,.5))
##     plot(NULL,xlab="",ylab="",xlim=c(-200,200),ylim=c(-.45,.25))
##     if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
##     abline(v=0,lwd=.5,col="gray")
##     df[df$age>M,]->df2
##     pf(df2,col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     age.matched.controls(df2,df.others,nm=nm)
##     legend("bottomright","Older",bty="n")
## }
## mtext(side=1,outer=TRUE,line=0,"Time from Spousal Death (months)")

## ##young v old, separate panels (by sex)
## mean(df$age[df$fpd],na.rm=TRUE)->M
## par(mfcol=c(2,4),mgp=c(2,1,0),oma=rep(2,4))
## formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## for (nm in names(formal.nm)) {
##     par(mar=c(2.5,2.5,1.5,.5))
##     plot(NULL,xlab="",ylab="",xlim=c(-80,80),ylim=c(-.45,.25))
##     if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
##     abline(v=0,lwd=.5,col="gray")
##     pf(df[df$age<M & df$male==1,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     pf(df[df$age<M & df$male==0,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     mtext(line=.5,formal.nm[nm])
##     legend("bottomright","Younger",bty="n")
##     ##legend("bottomright",bty="n",formal.nm[nm],cex=1)
##     ##
##     par(mar=c(2.5,2.5,.5,.5))
##     plot(NULL,xlab="",ylab="",xlim=c(-80,80),ylim=c(-.45,.25))
##     if (nm=="iadlza") mtext(side=2,line=2,"Fitted value (Standardized)")
##     abline(v=0,lwd=.5,col="gray")
##     pf(df[df$age>M & df$male==1,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     pf(df[df$age>M & df$male==0,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     legend("bottomright","Older",bty="n")
##     #legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
##     #if (nm==names(formal.nm)[1]) {
##     #    legend("bottomleft",bty="n",c("Older","Younger"),fill=c("red","blue"))
##     #}
## }
## mtext(side=1,outer=TRUE,line=0,"Time from Spousal Death (months)")



## split(df.others,df.others$hhidpn)->z
## f1.star<-function(x) {
##     x[order(x$wave),]->x
##     which(x$fpd)->ii
##     bef<-max(sum(!is.na(x$adla[1:ii]))-1,0)
##     aft<-max(sum(!is.na(x$adla[ii:nrow(x)]))-1,0)
##     c(x$age[ii],min(bef,5),min(aft,5))
## }
## lapply(z,f1)->tmp
## do.call("rbind",tmp)->tmp
## cut(tmp[,1],c(-Inf,qu,Inf),labels=1:4)->gr
## split(data.frame(tmp),gr)->tmp2
## lapply(tmp2,f2)


## ##raw metric
## ifelse(df$hsptim>5,5,df$hsptim)->df$hsptim
## mean(df$age[df$fpd],na.rm=TRUE)->M
## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.5,3.5,1,1),oma=rep(1,4))
## formal.nm<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
## for (nm in names(formal.nm)) {
##     if (nm=="conde") quantile(df[[nm]],c(.25,.75),na.rm=TRUE)->yl else 0:1->yl
##     plot(NULL,xlab="Time from Spousal Death (months)",ylab="Mean",xlim=c(-60,60),#ylim=c(-.5,.7))
##          ylim=yl)
##     abline(v=0,lwd=.5,col="gray")
##     pf(df[df$age<M,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     legend("bottomright",bty="n",formal.nm[nm],cex=1)
##     pf(df[df$age>M,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     #legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
##     if (nm==names(formal.nm)[1]) {
##         legend("bottomleft",bty="n",c("Older","Younger"),fill=c("red","blue"))
##     }
## }

## ##any
## mean(df$age[df$fpd],na.rm=TRUE)->M
## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.5,3.5,1,1),oma=rep(1,4))
## formal.nm<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits")
## for (nm in names(formal.nm)) {
##     ifelse(df[[nm]]>0,1,df[[nm]])->df[[nm]]
##     plot(NULL,xlab="Time from Spousal Death (months)",ylab="Mean",xlim=c(-60,60),#ylim=c(-.5,.7))
##          ylim=c(0,1))
##     abline(v=0,lwd=.5,col="gray")
##     pf(df[df$age<M,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     legend("bottomright",bty="n",formal.nm[nm],cex=1)
##     pf(df[df$age>M,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
##     #legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
##     if (nm==names(formal.nm)[1]) {
##         legend("bottomleft",bty="n",c("Older","Younger"),fill=c("red","blue"))
##     }
## }


## ##looking at memory
## is.na(df$tr) ->df$nomem
## mean(df$age[df$fpd],na.rm=TRUE)->M
## plot(NULL,xlab="Time from Spousal Death (months)",ylab="Mean",xlim=c(-40,40),ylim=c(0,1))
## abline(v=0,lwd=.5,col="gray")
## pf(df[df$age<M,],col.band="blue",points=FALSE,nobs=60,nm="nomem",std=FALSE)
## pf(df[df$age>M,],col.band="red",points=FALSE,nobs=60,nm="nomem",std=FALSE)





