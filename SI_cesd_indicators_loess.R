load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")

df[df$fpd,]->df.first
df.first$hhidpn ->ids
df[df$hhidpn %in% ids,]->df.sub
nrow(df.sub)
length(unique(df.sub$hhidpn))
df.sub->df
df$delta.death->df$delta
df[!is.na(df$delta),]->df
dim(df)
df[abs(df$delta)<=40,]->df
dim(df)

cc<-col2rgb("blue")
cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=45)

c(flone="lonely",fsad="sad",depres="depress",nhappy="not happy",nenlife="not enjoylife",going="motivation",sleepr="sleep",effort="effort")->vars
par(mfrow=c(2,4),mgp=c(2,1,0),mar=c(3.5,3.5,1,1))
for (nm in names(vars)) {
    plot(NULL,xlab="Months after Spousal Death",ylab="",xlim=c(-40,40),ylim=c(0,1),bty="n")
    abline(v=0)
    pf(df,points=TRUE,nobs.per.point=60,nm=nm,std=FALSE,col.points=cc)
    legend("topright",bty="n",vars[nm],cex=1.7)
}




##m/f
par(mfrow=c(2,8),mgp=c(2,1,0),mar=c(3.5,3.5,1,1))
mean(df$age[df$fpd],na.rm=TRUE)->M
df->hold
df->hold
for (nm in names(vars)) {
    as.numeric(hold[[nm]])->z
    (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->hold[[nm]]
}
for (male in 0:1) {
    hold[hold$male==male,]->df
    for (nm in names(vars)) {
        plot(NULL,xlab="Time from Spousal Death (months)",ylab="Mean",xlim=c(-40,40),ylim=c(-1,1))
        abline(v=0)
        pf(df[df$age<M,],col.band="blue",points=FALSE,nobs=60,nm=nm,std=FALSE)
        legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
        pf(df[df$age>M,],col.band="red",points=FALSE,nobs=60,nm=nm,std=FALSE)
        legend("bottomright",bty="n",formal.nm[nm],cex=1.5)
        legend("bottomright",bty="n",vars[nm],cex=1.5)
        if (nm==names(vars)[1]) {
            legend("topleft",bty="n",ifelse(male==1,"Males","Females"))
            if (male==0) legend("bottomleft",bty="n",c("Older","Younger"),fill=c("red","blue"))
        }
    }
}
hold->df
