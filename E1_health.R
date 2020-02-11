###############################################################################33
vars<-c(cesd.std="CESD",iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
#vars<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits",grip="Grip",gait="Gait")

names(vars)->vars
std.vars<-FALSE


## ##run old analysis, should match main_v2
## paste(vars,".raw",sep="")->vars
## std.vars<-TRUE

source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/E0_precursor.R")

## by(df$dead.spouse,df$hhidpn,sum)->N
## data.frame(hhidpn=names(N),nobs=as.numeric(N))->tmp
## merge(df,tmp,all.x=TRUE)->df

df[df$fpd==1,]->tmp
tmp[,c("hhidpn","age.raw")]->tmp
names(tmp)[2]<-"age.fpd"
merge(df,tmp,all.x=TRUE)->df

df[df$hhidpn %in% df$hhidpn[df$fpd==1] & df$delta.death<=60,]->df
df[!is.na(df$hhidpn),]->df

## apply(tmp,1,function(x) length(unique(x[!is.na(x)])))->nn
## x$hhidpn[nn==1]->id
## df[df$hhidpn %in% id,]->df
## dim(df)

##only those observed at both time points
df[df$delta.death>0 & df$delta.death<24,]->tmp1
df[df$delta.death>24 & df$delta.death<48,]->tmp2
intersect(tmp1$hhidpn,tmp2$hhidpn)->id
df[df$hhidpn %in% id,]->df

##making sense of rising lines in figure 4
## summary(lm(conde~age,df))
## summary(felm(conde~age|hhidpn|0|hhidpn,df))

###############################################################################33
##looking just at the RD
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
ff<-function(var,zz) {
    zz[[var]]->zz$cond
    #zz[[paste(var,".nexttime",sep="")]]->zz$cond.nexttime
    zz[[paste(var,".lasttime",sep="")]]->zz$cond.lasttime
    library(lfe)
    m<-list()
    ## felm(cond~fpd+age+cond.lasttime|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],])->m[[1]]
    ## felm(cond~fpd*age+(fpd+age)*(cond.lasttime)|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepX=TRUE)->m[[2]]
    felm(cond~dead.spouse+age+cond.lasttime|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[[1]]
    felm(cond~dead.spouse*age+(dead.spouse+age)*(cond.lasttime)|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[[2]]
    ##
    zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],]->foo
    foo[foo$delta.death<24,]->foo
    felm(cond~dead.spouse+age+cond.lasttime|hhidpn|0|hhidpn,foo,keepModel=TRUE)->m[[3]]
    felm(cond~dead.spouse*age+(dead.spouse+age)*(cond.lasttime)|hhidpn|0|hhidpn,foo,keepModel=TRUE)->m[[4]]
    ##
    zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],]->foo
    foo[foo$delta.death<0 | (foo$delta.death>24 & foo$delta.death<48),]->foo
    felm(cond~dead.spouse+age+cond.lasttime|hhidpn|0|hhidpn,foo,keepModel=TRUE)->m[[5]]
    felm(cond~dead.spouse*age+(dead.spouse+age)*(cond.lasttime)|hhidpn|0|hhidpn,foo,keepModel=TRUE)->m[[6]]
    ##
    m
}
L1<-list()
for (var in vars) ff(var,df)->L1[[var]]

do.call("c",L1)->m
table.lm(m,se=FALSE,pv=FALSE)
table.lm(m,se=TRUE)->tab
grep("[35]$",colnames(tab))->index
tab[,index]->tmp.forplot
write.csv(tmp[c(1:3,nrow(tmp)),])
grep("[3456]$",colnames(tab))->index
tab[,index]->tmp
write.csv(tmp[c(1:3,nrow(tmp)),])
write.csv(tmp)
#write.csv(tab[,index],"")


##main text figure 2
make.bar<-function(tmp) {
    ##make a barplot
    #vars<-c(cesd.std="CESD",iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
    vars<-c(hsptim="Hospital Visits",conde="Chronic Health Conditions",adla="Disability - ADLs",iadlza="Disability - IADLs",cesd.std="Depressive Symtoms")
    grep("^dead.spouse$",rownames(tmp))->i
    tmp[i,]->x2
    tmp[i+1,]->s2
    mat1<-mat2<-list()
    for (var in names(vars)) {
        grep(var,names(x2))->index
        x2[index]->mat1[[var]]
        s2[index]->mat2[[var]]
    }
    do.call("cbind",mat1)->mat1
    do.call("cbind",mat2)->mat2
    library(gplots)
    par(mar=c(3,10.5,1,1),mgp=c(2,1,0),oma=rep(.5,4))
    barplot2(mat1,beside=TRUE,names.arg=vars,
             plot.ci=TRUE,ci.u=mat1+1.96*mat2,ci.l=mat1-1.96*mat2,
             horiz=TRUE,las=2,xaxt="n",xlim=c(0,.65),col=c("lightblue","royalblue"),
             lwd=1.5,xlab="Effect size (Cohen's d)",cex.axis=.7
             )->vals
    for (i in 1:ncol(mat1)) text(mat1[,i]+1.96*mat2[,i],vals[,i],round(mat1[,i],2),cex=.7,pos=4)
    axis(side=1)
    legend("bottomright",bty="n",fill=c("royalblue","lightblue"),title="Focal post-death\nobservations",legend=c("24-48 months","0-24 months"),cex=.7)
}
#tiff("/tmp/fig2.tiff", width = 3.2, height = 3, units = 'in', res = 300,pointsize=8)
make.bar(tmp.forplot)
#dev.off()

L1->L.rd
save(L.rd,file="lrd.Rdata")






## #################################################################################################
## ##stratified by nobs
## ##all
## ftmp<-function(df) {
##     L1<-list()
##     for (var in vars) ff(var,df)->L1[[var]]
##     do.call("c",L1)->m
##     table.lm(m,se=TRUE)->tab
##     tab
## }
## ftmp2<-function(df) {
##     ftmp(df[df$nobs==1,])->tab1
##     ftmp(df[df$nobs==2,])->tab2
##     ftmp(df[df$nobs>2,])->tab3
##     c(19:21)->index2
##     print(rownames(tab1)[index])
##     grep("3",colnames(tab))->index1
##     t2<-list()
##     t2[[1]]<-tab1[index2,index1]
##     t2[[2]]<-tab2[index2,index1]
##     t2[[3]]<-tab3[index2,index1]
##     est<-do.call("rbind",lapply(t2,function(x) x[1,]))
##     se<-do.call("rbind",lapply(t2,function(x) x[2,]))
##     library(gplots)
##     barplot2(est,beside=TRUE,plot.ci=TRUE,ci.l=est-1.96*se,ci.u=est+1.96*se)
## }
## ftmp2(df)

## ##age-stratified
## mean(df$age.raw[df$fpd==1],na.rm=TRUE)->M
## par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
## ftmp2(df[df$age.raw<M,])
## ftmp2(df[df$age.raw>=M,])




############################################################################################
##by sex and race
ff2<-function(df) {
    L1<-list()
    for (var in vars) ff(var,df)->L1[[var]]
    do.call("c",L1)->m
    table.lm(m,se=TRUE)->tab
    grep("[13]",colnames(tab))->index
    tab[,index]
}
split(df,df$ragender)->ll
lapply(ll,ff2)->tab1
tab2<-list()
ff2(df[df$rahispan=="1.hispanic",])->tab2$hispanic
ff2(df[df$raracem=="1.white/caucasian" & df$rahispan=="0.not hispanic",])->tab2$white
ff2(df[df$raracem=="2.black/african american" & df$rahispan=="0.not hispanic",])->tab2$black
cbind(do.call("cbind",tab1),do.call("cbind",tab2))->tab
tab[c(1:3,19:21,28),]->tmp
write.csv(tmp,"")







## ##sex differences
## ff<-function(var,zz) {
##     zz[[var]]->zz$cond
##     #zz[[paste(var,".nexttime",sep="")]]->zz$cond.nexttime
##     zz[[paste(var,".lasttime",sep="")]]->zz$cond.lasttime
##     library(lfe)
##     m<-list()
##     felm(cond~dead.spouse+dead.spouse:male+age+cond.lasttime|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],])->m[['3']]
##     ##table.lm(m)
##     m
## }
## L1<-list()
## ifelse(df$dead.spouse==1 & df$male==1,"m",0)->df$dead.spouse2
## ifelse(df$dead.spouse==1 & df$male==0,"f",df$dead.spouse2)->df$dead.spouse2
## factor(df$dead.spouse2,levels=c(0,"m","f"))->df$dead.spouse2
## for (var in vars) ff(var,df[df$age>0,])->L1[[var]]
## do.call("c",L1)->m
## table.lm(m)->tab
## write.csv(tab,"")


## ##for grip and gait
## source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/make_lm_table.R")
## vars<-c(grip="grip",gait="gait")
## ff<-function(var,zz) {
##     zz[[var]]->zz$cond
##     #zz[[paste(var,".nexttime",sep="")]]->zz$cond.nexttime
##     zz[[paste(var,".lasttime",sep="")]]->zz$cond.lasttime
##     library(lfe)
##     m<-list()
##     felm(cond~fpd+age|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],])->m[[1]]
##     felm(cond~fpd*age+(fpd+age)|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepX=TRUE)->m[[2]]
##     felm(cond~dead.spouse+age|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],])->m[[3]]
##     felm(cond~dead.spouse*age+(dead.spouse+age)|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[[4]]
##     felm(cond~fpd+age|hhidpn|0|hhidpn,zz)->m[[5]]    #felm(cond~fpd*age+(fpd+age)*(cond.lasttime)|hhidpn|0|hhidpn,zz)->m[[5]]
##     felm(cond~dead.spouse+age|hhidpn|0|hhidpn,zz)->m[[6]]    #felm(cond~dead.spouse*age+(dead.spouse+age)*(cond.lasttime)|hhidpn|0|hhidpn,zz)->m[[6]]
##     ##table.lm(m)
##     m
## }
## L1<-list()
## for (var in vars) ff(var,df)->L1[[var]]
## do.call("c",L1)->m
## table.lm(m,se=TRUE)->tab
## grep("[24]",colnames(tab))->index
## tab[,index]



