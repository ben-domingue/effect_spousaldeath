#########################################################################################################
#vars<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
vars<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits",grip="Grip",gait="Gait")
names(vars)->vars
std.vars<-FALSE
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/E0_precursor.R")

ifelse(!is.na(df$del.interview) & df$del.interview<2,1,0)->df$dead.2year
ifelse(!is.na(df$del.interview) & df$del.interview<5,1,0)->df$dead.5year

df[df$fpd==1,]->tmp
table(is.na(tmp$del.interview))/nrow(tmp)
##
table(df$dead.2year)/nrow(df)
table(tmp$dead.2year)/nrow(tmp)
##
table(df$dead.5year)/nrow(df)
table(tmp$dead.5year)/nrow(tmp)

df[df$fpd==1,]->tmp
#cut(tmp$age.raw,c(-Inf,quantile(tmp$age.raw,c(1/3,2/3)),Inf),labels=1:3)->tmp$gr
mean(tmp$age.raw,na.rm=TRUE)->m
ifelse(tmp$age.raw<m,1,2)->tmp$gr
tmp[,c("hhidpn","gr")]->tmp
merge(df,tmp,all.x=TRUE)->df

df$age^2 -> df$age2

###############################################################################################################

##mortality pressure higher amongst those who experience spousal daeth
library(lfe)
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
m<-list()
m[[1]]<-felm(dead.5year~dead.spouse+age+age2|hhidpn|0|hhidpn,df[is.na(df$delta.death) | df$delta.death<60,])
m[[2]]<-felm(dead.5year~dead.spouse+age+age2+dead.spouse:age+dead.spouse:age2|hhidpn|0|hhidpn,df[is.na(df$delta.death) | df$delta.death<60,])
m[[3]]<-felm(dead.5year~dead.spouse+age+age2+dead.spouse:age+dead.spouse:age2|hhidpn|0|hhidpn,df[df$ragender=="1.male" & (is.na(df$delta.death) | df$delta.death<60),])
m[[4]]<-felm(dead.5year~dead.spouse+age+age2+dead.spouse:age+dead.spouse:age2|hhidpn|0|hhidpn,df[df$ragender=="2.female" & (is.na(df$delta.death) | df$delta.death<60),])
#m[[3]]<-felm(dead.5year~dead.spouse+age+age2|hhidpn|0|hhidpn,df[df$ragender=="1.male" & is.na(df$delta.death) | df$delta.death<60,])
#m[[4]]<-felm(dead.5year~dead.spouse+age+age2|hhidpn|0|hhidpn,df[df$ragender=="2.female" & is.na(df$delta.death) | df$delta.death<60,])
## df[df$gr==1,]->df2
## m[[3]]<-felm(dead.5year~dead.spouse+age+age2|hhidpn|0|hhidpn,df2[is.na(df2$delta.death) | df2$delta.death<60,])
## df[df$gr==2,]->df2
## m[[4]]<-felm(dead.5year~dead.spouse+age+age2|hhidpn|0|hhidpn,df2[is.na(df2$delta.death) | df2$delta.death<60,])
## df$hhidpn[df$fpd==1]->id
## df[df$hhidpn %in% id,]->df2
## m[[3]]<-felm(dead.5year~dead.spouse+age+age2|hhidpn|0|hhidpn,df2[is.na(df2$delta.death) | df2$delta.death<60,])
## m[[4]]<-felm(dead.5year~dead.spouse+age+age2+dead.spouse:age+dead.spouse:age2|hhidpn|0|hhidpn,df2[is.na(df2$delta.death) | df2$delta.death<60,])
table.lm(m,se=TRUE)->tab
write.csv(tab)

tmp<-list()
eval(m[[2]]$model)->tmp[[1]]
eval(m[[3]]$model)->tmp[[2]]
eval(m[[4]]$model)->tmp[[3]]
save(tmp,file="/tmp/bs.Rdata")

#on ozzy
#load pred from E2b_...
age<-c(0,1)
tab<-list()
for (i in 1:length(tmp)) tab[[i]]<-pred(tmp[[i]],as.formula("dead.5year~dead.spouse+age+age2+dead.spouse:age+dead.spouse:age2|hhidpn|0|hhidpn"),nb=100,proto.vars=list(dead.spouse=0:1,age=age,age2=age^2)) #on ozzy
f<-function(z) {
    z<-z[z$age==z$age2,]
    z
}
lapply(tab,f)




## m<-list()
## source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
## m[[1]]<-lm(dead.5year~dead.spouse+male+age+age2+factor(wave),df[is.na(df$delta.death) | df$delta.death<60,])
## m[[2]]<-lm(dead.5year~dead.spouse*male+age+age2+factor(wave),df[is.na(df$delta.death) | df$delta.death<60,])
## m[[3]]<-lm(dead.5year~dead.spouse*age*age2+factor(wave)+male,df[is.na(df$delta.death) | df$delta.death<60,])
## m[[4]]<-lm(dead.5year~dead.spouse*age*male*age2+factor(wave),df[is.na(df$delta.death) | df$delta.death<60,])
## table.lm(m,se=TRUE)->tab
## write.csv(tab)

## set.seed(8030301)
## m[[1]]->M
## predict(M,data.frame(age=0,male=0,wave=10,dead.spouse=0:1),se.fit=TRUE)$fit->z
## z[2]/z[1]->est
## boot<-numeric()
## for (i in 1:1000) {
##     M$model->tmp
##     sample(1:nrow(tmp),nrow(tmp),replace=TRUE)->index
##     lm(dead.5year~dead.spouse+male+age+age2+wave,tmp[index,])->mm
##     predict(mm,data.frame(age=0,male=0,wave=10,dead.spouse=0:1))->z
##     z[2]/z[1]->boot[i]
## }    
## est*100
## quantile(boot,c(.025,.975))*100

## m[[3]]->M
## data.frame(age=c(-1),male=0,wave=10,dead.spouse=0:1)->x1
## data.frame(age=c(1),male=0,wave=10,dead.spouse=0:1)->x2
## x1$age^2->x1$age2
## x2$age^2->x2$age2
## data.frame(rbind(x1,x2))->pred
## pred->pred1
## 1->pred1$male
## predict(M,pred,se.fit=TRUE)$fit->z
## cbind(pred,z)
## m[[4]]->M
## data.frame(age=c(-1),male=0,wave=10,dead.spouse=0:1)->x1
## data.frame(age=c(1),male=0,wave=10,dead.spouse=0:1)->x2
## x1$age^2->x1$age2
## x2$age^2->x2$age2
## data.frame(rbind(x1,x2))->pred
## pred->pred1
## 1->pred1$male
## data.frame(rbind(pred,pred1))->pred
## predict(M,pred,se.fit=TRUE)$fit->z
## M
## cbind(pred,z)


#######################################################################################

## load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")

## df[df$hhidpn %in% df$hhidpn[df$fpd],]->df
## df[!is.na(df$delta.death),]->df

## split(df,df$hhidpn)->z
## f1<-function(x) {
##     x[order(x$wave),]->x
##     which(x$fpd)->ii
##     bef<-max(sum(!is.na(x$adla[1:ii]))-1,0)
##     aft<-max(sum(!is.na(x$adla[ii:nrow(x)]))-1,0)
##     c(x$age[ii],min(bef,5),min(aft,5))
## }
## lapply(z,f1)->tmp
## do.call("rbind",tmp)->tmp
## quantile(tmp[,1],c(.25,.5,.75))->qu
## cut(tmp[,1],c(-Inf,qu,Inf),labels=1:4)->gr
## split(data.frame(tmp),gr)->tmp2
## f2<-function(x) {
##     table(x[,2])/nrow(x)->tab1
##     table(x[,3])/nrow(x)->tab2
##     list(cumsum(rev(tab1)),cumsum(rev(tab2)))
## }
## lapply(tmp2,f2)


## by(df$dead.spouse,df$hhidpn,sum)->N
## data.frame(hhidpn=names(N),nobs=as.numeric(N))->tmp
## merge(df,tmp)->df
## par(mfcol=c(2,2),mgp=c(2,1,0),oma=rep(.5,4),mar=c(3,3,1,1))
## formal.nm<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
## for (nm in names(formal.nm)) {
##     paste(nm,"~age+nobs")->fm
##     lm(fm,df[df$fpd,])->m
##     print(summary(m))
##     ##
##     by(df[[nm]],df$nobs,mean,na.rm=TRUE)->m
##     data.frame(names(m),as.numeric(m))->m
##     plot(m,ylim=c(-.5,.5),xlab="Number of Observations",ylab=formal.nm[nm],type="b",col="red")
## }

## ##health
## df[df$hhidpn %in% df$hhidpn[df$fpd==1] & df$delta.death<=60,]->df
## source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/make_lm_table.R")
## ff<-function(var,zz) {
##     zz[[var]]->zz$cond
##     zz[[paste(var,".lasttime",sep="")]]->zz$cond.lasttime
##     library(lfe)
##     m<-list()
##     if (var %in% c("grip","gait")) {
##         felm(cond~fpd+age|hhidpn|0|hhidpn,zz)->m[[1]]
##         felm(cond~fpd*age+(fpd+age)|hhidpn|0|hhidpn,zz,keepX=TRUE)->m[[2]]
##         felm(cond~dead.spouse+age|hhidpn|0|hhidpn,zz)->m[[3]]
##         felm(cond~dead.spouse*age+(dead.spouse+age)|hhidpn|0|hhidpn,zz,keepModel=TRUE)->m[[4]]
##     } else {
##         felm(cond~fpd+age+cond.lasttime|hhidpn|0|hhidpn,zz)->m[[1]]
##         felm(cond~fpd*age+(fpd+age)*(cond.lasttime)|hhidpn|0|hhidpn,zz,keepX=TRUE)->m[[2]]
##         felm(cond~dead.spouse+age+cond.lasttime|hhidpn|0|hhidpn,zz)->m[[3]]
##         felm(cond~dead.spouse*age+(dead.spouse+age)*(cond.lasttime)|hhidpn|0|hhidpn,zz,keepModel=TRUE)->m[[4]]
##         lm(cond.nexttime~fp.cesd.res+age+male+cesd.pre+cond,zz[zz$fpd==1,])->m[['5']]
##     }
##     m
## }
## L1<-list()
## for (var in vars[1:4]) ff(var,df)->L1[[var]]
## do.call("c",L1)->m
## table.lm(m,se=FALSE,pv=FALSE)
## table.lm(m,se=TRUE)->tab
## tab

## L<-list()
## for (cond in vars[1:4]) {
##     m<-list()
##     df[[var]]->df$cond
##     df[[paste(var,".nexttime",sep="")]]->df$cond.nexttime
##     fm<-formula(cond~delta.death+dead.spouse+delta.death:dead.spouse+delta.death:age.death+dead.spouse:age.death+delta.death:dead.spouse:age.death|hhidpn|0|hhidpn)
##     fm.cesd<-formula(cond~delta.death+fp.cesd.res+delta.death:fp.cesd.res+delta.death:age.death+fp.cesd.res:age.death+delta.death:fp.cesd.res:age.death|hhidpn|0|hhidpn)
##     felm(fm,df)->m[['a']]
##     felm(fm.cesd,df)->m[['b']]
##     m->L[[cond]]
## }
