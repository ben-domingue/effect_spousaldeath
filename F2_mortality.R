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


#############################################################################################
##residualized depression
##firs the descriptive figure, looking at time since spousal death
df[df$fpd==1 & df$gr==2,]->tmp
tmp[!is.na(tmp$fp.cesd.res),]->tmp
dim(tmp)
##
df[df$hhidpn %in% tmp$hhidpn,]->z
z[z$delta.death>0,]->z
split(z,z$hhidpn)->L
f<-function(x) {
    x[order(x$wave),]->x
    x[!is.na(x$iwend),]->x
    x$iwend[nrow(x)]-unique(x$ddate,na.rm=TRUE) -> del
    #x$iwend[nrow(x)]-x$iwend[1] -> del
    x$del.spdeath[1]->dsd
    is.na(dsd)->test
    if (test) {
        c(0,del)
    } else {
        c(1,dsd)
    }
}
lapply(L,f)->z
data.frame(hhidpn=names(z),do.call("rbind",z))->foo
names(foo)[2:3]<-c("dead","time")
merge(tmp,foo)->tmp
quantile(tmp$fp.cesd.res,c(.25,.75),na.rm=TRUE)->qu
tmp[tmp$fp.cesd.res<=qu[1],]->x1
tmp[tmp$fp.cesd.res>=qu[2],]->x2
##

##main text figure 4
tiff("/tmp/fig4.tiff", width = 3.2, height = 3, units = 'in', res = 300,pointsize=8)
library(survival)
par(mgp=c(2,1,0))
plot(NULL,ylim=c(0.6,1),xlim=c(0,5.5),xlab="Time since spousal death (years)",ylab="Proportion Surviving")
f<-function(tmp,col) {
    print(nrow(tmp))
    survfit(Surv(time=time,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod$time,mod$surv,lty=1,lwd=3,col=col)
    c(mod$surv+1.96*mod$std.err,rev(mod$surv-1.96*mod$std.err))->y.ci
    ifelse(!is.finite(y.ci),2,y.ci)->y.ci
    col2rgb(col)->cc
    polygon(c(mod$time,rev(mod$time)),y.ci,col=rgb(cc[1],cc[2],cc[3],max=255,alpha=90))
}
f(x1,col="black")
f(x2,col="red")
legend("bottomleft",bty="n",fill=c("black","red"),paste(c("Lowest 25%","Highest 25%"),c(nrow(x1),nrow(x2)),sep=", N="),title="Bereavement depressive symtoms")
dev.off()

##then the models
m<-list()
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
m[[1]]<-lm(dead.2year~age+age2+male+wave+fp.cesd.res,df[df$fpd==1 & df$gr==1,])
m[[2]]<-lm(dead.5year~age+age2+male+wave+fp.cesd.res,df[df$fpd==1 & df$gr==1,])
m[[3]]<-lm(dead.2year~age+age2+male+wave+fp.cesd.res,df[df$fpd==1 & df$gr==2,])
m[[4]]<-lm(dead.5year~age+age2+male+wave+fp.cesd.res,df[df$fpd==1 & df$gr==2,])
m[[5]]<-lm(dead.5year~age+age2+wave+fp.cesd.res,df[df$fpd==1 & df$gr==2 & df$ragender=="1.male",])
m[[6]]<-lm(dead.5year~age+age2+wave+fp.cesd.res,df[df$fpd==1 & df$gr==2 & df$ragender=="2.female",])
#do.call("c",L1)->m
table.lm(m,se=TRUE)->tab
write.csv(tab[,c(2,4:6)])

##risk for older respondents
set.seed(8030301)
df[df$fpd==1 & df$gr==2,]->tmp
mean(tmp$age)->fix.age
quantile(tmp$fp.cesd.res,c(.25,.75),na.rm=TRUE)->qu
m[[4]]->M
predict(M,data.frame(age=fix.age,age2=fix.age^2,male=0,wave=10,fp.cesd.res=qu),se.fit=TRUE)$fit->z
z[2]
z[1]
z[2]/z[1]
boot<-list()
for (i in 1:1000) {
    M$model->tmp
    sample(1:nrow(tmp),nrow(tmp),replace=TRUE)->index
    lm(dead.5year~age+age2+male+wave+fp.cesd.res,tmp[index,])->mm
    predict(mm,data.frame(age=fix.age,age2=fix.age^2,male=0,wave=10,fp.cesd.res=qu),se.fit=TRUE)$fit->z
    c(z[2],z[1])->boot[[i]]
}    
do.call("rbind",boot)->boot
apply(boot,2,quantile,c(.025,.975))


##younger respondents
set.seed(8030301)
df[df$fpd==1 & df$gr==1,]->tmp
mean(tmp$age)->fix.age
quantile(tmp$fp.cesd.res,c(.25,.75),na.rm=TRUE)->qu
m[[2]]->M
predict(M,data.frame(age=fix.age,age2=fix.age^2,male=0,wave=10,fp.cesd.res=qu),se.fit=TRUE)$fit->z
z[2]
z[1]
100*(z[2]/z[1])
boot<-list()
for (i in 1:1000) {
    M$model->tmp
    sample(1:nrow(tmp),nrow(tmp),replace=TRUE)->index
    lm(dead.5year~age+age2+male+wave+fp.cesd.res,tmp[index,])->mm
    predict(mm,data.frame(age=fix.age,age2=fix.age^2,male=0,wave=10,fp.cesd.res=qu),se.fit=TRUE)$fit->z
    c(z[2],z[1])->boot[[i]]
}    
do.call("rbind",boot)->boot
apply(boot,2,quantile,c(.025,.975))
