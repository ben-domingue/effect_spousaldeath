load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
##as.numeric(df$cancr)->df$cancr
df$age->df$age.raw
df$age/10->df$age
df$age-mean(df$age[df$fpd],na.rm=TRUE) -> df$age
(df$cesd-mean(df$cesd,na.rm=TRUE))/sd(df$cesd,na.rm=TRUE)->df$cesd.std
ifelse(df$fpd,1,0)->df$fpd
print(table(df$fpd))


std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
if (exists("std.vars") && std.vars) {
    for (var in vars) {
        #ifelse(df[[var]]>3,3,df[[var]])->df[[var]]
        print(var)
        print(table(df[[var]]))
        std(df[[var]])->df[[var]]
    }
}


df[,c("wave","hhidpn",vars,"cesd")]->tmp
tmp$wave <- tmp$wave +1
names(tmp)->nms
paste(nms[-(1:2)],".lasttime",sep="")->nms[-(1:2)]
nms->names(tmp)
merge(df,tmp,all.x=TRUE)->df

df[,c("wave","hhidpn",vars,"cesd")]->tmp
tmp$wave <- tmp$wave -1
names(tmp)->nms
paste(nms[-(1:2)],".nexttime",sep="")->nms[-(1:2)]
nms->names(tmp)
merge(df,tmp,all.x=TRUE)->df

df[,c("wave","hhidpn",vars,"cesd")]->tmp
tmp$wave <- tmp$wave -2
names(tmp)->nms
paste(nms[-(1:2)],".nexttime.2",sep="")->nms[-(1:2)]
nms->names(tmp)
merge(df,tmp,all.x=TRUE)->df

df[df$fpd==1,]->fp
fp[,c("hhidpn","cesd")]->fp
names(fp)[2]<-"fp.cesd"
merge(df,fp,all.x=TRUE)->df
ifelse(!is.na(df$dead.spouse) & df$dead.spouse==0,0,df$fp.cesd)->df$fp.cesd #this is just an indicator of their depression at the fpd wave
df->hold

##residualize
load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
df[df$fpd,]->fp
fp[!is.na(fp$delta.death) & fp$delta.death<36,]->fp
fp[rowSums(is.na(fp[,c("cesd","delta.death")]))==0,]->fp
##
start<-list(
    b0=mean(fp$cesd[fp$delta.death>20]),
    b2=.4,
    lambda=20
)
fm<-as.formula(cesd~b0+b2*exp(-1*delta.death/(lambda)))
##
## start<-list(
##     b0=mean(df$cesd[df$delta.death>20],na.rm=TRUE),
##     b.male=0,
##     b.age=0,
##     b2=.4,
##     lambda=20
## )
## fm<-as.formula(cesd~b0+
##                    b.male*male+b.age*age+
##                    (b2)*exp(-1*delta.death/(lambda)))
##
library(minpack.lm)
mod<-nlsLM(fm,data=fp,
           start=start
           )    
print(mod)
length(residuals(mod))
coef(mod)->co
fp$cesd-fitted(mod)->fp$fp.cesd.res
fitted(mod)->fp$fp.cesd.fit
fp[,c("hhidpn","fp.cesd.res","fp.cesd.fit")]->fp
hold->df
merge(df,fp,all.x=TRUE)->df
ifelse(!is.na(df$dead.spouse) & df$dead.spouse==0,0,df$fp.cesd.res)->df$fp.cesd.res

df[df$dead.spouse==0,]->tmp
by(tmp$cesd,tmp$hhidpn,mean,na.rm=TRUE)->m
data.frame(hhidpn=names(m),cesd.pre=as.numeric(m))->tmp
merge(df,tmp,all.x=TRUE)->df

by(df$dead.spouse,df$hhidpn,max,na.rm=TRUE)->m
data.frame(hhidpn=names(m),anydeath=as.numeric(m))->tmp
merge(df,tmp,all.x=TRUE)->df
