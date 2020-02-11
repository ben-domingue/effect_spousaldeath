###############################################################################33
vars<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
#vars<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs")#,mobila="Mobility Problems",lgmusa="Large muscle",grossa="Gross motor problems",finea="Fine motor problems")

names(vars)->vars
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/E0_precursor.R")
std.vars<-FALSE

df[df$hhidpn %in% df$hhidpn[df$fpd==1] & df$delta.death<=60,]->df

ifelse(!is.na(df$del.interview) & df$del.interview<2,1,0)->df$dead.2year
ifelse(!is.na(df$del.interview) & df$del.interview<5,1,0)->df$dead.5year

df[df$fpd==1,]->tmp
#cut(tmp$age.raw,c(-Inf,quantile(tmp$age.raw,c(1/3,2/3)),Inf),labels=1:3)->tmp$gr
mean(tmp$age.raw,na.rm=TRUE)->m
ifelse(tmp$age.raw<m,1,2)->tmp$gr
tmp[,c("hhidpn","gr")]->tmp
merge(df,tmp,all.x=TRUE)->df
df[df$gr==2,]->df

df[df$fpd==1,]->tmp
cor(tmp$fp.cesd.res,tmp$cesd.pre,use='p')

###############################################################################33
##depression as mediator observations
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
ff<-function(var,zz) {
    zz[[var]]->zz$cond
    zz[[paste(var,".nexttime",sep="")]]->zz$cond.nexttime
    zz[[paste(var,".nexttime.2",sep="")]]->zz$cond.nexttime.2
    zz[[paste(var,".lasttime",sep="")]]->zz$cond.lasttime
    zz[zz$dead.spouse==1,]->tmp
    by(tmp$cond,tmp$hhidpn,max,na.rm=TRUE)->mm
    data.frame(hhidpn=names(mm),cond.after=as.numeric(mm))->tmp
    ifelse(!is.finite(tmp[,2]),NA,tmp[,2])->tmp[,2]
    merge(zz,tmp,all.x=TRUE)->zz
    zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],]->tmp
    split(tmp,tmp$hhidpn)->tmp
    ff<-function(x) {
        x$wave[x$fpd==1]->w
        x[x$wave %in% ((w+1):(w+3)),]->x
        mean(x$cond,na.rm=TRUE)->m
        if (is.finite(m)) m else NA
    }
    data.frame(hhidpn=names(tmp),cond.mean3=sapply(tmp,ff))->tmp
    merge(zz,tmp,all.x=TRUE)->zz
    ##
    library(lfe)
    m<-list()
    lm(cond.nexttime~fp.cesd.res+age+male+cesd.pre+cond,zz[zz$fpd==1,])->m[['1']]
    #lm(cond.nexttime.2~fp.cesd.res+age+male+cesd.pre+cond,zz[zz$fpd==1,])->m[['2']]
    m
}
L1<-list()
for (var in vars) ff(var,df[df$age.raw>70,])->L1[[var]]
#for (var in vars) ff(var,df)->L1[[var]]
do.call("c",L1)->m
table.lm(m,se=TRUE)->tab
tab
write.csv(tab,"")

