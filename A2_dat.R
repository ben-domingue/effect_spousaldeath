##need to be on ozzy
##mainly for residualization

fn<-"~/hrs/randhrs1992_2016v1.dta"

set.seed(801301)
load(file="df_prelim.Rdata")

##remove psych conditions from conde
library(foreign)
read.dta(fn)->x
grep("r.+psyche",names(x))->index
x[,index]->tmp
L<-list()
for (i in 1:ncol(tmp)) {
    names(tmp)[i]->nm
    gsub("psyche","",nm)->nm
    gsub("r","",nm)->w
    as.numeric(substr(tmp[,i],1,1))->psych
    data.frame(hhidpn=x$hhidpn,wave=w,psych=psych)->L[[i]]
}
data.frame(do.call("rbind",L))->tmp
merge(df,tmp,all.x=TRUE)->df
df$conde->df$oldconde.with.psych
df$conde-df$psych -> df$conde

##

load("~/hrs/grip_gait/grip_gait.Rdata")
-1*x$gait->x$gait
merge(df,x,all.x=TRUE)->df

formal.nm<-c(conde="# Conditions",iadlza="IADLs",adla="ADLs",hsptim="# Hospital Visits",grip="Grip",gait="Gait")
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)


ifelse(df$hsptim>10,10,df$hsptim)->df$hsptim
deg<-5
library(splines)
bs(df$age,degree=deg)->tmp
for (i in 1:deg) tmp[,i]->df[[paste("age.spline",i,sep="")]]
ff<-function(x) {
    sample(1:nrow(x),1,replace=FALSE)->ii
    x[ii,]
}

for (nm in names(formal.nm)) {
    print(nm)
    df[[nm]]->df[[paste(nm,".raw",sep="")]]
    fm<-formula(paste(nm,"~factor(wave)+male*(",paste("age.spline",1:deg,sep="",collapse="+"),")",sep=""))
    #fm<-formula(paste(nm,"~factor(wave)*male*(",paste("age.spline",1:deg,sep="",collapse="+"),")",sep=""))
    df[,c("hhidpn",all.vars(fm))]->tmp
    tmp[rowSums(is.na(tmp))==0,]->tmp
    split(tmp,tmp$hhidpn)->L
    co<-list()
    for (i in 1:100) {
        lapply(L,ff)->L2
        data.frame(do.call("rbind",L2))->L2
        if (nm %in% c("grip","gait")) {
            lm(fm,L2)->m
        } else {
            glm(fm,L2,family="poisson")->m
        }
        coef(m)->co[[i]]
    }
    ##testing
    co[[length(co)]]->co.test
    model.matrix(fm,L2)->mm
    mm %*% matrix(co.test,ncol=1)->kern
    if (!(nm %in% c("grip","gait"))) {
        exp(kern)->fit.test #for poisson variables
    } else {
        kern->fit.test
    }
    print(sum(abs(fit.test-fitted(m))))
    L2[[nm]]-fit.test->res.test
    resid(m,type="response")->res2.test
    print(sum(abs(res.test-res2.test)))
    ##
    do.call("rbind",co)->co
    colMeans(co)->co
    model.matrix(fm,tmp)->mm
    mm %*% matrix(co,ncol=1)->kern
    if (!(nm %in% c("grip","gait"))) {
        exp(kern)->fit #for poisson
    } else {
        kern->fit
    }
    tmp[[nm]]-fit->res
    std(res)->z
    as.numeric(z)->tmp[[nm]]
    NULL->df[[nm]]
    tmp[,c("hhidpn","wave",nm)]->tmp
    merge(df,tmp,all.x=TRUE)->df
}

save(df,file="resid.Rdata")


library(foreign)
read.dta(fn)->x
x[,c("hhidpn","raddate")]->tmp
merge(df,tmp,all.x=TRUE)->df
as.POSIXct(df$raddate*24*60^2,origin = "1960-01-01")->df$raddate.raw
#df[,c("raddate","raddate.raw","radyear","ranyear")]
unclass(df$raddate.raw)/(24*60^2)->df$raddate
unclass(df$iwend.raw)/(24*60^2)->df$iwend
df$raddate-df$iwend -> df$del.interview
df$raddate-df$ddate -> df$del.spdeath
ifelse(df$del.spdeath<0 | !is.finite(df$del.spdeath),NA,df$del.spdeath)->df$del.spdeath
df$del.spdeath/365->df$del.spdeath
df$del.interview/365->df$del.interview
ifelse(df$del.interview<0,NA,df$del.interview)->df$del.interview
#df[,c("raddate","raddate.raw","radyear","ranyear","iwend.raw","iwend","del.interview","del.spdeath","ddate","dyear","ddate.raw")]->tmp



##only first marriages
library(foreign)
read.dta(fn)->x
grep("s.+hhidpn",names(x))->index
x[,index]->tmp
for (i in 1:ncol(tmp)) ifelse(tmp[,i]=="0",NA,tmp[,i])->tmp[,i]
apply(tmp,1,function(x) length(unique(x[!is.na(x)])))->N
length(N[N>1])
x$hhidpn[N<2]->id
x$hhidpn[N>1]->id.remarriers
remarry<-df[df$hhidpn %in% id.remarriers,]
df[df$hhidpn %in% id,]->df
dim(df)
table(df$fpd)
## spouses<-list()
## for (i in 1:ncol(tmp)) {
##     cbind(x$hhidpn,tmp[,i],i)->spouses[[i]]
## }
## do.call("rbind",spouses)->spouses
## data.frame(spouses)->spouses
## names(spouses)<-c("hhidpn","spouse","wave")
## merge(df,spouses,all.x=TRUE)->df
## ##need to get lag. if you marry someone new, i'm worried delta.death ends up back at 0
## df[,c("hhidpn","wave","delta.death")]->tmp
## tmp$wave+1 -> tmp$wave
## names(tmp)[3]<-"dd.lag"
## merge(df,tmp,all.x=TRUE)->df
## dim(df)
## table(df$fpd)
## df[df$delta.death<0 | (df$delta.death>0 & is.na(df$spouse)),]->df #get rid of those who remarry
## dim(df)
## table(df$fpd)
## df[!(df$dd.lag>0 & df$delta.death<0),]->df #get rid of those who remarry and have delta.death reset at that time
## dim(df)
## table(df$fpd)

save(df,file="df.Rdata")

###################################################################
##compare those in sample to remarriers

1->remarry$remarry
0->df$remarry
df<-data.frame(rbind(df,remarry))
df<-df[df$fpd,]
ifelse(df$ragender=="1.male",1,0)->df$male
desc<-function(x) {
    vars<-c("male","rabyear","raedyrs","cesd.mean")
    out<-list()
    for (var in vars) {
        mean(x[[var]],na.rm=TRUE)->m
        sd(x[[var]],na.rm=TRUE)->s
        c(m,s)->out[[var]]
    }
    c(nrow(x),unlist(out))
}
desc(df[df$remarry==0,])->tab.all
desc(df[df$remarry==1,])->tab.sd

rbind(tab.all,tab.sd)->tab
write.csv(tab,"")



