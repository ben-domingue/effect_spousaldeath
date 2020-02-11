library(foreign)
#read.dta("randhrs1992_2014v2.dta")->x
read.dta("~/hrs/randhrs1992_2016v1.dta")->x

#x[x$rahispan=="0. not hispanic" & x$raracem=="1.white/caucasian",]->x
x[,c("hhidpn","ragender","raedyrs","rabyear","racohbyr","radyear","raracem","rahispan")]->df
NA->x$r2nhmmvy->x$r2nhmmvm->x$r2cogtot->x$r2tr20->x$r2drinkd#->x$r11cogtot->x$r11tr20->x$r11ser7

#cesd
grep("r[[:digit:]]+cesd$",names(x))->index
x[,index]->tmp
rowMeans(tmp,na.rm=TRUE)->df$cesd.mean

####################################################################
#get all long data
out<-list()
for (i in 2:13) {
    tmp.nms<-c("hhidpn",paste("r",i,"cesd",sep=""),paste("s",i,"hhidpn",sep=""),
               paste("r",i,"depres",sep=""),
               paste("r",i,"effort",sep=""),
               paste("r",i,"sleepr",sep=""),
               paste("r",i,"flone",sep=""),
               paste("r",i,"fsad",sep=""),
               paste("r",i,"going",sep=""),
               paste("r",i,"whappy",sep=""),
               paste("r",i,"enlife",sep=""),
               #paste("s",i,"dyear",sep=""),paste("s",i,"dmonth",sep=""),paste("s",i,"ddate",sep=""),
               paste("s",i,"dyear",sep=""),paste("s",i,"dmonth",sep=""),
               paste("r",i,"retyr",sep=""),paste("r",i,"retmon",sep=""),
               paste("s",i,"ddate",sep=""),
               paste("r",i,"iwendy",sep=""),paste("r",i,"iwendm",sep=""),
               paste("r",i,"iwend",sep=""),
               #paste("r",i,"nhmmvy",sep=""),
               #paste("r",i,"nhmmvm",sep=""),
               #paste("radappy",i,sep=""),
               #paste("radappm",i,sep=""),
               paste("r",i,"conde",sep=""),
               paste("r",i,"bmi",sep=""),
               paste("r",i,"iadlza",sep=""),
               paste("r",i,"cogtot",sep=""),
               paste("r",i,"tr20",sep=""),
               paste("r",i,"ser7",sep=""),
               paste("r",i,"drinkd",sep=""),
               paste("r",i,"cancr",sep=""))
    tmp.index<-    tmp.nms %in% names(x)
    x[,tmp.nms[tmp.index]]->tmp
    ##in case any variables aren't available
    bad.nms<-tmp.nms[!tmp.index]
    if (length(bad.nms)>0) {
        for (nm in bad.nms) tmp[[nm]]<-NA
        tmp<-tmp[,tmp.nms]
    }
    ##
    grep("s.hhidpn",names(tmp))->index
    paste(names(tmp)[index],".spouse",sep="")->names(tmp)[index]
    strsplit(names(tmp),"[[:digit:]]")->nms
    nm.fun<-function(x) {
        x[x!=""]->x
        length(x)->L
        if (L==1) x[1] else x[2] #the radappyW variables are problematic here
    }
    #sapply(nms,function(x) x[length(x)])->names(tmp)
    sapply(nms,nm.fun)->names(tmp)
    ##names(tmp)<-c("hhidpn","cesd","spouse.dyear","interview.year","conde","cancer")
    i->tmp$wave
    substr(tmp$cancr,1,1)->tmp$cancr
    ifelse(tmp$cancr>1,NA,tmp$cancr)->tmp$cancr
    #
    #
    tmp->out[[as.character(i)]]
}
data.frame(do.call("rbind",out))->z

as.POSIXct(z$ddate*24*60^2,origin = "1960-01-01")->z$ddate.raw
as.POSIXct(z$iwend*24*60^2,origin = "1960-01-01")->z$iwend.raw
unclass(z$ddate.raw)/(24*60^2)->z$ddate
unclass(z$iwend.raw)/(24*60^2)->z$iwend

ifelse(z$retyr<1990,NA,z$retyr)->z$retyr
as.POSIXct(paste(z$retyr,z$retmon,1,sep="-"),format="%Y-%m-%d")->z$retdate.raw
unclass(z$retdate.raw)/(24*60^2)->z$retdate


####################################################################

split(z,z$hhidpn)->tmp
fun<-function(x) {
    x[order(x$wave),]->x
    #first deal with spouses
    ifelse(x$hhidpn.spouse=="0",NA,x$hhidpn.spouse)->x$hhidpn.spouse
    #min(x$spouse.dyear,na.rm=TRUE)->x$spouse.dyear
    if (!all(is.na(x$hhidpn.spouse))) {
        x$hhidpn.spouse[1]->id
        for (i in 2:nrow(x)) {
            if (is.na(x$hhidpn.spouse[i])) x$hhidpn.spouse[i]<-id else x$hhidpn.spouse[i]->id
        }
    } else 0->x$hhidpn.spouse
    split(x,x$hhidpn.spouse)->z
    infun<-function(x) {
        min(x$ddate,na.rm=TRUE)->x$ddate
        (x$iwend-x$ddate)/30->x$delta.death
        #ifelse(x$delta.death<0,NA,x$delta.death)->x$delta.death
        ifelse(!is.finite(x$delta.death),NA,x$delta.death)->x$delta.death
        which.min(ifelse(x$delta.death<0,NA,x$delta.death))->index
        rep(0,nrow(x))->x$death.shock
        1->x$death.shock[index]
        x
    }
    lapply(z,infun)->z
    data.frame(do.call("rbind",z))->z
    #retirement
    which.max(z$retdate)->M
    if (length(M)>0) {
        z$retdate[M]->z$retdate
        z$retdate.raw[M]->z$retdate.raw
    } else {
        NA->z$retdate
        NA->z$retdate.raw
    }
    (z$iwend-z$retdate)/30->z$delta.ret
    ifelse(!is.finite(z$delta.ret),NA,z$delta.ret)->z$delta.ret
    ##health conditions
    c(NA,diff(z$conde))->z$health.shock
    ifelse(z$health.shock>1,1,z$health.shock)->z$health.shock
    ifelse(z$health.shock<0,0,z$health.shock)->z$health.shock
    ##
    z
}
tmp2<-list()
for (i in 1:length(tmp)) fun(tmp[[i]])->tmp2[[i]]
data.frame(do.call("rbind",tmp2))->z

ifelse(z$ddate<z$iwend,1,0)->z$dead.spouse

merge(df,z)->y
#y[!is.na(y$cesd),]->y

as.POSIXct("2001-9-11")->dd
unclass(dd)/(24*60^2)->dd
y$iwend-dd->dd
dd/30->y$delta.911

##################################################
split(y,y$hhidpn)->tmp
## fun<-function(x) {
##     x[order(x$wave),]->x
##     x$hhidpn.spouse[1]
## }
## sapply(tmp,fun)->ids

fun<-function(x) length(unique(x$hhidpn.spouse[!is.na(x$hhidpn.spouse)]))
sapply(tmp,fun)->N
table(N)
#tmp[N==1]->tmp
data.frame(do.call("rbind",tmp))->y

y->df #y[y$rabyear %in% 1919:1959,]->df

############################################################33
##this is going to identify the first post death (fpd) observation
##old school
df[df$delta.death>0,]->z
split(z,z$hhidpn)->tmp
fun<-function(x) {
    x[x$delta.death>0,]->x
    which.min(x$wave)->index
    x[index,]
}
lapply(tmp,fun)->tmp
data.frame(do.call("rbind",tmp))->df.first
nrow(df.first)->N.old

split(df,df$hhidpn)->tmp
fun<-function(x) {
    rep(FALSE,nrow(x))->x$fpd
    L<-list()
    x[is.na(x$delta.death),]->L[[1]]
    x[!is.na(x$delta.death),]->x
    x[x$delta.death<=0,]->L[[2]]
    x[x$delta.death>0,]->x
    if (nrow(x)>0) {
        which.min(x$wave)->index
        x$fpd[index]<-TRUE
        x->L[[3]]
    }
    data.frame(do.call("rbind",L))
}
lapply(tmp,fun)->tmp
data.frame(do.call("rbind",tmp))->df
sum(df$fpd,na.rm=TRUE)==N.old

split(df,df$hhidpn)->tmp
fun<-function(x) {
    rep(FALSE,nrow(x))->x$lpd
    L<-list()
    x[is.na(x$delta.death),]->L[[1]]
    x[!is.na(x$delta.death),]->x
    x[x$delta.death>=0,]->L[[2]]
    x[x$delta.death<0,]->x
    if (nrow(x)>0) {
        which.max(x$wave)->index
        x$lpd[index]<-TRUE
        x->L[[3]]
    }
    data.frame(do.call("rbind",L))
}
lapply(tmp,fun)->tmp
data.frame(do.call("rbind",tmp))->df

##RwDEPRES, RwEFFORT, RwSLEEPR, RwFLONE, RwFSAD, RwGOING, RwWHAPPY, and RwENLIFE 
for (nm in c("depres","effort","sleepr","flone","fsad","going","whappy","enlife")) {
    df[[nm]]->x
    as.numeric(substr(x,1,1))->df[[nm]]
}

ifelse(df$ragender=="1.male",1,0)->df$male
df$iwendy-df$rabyear -> df$age

#save(df,file="/home/domingue/hrs/spousal_death_cesd/df.Rdata")
1-df$whappy -> df$nhappy
1-df$enlife -> df$nenlife

df[!is.na(df$age) & df$age>50,]->df


library(foreign)
read.dta("~/hrs/randhrs1992_2016v1.dta")->x
c("hhidpn",
  paste("r",2:13,"iadla",sep=""),
  paste("r",2:13,"phonea",sep=""),
  paste("r",2:13,"moneya",sep=""),
  paste("r",2:13,"medsa",sep=""),
  paste("r",2:13,"shopa",sep=""),
  paste("r",2:13,"mealsa",sep=""),
  paste("r",2:13,"adla",sep=""),
  paste("r",2:13,"mobila",sep=""),
  paste("r",2:13,"lgmusa",sep=""),
  paste("r",2:13,"grossa",sep=""),
  paste("r",2:13,"finea",sep="")
  )->vars
x[,vars]->y
names(y)->nms
L<-list()
for (i in 2:13) {
    grep(paste("r",i,sep=""),nms)->index
    y[,c("hhidpn",nms[index])]->tmp
    names(tmp)->xx
    gsub("^r[1234567890]+","",xx)->names(tmp)
    ##c("hhidpn","iadla","phone","money","meds","shop","meals","adl","mobil","lgmus","gross","fine")->names(tmp)
    i->tmp$wave
    tmp->L[[as.character(i)]]
}
do.call("rbind",L)->L
data.frame(L)->L
for (nm in c("phonea","moneya","medsa","shopa","mealsa")) as.numeric(substr(L[[nm]],1,1))->L[[nm]]
merge(df,L,all.x=TRUE)->df

##adding behaviors
c("hhidpn",
  paste("r",2:13,"shlt",sep=""),
  paste("r",2:13,"hsptim",sep=""),
  paste("r",2:13,"hspnit",sep=""),
  paste("r",3:13,"nhmliv",sep=""),
  paste("r",2:13,"doctim",sep=""),
  paste("r",2:13,"oopmd",sep=""),
  paste("r",7:13,"mdactx",sep=""),
  paste("r",7:13,"vgactx",sep=""),
  paste("r",7:13,"ltactx",sep=""),
  paste("r",3:13,"drinkn",sep=""),
  paste("r",2:13,"liv10",sep=""),
  paste("r",2:13,"finpln",sep=""),
  paste("r",2:13,"pnhm5y",sep=""),
  paste("r",2:13,"cholst",sep="")
  )->vars
vars[vars %in% names(x)]->vars
x[,vars]->y
names(y)->nms
L<-list()
for (i in 2:13) {
    grep(paste("r",i,sep=""),nms)->index
    y[,c("hhidpn",nms[index])]->tmp
    names(tmp)->xx
    gsub("^r[1234567890]+","",xx)->names(tmp)
    i->tmp$wave
    tmp->L[[as.character(i)]]
}
unique(unlist(sapply(L,names)))->nms
for (i in 1:length(L)) {
    L[[i]]->tmp
    nms %in% names(tmp)->index
    grep(FALSE,index)->index
    for (j in index) rep(NA,nrow(tmp))->tmp[[nms[j] ]]
    tmp->L[[i]]
}
do.call("rbind",L)->L
data.frame(L)->L

merge(df,L,all.x=TRUE)->df
formal.nm<-c(drinkd="drinks/day",drinkn="# drinks",hsptim="times in hospital",doctim="times at doctor",oopmd="out of pocket medical xpens",
             ltactx="light activity (rc)",mdactx="light activity (rc)",vgactx="light activity (rc)",
             nhmliv="live nurs home",shlt="self report health",liv10="live 10y",finpln="financial planning window",pnhm5y="nurs home in 5y",cholst="prevent cholesterol")
for (nm in names(formal.nm)) {
    print(nm)
    print(table(df[[nm]]))
}
as.numeric(substr(df$nhmliv,1,1))->df$nhmliv
-1*as.numeric(substr(df$shlt,1,1))->df$shlt
-1*as.numeric(substr(df$ltactx,1,1))->df$ltactx
-1*as.numeric(substr(df$mdactx,1,1))->df$mdactx
-1*as.numeric(substr(df$vgactx,1,1))->df$vgactx
as.numeric(substr(df$finpln,1,1))->df$finpln
as.numeric(substr(df$cholst,1,1))->df$cholst

-1*df$tr -> df$tr
ifelse(!is.na(df$delta.death) & df$delta.death<36 & df$fpd==1,1,0)->df$fpd
as.logical(df$fpd)->df$fpd

ifelse(df$hsptim>25,25,df$hsptim)->df$hsptim
save(df,file="df_prelim.Rdata")









## load(file="/hrsshare/hrs_linked_N.Rdata")
##                                         #x[x$rahispan=="0. not hispanic" & x$raracem=="1.white/caucasian",]->x
## x[,c("subjectID","hhidpn","ragender","raedyrs","rabyear","racohbyr","radyear","raracem","rahispan")]->df
## NA->x$r2nhmmvy->x$r2nhmmvm

## #cesd
## grep("r[[:digit:]]+cesd$",names(x))->index
## x[,index]->tmp
## rowMeans(tmp,na.rm=TRUE)->df$cesd.mean

## ####################################################################
## #get all long data
## out<-list()
## for (i in 2:11) {
##     x[,c("hhidpn",paste("r",i,"cesd",sep=""),paste("s",i,"hhidpn",sep=""),
##          paste("r",i,"depres",sep=""),
##          paste("r",i,"effort",sep=""),
##          paste("r",i,"sleepr",sep=""),
##          paste("r",i,"flone",sep=""),
##          paste("r",i,"fsad",sep=""),
##          paste("r",i,"going",sep=""),
##          paste("r",i,"whappy",sep=""),
##          paste("r",i,"enlife",sep=""),
##                                         #paste("s",i,"dyear",sep=""),paste("s",i,"dmonth",sep=""),paste("s",i,"ddate",sep=""),
##          paste("s",i,"dyear",sep=""),paste("s",i,"dmonth",sep=""),
##          paste("r",i,"retyr",sep=""),paste("r",i,"retmon",sep=""),
##          paste("s",i,"ddate",sep=""),
##          paste("r",i,"iwendy",sep=""),paste("r",i,"iwendm",sep=""),
##          paste("r",i,"iwend",sep=""),
##          #paste("r",i,"nhmmvy",sep=""),
##          #paste("r",i,"nhmmvm",sep=""),
##          #paste("radappy",i,sep=""),
##          #paste("radappm",i,sep=""),
##          paste("r",i,"conde",sep=""),
##          paste("r",i,"cancr",sep=""))]->tmp
##     grep("s.hhidpn",names(tmp))->index
##     paste(names(tmp)[index],".spouse",sep="")->names(tmp)[index]
##     strsplit(names(tmp),"[[:digit:]]")->nms
##     nm.fun<-function(x) {
##         x[x!=""]->x
##         length(x)->L
##         if (L==1) x[1] else x[2] #the radappyW variables are problematic here
##     }
##     #sapply(nms,function(x) x[length(x)])->names(tmp)
##     sapply(nms,nm.fun)->names(tmp)
##     ##names(tmp)<-c("hhidpn","cesd","spouse.dyear","interview.year","conde","cancer")
##     i->tmp$wave
##     substr(tmp$cancr,1,1)->tmp$cancr
##     ifelse(tmp$cancr>1,NA,tmp$cancr)->tmp$cancr
##     #
##     #
##     tmp->out[[as.character(i)]]
## }
## data.frame(do.call("rbind",out))->z

## as.POSIXct(z$ddate*24*60^2,origin = "1960-01-01")->z$ddate.raw
## as.POSIXct(z$iwend*24*60^2,origin = "1960-01-01")->z$iwend.raw
## unclass(z$ddate.raw)/(24*60^2)->z$ddate
## unclass(z$iwend.raw)/(24*60^2)->z$iwend

## ifelse(z$retyr<1990,NA,z$retyr)->z$retyr
## as.POSIXct(paste(z$retyr,z$retmon,1,sep="-"),format="%Y-%m-%d")->z$retdate.raw
## unclass(z$retdate.raw)/(24*60^2)->z$retdate


## ####################################################################

## split(z,z$hhidpn)->tmp
## fun<-function(x) {
##     x[order(x$wave),]->x
##     #first deal with spouses
##     ifelse(x$hhidpn.spouse=="0",NA,x$hhidpn.spouse)->x$hhidpn.spouse
##     #min(x$spouse.dyear,na.rm=TRUE)->x$spouse.dyear
##     if (!all(is.na(x$hhidpn.spouse))) {
##         x$hhidpn.spouse[1]->id
##         for (i in 2:nrow(x)) {
##             if (is.na(x$hhidpn.spouse[i])) x$hhidpn.spouse[i]<-id else x$hhidpn.spouse[i]->id
##         }
##     } else 0->x$hhidpn.spouse
##     split(x,x$hhidpn.spouse)->z
##     infun<-function(x) {
##         min(x$ddate,na.rm=TRUE)->x$ddate
##         (x$iwend-x$ddate)/30->x$delta.death
##         #ifelse(x$delta.death<0,NA,x$delta.death)->x$delta.death
##         ifelse(!is.finite(x$delta.death),NA,x$delta.death)->x$delta.death
##         which.min(ifelse(x$delta.death<0,NA,x$delta.death))->index
##         rep(0,nrow(x))->x$death.shock
##         1->x$death.shock[index]
##         x
##     }
##     lapply(z,infun)->z
##     data.frame(do.call("rbind",z))->z
##     #retirement
##     which.max(z$retdate)->M
##     if (length(M)>0) {
##         z$retdate[M]->z$retdate
##         z$retdate.raw[M]->z$retdate.raw
##     } else {
##         NA->z$retdate
##         NA->z$retdate.raw
##     }
##     (z$iwend-z$retdate)/30->z$delta.ret
##     ifelse(!is.finite(z$delta.ret),NA,z$delta.ret)->z$delta.ret
##     ##health conditions
##     c(NA,diff(z$conde))->z$health.shock
##     ifelse(z$health.shock>1,1,z$health.shock)->z$health.shock
##     ifelse(z$health.shock<0,0,z$health.shock)->z$health.shock
##     ##
##     z
## }
## tmp2<-list()
## for (i in 1:length(tmp)) fun(tmp[[i]])->tmp2[[i]]
## data.frame(do.call("rbind",tmp2))->z

## ifelse(z$ddate<z$iwend,1,0)->z$dead.spouse

## merge(df,z)->y
## #y[!is.na(y$cesd),]->y

## as.POSIXct("2001-9-11")->dd
## unclass(dd)/(24*60^2)->dd
## y$iwend-dd->dd
## dd/30->y$delta.911

## ##################################################
## split(y,y$hhidpn)->tmp
## ## fun<-function(x) {
## ##     x[order(x$wave),]->x
## ##     x$hhidpn.spouse[1]
## ## }
## ## sapply(tmp,fun)->ids

## fun<-function(x) length(unique(x$hhidpn.spouse[!is.na(x$hhidpn.spouse)]))
## sapply(tmp,fun)->N
## table(N)
## #tmp[N==1]->tmp
## data.frame(do.call("rbind",tmp))->y

## y->df #y[y$rabyear %in% 1919:1959,]->df

## ############################################################33
## ##this is going to identify the first post death (fpd) observation
## ##old school
## df[df$delta.death>0,]->z
## split(z,z$hhidpn)->tmp
## fun<-function(x) {
##     x[x$delta.death>0,]->x
##     which.min(x$wave)->index
##     x[index,]
## }
## lapply(tmp,fun)->tmp
## data.frame(do.call("rbind",tmp))->df.first
## nrow(df.first)->N.old

## split(df,df$hhidpn)->tmp
## fun<-function(x) {
##     rep(FALSE,nrow(x))->x$fpd
##     L<-list()
##     x[is.na(x$delta.death),]->L[[1]]
##     x[!is.na(x$delta.death),]->x
##     x[x$delta.death<=0,]->L[[2]]
##     x[x$delta.death>0,]->x
##     if (nrow(x)>0) {
##         which.min(x$wave)->index
##         x$fpd[index]<-TRUE
##         x->L[[3]]
##     }
##     data.frame(do.call("rbind",L))
## }
## lapply(tmp,fun)->tmp
## data.frame(do.call("rbind",tmp))->df
## sum(df$fpd,na.rm=TRUE)==N.old

## split(df,df$hhidpn)->tmp
## fun<-function(x) {
##     rep(FALSE,nrow(x))->x$lpd
##     L<-list()
##     x[is.na(x$delta.death),]->L[[1]]
##     x[!is.na(x$delta.death),]->x
##     x[x$delta.death>=0,]->L[[2]]
##     x[x$delta.death<0,]->x
##     if (nrow(x)>0) {
##         which.max(x$wave)->index
##         x$lpd[index]<-TRUE
##         x->L[[3]]
##     }
##     data.frame(do.call("rbind",L))
## }
## lapply(tmp,fun)->tmp
## data.frame(do.call("rbind",tmp))->df

## ##RwDEPRES, RwEFFORT, RwSLEEPR, RwFLONE, RwFSAD, RwGOING, RwWHAPPY, and RwENLIFE 
## for (nm in c("depres","effort","sleepr","flone","fsad","going","whappy","enlife")) {
##     df[[nm]]->x
##     as.numeric(substr(x,1,1))->df[[nm]]
## }

## save(df,file="/home/domingue/hrs/spousal_death_cesd/df.Rdata")



