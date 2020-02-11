
##local
forsave<-list()
for (gender in c("1.male","2.female")) {
    ##############################################################################################
    vars<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
    names(vars)->vars
    source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/E0_precursor.R")

    if (!is.null(gender)) df<-df[df$ragender==gender,]

    ##linear
    df[!is.na(df$delta.death),]->df
    df[!is.na(df$fp.cesd.res),]->df
    ifelse(df$delta.death<0,abs(df$delta.death),0)->df$tl
    ifelse(df$delta.death>=0,abs(df$delta.death),0)->df$th

    ##adding age at spousal death
    df[df$fpd==1,]->tmp
    tmp$delta.death/(12*10) -> del
    tmp$age-del -> tmp$age.death
    tmp[,c("hhidpn","age.death")]->tmp
    merge(df,tmp)->df


    df[df$fpd==1,]->tmp
    #cut(tmp$age.raw,c(-Inf,quantile(tmp$age.raw,c(1/3,2/3)),Inf),labels=1:3)->tmp$gr
    mean(tmp$age.raw,na.rm=TRUE)->m
    ifelse(tmp$age.raw<m,1,2)->tmp$gr
    tmp[,c("hhidpn","gr")]->tmp
    merge(df,tmp)->df

    ## dim(df)
    ## df[abs(df$delta.death)<60,]->df
    ## dim(df)

    df[df$hhidpn %in% df$hhidpn[df$fpd==1] & df$delta.death<=60,]->df

    df$delta.death/60->df$delta.death
    ## ifelse(!is.na(df$del.interview) & df$del.interview<5,1,0)->df$dead.5year
    ## df[df$dead.5year==1,]->tmp
    ## df[!(df$hhidpn %in% tmp$hhidpn),]->df

    split(df,df$gr)->LL
    forsave[[gender]]<-LL
}
save(forsave,file="LL_gender.Rdata")



#####################################################################################
load("LL_gender.Rdata")
ci<-TRUE

outfun<-function(df,fm,fm.cesd,ci=ci,nb=1000) {
    pred<-function(zz,fm,nb,pm,ci) {
        library(lfe)
        felm(fm,zz)->m
        print(summary(m))
        #print(m)
        #print(length(m$resid))
        ## for (iii in 1:length(proto.vars)) assign(names(proto.vars)[iii],proto.vars[[iii]])
        ## expand.grid(proto.vars)->y
        pm->y
        coef(m)->co
        names(co)->nms
        grep(":",nms)->ii
        if (length(ii)>0) {
            for (i in ii) {
                nms[i]->nm
                strsplit(nm,":")[[1]]->tmp
                gsub(":",".",nm,fixed=TRUE)->nm
                y[[tmp[1] ]]*y[[tmp[2] ]]->y[[nm]]
                nm->nms[i]
            }
        }
        nms->names(co)
        y[,nms,drop=FALSE]->y
        as.numeric(as.matrix(y) %*% matrix(co,ncol=1)) -> resp
        ##
        if (!ci) {
            return(m)
        } else {
            out<-list()
            parfun<-function(i,zz,y,fm) {
                sample(1:nrow(zz),nrow(zz),replace=TRUE)->index
                zz[index,]->zzb
                library(lfe)
                felm(fm,zzb)->m
                coef(m)->co
                as.numeric(as.matrix(y) %*% matrix(co,ncol=1))
            }
            library(parallel)
            RNGkind("L'Ecuyer-CMRG")
            ## makeCluster(20)->cl
            ## clusterApply(cl,1:nb,parfun,zz=zz,y=y,fm=fm)->out
            ## stopCluster(cl)
            mclapply(1:nb,parfun,zz=zz,y=y,fm=fm,mc.cores=10)->out
            do.call("cbind",out)->out
            apply(out,1,function(x) quantile(x,.025))->ql
            apply(out,1,function(x) quantile(x,.975))->qh
            apply(out,1,function(x) sd(x))->s
            resp->y$resp
            ql->y$ql
            qh->y$qh
            s->y$s
            return(y)
        }
    }
    df[df$fpd==1,]->tmp
    paste("Age at death\nrange: ",min(tmp$age.raw),"-",max(tmp$age.raw),sep="")->tr.txt
    ##
    #source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/make_lm_table.R")
    ff<-function(var,zz) {
        zz[[var]]->zz$cond
        library(lfe)
        m<-list()
        ##v3
        #felm(cond~age|hhidpn|0|hhidpn,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['0']]
        felm(fm,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['a']]
        felm(fm.cesd,zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],],keepModel=TRUE)->m[['b']]
        ##get numbers for figure captions
        numfun<-function(m) {
            print(nrow(m$model))
            print(length(unique(m$model$hhidpn)))
        }
        print(var)
        lapply(m,numfun)
        m
    }
    L1<-list()
    for (var in vars) ff(var,df)->L1[[var]]
    do.call("c",L1)->m
    if (!ci) {
        source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
        table.lm(m,se=TRUE,pv=TRUE)
    } else {
        pL1<-pL2<-list() 
        for (i in 1:length(L1)) {
            tl<- seq(-12,-2,by=.01) #this time is in months. 
            th<- seq(2,60,by=.01)
            tl/60 -> tl
            th/60 -> th
            dd<-c(tl,th)
            df[df$fpd==1,]->tmp
            mean(tmp$age.death)->ad
            quantile(tmp$fp.cesd.res,c(.75,.25))->qu
            ##ad-dd/120 -> age
            data.frame(delta.death=dd,age.death=ad)->xx
            ifelse(xx$delta.death>0,1,0)->xx$dead.spouse
            print(i)
            ##
            to.pred<-list()
            1->xx$gr
            to.pred[[1]]<-xx
            xx->xx2
            0->xx2$dead.spouse
            2->xx2$gr
            xx2->to.pred[[2]]
            ##
            data.frame(do.call("rbind",to.pred))->xx.use
            pred(L1[[i]]$a$model,fm=fm,nb=nb,pm=xx.use,ci=ci)->y
            data.frame(t=y$delta.death,resp=y$resp,ql=y$ql,qh=y$qh,s=y$s,gr=xx.use$gr)->pL1[[names(L1)[i] ]]
            ##
            xx->xx2a
            qu[1]->xx2a$fp.cesd.res 
            1->xx2a$gr
            xx->xx2b
            qu[2]->xx2b$fp.cesd.res 
            2->xx2b$gr
            data.frame(rbind(xx2a,xx2b))->xx.use
            ifelse(xx.use$delta.death<0,0,xx.use$fp.cesd.res)->xx.use$fp.cesd.res #this is only non-zero at obs post-death
            pred(L1[[i]]$b$model,fm=fm.cesd,nb=nb,pm=xx.use,ci=ci)->y
            data.frame(t=y$delta.death,resp=y$resp,ql=y$ql,qh=y$qh,s=y$s,gr=xx.use$gr,fp.cesd.res=xx.use$fp.cesd.res)->pL2[[names(L1)[i] ]]
        }
        list(tr.txt=tr.txt,pL1=pL1,pL2=pL2)
    }
}

##linear age effect
fm<-formula(cond~delta.death+dead.spouse+delta.death:dead.spouse+delta.death:age.death+dead.spouse:age.death+delta.death:dead.spouse:age.death|hhidpn|0|hhidpn)
fm.cesd<-formula(cond~dead.spouse*delta.death+delta.death*fp.cesd.res|hhidpn|0|hhidpn)
for (ii in 1:length(forsave)) {
    LL<-forsave[[ii]]
    LL2<-list()
    for (i in 1:length(LL)) outfun(LL[[i]],fm=fm,fm.cesd=fm.cesd,ci=ci)->LL2[[i]]
    forsave[[ii]]<-LL2
}

save(forsave,file="LL_gender_boot.Rdata")






########################################################################################
##barplots
##main text figure 3

par(mfrow=c(1,2),mar=c(4,3,1,1),mgp=c(2,1,0),oma=c(.5,10,.5,.5))
load("LL_gender_boot.Rdata")
for (iii in 1:length(forsave)) {
    LL2<-forsave[[iii]]
    gender<-names(forsave)[iii]
    f<-function(x) {
        x[x$t==1,]->tmp ##1=5 years
        tmp[order(tmp$fp.cesd.res),]->tmp
        tmp[,c("resp","s")]
    }
    lapply(rev(LL2[[2]]$pL2),f)->L
    lapply(L,function(x) x[,1])->est
    do.call("cbind",est)->est
    lapply(L,function(x) x[,2])->se
    do.call("cbind",se)->se
    library(gplots)
    vars<-c(hsptim="Hospital Visits",conde="Chronic Health Conditions",adla="Disability - ADLs",iadlza="Disability - IADLs",cesd.std="Depressive Symtoms")
    #tiff("/tmp/fig3.tiff", width = 3.2, height = 3, units = 'in', res = 300,pointsize=8)
    if (gender=="2.female") NULL-> names.txt else vars[colnames(est)]->names.txt
    barplot2(est,horiz=TRUE,beside=TRUE,col=c("lightblue","blue"),
             names=names.txt,las=2,xaxt="n",xlim=c(-0.05,1),
             plot.ci=TRUE,ci.u=est+1.96*se,ci.l=est-1.96*se,cex.axis=.7,
             xlab="")->vals
    mtext(side=1,line=3,"Projection (SD units) 5 years\nfollowing spousal death",cex=1)
    axis(side=1)
    legend("bottomright",bty="n",title="Depression",c("75th %-ile","25th %-ile"),cex=.6,fill=c("blue","lightblue"))
    for (i in 1:ncol(est)) text(est[,i]+1.96*se[,i],vals[,i],round(est[,i],2),cex=.7,pos=4)
    if (!is.null(gender)) mtext(side=3,line=0,gender)
}
