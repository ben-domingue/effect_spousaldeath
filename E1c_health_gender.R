par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=c(.5,10,.5,.5))
for (gender in c("1.male","2.female")) {
    ###############################################################################33
    vars<-c(cesd.std="CESD",iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
    
    names(vars)->vars
    std.vars<-FALSE
    
    
    ## ##run old analysis, should match main_v2
    ## paste(vars,".raw",sep="")->vars
    ## std.vars<-TRUE
    
    source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/E0_precursor.R")
    
    if (!is.null(gender)) df<-df[df$ragender==gender,]
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
        if (gender=="2.female") NULL-> names.txt else vars->names.txt
        barplot2(mat1,beside=TRUE,names.arg=names.txt,
                 plot.ci=TRUE,ci.u=mat1+1.96*mat2,ci.l=mat1-1.96*mat2,
                 horiz=TRUE,las=2,xaxt="n",xlim=c(0,.75),col=c("lightblue","royalblue"),
                 lwd=1.5,xlab="Effect size (Cohen's d)",cex.axis=.7
                 )->vals
        for (i in 1:ncol(mat1)) text(mat1[,i]+1.96*mat2[,i],vals[,i],round(mat1[,i],2),cex=.7,pos=4)
        axis(side=1)
        legend("bottomright",bty="n",fill=c("royalblue","lightblue"),title="Focal post-death\nobservations",legend=c("24-48 months","0-24 months"),cex=.7)
    }
    #tiff("/tmp/fig2.tiff", width = 3.2, height = 3, units = 'in', res = 300,pointsize=8)
    make.bar(tmp.forplot)
    #dev.off()
    if (!is.null(gender)) mtext(side=3,line=0,gender)
}
