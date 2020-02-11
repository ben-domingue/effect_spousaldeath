load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(1,4))
library(gplots)


####################################################################################
##unconditional
c(flone="lonely",fsad="sad",depres="depress",nhappy="not happy",nenlife="not enjoylife",going="motivation",sleepr="sleep",effort="effort")->vars
se.out<-out<-list()
for (var in names(vars)) {
    se<-function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))
    by(df[[var]],df$fpd,mean,na.rm=TRUE)->tr1
    by(df[[var]],df$fpd,se)->se1
                                        #c(tr2,tr1)->out[[var]]
                                        #c(tr2[1],tr1[1],tr2[2],tr1[2])->out[[var]]
                                        #c(se2[1],se1[1],se2[2],se1[2])->se.out[[var]]
    c(tr1)->out[[var]]
    c(se1)->se.out[[var]]
}
do.call("rbind",out)->tab
do.call("rbind",se.out)->se.tab
all(rev(rownames(tab)[order(tab[,2]-tab[,1])])==names(vars))
                                        #c("lightgray","darkgray","lightblue","blue")->col
c("lightgray","lightblue")->cols
barplot2(
    t(tab),
    beside=TRUE,ylim=c(0,1),
    ylab="Indicator Prevalence",
    names.arg=vars,
    col=cols,
    plot.ci=TRUE,ci.u=t(tab)+1.96*t(se.tab),ci.l=t(tab)-1.96*t(se.tab)
)->vals
legend("topleft",bty="n",fill=cols,c("Non-Death","First post death"))
#mtext(side=3,adj=1,"A")
for (i in 1:nrow(tab)) text(vals[,i],tab[i,]+1.96*se.tab[i,],round(tab[i,],2),cex=.75,pos=3)
mtext(side=3,adj=0,"A")



c(flone="lonely",fsad="sad",depres="depress",nhappy="not happy",nenlife="not enjoylife",going="motivation",sleepr="sleep",effort="effort")->vars
##conditional
se.out<-out<-list()
for (var in names(vars)) {
    grep(var,names(vars))->ii
    df[,names(vars)[-ii]]->tmp
    rowSums(tmp,na.rm=TRUE)->df$cesd.loo
    df[df$fpd,]->fp
    df[!df$fpd,]->lp
    se<-function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))
    by(fp[[var]],fp$cesd.loo,mean,na.rm=TRUE)->tr1
    by(lp[[var]],lp$cesd.loo,mean,na.rm=TRUE)->tr2
    by(fp[[var]],fp$cesd.loo,se)->se1
    by(lp[[var]],lp$cesd.loo,se)->se2
                                        #c(tr2,tr1)->out[[var]]
                                        #c(tr2[1],tr1[1],tr2[2],tr1[2])->out[[var]]
                                        #c(se2[1],se1[1],se2[2],se1[2])->se.out[[var]]
    c(tr1-tr2)->out[[var]]
    c(sqrt(se1^2+se2^2))->se.out[[var]]
}
do.call("rbind",out)->tab
do.call("rbind",se.out)->se.tab
                                        #c("lightgray","darkgray","lightblue","blue")->col
par(mgp=c(2,1,0),mar=c(3,3.5,1,1))
library(gplots)
barplot2(
    t(tab),
    beside=TRUE,ylim=c(-.3,.5),
    ylab="Prevalence (post-death minus non-death)",
    names.arg=vars,
    cex.lab=.8,cex.axis=.8,cex.names=.8,
                                        #col=col,
    plot.ci=TRUE,ci.u=t(tab)+1.96*t(se.tab),ci.l=t(tab)-1.96*t(se.tab)
)
legend("topright",bty="n",fill=heat.colors(ncol(tab)),legend=0:(ncol(tab)-1),title="CESD LOO",cex=.7,ncol=2)
mtext(side=3,adj=0,"B")


## ####################################################################################
## ##conditional
## se.out<-out<-list()
## for (var in names(vars)) {
##     grep(var,names(vars))->ii
##     df[,names(vars)[-ii]]->tmp
##     rowSums(tmp,na.rm=TRUE)->df$cesd.loo
##     df[df$fpd,]->fp
##     df[!df$fpd,]->lp
##     se<-function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))
##     by(fp[[var]],fp$cesd.loo,mean,na.rm=TRUE)->tr1
##     by(lp[[var]],lp$cesd.loo,mean,na.rm=TRUE)->tr2
##     by(fp[[var]],fp$cesd.loo,se)->se1
##     by(lp[[var]],lp$cesd.loo,se)->se2
##                                         #c(tr2,tr1)->out[[var]]
##                                         #c(tr2[1],tr1[1],tr2[2],tr1[2])->out[[var]]
##                                         #c(se2[1],se1[1],se2[2],se1[2])->se.out[[var]]
##     c(tr1-tr2)->out[[var]]
##     c(sqrt(se1^2+se2^2))->se.out[[var]]
## }
## do.call("rbind",out)->tab
## do.call("rbind",se.out)->se.tab

##                                         #c("lightgray","darkgray","lightblue","blue")->col
## par(mgp=c(2,1,0),mar=c(3,3.5,1,1))
## library(gplots)
## barplot2(
##     t(tab),
##     beside=TRUE,ylim=c(-.3,.5),
##     ylab="Prevalence (post-death minus non-death)",
##     names.arg=vars,
##                                         #col=col,
##     plot.ci=TRUE,ci.u=t(tab)+1.96*t(se.tab),ci.l=t(tab)-1.96*t(se.tab)
## )
## legend("topright",bty="n",fill=heat.colors(ncol(tab)),legend=0:(ncol(tab)-1),title="CESD LOO",cex=.7)
## mtext(side=3,adj=1,"B")



## library(lfe)
## m<-list()
## for (var in names(vars)) {
##     grep(var,names(vars))->ii
##     df[,names(vars)[-ii]]->tmp
##     rowSums(tmp,na.rm=TRUE)->df$cesd.loo
##     paste(var,"~fpd+cesd.loo+age|hhidpn|0|hhidpn",sep="")->fm
##     felm(as.formula(fm),df)->m[[var]]
## }

## lapply(m,function(x) summary(x)$coef[1,])
