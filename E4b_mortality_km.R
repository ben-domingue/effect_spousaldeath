## load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
## library(survival)

## f<-function(x) {
##     unique(x$radyear)->dy
##     if (is.na(dy)) {
##         max(x$age)->a
##         0->d
##     } else {
##         1->d
##         unique(x$radyear)-unique(x$rabyear)->a
##     }
##     c(a,d,max(x$fpd))
## }

## split(df,df$hhidpn)->L
## lapply(L,f)->tmp
## data.frame(do.call("rbind",tmp))->x
## names(x)<-c("age","dead","ds")
## split(x,x$ds)->L

## plot(NULL,ylim=c(0,1),main=nm,xlim=c(50,100),xlab="Age",ylab="")
## f<-function(tmp,col) {
##     survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
##     lines(mod$time,mod$surv,lty=1,lwd=3,col=col)
##     c(mod$surv+1.96*mod$std.err,rev(mod$surv-1.96*mod$std.err))->y.ci
##     ifelse(!is.finite(y.ci),2,y.ci)->y.ci
##     col2rgb(col)->cc
##     polygon(c(mod$time,rev(mod$time)),y.ci,col=rgb(cc[1],cc[2],cc[3],max=255,alpha=90))
## }
## f(L$`0`,col="black")
## f(L$`1`,col="red")
