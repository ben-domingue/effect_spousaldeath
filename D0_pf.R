
pf<-function(df,nm,col.band="red",col.points="blue",points=TRUE,nobs.per.point=60,std=TRUE,return.loess=FALSE,
             pt.cex=.65,
             min.N=25) {
    as.numeric(df[[nm]])->z
    if (std) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->df$outcome else z->df$outcome
    df[df$delta<0,]->z1
    df[df$delta>0,]->z2
    print(nrow(z1))
    print(nrow(z2))
    list(z1,z2)->zz
    sapply(zz,nrow)->nn
    zz[nn>min.N]->zz
    tr<-NULL
    for (z in zz) {
        loess(outcome~delta,z)->mod
        #range(z$delta,na.rm=TRUE)->ran
        quantile(z$delta,c(.01,.99),na.rm=TRUE)->ran
        print(ran)
        data.frame(delta=seq(ran[1],ran[2],length.out=10000))->tmp1
        predict(mod,tmp1,se=TRUE)->yv1
        data.frame(tmp1[,1],yv1$fit,yv1$se.fit)->foo
        foo[rowSums(is.na(foo))==0,]->foo
        lines(foo[,1],foo[,2],type="l",col=col.band,lwd=2)
        abline(v=0)
        col2rgb(col.band)->cc
        polygon(c(foo[,1],rev(foo[,1])),c(foo[,2]+1.96*foo[,3],rev(foo[,2]-1.96*foo[,3])),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=55)) 
        if (points) {
            z[,c("delta","outcome")]->z
            #ecdf(z[,1])->f2
            #f2(z[,1])->qu
            #cut(qu,seq(0,1,length.out=50))->gr
            z[order(z[,1]),]->z
            floor(nrow(z)/nobs.per.point)->n.points
            rep(1:(n.points+1),length.out=nrow(z),each=nobs.per.point)->gr
            split(z,gr)->yy
            lapply(yy,colMeans,na.rm=TRUE)->xx
            do.call("rbind",xx)->xx
            data.frame(xx)->xx
            points(xx[,1],xx[,2],col=col.points,pch=19,cex=pt.cex)
            #abline(h=mean(z[,2],na.rm=TRUE),col="gray",lty=1,lwd=1.5)
        }
        range(c(tr,foo[,1]))->tr
    }
    if (return.loess) foo else tr
}
