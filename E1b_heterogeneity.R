#run top of E1_...

###############################################################################33
##looking just at the RD
source("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/src/Z_table.R")
ff<-function(var,zz) {
    zz[[var]]->zz$cond
    #zz[[paste(var,".nexttime",sep="")]]->zz$cond.nexttime
    zz[[paste(var,".lasttime",sep="")]]->zz$cond.lasttime
    library(lfe)
    m<-list()
    ##
    zz[zz$hhidpn %in% zz$hhidpn[zz$fpd==1],]->foo
    foo[foo$delta.death<0,]->foo0
    0->foo0$second.post.death.obs
    ##
    foo[foo$delta.death<24 & foo$delta.death>0,]->foo1
    0->foo1$second.post.death.obs
    ##
    foo[foo$delta.death>24 & foo$delta.death<48,]->foo2
    1->foo2$second.post.death.obs
    ##
    data.frame(rbind(foo0,foo1,foo2))->zz
    felm(cond~dead.spouse+dead.spouse:second.post.death.obs+age+cond.lasttime|hhidpn|0|hhidpn,zz,keepModel=TRUE)->m[[1]]
    ##
    m
}
L1<-list()
for (var in vars) ff(var,df)->L1[[var]]
do.call("c",L1)->m
table.lm(m,se=TRUE)->tab

