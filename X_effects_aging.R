load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
vars<-c(iadlza="IADLs",adla="ADLs",conde="# Conditions",hsptim="# Hospital Visits")
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
library(lfe)
tab<-list()
for (age in c(55,65,75)) {
    x<-df[df$age>age & df$age<=(age+10),]
    out<-list()
    for (var in names(vars)) {
        yv<-x[[paste(var,'.raw',sep='')]]
        x$health<-std(yv)
        m<-felm(health~age|hhidpn|0|hhidpn,x)
        out[[var]]<-coef(m)
    }
    tab[[as.character(age)]]<-unlist(out)
}
do.call("rbind",tab)
