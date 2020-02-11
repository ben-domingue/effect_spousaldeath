load(file="/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/df.Rdata")
df[df$fpd==1,]->tmp

df<-df[df$hhidpn %in% tmp$hhidpn,]
vars<-c(cesd="CESD",conde.raw="# Conditions",iadlza.raw="IADLs",adla.raw="ADLs",hsptim.raw="# Hospital Visits")
N<-numeric()
for (var in names(vars)) {
    N[var]<-sum(is.na(df[[var]]))/nrow(df)
}

