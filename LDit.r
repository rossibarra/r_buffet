LDit<-function(x,n){

d=(x$sitej-x$sitei);
y<-x[order(d),]
ld=y$rsq

row_bp=unique(x$sitei)
col_bp=unique(x$sitej)
segsites=length(row_bp)+1
vlen=length(x$rsq)

Er<-function(C_,d){
length(d)
res<-((10+C_*d)/((2+C_*d)*(11+C_*d)))*(1+((3+C_*d)*(12+12*C_*d+(C_*d)^2)/(n*(2+C_*d)*(11+C_*d))))
return(res)
}

nlm<-nls(ld~Er(C_,d[order(d)]),start=list(C_=0.01))
quartz()
plot(d[order(d)],ld,cex=.5,pch=19,col="grey",xlab="distance",ylab=expression(r^2))
C_<-summary(nlm)$coefficients[1]
lines(d[order(d)],Er(C_,d[order(d)]),col="black",lwd=2)
require(lattice)
quartz()
#cm<-matrix(nrow=segsites,ncol=segsites,dimnames=list(c(row_bp,col_bp[segsites-1]),col_bp))#
cm<-matrix(nrow=segsites,ncol=segsites,dimnames=list(c(row_bp,col_bp[segsites-1]),c(row_bp[1],col_bp)))
count=0;
nrows=segsites-1;
for(r in 1:nrows){ start=r+1; for(c in start:segsites){ count=count+1; cm[r,c]=x$rsq[count] }}
levelplot(cm,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),col.regions=rev(grey.colors(10)),scales=list(cex=.4,x=list(cex=.4),tck=c(1,0),alternating=c(1)),ylab="",xlab="")
}

