function(vec1,vec2,xbks="auto",ybks="auto",space=1,L=0,xl="",yl="",mn="",lowx="",hix="",lowy="",hiy="",limy=0,hpoint=F,hcol="white",leg=T,return.data=F){

total=length(vec1);

if(xbks=="auto"){
xs=hist(vec1,plot=F)$breaks
bklongx=space*length(xs)
xs=hist(vec1,plot=F,breaks=bklongx)$breaks
}
else {
xs=xbks;
}

if(ybks=="auto"){
ys=hist(vec2,plot=F)$breaks
bklongy=space*length(ys)
ys=hist(vec2,plot=F,breaks=bklongy)$breaks
}
else {
ys=ybks;
}

comb=data.frame(vec1,vec2);

library(grDevices);

c=matrix(0,(length(xs)-1),(length(ys)-1))

for( i in 1:(length(xs)-1)){
for( j in 1:(length(ys)-1)){

ay=subset(comb,comb$vec1>=xs[i]);
bee=subset(ay,ay$vec1<xs[i+1]);
cee=subset(bee,bee$vec2>=ys[j]);
d=subset(cee,cee$vec2<ys[j+1])
c[i,j]=length(d$vec2)/total;


}
}

if(leg==T){
layout(matrix(c(1,2),2,2,byrow=TRUE), c(3.75,.5), TRUE); 
par(mar=c(6,5,1.5,1.5)); 

}

if(lowy==""){ lowy=ys[1] }
if(lowx==""){ lowx=xs[1] }
if(hix==""){ hix=xs[length(xs)] }
if(hiy==""){ hiy=ys[length(ys)] }

plot(vec1[1]~vec2[1],col="white",xlim=c(lowx,hix),ylim=c(lowy,hiy),xlab=xl,ylab=yl,main=mn)



for( i in 1:(length(xs)-1)){
for( j in 1:(length(ys)-1)){


den=c[i,j]/max(c);
rect(xs[i],ys[j],xs[i+1],ys[j+1],border=rgb(red=0,blue=0,green=0,alpha=L),col=(rgb(red=0,blue=0,green=0,alpha=den)))
if(den==1 && hpoint==T){ points((xs[i+1]+xs[i])/2,(ys[j+1]+ys[j])/2,pch=19,col=hcol); }


}
}

if(leg==T){
#empty plot
par(mar=c(8,0,6,4));
plot(0:10/10,0:10/10,ylim=c(0,1),xlab="",xaxt="n",cex.main=0.5,yaxt="n",ylab="", cex=0,cex.lab=1);  

#draw gradient
for(i in 1:99){ rect(0,(i-1)/100,1,(i+2)/100,lwd=0,lty=0, col=rgb(red=0,blue=0,green=0,alpha=i/100)) };
axis(side=4,at=0:10/10,cex.axis=0.8);  
text(4,0.5, srt = 90, labels = "relative density", xpd = TRUE) ;
rect(0,0.99,1,1.05,col=rgb(red=0,blue=0,green=0,alpha=1),lty=0, lwd=0)
}
if(return.data==T){ return(xs,ys,c); }
}