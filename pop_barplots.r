#some fake data
bp=1; for(i in 2:3000 ){ bp[i]=bp[i-1]+rexp(1,.00002) }
ted=matrix(nrow=20,ncol=3000)
for(i in 1:5){
  temp=runif(1);
  for(j in 2:3000){
    temp[j]=temp[j-1]+runif(1)/11-runif(1)/10; 
    if(temp[j]<=0){ temp[j]=0} 
    if(temp[j]>=1){temp[j]=1}
  } 
  ted[i*2-1,]=temp; ted[i*2,]=1-temp;
}
for(i in 6:10){
  temp=runif(1);
  for(j in 2:3000){
    temp[j]=temp[j-1]+runif(1)/10-runif(1)/11; 
    if(temp[j]<=0){ temp[j]=0} 
    if(temp[j]>=1){temp[j]=1}
  } 
  ted[i*2-1,]=temp; ted[i*2,]=1-temp;
}
pos=bp;

#make the bar plots
for(i in 2:3000){ pos[i]=bp[i]-bp[i-1]; }
barplot(height=ted,width=pos,beside=F,border="NA",yaxt="n",space=0,col=rep(c("green","blue"),5))
barplot(height=ted,beside=F,border="NA",yaxt="n",space=0,col=rep(c("green","blue"),5))

axis(1,at=c(0:15*200),labels=c(0:15*10 ))
mtext(side=1,"Mb",line=3,cex=1.5)

