#WF function: run as “WF(10,50,0.3)” to simulate a biallelic locus in a population of size N=50 (100 alleles) that starts at freq. p=0.3.

WF<-function(t,N,p){
  print(c("gen","number of A's","freq. A (p)"))
  for( g in 1:t ){
    A=rbinom(1,N*2,p)
    p=A/(2*N);
    print(c(g, A, p),quote=F)
  }
}

#A slightly more sophisticated function that graphs X iterations.
#WFgraf function: run as “WFgraf(10,50,0.3,10)” to simulate as above, but graphing 10 iterations. 

WFgraf<-function(t,N,p,X){
  
  freq=as.numeric();
  gens=1:t
  pnew=p
  for( g in 1:t ){
    A=rbinom(1,2*N,pnew)
    pnew=A/(N*2);
    freq[length(freq)+1]=pnew;
  }
  bob=data.frame(gens,freq)
  plot(freq~gens,type="l",ylim=c(0,1),xlab="generations",ylab="frequency of A")
  
  for( n in 2:X ){
    freq=as.numeric();
    gens=1:t
    pnew=p
    for( g in 1:t ){
      A=rbinom(1,N*2,pnew)
      pnew=A/(N*2);
      freq[length(freq)+1]=pnew;  
    }
    bob=data.frame(gens,freq)
    lines(freq~gens,type="l",ylim=c(0,1),col=colors()[runif(1,1,657)])
  }
}
