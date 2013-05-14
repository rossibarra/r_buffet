#Function to calculate formula

Mcalc<-function(x,y){
pm=0.01
e=0.3
n=100
Nf=250
N=Nf*250
Nfm=y
mg=x

output="Fst"
m=Nfm/Nf
P1=1/(2*(Nf-Nfm))
P1<-ifelse(Nf==Nfm,0,P1)
P2=1/(2*(Nfm))
P2<-ifelse(Nfm==0,0,P2)
P3=1/(2*(Nf))
P4=1/(2*(N))
a1=1/4*(1-e)*pm*(1-m)^2
a2=1/4*(1-e)*pm*m^2
a3=1/4*(1-(1-e)*pm)
a4=(3/4-mg*(1-1/4*mg))*(1-(1-e)*2*pm*m*(1-m))+(mg*(2-mg)*(1-e)*pm*m*(1-m)+(1/4)*mg^2)/(n-1)
b4=(3/4-mg*(1-1/4*mg))*(1-(1-e)^2*(1-pm*m)^2)/(n*(1-e)-1)+mg*(1-1/4*mg)/(n-1)
b5=1/4*(1-(1-e)^2*(1-pm*m)^2)/(n*(1-e)-1)

Q=b5/(b4+b5)
T0= ( (1-(a1+a2+a3+a4))/(b1+b2) +1)/(  P4*Q*(1-(a1+a2+a3+a4))+(a1*P1+a2*P2+a3*P3+a4*P4) )
T1=T0*(1-Q*P4)+1/(b4+b5)
T=T0/n+T1*(1-1/n)
Fst=(T-T0)/T
if (output=="T0") {o=T0}
if (output=="T1") {o=T1}
if (output=="T") {o=T}
if (output=="Fst") {o=Fst}
o

}

x=(5:105/500)
y=0:100/110

res<-outer(x,y,"Mcalc")
#persp(x,y,res,phi=30,theta=200,ticktype="detailed")


persp3d(x,y,res,col="blue")
x<-c(matrix(rep(x,length(x)),,length(x)))
y<-c(matrix(rep(y,length(y)),,length(y),byrow=TRUE))
m<-cbind(x,y,c(res))
m1<-xyz.coords(m)
points3d(m1,size=1,col="black")
