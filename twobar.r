twobar<-function(a,b,space=2,breaks="auto",AL=0.5,nameA="A",nameB="B",xl="",yl="",mn="",legx="topright"){

        #a and b are two vectors
        
        # space is the fold increase in breaks for histograms. 
        # e.g. if you want twice has many breaks as R will pick, space=2
     
        #legx can be any of the options for legend, xl, yl, and mn are titles for the x axis, y axis, and main
     
        #nameA and nameB are the names you want to appear in the legend
     
        # AL is the alpha setting for transparency
        # set AL to 1 for x11, and AL to <1 for transparency on a pdf 

        #breaks is automatically calculated if left as default, but user-specified breaks can be added instead

	#BEGIN CODE:
		
        # figure out which breaks to use
        aprime=a;
        bprime=b;
        if(length(a)>length(b)){ bprime=b; aprime=sample(a,length(b),replace=F) }
        if(length(a)<length(b)){ aprime=a; bprime=sample(b,length(a),replace=F) }
          
        if(breaks=="auto"){
             bks=hist(c(aprime,bprime),plot=F)$breaks
             bklong=space*length(bks)
             bks=hist(c(aprime,bprime),plot=F,breaks=bklong)$breaks
        }
        else{
             bks=breaks
        }
              
        h1=hist(a,breaks=bks,plot=F)
        h2=hist(b,breaks=bks,plot=F)

        #weight so sums to 1
        w1=sum(h1$density)
        w2=sum(h2$density)

        #figure out height to set
        d1=max(h1$density)/w1
        d2=max(h2$density)/w2
        d=max(d1,d2)

        #plot reg
        par(lwd=1)
        x=barplot(h1$density/w1,col="white",border=par("fg"),ylim=c(0,d),width=.8,space=.2,ylab=yl,xlab=xl,main=mn)
        y=c(x,x[length(x)]+.96)-.5
        axis(side=1,at=y,labels=h1$breaks)
        par(lwd=2,lty=1)
        barplot(h2$density/w2,col=rgb(red=.25,blue=.25,green=.25,alpha=AL),border=rgb(red=.25,blue=.25,green=.25,alpha=AL),ylim=c(0,d/w2),width=.8,space=.2,add=T)

        #draw legend
        legend(legx,legend=c(nameA,nameB),fill=c("white",rgb(red=.25,blue=.25,green=.25,alpha=.5)),cex=1.5,bty="n")

}

