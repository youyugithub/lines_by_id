lines_by_id<-function(x,y,id,n.lines=10,min.obs=2){
  id<-as.integer(as.factor(id))
  subid<-sample(which(table(id)>3),n.lines)
  tempcolor<-hsv(h=x/max(x),s=runif(n.lines),v=1)
  for(ii in subid){
    tempsub<-id==ii
    tempx<-x[tempsub]
    tempy<-y[tempsub]
    lines(tempx,tempy,col=tempcolor[ii])
  }
}
