CalNN <- function(t1,t2) 
{ 
  t1<-as.matrix(t1)
  t2<-as.matrix(t2)
  
  nn <- rep(NA,nrow(t1))
  
  #loop through each point
  for (i in 1:nrow(t1))
  {
    #find nearest neighbour
    dt <- matrix(data=NA, nrow = nrow(t2) , ncol = ncol(t1))
    for (j in 1:ncol(t1))
    {
      dt[,j]=(t2[,j]-t1[i,j])^2
    }
    dt <- rowSums(dt)^0.5 
    nn[i] = min(dt)
  }
  nn <- mean(nn)
  return (nn)
}