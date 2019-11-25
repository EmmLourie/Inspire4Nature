# Median Filter

#basic median filter by Ingo Schiffner 2017
#calculates median x y localizations and time (t) over a predefined window (win) in ms
#assumes x and y coordinates are given in a projected format in meters and time(t) given as ATLASTimeStamp(ms)
#returns a data.frame containing filtered x,y and time(t)
MedFilt <- function(TAG, Night, x,y,t,win) 
{
  tb = trunc((t - t[1])/win)+1
  mx <- aggregate(x ~ tb+TAG+Night, FUN = median)
  my <- aggregate(y ~ tb+TAG+Night, FUN = median)
  mt <- aggregate(t ~ tb+TAG+Night, FUN = median)
  mtf <- data.frame(TAG=mx$TAG,Night=mx$Night, X=as.numeric(mx$x),Y=as.numeric(my$y),TIME=as.numeric(mt$t)) 
  return (mtf)
}
