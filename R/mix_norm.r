mix_norm<-function(n,mu,sigma,p){
 
  x<-rep(0,n)
 for (i in 1:n) {
   u = runif(1)
  if(p<u){
    x[i]<-rnorm(1,mu[1],sigma[1])
  }
  else{
    x[i]<-rnorm(1,mu[2],sigma[2])
  }
 }
  mix_norm<-x
}


