shuffle <- function( X, s, n.factor, dist="other", mode="post")
{
  
  nX1 = dim(X)[1]
  nS1 = dim(X)[2]
  X.r = t(apply ( X,1,rank, ties.method="random", na.last="keep") )   # method breaks down with "average" tied ranks
  
  if( n.factor != 0){
    if( dist == "gauss" ) {
      noise = rnorm(nX1)
      X.s = (X.r[,s]*((100-n.factor)/100) + nS1*noise*(n.factor/100))
      # X.s = (X.r[,s] + nS1*noise*(n.factor/100))
      # X.s = X.r[,s] + nS1*noise*(n.factor/100)
      
      
    } else {
      noise = runif(nX1, min=-n.factor/100, max=n.factor/100)
      X.s = X.r[,s] + nS1*noise
    }
    
    
    X.e = (sapply( 1:nX1, function(i) get_approx_expression(X[i,], X.r[i,], X.s[i], nS1, 1), simplify=T))  # this does not work with tied ranks
    if( mode == "post") {
      o1 = order(X.e)
      o2 = order(X[,s])
      X[o2,s] = X[o1,s]
    } else {
      X[,s] = X.e
    }
  }
  return(X)
}