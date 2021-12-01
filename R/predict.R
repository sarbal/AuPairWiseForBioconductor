##########################################
predict_sample <- function( X, s, n.factor, k, nS, nK, filter, dist="other", mode="post" ){

  nX = dim(X)[1]
  X.new  = shuffle(X, s, n.factor, dist, mode)
  X.r    = t(apply (X.new,1,rank, ties.method="average", na.last="keep") )
  Z.ranks = t(sapply( 1:nK, function(i) lm.studentized( X.r[k[i,1],],X.r[k[i,2],]), simplify=T ))

  k2 = t(apply(all_pairs ( cbind(sample(nX,nK*2), sample(nX,nK*2))), 1, as.numeric))
  Z.ranks.r = t(sapply( 1:nK, function(i) lm.studentized( X.r[k2[i,1],],X.r[k2[i,2],]), simplify=T ))

  ranks = matrix(unlist(Z.ranks) , ncol=nS, nrow=nK, byrow=F)
  all = ranks == nS

  scores    = rank(colSums(all), ties.method="random")
  labels    = scores*0
  labels[s] = 1

  ranks.r = matrix(unlist(Z.ranks.r) , ncol=nS, nrow=nK, byrow=F)
  all.r = ranks.r == nS

  scores.r    = rank(colSums(all.r), ties.method="random")

  scores = c(scores,scores.r)

  if( !missing(filter)){
    scores.filters =  list()

    for (i in 1:(length(filter))){
      temp = rank(colSums(all[filter[[i]],]))
      temp.r = rank(colSums(all.r[filter[[i]],]))

      scores.filters = c(scores.filters, temp, temp.r)
    }
    scores = unlist(scores.filters)
  }

  return( c(labels,scores) )

}
