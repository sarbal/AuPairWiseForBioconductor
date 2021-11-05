## Calculates the ROC for a given list of scores(or ranks) and the known labels
roc_score <- function(scores,labels)
{
  negatives = which(labels == 0, arr.ind=T)
  scores[negatives] <- 0

  p  = sum(scores)
  nL = length(labels)
  np = sum(labels)
  nn = nL - np

  roc  <-  (p/np - (np+1)/2)/nn

  return(roc)
}

##########################################
calc_rates <- function(scores, labels, thresh){
  tp = sum((scores >= thresh) * labels)
  fp = sum ((scores >= thresh) * !labels)
  p  = sum(labels)
  n = sum(!labels)
  #fn = p - tp
  #tn = n - fp
  fpr = (fp/n)
  tpr = (tp/p)

  #return(cbind(tp,fp, tn, fn, p, n, fpr,tpr))
  return(cbind(fpr,tpr))
}

##########################################
run_factor <- function(n.factor, n.repeats, pairs, NN, nS, X, k, nK, filter, length)
{
  tictoc::tic()
  repeats = list()
  print(paste("Noise factor: ", n.factor))
  shuff = sample(nS, n.repeats, replace=T)
  subS =  t(sapply( 1:n.repeats, function(i) sort(sample(NN, nS))))
  repeats$noise = sapply((1:n.repeats), function(i) predict_sample(X[,subS[i,]], shuff[i], n.factor, k , nS, nK, filter) , simplify=F)

  for ( j in 1:(length*2) ){
    repeats$rocs[[j]]   = sapply(1:n.repeats, function(i) get_roc_curve(repeats$noise[[i]][(1:nS)+(nS*j)] ,repeats$noise[[i]][(1:nS)+(nS*0)]), simplify=F)
    repeats$aurocs[[j]] = sapply(1:n.repeats, function(i) get_auc(repeats$rocs[[j]][[i]][,1], repeats$rocs[[j]][[i]][,2]))
    repeats$avgroc[[j]] = get_avgroc_curve( repeats$rocs[[j]], n.repeats, nS+1)
  }

  temp = matrix( unlist(repeats$aurocs), nrow=(length*2), ncol=n.repeats, byrow=T)
  rownames(temp) = array(rbind( pairs$labels, "Random") )

  # print(temp[i,], temp[i+1,])

  repeats$stats = sapply( ((1:length)*2)-1, function(i) wilcox.test( temp[i,],temp[i+1,])$p.val )
  # Write out results
  # write.table( temp, file=paste(out, ".sample.", nS, ".noise.", n.factor,".avg.aurocs", sep=""), col.names=F)
  # write.table( matrix( unlist(repeats$avgroc), nrow=nS+1, ncol=length*2, byrow=F), file=paste(out, ".sample.", nS, ".noise.", n.factor,".avg.aurocs.fpr.tpr", sep=""), col.names=F)
  # write.table( matrix(unlist(repeats$noise), nrow=n.repeats, byrow=T), file=paste(out,  ".sample.", nS, ".noise.", n.factor,".labels.scores", sep=""), col.names=F)

  # save(n.factors, n.repeats, repeats, file=paste(out,  ".sample.", nS, ".noise.", n.factor,".Rdata", sep="")) # removed 30/10 SG
  tictoc::toc()

  return (repeats)
}
