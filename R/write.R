# Writes all results in output files in given path
write_out_summary <- function(out, results.all, length, pairs, n.factors, n.repeats, AUROC.default = 0.8, nn=100){

  # Summary results
  aurocs = matrix(0, nrow=length*2, ncol=length(n.factors), dimnames=list( array(rbind( pairs$labels, "Random") ) ,n.factors))
  aurocs.sd = aurocs
  aurocs.se = aurocs
  pvals = matrix( 0, nrow=length, ncol=length(n.factors), dimnames=list( pairs$labels,n.factors) )

  i = 1
  for( n.factor in n.factors){
    data = matrix(unlist(results.all[[i]]$aurocs), ncol=n.repeats , byrow=T )
    aurocs[,i] = rowMeans( data )
    aurocs.sd[,i] = apply(data, 1,sd)
    aurocs.se[,i] = aurocs.sd[,i]/sqrt(dim(data)[2])
    pvals[,i] = sapply( (1:length)*2 -1, function(j) wilcox.test(data[j,], data[j+1,], exact=FALSE)$p.val )
    i = i + 1

  }

  data = list()
  data$aurocs = aurocs
  data$aurocs.se = aurocs.se
  data$aurocs.sd = aurocs.sd
  data$n.factors = n.factors
  data$pvals = pvals

  write.table (data$aurocs, file = paste(out,".avg.aurocs.summary", sep=""))
  write.table (data$aurocs.sd, file = paste(out,".avg.aurocs.summary", sep=""), append=T, col.names=F, row.names=paste(array(rbind( pairs$labels, "Random") ), "- SD"))
  write.table (data$aurocs.se, file = paste(out,".avg.aurocs.summary", sep=""), append=T, col.names=F, row.names=paste(array(rbind( pairs$labels, "Random") ), "- SE"))


  # Predictions
  stats = matrix(NA, ncol=nn, nrow=length*2, dimnames=list( array(rbind( pairs$labels, "Random"), 1:nn /nn )))
  range = n.factors[-1]


  for ( i in 1:(length*2) ){
    temp = data$aurocs[i,][-1]
    temp.se = data$aurocs.se[i,][-1]
    temp.sd = data$aurocs.sd[i,][-1]
    fit = glm( temp ~ range, family=binomial) #  Default family
    predictions = predict(fit, data.frame(range=1:100),type = "response")
    for (j in 1:10){
      AUROC = j/10
      max.pred = max(which(predictions <= AUROC))
      min.pred = min(which(predictions > AUROC))
      n.pred = get_value_x( max.pred, min.pred, predictions[max.pred], predictions[min.pred], AUROC)
      stats[i,j] = n.pred
    }

  }

  data$stats = stats
  # write.table (data$stats, file = paste(out,".avg.aurocs.predictions", sep="")  )

  save(data, results.all, file=paste(out,".avg.aurocs.Rdata", sep="") )
  return(data)
}
