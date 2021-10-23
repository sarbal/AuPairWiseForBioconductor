##########################################
get_approx_expression <- function( temp.x, temp.x.r, temp.x.s, max.r, min.r)
{
  if( temp.x.s < min.r ){
    x0 = 1
    x1 = 2
    xn = min.r
  } else if( temp.x.s > max.r ){
    x0 = max.r-1
    x1 = max.r
    xn = max.r

  }  else {
    x0 = floor(temp.x.s)
    x1 = ceiling(temp.x.s)
    xn = temp.x.s
  }
  new.x = get_value(  x0, x1, temp.x[which( x0 ==  temp.x.r )] ,temp.x[which( x1 ==  temp.x.r )], xn)
  return (new.x)
}

##########################################
get_replicability <- function( data, AUROC ){
  range = data$n.factors[-1]   # remove 0 factor
  fit = glm( data$aurocs[-1] ~ range)
  predictions = predict(fit, data.frame(range=1:100),type = "response")
  max.pred = max(which(predictions <= AUROC))
  min.pred = min(which(predictions >= AUROC))
  n.pred = get_value_x( max.pred, min.pred, predictions[max.pred], predictions[min.pred], AUROC)

  plot(log10(1:100), predictions, ylim=c(0.4,1), type="l", lwd=3, col="lightgrey", xlab="Noise factor", ylab="AUROC", axes=F)
  axis(2)
  axis(1, lab=data$n.factors, at=log10(data$n.factors) )
  abline(h= data$aurocs[1], lty=2, col="lightgrey", lwd=2)
  points( log10(data$n.factors), data$aurocs, pch=19)
  abline( h = AUROC, lwd=3, lty=3, col=2)
  abline( v = log10(n.pred), lwd=3, lty=3, col=2)
  segments( log10(data$n.factors), data$aurocs-data$aurocs.se, log10(data$n.factors), data$aurocs+data$aurocs.se)

  return(n.pred)

}

##########################################
get_indices_stoich_pairs <- function( stoich.pairs, genes.list )
{
  if( missing(stoich.pairs) ){ }
  if( missing(genes.list)  ) { }

  m = match( stoich.pairs[,1], genes.list )
  p1 = !is.na(m)
  x1 = m[p1]

  m = match( stoich.pairs[p1,2], genes.list )
  p2 = !is.na(m)
  x2 = m[p2]

  m = match( stoich.pairs[p1,1][p2], genes.list )
  p3 = !is.na(m)
  x1 = m[p3]

  indices = list()
  indices$p1 = p1
  indices$p2 = p2
  indices$x1 = x1
  indices$x2 = x2

  return (indices)
}

##########################################
get_gene_corrs <- function(X, m )
{
  gene.corr = cor(t(X), method=m)
  #na = is.na(gene.corr)
  #gene.corr[na] = 0
  #diag(gene.corr) = 1
  return(gene.corr)
}

# Given x and two points
get_value <- function( x1, x2, y1,y2, x) {
  m = (y2 - y1) / (x2 - x1 )
  y = y1 + m *( x - x1)
  return(y)
}

# Given y and two points
get_value_x <- function( x1, x2, y1,y2, y) {
  m = (y2 - y1) / (x2 - x1 )
  x = x1 + (y - y1)/m
  return(x)
}

## Formats the density distribution from the histogram function
get_density <- function(hist)
{
  x = sort(rep(hist$breaks,2))
  y = matrix(rbind(hist$density, hist$density))
  y = c(0,y,0)

  return(cbind(x,y))
}


## Formats the counts distribution from the histogram function
get_counts <- function(hist)
{
  x = sort(rep(hist$breaks,2))
  y = matrix(rbind(hist$counts, hist$counts))
  y = c(0,y,0)

  return(cbind(x,y))
}

##########################################
get_roc_curve <- function(scores,labels){
  #dups = rev(duplicated(rev(scores)))
  #cutoffs = c(Inf, scores[!dups])
  o = order(scores, decreasing=T)
  cutoffs = c(Inf, length(scores):1)
  roc = sapply( (1:length(cutoffs)), function(i) calc_rates(scores[o], labels[o],cutoffs[i]) , simplify=F)
  rocs = matrix(unlist(roc), byrow=T, ncol=2)
  colnames(rocs) = c("fpr", "tpr")
  return(rocs)
}

##########################################
get_avgroc_curve <- function( rocs,n.repeats, n){

  sum = matrix(0, ncol=2, nrow = n)
  colnames(sum) = c("tpr", "fpr")
  for( i in 1:n.repeats) {
    sum = rocs[[i]] + sum
  }
  sum = sum/n.repeats
  return(sum)
}


##########################################
get_auc <- function(x,y){
  o = order(x)
  auc <- sum(diff(x[o])*zoo::rollmean(y[o],2))
  return (auc)
}

##########################################
get_expression_levels <- function( indices, X, fx){

  if( missing(fx) ){
    xp1 = X[indices$x1,]
    xp2 = X[indices$x2,]
  } else {
    xp1 = model.fx(X[indices$x1,], fx)     # protein 1, in rep X
    xp2 = model.fx(X[indices$x2,], fx)     # protein 2, in rep X
  }
  results = list()
  results$xp1 = xp1
  results$xp2 = xp2
  return(results)
}
