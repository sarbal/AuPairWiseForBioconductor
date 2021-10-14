##########################################
plot_samples_noise_aurocs <- function( data, S )
{
  range = data$n.factors[-1]     # remove 0 factor
  cols = gplots::colorpanel(nS, "red", "purple", "blue")

  plot(log10(range), data$aurocs[-1] ,ylim=c(0.4,1), type="l", lwd=3, col=0, xlab="Noise factor", ylab="AUROC", axes=F)
  axis(2)
  axis(1, lab=range, at=log10(range) )

  for (i.s in 1:length(S)) {
    fit = glm( data$aurocs.s.comb[-1,i.s] ~ range)
    points( log10(range), data$aurocs.s.comb[-1,i.s], pch=19, col=cols[i.s])
    segments( log10(range), data$aurocs.s.comb[-1,i.s]-data$aurocs.s.comb.se[-1,i.s], log10(range), data$aurocs.s.comb[-1,i.s]+data$aurocs.s.comb.se[-1,i.s], col=makeTransparent(cols[i.s]))
    lines( log10(range), fit$fitted, lwd=2, col=makeTransparent(cols[i.s]))
  }
}


##########################################
plot_cummulative_counts <- function(out, X)
{

  xN    = dim(X)[2]
  Xtemp = apply(X, 2, sort, decreasing=T )
  XSum  = apply(Xtemp, 2, cumsum)
  col   = gplots::colorpanel(xN, "red","purple","blue")
  maxx = max(XSum)

  png( paste( out, ".all.csum", ".png", sep=""))
  plot(XSum[,1], col=0, ylim =c(0, maxx), xlab="Cummulative gene count", ylab="Expression levels covered", bty="n")
  for (i in 1:xN){
    lines(XSum[,i], col=col[i])
  }
  dev.off()

  png( paste( out, ".csum", ".png", sep=""))
  plot(XSum[,1]/max(XSum[,1]), col=0, ylim =c(0, 1), xlab="Cummulative gene count", ylab="Proportion of expression levels covered", bty="n")
  for (i in 1:xN){
    lines(XSum[,i]/max(XSum[,i]), col=col[i])
  }
  abline(h=0.5,lty=2, col="lightgrey")
  abline(h=0.75,lty=3, col="lightgrey")
  abline(h=0.95,lty=2, col="lightgrey")
  dev.off()


}

##########################################
plot_expression_props <- function( out, m.X, sd.X, genes.stoich)
{
  if( missing(genes.stoich)){
    png( paste( out, ".exprs.png", sep=""))
  } else {
    png( paste( out, ".exprs.pairs.png", sep=""))
  }

  plot( (m.X), (sd.X), pch=19, cex=0.1, col=makeTransparent(1), xlab="Average expression", ylab="SD", bty="n")

  if( !missing(genes.stoich)){
    points( m.X[genes.stoich], sd.X[genes.stoich], pch=19, col=makeTransparent("red"))
  }
  dev.off()

}

##########################################
plot_expression_density <- function( out, X, xlab)
{
  png( paste( out, ".density.png", sep="") )
  plot_density("", xlab, 0.1, X)
  dev.off()
}

##########################################
plot_density <- function(labels, xlab, b, a)
{
  n = dim(a)[2]

  l = density( (unlist(a)), bw=b, na.rm=T)
  all = cbind(l$x, l$y)

  for (i in 1:n){
    l = density((a[,i]), bw=b)
    all = cbind(all, l$x, l$y)
  }

  # Setup plot variables
  odd = (1:(dim(all)[2]/2))*2 - 1
  even = (1:(dim(all)[2]/2))*2
  ymax=max( all[,even])
  ymin=min( all[,even])
  xmin=min( all[,odd])
  xmax=max( all[,odd])

  cols = gplots::colorpanel(n, "red", "blue", "green")

  legcols = list()

  plot(-xmax,-ymax, type="l", lwd=2, col="grey", ylim=c(ymin,ymax),xlim=c(xmin,xmax), ylab="Density", xlab=xlab)
  j = 1
  for (i in odd){
    lines( all[,c(i,i+1)], lwd=0.1, col=cols[j])
    legcols = append(legcols, cols[j])
    j = j + 1
  }
}

##########################################
plot_stoich_cors <- function(out, length, filter, pairs, X){
  cols = gplots::colorpanel(length, "red", "purple", "blue")
  stats = list()
  h.all = list()
  m = "spearman"
  cors = diag( cor( t(X[indices$x1,]), t(X[indices$x2,]), method=m))
  r1 = sample(indices$x1)
  r2 = sample(indices$x2)

  cors.r = diag(cor( t(X[r1,]), t(X[r2,]), method=m))

  # Total
  h.total = get_density(hist(cors.r, plot=F))
  m.total = mean(cors.r, na.rm=T)
  y.max = max(h.total[,2])

  for( i in 1:length ){
    h = get_density( hist(cors[filter[[i]]], breaks=50, plot=F ))
    m = mean(cors[filter[[i]]], na.rm=T)
    stats[[i]] = m
    h.all[[i]] = h
    if( y.max < max(h[,2]) ){ y.max=max(h[,2]) }
  }

  png( paste( out, ".hist.cors.pairs.png", sep=""))
  plot(h.total, type="l", xlim=c(-1,1), ylim=c(0,y.max), col=makeTransparent(1,5), lwd=4, xlab="Correlation coefficients", ylab="Density" )
  abline(v = m.total, lty=2, col=makeTransparent(1,5))
  for( i in 1:length ){
    lines( h.all[[i]], col=cols[i], lwd=2)
    abline(v = stats[[i]], lty=2, col=makeTransparent(cols[i]) )
  }
  legend( "topleft", legend=pairs$labels, col=cols, lwd=2, bty="n")

  dev.off()
}

##########################################
plot_summary_results <- function(data, out, AUROC.default=0.8){
  if( !missing(out) ){
    png( paste(out, ".predictions.png", sep="")  )
  }

  col.def = which(colnames(data$stats)==AUROC.default)

  n = dim(data$aurocs)[1]

  plot( log10(data$n.factors), data$aurocs[1,], ylim=c(0.4,1), type="l", lwd=3, col=0, xlab="Noise factor", ylab="AUROC", axes=F)
  axis(2)
  axis(1, lab=data$n.factors, at=log10(data$n.factors) )
  cols = makeTransparent(gplots::colorpanel(n, "black", "lightgrey"),150)

  for( i in 1:n){
    lines( log10(data$n.factors), data$aurocs[i,], lwd=3, col=cols[i])
    segments(log10(data$n.factors), data$aurocs[i,]-data$aurocs.se[i,], log10(data$n.factors),data$aurocs[i,]+data$aurocs.se[i,],col=cols[i])
    points( log10(data$n.factors), data$aurocs[i,], pch=19,col=cols[i])
  }

  abline( h = AUROC.default, lwd=3, lty=3, col=2)
  abline( v = log10(data$stats[1,col.def]), lwd=3, lty=3, col=2)
  text(log10(max(data$n.factors)/2), 0.5, paste("For an AUROC of:", AUROC.default, "\nthe estimated noise factor is:\n", round(data$stats[1,col.def],1), "%") )
  legend("topleft", legend=rownames(data$aurocs), col=cols, lwd=3, pch=19)

  if( !missing(out) ){
    dev.off()
  }
}

# Calculates and plots the ROC for a given list of scores(or ranks) and the known labels
plot_roc2 <- function(scores,labels) {
  o = order(scores, decreasing=T)

  h1 = labels[o]
  h2 = !labels[o]

  auroc  <-  roc_score(scores, labels)
  tpr = c(0,cumsum(h1)/sum(h1))
  fpr = c(0,cumsum(h2)/sum(h2))
  plot(fpr,tpr, xlab="FPR", ylab="TPR", xlim=c(0,1), ylim=c(0,1), type="l" )
  lines( c(0,1), c(0,1), col="grey")
  text(labels=auroc, 0.5, 1 )
  pval = wilcox.test(scores[labels], scores[!labels])
  return(cbind(fpr,tpr))
}

# Calculates and plots the ROC for a given list of scores(or ranks) and the known labels
plot_roc <- function(scores,labels, file)
{
  rocs = get_roc_curve(scores, labels)
  auroc  <-  roc_score(scores, labels)
  tpr = rocs$tpr
  fpr = rocs$fpr
  png(file)
  plot(fpr,tpr,  type="l", lwd=2, xlim=c(0,1), ylim=c(0,1),xlab="FPR", ylab="TPR")
  lines( c(0,1), c(0,1), col="grey")
  dev.off()
  pval = wilcox.test(scores[labels], scores[!labels])
  return(cbind(fpr,tpr))
}
