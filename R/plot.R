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



########################################
plot_summary_results <- function(data, out, AUROC.default=0.8){
  if( !missing(out) ){
    png( paste(out, ".predictions.png", sep="")  )
  }

  col.def = which(colnames(data$stats)==AUROC.default)

  n = dim(data$aurocs)[1]

  plot( log10(data$n.factors), data$aurocs[1,], ylim=c(0.4,1), type="l", lwd=3, col=0, xlab="Noise factor", ylab="AUROC", axes=F)
  axis(2)
  axis(1, lab=data$n.factors, at=log10(data$n.factors) )
  cols = makeTransparent(colorpanel(n, "black", "lightgrey"),150)

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

