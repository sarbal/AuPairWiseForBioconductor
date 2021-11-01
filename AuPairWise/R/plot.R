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
