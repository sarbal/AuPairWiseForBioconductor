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

# Given y and two points
get_value_x <- function( x1, x2, y1,y2, y) {
  m = (y2 - y1) / (x2 - x1 )
  x = x1 + (y - y1)/m
  return(x)
}

# Given x and two points
get_value <- function( x1, x2, y1,y2, x) {
  m = (y2 - y1) / (x2 - x1 )
  y = y1 + m *( x - x1)
  return(y)
}

##########################################
get_roc_curve <- function(scores,labels){
  #dups = rev(duplicated(rev(scores)))
  #cutoffs = c(Inf, scores[!dups])
  print(scores)
  print(labels)
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


