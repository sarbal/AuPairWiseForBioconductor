# List of pairs to a matrix
make_network_from_data<- function(pairs, genes){
  n = length(genes)
  net = matrix(0, ncol=n, nrow=n)
  
  m = match( as.numeric(pairs[,1]), genes  )
  p1 = !is.na(m)
  m1 = m[p1]
  
  m = match( as.numeric(pairs[p1,2]), genes )
  p2 = !is.na(m)
  m2 = m[p2]
  
  net[cbind(m1,m2)] = 1
  net[cbind(m2,m1)] = 1
  diag(net) = 0
  
  colnames(net) = genes
  rownames(net) = genes
  return(net)
}

# List of pairs to a matrix
make_network_from_data<- function(data, listA, listB){
  nr = length(listA)
  nc = length(listB)
  
  net = matrix(0, ncol=nc, nrow=nr)
  
  m = match( (data[,1]), listA  )
  p1 = !is.na(m)
  m1 = m[p1]
  
  m = match( (data[p1,2]), listB )
  p2 = !is.na(m)
  m2 = m[p2]
  
  net[cbind(m1,m2)] = 1
  
  colnames(net) = listB
  rownames(net) = listA
  return(net)
}

##########################################
make_hist_matrix <- function(x1,y1){
  x = sort(rep(x1,2))
  y = matrix(rbind(y1, y1))
  y = c(0,y,0)
  
  return(cbind(x,y))
}
