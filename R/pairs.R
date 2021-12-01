##########################################
all_pairs <- function(list){
  list = list[(list[,1] != list[,2]),]
  list = sort(c( paste( list[,1],list[,2]), paste( list[,2], list[,1])))
  list = matrix ( unlist(strsplit(as.character(list), " ") ), ncol=2, nrow= length(list), byrow=T )
  list = unique( t(apply( list, 1, sort) ))
  return(list)
}

##########################################
unique_all_pairs <- function(pairs){
  list = pairs[[1]]
  if( length(pairs) < 2 ) { return(list) }
  
  for( i in 2:length(pairs)){
    list = rbind(list,pairs[[i]])
  }
  list = unique(list)
  return(list)
}

##########################################
filter_pairs <- function(pairs, indices, length ){
  
  temp = pairs$all[indices$p1,][indices$p2,]
  indexed = paste(temp[,1], temp[,2])
  
  filter = list()
  for( i in 1:length){
    m = match(indexed, paste(pairs[[i]][,1], pairs[[i]][,2]))
    filt = !is.na(m)
    filter[[i]] = filt
  }
  return(filter)
}
