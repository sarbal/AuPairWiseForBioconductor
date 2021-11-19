#################################################################################################
#	AuPairWise: A tool to assess biological replicability without using replicates		#
#################################################################################################

##  Written: Sara Ballouz
##  Date: September 29th, 2014
##  Modified: April 8th 2015
## 	- Renamed
##  Modified: August 13th 2015
## 	- Updated output to include p-value calculation
##  Modified: September 2nd 2015
## 	- Updated output to return summary variable

#' Run AuPairWise
#'
#' This function runs AuPairWise.
#' It calculates the average AUROC for the expression matrix
#' with different noise factors and outputs results into an Rdata file.
#' The noise factors and the number of repeats can be changed in the function parameters.
#'
#' @param expr expression matrix the be assessed
#' @param out path for summary to be outputted
#' @param stoic.pairs co-expressed genes used to calculate AUROC
#' @param n.factors increment of noise factors
#' @param n.repeats number of repeats
#' @param dist dist
#' @param mode mode
#' @param ranked boolean of whether data is ranked
#' @param label.default label for the stoic pairs
#' @return summary of results
#' @export
run_APW <- function(exprs, out, stoich.pairs,  n.factors=c(0,1,5,10,20,50,100), n.repeats=10, dist ="other", mode ="post" , ranked=FALSE, labels.default="Stoichiometric pairs" ){

  if (missing(exprs) || is.null(exprs) || is.null(exprs@assayData[["exprs"]])) {
    stop("ERROR: expression matrix is empty or missing")
  }

  if (typeof(exprs) != "S4" || typeof(exprs@assayData[["exprs"]]) != "double") {
    stop("ERROR: expression data is in the wrong format")
  }

  if (missing(stoich.pairs) or is.null(stoich.pairs)) {
    stop("ERROR: stoich pairs is empty or missing")
  }

  if (!is.vector(n.factors)){
    stop("ERROR: n.factors need to be in vector form")
  }

  if (length(n.factors) < 2) {
    stop("ERROR: n.factors need to be have more than 1 factor")
  }

  if(n.repeats < 1) {
    stop("ERROR: n.repeats need to greater than or equal to 1")
  }

  if (out == "" || !dir.exists(out) ) {
    warning("WARNING: output path does not exist, outputting all results to current working dir")
    out = getwd()
  }

  # default factors: (0,1,2,5,10,15,20,25,50,100) 30/10 SG

  X <- exprs@assayData[["exprs"]]
  # Filter data
  # X = exprs

  # Remove samples with no expression data
  filterout = colSums(X) != 0
  X = X[,filterout]

  # Remove genes with too few counts
  X[which( log10(X) < -5 )] = 0

  # Set up variables
  NN = dim(X)[2]    		# Number of samples
  N = dim(X)[1]     		# Number of genes/transcripts
  S = 1:NN          		# Indices for samples
  nS = NN               # If subsampling, currently not implemented

  # Visualize data so far
  # plot_cummulative_counts(out, X)

  # Transform to log2
  Med <- median(X, na.rm = T)
  if (Med > 16) X <- model.fx(X, log2)

  # Transform data
  if( ranked == T){
    X = apply(X, 2 ,rank, ties.method="average", na.last="keep")
    colnames(X) = samples.list
    rownames(X) = genes.list
    out=paste(out, "ranked", sep=".")
    plot_cummulative_counts(out, X)
  }

  # Properties of expression dataset
  m.X = rowMeans(X, na.rm=T) 	# Mean expression of genes/transcripts across samples
  sd.X = apply(X,1,sd, na.rm=T)	# SD of genes/transcripts expression across samples
  # plot_expression_props(out, m.X, sd.X)

  # Update data
  genes.list = rownames(X)
  samples.list = colnames(X)

  # Adjust list of pairs
  pairs = list()
  pairs$stoich = all_pairs(stoich.pairs)
  pairs$all    = unique_all_pairs( pairs )
  pairs$labels = labels.default
  length       = length(pairs$labels)


  # Get indices of pairs
  indices =  get_indices_stoich_pairs(pairs$all, genes.list)
  indices.stoich = get_indices_stoich_pairs(pairs$stoich, genes.list)
  nK = length(indices$x1)
  genes.stoich = sort(unique(c(indices.stoich$x1, indices.stoich$x2)))
  k = cbind( indices$x1, indices$x2)

  filter = filter_pairs(pairs, indices,length)
  plot_expression_props(out, m.X, sd.X,genes.stoich)

  # Plot correlation distributions of pairs
  # plot_stoich_cors(out, length, filter, pairs, X)

  # Calculate AUROCs for each noise factor, using each pair set
  # results.all = calc_auroc(n.factors, n.repeats, pairs, NN, nS, X, k, nK, filter, length)
  results.all = list()
  r = 1
  for (n.factor in n.factors) {
    results.all[[r]]= run_factor(n.factor, n.repeats, pairs, NN, nS, X, k, nK, filter, length)
    r = r + 1
  }

  # Summary results
  summary = write_out_summary(out, results.all, length, pairs, n.factors, n.repeats)
  return( summary )
}


