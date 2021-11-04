library(AuPairWise)
library(Biobase)

# Please enter your working directory
masterdir = "/Users/amyxu/Documents/Uni/2021/BINF6111/AuPairWiseForBioconductor"

# Loading expression data
load(paste(masterdir,"/SampleData/sample_brainspan.Rdata",sep=""))

# Loading stoic pair data
load(paste(masterdir,"/SampleData/pairs.Rdata",sep=""))

# Setting output directory
out = paste(masterdir,"/Output/results",sep="")

minimalSet <- ExpressionSet(assayData=exprs)

summary = run_APW(sample.minimalSet, out, stoich.pairs, n.factors=c(0,1), n.repeats=3)
