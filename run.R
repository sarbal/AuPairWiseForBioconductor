library(AuPairWise)
library(Biobase)

# Please enter your working directory
masterdir = "/Users/amyxu/Documents/Uni/2021/BINF6111/AuPairWiseForBioconductor"
# masterdir = "~/GitHub/AuPairWiseForBioconductor"

# Loading expression data
load(paste(masterdir,"/SampleData/sample_brainspan.Rdata",sep=""))

# Loading stoic pair data
load(paste(masterdir,"/SampleData/pairs.Rdata",sep=""))

# Setting output directory
out = paste(masterdir,"/Output/results",sep="")

# generate an expression set with sample data
minimalExpressionSet <- ExpressionSet(assayData=exprs)

summary = run_APW(minimalExpressionSet, out, stoich.pairs, n.factors=c(0,1,5,10,20,50,100), n.repeats=3)


