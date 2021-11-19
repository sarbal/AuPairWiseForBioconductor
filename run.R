library(AuPairWise)
library(Biobase)

# Please enter your working directory
# masterdir = "/Users/amyxu/Documents/Uni/2021/BINF6111/AuPairWiseForBioconductor"
masterdir = "~/GitHub/AuPairWiseForBioconductor"

# Loading expression data
data("sampleBrainspanExpressionSet")

# Loading pair data
data("samplePairs")

# Setting output directory
out = paste(masterdir,"/Output/results",sep="")

# generate an expression set with sample data
summary = run_APW(sampleBrainspanExpressionSet, out, samplePairs, n.factors=c(0,5,20,100), n.repeats=2)
