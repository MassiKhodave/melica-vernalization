library(DESeq2)
library(ggplot2)
library(stringr)
library(edgeR)
library(ggrepel)


# reading the count table 
countsTable <- read.table("data/countMatrix.rsem.txt", header= TRUE, row.names=1)
countsTableRound <- round(countsTable)

# cleaning the column names
cnames <- names(countsTableRound)
for (i in 1:length(names(countsTableRound))){
  cnames[i] = str_replace(cnames[i], "expressions.", "")
  cnames[i] = str_replace(cnames[i], ".genes.results", "")
}
names(countsTableRound) <- cnames

# filtering based 
filteredCountsTableRound <- countsTableRound[rowSums(countsTableRound)>10*ncol(countsTableRound),]


#=====
conds <- read.table("data/samples.txt", header=TRUE ,stringsAsFactors=TRUE, row.names=1)

colnames(filteredCountsTableRound) <- rownames(conds)

dds <-DESeqDataSetFromMatrix(filteredCountsTableRound,
                             colData = conds, 
                             design =~ Genotype)

# TMM normalization
normFactors <- calcNormFactors(filteredCountsTableRound, method="TMM")
N <- colSums(countsTableRound) #vector of library size
tmm.counts <- N*normFactors/exp(mean(log(N*normFactors)))
sizeFactors(dds) <- tmm.counts
dds <- DESeq(dds)


vsd <- vst(dds, blind=FALSE, nsub=100)


dev.new()
data <- plotPCA(vsd, intgroup=c("Genotype"), returnData=TRUE)
percentVar <- round(100 * attr(data,"percentVar"))

lbs = sub("\\_.*", "", data$name)
lbs = substr(lbs, start=1, stop = 2)
data$Population <- as.factor(lbs)
genotypes <- as.character(data$Genotype)
genotypes[1:5] <- "T0-M"
genotypes[6:8] <- "T0-M"
genotypes[9:11] <- "T0-E"
genotypes[12] <- "T0-M"
genotypes[13:16] <- "T0-E"
genotypes[17:23] <- "T1-E"
genotypes[24:30] <- "T3-M"
genotypes[30:37] <- "T4-M"
data$Genotype <- as.factor(genotypes)

labels_expr <- list(
  "T0-M" = expression(T[0]-M),
  "T0-E"  = expression(T[0]-E),
  "T0-E"  = expression(T[0]-E)
)

pl <- ggplot(data, aes(PC1,PC2, color=Genotype, shape=Population, label=lbs)) +
  geom_point(size=4, alpha=0.85) +
  #geom_text(hjust=1.25, vjust=0) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) +
  ylab(paste0("PC2: ",percentVar[2],"% variance")) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

pl + geom_text_repel(aes(label = lbs),
                     point.padding = 0.5,
                     max.overlaps = Inf)
