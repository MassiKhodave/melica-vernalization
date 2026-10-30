library(DESeq2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(stringr)
library(edgeR)
library(pheatmap)
library("RColorBrewer")
library(ggrepel)

# reading and organizing the count table 
countsTable <- read.table("data/countMatrix.rsem.txt", header= TRUE, row.names=1)
countsTableRound <- round(countsTable)

# shortening row names
cnames <- names(countsTableRound)
for (i in 1:length(names(countsTableRound))){
  cnames[i] = str_replace(cnames[i], "expressions.", "")
  cnames[i] = str_replace(cnames[i], ".genes.results", "")
}
names(countsTableRound) <- cnames
rownames(countsTableRound) <- sub("^(.+)_\\1$", "\\1", rownames(countsTableRound))

# filtering out too low reads
filteredCountsTableRound <- countsTableRound[rowSums(countsTableRound)>10*ncol(countsTableRound),]


# shortening column names (manually copied in samples.txt)
conds <- read.table("data/samples.txt", header=TRUE,
                    stringsAsFactors=TRUE, row.names=1)
colnames(filteredCountsTableRound) <- rownames(conds)


# heatmap for FR
# =================
conds <- read.table("data/samples_FRaFRb_T0T3T4.txt", header=TRUE ,stringsAsFactors=TRUE, row.names=1)

sub_countsTableRound <- filteredCountsTableRound[, rownames(conds)]

dds <- DESeqDataSetFromMatrix(countData = sub_countsTableRound, 
                              colData = conds, 
                              design = ~ Genotype)
normFactors <- calcNormFactors(sub_countsTableRound, method="TMM")
N <- colSums(sub_countsTableRound) #vector of library size
tmm.counts <- N*normFactors/exp(mean(log(N*normFactors)))
sizeFactors(dds) <- tmm.counts
dds <- DESeq(dds)
FR.cols <- as.data.frame(colData(dds)[c("Genotype")])

# separate T3-T4
degs_3 = read.table("data/FR_T0T4.csv", header=TRUE, sep=',')
degs_4 = read.table("data/FR_T0T9.csv", header=TRUE, sep=',')
degs <- intersect(degs_3$gene, degs_4$gene)


vsd <- vst(dds, blind=FALSE, nsub=100)
FR.mat <- assay(vsd[degs[1:min(length(degs),5000)],])
FR.mat <- FR.mat - rowMeans(FR.mat)


# heatmap for BI
# =================
conds <- read.table("data/samples_BIdBIe_T0T3T4.txt", header=TRUE ,stringsAsFactors=TRUE, row.names=1)

sub_countsTableRound <- filteredCountsTableRound[, rownames(conds)]

dds <- DESeqDataSetFromMatrix(countData = sub_countsTableRound, 
                              colData = conds, 
                              design = ~ Genotype)
normFactors <- calcNormFactors(sub_countsTableRound, method="TMM")
N <- colSums(sub_countsTableRound) #vector of library size
tmm.counts <- N*normFactors/exp(mean(log(N*normFactors)))
sizeFactors(dds) <- tmm.counts
dds <- DESeq(dds)
BI.cols <- as.data.frame(colData(dds)[c("Genotype")])

# separate T3-T4
degs_3 = read.table("data/BI_T0T4.csv", header=TRUE, sep=',')
degs_4 = read.table("data/BI_T0T9.csv", header=TRUE, sep=',')
degs <- intersect(degs_3$gene, degs_4$gene)

vsd <- vst(dds, blind=FALSE, nsub=100)
BI.mat <- assay(vsd[degs[1:min(length(degs),5000)],])
BI.mat <- BI.mat - rowMeans(BI.mat)


# global max and min
both.mats <- c(FR.mat, BI.mat)
maxv <- max(both.mats)
minv <- min(both.mats)
#symmetry
maxsv <- max(maxv, abs(minv))
minsv <- -maxsv


colors = c("#CD9600", "#00A9FF", "#FF61CC")
my.colors <- colorRampPalette(
  rev(RColorBrewer::brewer.pal(11,"RdBu")))(50)
my.breaks <- seq(minsv, maxsv, length.out = length(my.colors)+1)

# FR (saving the figure to current directory)
pheatmap(FR.mat, 
         annotation_colors=list(Genotype=c(T0=colors[1],
                                           T3=colors[2],
                                           T4=colors[3])),
         annotation_col = FR.cols, 
         show_rownames = FALSE,
         color = my.colors,
         breaks = my.breaks,
         filename = "./FR_heatmap.pdf")

# BI (saving the figure to current directory)
pheatmap(BI.mat, 
         annotation_colors=list(Genotype=c(T0=colors[1],
                                           T3=colors[2],
                                           T4=colors[3])),
         annotation_col = BI.cols, 
         show_rownames = FALSE,
         color = my.colors,
         breaks = my.breaks,
         filename = "./BI_heatmap.pdf")
