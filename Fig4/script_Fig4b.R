library(VennDiagram)
library("RColorBrewer")

degs_30_FR = read.table("data/FR_T0T4.csv", header=TRUE, sep=',')
degs_40_FR = read.table("data/FR_T0T9.csv", header=TRUE, sep=',')
degs_30_BI = read.table("data/BI_T0T4.csv", header=TRUE, sep=',')
degs_40_BI = read.table("data/BI_T0T9.csv", header=TRUE, sep=',')


# NOTE: the down- and up-regulated images are merged and edited (by changing colors, adding labels, etc.) using Adobe Illustrator to get the final version of Fig4b.


# upregulated genes
# ----------------
sub1 = degs_30_FR[degs_30_FR$log2FoldChange>0,]$gene
sub2 = degs_40_FR[degs_40_FR$log2FoldChange>0,]$gene
sub3 = degs_30_BI[degs_30_BI$log2FoldChange>0,]$gene
sub4 = degs_40_BI[degs_40_BI$log2FoldChange>0,]$gene

ov12 <- intersect(sub1, sub2)
ov13 <- intersect(sub1, sub3)
ov14 <- intersect(sub1, sub4)
ov23 <- intersect(sub2, sub3)
ov24 <- intersect(sub2, sub4)
ov34 <- intersect(sub3, sub4)
ov123 <- intersect(ov12, sub3)
ov124 <- intersect(ov12, sub4)
ov134 <- intersect(ov13, sub4)
ov234 <- intersect(ov23, sub4)
ov1234 <- intersect(ov123, sub4)

colors=brewer.pal(n = 4, name = "Set3")

dev.new()
draw.quad.venn(
  area1=length(sub1), 
  area2=length(sub2), 
  area3=length(sub3), 
  area4=length(sub4), 
  n12=length(ov12), 
  n13=length(ov13), 
  n14=length(ov14), 
  n23=length(ov23), 
  n24=length(ov24), 
  n34=length(ov34), 
  n123=length(ov123), 
  n124=length(ov124), 
  n134=length(ov134), 
  n234=length(ov234), 
  n1234=length(ov1234),
  fill=colors,
)


# downregulated genes
# ----------------
sub1 = degs_30_FR[degs_30_FR$log2FoldChange<0,]$gene
sub2 = degs_40_FR[degs_40_FR$log2FoldChange<0,]$gene
sub3 = degs_30_BI[degs_30_BI$log2FoldChange<0,]$gene
sub4 = degs_40_BI[degs_40_BI$log2FoldChange<0,]$gene

ov12 <- intersect(sub1, sub2)
ov13 <- intersect(sub1, sub3)
ov14 <- intersect(sub1, sub4)
ov23 <- intersect(sub2, sub3)
ov24 <- intersect(sub2, sub4)
ov34 <- intersect(sub3, sub4)
ov123 <- intersect(ov12, sub3)
ov124 <- intersect(ov12, sub4)
ov134 <- intersect(ov13, sub4)
ov234 <- intersect(ov23, sub4)
ov1234 <- intersect(ov123, sub4)


colors=brewer.pal(n = 4, name = "Set3")

dev.new()
draw.quad.venn(
  area1=length(sub1), 
  area2=length(sub2), 
  area3=length(sub3), 
  area4=length(sub4), 
  n12=length(ov12), 
  n13=length(ov13), 
  n14=length(ov14), 
  n23=length(ov23), 
  n24=length(ov24), 
  n34=length(ov34), 
  n123=length(ov123), 
  n124=length(ov124), 
  n134=length(ov134), 
  n234=length(ov234), 
  n1234=length(ov1234),
  fill=colors,
)


