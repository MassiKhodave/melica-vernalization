library(DESeq2)
library(WGCNA)
library(stringr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)

countsTable <- read.table("data/countMatrix.rsem.txt", header= TRUE, row.names=1)
countsTableRound <- round(countsTable)


cnames <- names(countsTableRound)
for (i in 1:length(names(countsTableRound))){
  cnames[i] = str_replace(cnames[i], "expressions.", "")
  cnames[i] = str_replace(cnames[i], ".genes.results", "")
}
names(countsTableRound) <- cnames

# using only the morning samples 
conds <- read.table("data/samples_mornings.txt", header=FALSE ,stringsAsFactors=TRUE, row.names=1)
sub_countsTableRound <- countsTableRound[, rownames(conds)]

# filtering out genes
min_count   <- 10
# removing all genes (features) that have a count of less than say 10 in more than 90% of the samples
keep <- rowSums(sub_countsTableRound >= min_count) >= ceiling(0.1*ncol(sub_countsTableRound))
filteredCountsTableRound <- sub_countsTableRound[keep,]

# MAD-based low-variance filtering
dds <- DESeqDataSetFromMatrix(filteredCountsTableRound, conds, design = ~ 1)
vsd <- vst(dds, blind = TRUE)   # blind = TRUE: design isn't used for the transformation
expr <- assay(vsd)
gene_mad <- apply(expr, 1, mad)
# keeping the top 75% most variable genes
keep_var <- gene_mad > quantile(gene_mad, 0.25)
norm.counts <- t(expr[keep_var, ])

# WGCNA
# ========================

# Choosing a set of soft-thresholding powers
power <- c(c(1:10), seq(from = 12, to = 30, by = 2))
sft <- pickSoftThreshold(norm.counts,
                         powerVector = power,
                         networkType = "signed",
                         verbose = 5)

sft.data <- sft$fitIndices 

# visualization to pick power
a1 <- ggplot(sft.data, aes(Power, SFT.R.sq, label = Power)) +
  geom_point() +
  geom_text(nudge_y = 0.1) +
  geom_hline(yintercept = 0.8, color = 'red') +
  labs(x = 'Power', y = 'Scale free topology model fit, signed R^2') +
  theme_classic()


a2 <- ggplot(sft.data, aes(Power, mean.k., label = Power)) +
  geom_point() +
  geom_text(nudge_y = 0.1) +
  labs(x = 'Power', y = 'Mean Connectivity') +
  theme_classic()

dev.new()
grid.arrange(a1, a2, nrow = 2)


# Creating the model
colnames(norm.counts) <- sub("^([^_]+_[^_]+)_.*$", "\\1", colnames(norm.counts), perl = TRUE)
soft_power <- 16
bwnet <- blockwiseModules(norm.counts,
                          maxBlockSize = 25000,
                          minModuleSize = 20, 
                          reassignThreshold=0,
                          TOMType = "signed",
                          networkType = "signed",
                          power = soft_power,
                          mergeCutHeight = 0.25,
                          numericLabels = F,
                          randomSeed = 1234,
                          verbose = 3)


# =================================
# Fig.5a
# dendogram 
dev.new()
plotDendroAndColors(bwnet$dendrograms[[1]], bwnet$colors,
                    "Module Color",
                    dendroLabels = FALSE,
                    addGuide = TRUE,
                    hang= 0.03,
                    guideHang = 0.05)


# saving eigengenes
write.csv(bwnet$MEs, 
          'data/eigengenes.csv', 
          quote=FALSE)

# ================================
# Fig.5b

colors <- c("brown", "salmon", "cyan", "greenyellow", "darkred",
            "darkturquoise", "magenta", "red", "lightgreen", "lightcyan")

eigs <- read.csv("data/eigengenes.csv", row.names = 1, check.names = FALSE)

samples_ticks <- c(rep("FR\n(T0)", 5), rep("FR\n(T4)", 4), rep("FR\n(T9)", 4),
                   rep("BI\n(T0)", 4), rep("BI\n(T4)", 3), rep("BI\n(T9)", 3))

# Rows are modules, columns are samples (same layout as diff_mods_eigs)
diff_mods_eigs <- t(as.matrix(eigs[, paste0("ME", colors)]))
rownames(diff_mods_eigs) <- colors

# Long format for ggplot
df <- data.frame(
  module = factor(rep(colors, times = ncol(diff_mods_eigs)), levels = rev(colors)),
  sample = rep(seq_len(ncol(diff_mods_eigs)), each = nrow(diff_mods_eigs)),
  value  = as.vector(diff_mods_eigs)
)

# Approximation of matplotlib's 'coolwarm' colormap
coolwarm <- c("#3B4CC0", "#DDDDDD", "#B40426")


# Heatmap size in cm (square tiles); the colorbar is given the same height
cell <- 0.6                                    # cm per tile
hm_h <- unit(cell * nrow(diff_mods_eigs), "cm")
hm_w <- unit(cell * ncol(diff_mods_eigs), "cm")

p <- ggplot(df, aes(x = sample, y = module, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = coolwarm,
    breaks  = c(-0.4, -0.2, 0, 0.2, 0.4),
    labels  = c("-0.4", "-0.2", "0", "0.2", "0.4"),
    name    = "eigengene values",
    guide   = guide_colourbar(
      title.position = "right",                # title beside the bar
      barheight      = hm_h,                   # same height as the heatmap
      barwidth       = unit(0.5, "cm"),
      frame.colour   = "black",                # colorbar border
      ticks.colour   = "black"
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title   = element_blank(),
    panel.grid   = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),  # heatmap border
    legend.title = element_text(angle = -90, hjust = 0.5)                        # rotated along the bar
  )

# Set the heatmap panel to the same height as the colorbar
g <- ggplotGrob(p)
panel <- g$layout[g$layout$name == "panel", ]
g$heights[panel$t] <- hm_h
g$widths[panel$l]  <- hm_w

dev.new()
grid.newpage()
grid.draw(g)
