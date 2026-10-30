# sets of genes
gene_sets <- list(`TEM2-RAV1` = c("MNUTANS.r1.contig_195G00147150"),     
                  `RVR1`      = c("MNUTANS.r1.contig_66G00333200"),
                  `VRN1`      = c("MNUTANS.r1.scaffold_52G00608510"),
                  `VRT2`      = c("MNUTANS.r1.contig_14G00070220"),
                  `FUL3`      = c("MNUTANS.r1.scaffold_252G00591500"), 
                  `TOE1`      = c('MNUTANS.r1.contig_196G00158300'),
                  `MDAS56`    = c("MNUTANS.r1.contig_178G00124890"),
                  `MDAS37`    = c("MNUTANS.r1.contig_138G00054140"),    
                  `CO1-2`     = c("MNUTANS.r1.contig_12G00023520"),
                  `FTL10`     = c("MNUTANS.r1.contig_225G00213200"),
                  `SPY`       = c("MNUTANS.r1.contig_138G00048100"),
                  `PHYC`      = c("MNUTANS.r1.scaffold_52G00608770"),
                  `PPD1`      = c("MNUTANS.r1.scaffold_105G00357800"),
                  `GAI`       = c("MNUTANS.r1.scaffold_52G00615020"), 
                  `FTIPS`     = c("MNUTANS.r1.contig_181G00130170",      # FTIP5
                                  "MNUTANS.r1.scaffold_115G00372510"     # FTIP10
                                  ),
                  `SPL`       = c("MNUTANS.r1.scaffold_13G00481470",     # SPL3
                                  "MNUTANS.r1.scaffold_13G00476970",     # SPL4
                                  "MNUTANS.r1.contig_66G00333360"        # SPL16  
                                  )
                  )


# reading the normalized counts and averaging across the samples
norm_counts <- read.csv("data/norm_counts.csv", row.names = 1, check.names = FALSE)
conds <- read.table("data/samples_mornings.txt", sep = "\t", header = FALSE)

get_mean <- function(pop, timepoint) {
  cols <- conds[conds$V2 == pop & conds$V3 == timepoint, 1]
  rowMeans(norm_counts[, colnames(norm_counts) %in% cols, drop = FALSE])
}
get_sd <- function(pop, timepoint) {
  cols <- conds[conds$V2 == pop & conds$V3 == timepoint, 1]
  apply(norm_counts[, colnames(norm_counts) %in% cols, drop = FALSE], 1, sd)
}

bi_t0 <- get_mean("BI", "T0") 
bi_t3 <- get_mean("BI", "T3") 
bi_t4 <- get_mean("BI", "T4")
fr_t0 <- get_mean("FR", "T0") 
fr_t3 <- get_mean("FR", "T3") 
fr_t4 <- get_mean("FR", "T4")

sd_bi_t0 <- get_sd("BI", "T0")
sd_bi_t3 <- get_sd("BI", "T3")
sd_bi_t4 <- get_sd("BI", "T4")
sd_fr_t0 <- get_sd("FR", "T0")
sd_fr_t3 <- get_sd("FR", "T3")
sd_fr_t4 <- get_sd("FR", "T4")

mean_tpm_df <- data.frame(FR_T0 = fr_t0, 
                          FR_T3 = fr_t3, 
                          FR_T4 = fr_t4,
                          BI_T0 = bi_t0, 
                          BI_T3 = bi_t3, 
                          BI_T4 = bi_t4)

sd_tpm_df <- data.frame(FR_T0 = sd_fr_t0, 
                        FR_T3 = sd_fr_t3, 
                        FR_T4 = sd_fr_t4,
                        BI_T0 = sd_bi_t0, 
                        BI_T3 = sd_bi_t3, 
                        BI_T4 = sd_bi_t4)

# solid, dashed (if there are two), dotted (if there are three)
line_types <- c("solid", "41", "12")  

# going through all sets of genes and generate/saving the figures individually
for (set_name in names(gene_sets)) {
  
  gene <- gene_sets[[set_name]]
  
  all_y <- c()
  gene_data <- list()
  
  for (g in gene) {
    gene_profiles <- as.numeric(mean_tpm_df[g, ])
    sd_gene_profiles <- as.numeric(sd_tpm_df[g, ])
    
    y_bi <- gene_profiles[4:6]
    sd_bi <- sd_gene_profiles[4:6]
    y_fr <- gene_profiles[1:3]
    sd_fr <- sd_gene_profiles[1:3]
    
    gene_data[[g]] <- list(y_bi = y_bi, sd_bi = sd_bi, y_fr = y_fr, sd_fr = sd_fr)
    all_y <- c(all_y, y_bi - sd_bi, y_bi + sd_bi, y_fr - sd_fr, y_fr + sd_fr)
  }
  
  x_bi <- 0:2
  x_fr <- 0:2 + 0.1
  
  # only for FT make ylim little wider, otherwise set automatically
  if (set_name == "FT") {
    y_range <- c(-20, 20)
  } else {
    y_range <- range(all_y)
  }
  
  fname <- paste0("gene_profile_", set_name, ".pdf")
  pdf(fname, width = 4., height = 3.5)
  par(lend = "butt", mar = c(2, 4, 1, 1))
  
  plot(NA, xlim = c(-0.15, 2.25), ylim = y_range,
       xaxt = "n", xlab = "", ylab = "", bty = "o", 
       cex.axis = 1.2, las = 1)
  
  for (i in seq_along(gene)) {
    g <- gene[i]
    d <- gene_data[[g]]
    lty_i <- line_types[i]
    
    lines(x_bi, d$y_bi, col = "#800080", lwd = 5, lty = lty_i)
    arrows(x_bi, d$y_bi - d$sd_bi, x_bi, d$y_bi + d$sd_bi,
           angle = 90, code = 3, length = 0.05, col = "#800080", lwd = 1.8)
    points(x_bi, d$y_bi, pch = 16, col = "#800080", cex = 1.8)
    
    lines(x_fr, d$y_fr, col = "#2E6F40", lwd = 5, lty = lty_i)
    arrows(x_fr, d$y_fr - d$sd_fr, x_fr, d$y_fr + d$sd_fr,
           angle = 90, code = 3, length = 0.05, col = "#2E6F40", lwd = 1.8)
    points(x_fr, d$y_fr, pch = 16, col = "#2E6F40", cex = 1.8)
  }
  
  dev.off()
}
