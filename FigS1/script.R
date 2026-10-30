# loading data
bio_pcs <- read.csv("data/bioclimatic_PCs.csv")
loads <- as.matrix(read.table("data/bioclimatic_PCA_loadings.txt"))

# colors of face and edge of the markers
fill <- list(
  AAb = "cadetblue2",
  FR  = "cadetblue2",
  HVb = "cadetblue2",
  KOa = "cadetblue2",
  KVC = "cadetblue2",
  TUb = "cadetblue2",
  USs = "cadetblue2",
  BI  = "cadetblue2",
  BOa = "cadetblue2",
  eligulata = "tan1",
  imperfecta = "plum",
  californica = "palegreen",
  jacquemontii = "lightpink1",
  `516979` = "khaki2",
  `636632` = "khaki2",
  `31675`  = "khaki2"
)

cols <- list(
  AAb = "cadetblue4",
  FR  = "cadetblue4",
  HVb = "cadetblue4",
  KOa = "cadetblue4",
  KVC = "cadetblue4",
  TUb = "cadetblue4",
  USs = "cadetblue4",
  BI  = "cadetblue4",
  BOa = "cadetblue4",
  eligulata = "tan4",
  imperfecta = "orchid4",
  californica = "palegreen4",
  jacquemontii = "indianred4",
  `516979` = "khaki4",
  `636632` = "khaki4",
  `31675`  = "khaki4"
)

dev.new()
par(tcl=-0.3, mgp = c(3, 0.5, 0))

plot(NA, xlim = c(-4.2, 6.2), ylim = c(-4.5, 6.5),
     xlab = "PC 1 (51%)", ylab = "PC 2 (24.7%)",
     cex.lab = 1.5, font.lab = 2, cex.axis = 1.2, las=1)

for (i in seq_len(nrow(bio_pcs))) {
  name <- as.character(bio_pcs$name[i])
  if (is.null(fill[[name]])) next  
  points(bio_pcs$PC_1[i], bio_pcs$PC_2[i], pch = 21,
         bg = fill[[name]], col = cols[[name]],
         cex = 2.5, lwd = 2)
}

load_scale <- 4
for (j in seq_len(nrow(loads))) {
  arrows(0, 0, loads[j,1]*load_scale, loads[j,2]*load_scale,
         col = rgb(0,0,0,0.3), length = 0.1, lwd = 2)
  text(loads[j,1]*load_scale*1.1, loads[j,2]*load_scale*1.1,
       labels = paste0("bio", j), col = rgb(0,0,0,0.6))
}
