FR_40 <- read.csv("data/FR_T0T4.csv")
FR_90 <- read.csv("data/FR_T0T9.csv")
BI_40 <- read.csv("data/BI_T0T4.csv")
BI_90 <- read.csv("data/BI_T0T9.csv")

n_up <- function(df) sum(df$log2FoldChange > 0)
n_dn <- function(df) sum(df$log2FoldChange < 0)

# setting the colors
FR_40_color <- rgb(11, 104, 53, maxColorValue = 255)
FR_90_color <- rgb(61, 179, 113, maxColorValue = 255)
BI_40_color <- rgb(110, 9, 135, maxColorValue = 255)
BI_90_color <- rgb(186, 85, 211, maxColorValue = 255)

W <- 0.17
ylims <- c(3100, 5600)

dev.new()
plot(NA, xlim = c(-0.5, 1.5), ylim = ylims, xaxt = "n", xlab = "",
     ylab = "DEG Count", cex.lab = 1.5, cex.axis = 1.2, bty = "o",
     yaxs="i")

axis(1, at = c(0, 1), labels = c("T0 vs. T4", "T0 vs. T9"), 
     cex.axis = 1.2, tick = FALSE)
# function for drawing hatched bars
draw_bar <- function(x, y, color, alpha, hatch = FALSE) {
  col_rgb <- col2rgb(color) / 255
  fill_col <- rgb(col_rgb[1], col_rgb[2], col_rgb[3], alpha = alpha)
  if (!hatch) {
    rect(x - W/2, ylims[1], x + W/2, y, col = fill_col, border = "black", lwd = 0.7)
  } else {
    clip(x - W/2, x + W/2, ylims[1], y)
    rect(x - W/2, ylims[1], x + W/2, y, density = 5, angle = 45,
         col = fill_col, border = NA, lwd = 10)
    do.call("clip", as.list(par("usr")))
    rect(x - W/2, ylims[1], x + W/2, y, col = NA, border = "black", lwd = 0.7)
  }
}

draw_bar(0 - 3*W/2, n_up(FR_40), FR_40_color, .74)
draw_bar(1 - 3*W/2, n_up(FR_90), FR_90_color, .6)
draw_bar(0 - W/2,   n_up(BI_40), BI_40_color, .74)
draw_bar(1 - W/2,   n_up(BI_90), BI_90_color, .6)

draw_bar(0 + W/2,   n_dn(FR_40), FR_40_color, .74, hatch = TRUE)
draw_bar(1 + W/2,   n_dn(FR_90), FR_90_color, .6,  hatch = TRUE)
draw_bar(0 + 3*W/2, n_dn(BI_40), BI_40_color, .74, hatch = TRUE)
draw_bar(1 + 3*W/2, n_dn(BI_90), BI_90_color, .6,  hatch = TRUE)

