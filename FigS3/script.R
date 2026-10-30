library(jsonlite)

nflowers <- fromJSON("data/nflowered.json")

names_all <- names(nflowers)
nfl   <- sapply(nflowers, function(x) x[1])
n_nfl <- sapply(nflowers, function(x) x[2])
n     <- nfl + n_nfl

nutans  <- names_all[1:9]
ciliata <- names_all[10:12]
others  <- c("M. eligulata", "M. imperfecta", "M. californica", "M. jacquemontii")

nfl_nutans  <- nfl[1:9]
n_nutans  <- n[1:9]
nfl_ciliata <- nfl[10:12]
n_ciliata <- n[10:12]
nfl_others  <- nfl[13:16]
n_others  <- n[13:16]

ord_nutans  <- order(-nfl_nutans)
ord_ciliata <- order(-nfl_ciliata)
ord_others  <- order(-nfl_others)

s_nutans <- nutans[ord_nutans]   
s_n_nutans <- n_nutans[ord_nutans]
s_nfl_nutans <- nfl_nutans[ord_nutans]
s_ciliata <- ciliata[ord_ciliata]
s_n_ciliata <- n_ciliata[ord_ciliata]
s_nfl_ciliata <- nfl_ciliata[ord_ciliata]
s_others <- others[ord_others]
s_n_others <- n_others[ord_others]
s_nfl_others <- nfl_others[ord_others]

bcolor <- rgb(.65, .65, .65)
nutans_color <- "lightseagreen"
ciliata_color <- "goldenrod"
others_color <- c("plum", "yellowgreen", "salmon", "darkorange")
w <- 0.7

s_n   <- c(s_n_nutans, s_n_ciliata, s_n_others)
s_nfl <- c(s_nfl_nutans, s_nfl_ciliata, s_nfl_others)
s_names <- c(s_nutans, s_ciliata, s_others)

bar_colors_bg <- rep(bcolor, length(s_n))
bar_colors_fg <- c(rep(nutans_color, length(s_n_nutans)),
                   rep(ciliata_color, length(s_n_ciliata)),
                   others_color)

dev.new()
par(mar = c(8, 4, 1, 1))

# total number
x <- barplot(s_n, col = bar_colors_bg, width = w, 
             space = (1 - w) / w, ylim = c(0, 140), yaxt = "n",
             border = NA, xaxt = "n", ylab = "", cex.axis = 1.2,
             names.arg = rep("", length(s_n)), las = 1)

# number of not flowering
barplot(s_nfl, col = bar_colors_fg, width = w, 
        space = (1 - w) / w, names.arg = rep("", length(s_n)),
        border = NA, add = TRUE, axes = FALSE)
box(which = "plot", bty = "o", lwd = 1)
axis(1, at = x, labels = FALSE, tck = -0.02)
axis(2, at = seq(0, 125, 25), cex.axis = 1.2, las = 1)

label_expr <- c(s_nutans, s_ciliata,
                sapply(s_others, function(sp) as.expression(bquote(italic(.(sp))))))

text(x, par("usr")[3] - 6, labels = s_names, srt = 45, adj = 1, xpd = TRUE, cex = 1)
