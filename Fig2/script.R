library(ggplot2)
library(jsonlite)
library(dplyr)

orange_palette <- c("#fdf1e6", 
                    "#f5d0a9", 
                    "#e8a06a", 
                    "#d97a3e", 
                    "#c1522a")  

pops <- list.files("./data/", pattern = "\\.json$")
max_vals = c(350,350,350, 100,100,100,500,500,100,100,500,500,100,100,100,100)

# creating and saving figures one by one in the loop below
for (i in seq_along(pops)) {
  pop <- pops[i]
  fpath <- file.path("./data", pop)
  dat <- fromJSON(fpath)
  
  df <- data.frame(x = as.character(dat$treats), y = dat$ndays)
  df <- df[df$y != max_vals[i], ]
  
  time_order <- if (nchar(substr(pop, 1, nchar(pop) - 5)) <= 3) {
    c("0", "3", "6", "9", "12")
  } else {
    c("0", "2", "4", "6", "8")
  }
  df$x <- factor(df$x, levels = time_order)
  
  # identify single-point groups
  single_pt <- df %>%
    group_by(x) %>%
    filter(n() == 1) %>%
    ungroup()
  
  # width of the line, roughly matching violin width
  line_width <- 0.35
  
  p <- ggplot(df, aes(x = x, y = y, fill = x)) +
    geom_violin(color = "grey30", trim = FALSE, scale = "width", adjust=1.35, n=1024) +
    geom_segment(data = single_pt,
                 aes(x = as.numeric(x) - line_width, xend = as.numeric(x) + line_width,
                     y = y, yend = y),
                 color = "grey30", linewidth = 0.6, inherit.aes = FALSE) +
    geom_jitter(width = 0, color = "black", alpha = 0.6, size = 2) +
    scale_fill_manual(values = orange_palette, drop = FALSE) +
    scale_x_discrete(drop = FALSE) +
    theme_classic(base_size = 14) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_text(size = 14),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))
  
  ggsave(filename = file.path("./", paste0(pop, ".pdf")),
         plot = p, width = 5, height = 4, bg = "white")
}
