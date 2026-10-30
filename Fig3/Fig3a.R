library(ggplot2)

lats <- list(
  AAb = 59.67218,
  FR  = 50.70708,
  HVb = 59.420,
  KOa = 60.111493,
  KVc = 62.57016,
  TUb = 59.76213,
  USs = 50.644,
  BI  = 52.68476,
  BOa = 46.49,
  Meligulata = 32.4279,
  Mimperfecta = 39.0458,
  Mcalifornica = 37,
  MJacquemontii = 34.5553,
  Mciliata.516979 = 32.068,
  Mciliata.636632 = 44.4042,
  Mciliata.31675  = 38.52
)

saturations <- list(
  AAb = 12,
  FR  = 12,
  HVb = 12,
  KOa = 6,
  KVc = 9,
  TUb = 3,
  USs = 9,
  BI  = 0,
  BOa = 0,
  Meligulata = 8,
  Mimperfecta = 0,
  Mcalifornica = 0,
  MJacquemontii = 0,
  Mciliata.516979 = 0,
  Mciliata.636632 = 0,
  Mciliata.31675  = 0
)

fill <- list(
  AAb = "cadetblue2",
  FR  = "cadetblue2",
  HVb = "cadetblue2",
  KOa = "cadetblue2",
  KVc = "cadetblue2",
  TUb = "cadetblue2",
  USs = "cadetblue2",
  BI  = "cadetblue2",
  BOa = "cadetblue2",
  Meligulata = "tan1",
  Mimperfecta = "plum",
  Mcalifornica = "palegreen",
  MJacquemontii = "lightpink1",
  Mciliata.516979 = "khaki2",
  Mciliata.636632 = "khaki2",
  Mciliata.31675  = "khaki2"
)

cols <- list(
  AAb = "cadetblue4",
  FR  = "cadetblue4",
  HVb = "cadetblue4",
  KOa = "cadetblue4",
  KVc = "cadetblue4",
  TUb = "cadetblue4",
  USs = "cadetblue4",
  BI  = "cadetblue4",
  BOa = "cadetblue4",
  Meligulata = "tan4",
  Mimperfecta = "orchid4",
  Mcalifornica = "palegreen4",
  MJacquemontii = "indianred4",
  Mciliata.516979 = "khaki4",
  Mciliata.636632 = "khaki4",
  Mciliata.31675  = "khaki4"
)


common_names <- intersect(names(lats), names(saturations))
xvals <- unlist(lats[common_names])
yvals <- unlist(saturations[common_names])
fill <- unlist(fill[common_names])
cols <- unlist(cols[common_names])

sptest <- cor.test(xvals, yvals, method = "spearman")
rho.sp <- round(sptest$estimate, 4)
p.sp <- round(sptest$p.value, 4)

print(paste("Spearman coeff.:", rho.sp))
print(paste("Spearman p-value:",p.sp))

# creating confidence interval
xrank <- rank(xvals)
yrank <- rank(yvals)
df <- data.frame(lat=xvals, saturation=yrank, 
                 fill=fill, color=cols)
model <- lm(saturation ~ lat, data = df)

x_grid <- data.frame(lat = seq(min(df$lat), max(df$lat), length.out = 200))
pred <- predict(model, newdata = x_grid, interval = "confidence")
df_pred <- cbind(x_grid, pred)

dev.new()
ggplot(df, aes(lat, saturation)) +
  geom_smooth(method = "lm", color='black') +
  geom_line(aes(x = lat, y = lwr), data=df_pred, 
            linetype = 2) +
  geom_line(aes(x = lat, y = upr), data=df_pred, 
            linetype = 2) +
  geom_point(aes(fill=fill, color=color), shape=21, size=5, stroke=1.5) +
  theme(panel.background = element_rect(fill = "white", 
                                        colour = "grey50"),
        panel.border     = element_rect(fill = NA, 
                                        color = "black", linetype = "solid"))+
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(breaks = c(0,2, 4, 6, 8,10,12,14,16)) +
  labs(x = "Latitude", y="Saturation Ranking") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))
  

