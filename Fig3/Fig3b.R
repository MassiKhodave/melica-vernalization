library(ggplot2)

PC1s <- list(
  AAb = -2.717806699,
  FR  = -1.533359504,
  HVb = -2.725215005,
  KOa = -2.552747662,
  KVc = -3.267275607,
  TUb = -2.683891919,
  USs = -2.436353559,
  BI  = -1.149596803,
  BOa = -1.244377396,
  Meligulata = 6.191907015,
  Mimperfecta = 4.082363159,
  Mcalifornica = 5.583553968,
  MJacquemontii = 1.215771352,
  Mciliata.516979 = 4.095789687,
  Mciliata.636632 = -0.490961526,
  Mciliata.31675  = -0.367799501
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


common_names <- intersect(names(PC1s), names(saturations))
xvals <- unlist(PC1s[common_names])
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
df <- data.frame(PC1=xvals, saturation=yrank, 
                 fill=fill, color=cols)
model <- lm(saturation ~ PC1, data = df)

x_grid <- data.frame(PC1 = seq(min(df$PC1), max(df$PC1), length.out = 200))
pred <- predict(model, newdata = x_grid, interval = "confidence")
df_pred <- cbind(x_grid, pred)

dev.new()
ggplot(df, aes(PC1, saturation)) +
  geom_smooth(method = "lm", color='black') +
  geom_line(aes(x = PC1, y = lwr), data=df_pred, 
            linetype = 2) +
  geom_line(aes(x = PC1, y = upr), data=df_pred, 
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


