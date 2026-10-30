library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


nutans_lon <- c(11.23838, 10.83125, 23.78680, 
                10.81445, 12.12281, 10.45180, 11.366111, 4.828,
                10.86)
nutans_lat <- c(50.70708, 59.76213, 52.68476, 
                59.67218, 60.111493, 62.57016, 46.49, 50.644,
                59.073)
nutans_codes <- c("FRa", "TUb", "BId", "AAb","KOa",
                  "KVc", "BOa", "USs", "HVb")

(nutans_sites <- data.frame(pop=nutans_codes, lon=nutans_lon, lat=nutans_lat))


 
ciliata_lon <- c(-6.525, 22.57, 33.825)
ciliata_lat <- c(32.068, 38.52, 44.4042)
ciliata_codes <- c("516979", "31675", "636632")
(ciliata_sites <- data.frame(pop=ciliata_codes, lon=ciliata_lon, lat=ciliata_lat))

  

sp_lon <- c(-118, 51.4, -120, 67)
sp_lat <- c(39.0458, 32.4279, 37, 34.5553)
sp_names <- c("imperfecta", "eligulata", "californica", "jacquemontii")
(sp_sites <- data.frame(pop=sp_names, lon=sp_lon, lat=sp_lat))


dev.new()
ggplot(data = world) +
  geom_sf(lwd = .5) +
  geom_point(data = nutans_sites, aes(x = lon, y = lat), size = 4, 
             shape = 21, fill = "cadetblue2", color="cadetblue4", stroke=1.5) +
  # imperfecta
  geom_point(data = sp_sites[1:1,], aes(x = lon, y = lat), size = 4, 
             shape = 21, fill = "plum", color="orchid4", stroke=1.5) +
  # eligulata
  geom_point(data = sp_sites[2:2,], aes(x = lon, y = lat), size = 4, 
             shape = 21, fill = "tan1", color="tan4", stroke=1.5) +
  # californica
  geom_point(data = sp_sites[3:3,], aes(x = lon, y = lat), size = 4, 
             shape = 21, fill = "palegreen", color="palegreen4", stroke=1.5) +
  
  # jacquemontii
  geom_point(data = sp_sites[4:4,], aes(x = lon, y = lat), size = 4, 
             shape = 21, fill = "lightpink1", color="indianred4", stroke=1.5) +
  geom_point(data = ciliata_sites, aes(x = lon, y = lat), size = 4, 
             shape = 21, fill = "khaki2", color="khaki4", stroke=1.5) +
  coord_sf(xlim = c(-130, 92), ylim = c(0, 70), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + theme(text = element_text(size=20))
