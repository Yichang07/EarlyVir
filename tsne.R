
col_family.host = c(
	"Bifidobacteriaceae" = "orchid4",
	"Bacteroidaceae" = "cornflowerblue",
	"Barnesiellaceae" = "mediumturquoise",
	"Flavobacteriaceae" = "cadetblue",
	"Odoribacteraceae" = "darkblue",
	"Rikenellaceae" = "deepskyblue4",
	"Prevotellaceae" = "dodgerblue3",
	"Tannerellaceae" = "deepskyblue",
	"Eukarya" = "gold",
	"Clostridiaceae" = "lawngreen",
	"Erysipelotrichaceae" = "yellowgreen",
	"Lachnospiraceae" = "olivedrab4",
	"Paenibacillaceae" = "greenyellow",
	"Peptostreptococcaceae" = "palegreen",
	"Ruminococcaceae" = "mediumaquamarine",
	"Streptococcaceae" = "green",
	"Veillonellaceae" = "seagreen4",
	"Enterobacteriaceae" = "red3",
	"Sutterellaceae" = "tomato2",
	"Akkermansiaceae" = "orchid3",
	"zOther" = "gray")


col_virus = c(
	"Arfiviricetes" = "sienna1",
	"Caudoviricetes" = "skyblue1",
	"Faserviricetes" = "cyan4",
	"Malgrandaviricetes" = "mediumvioletred",
	"Tectiliviricetes" = "olivedrab3",
	"Unclassified Fragments" = "gray",
	"Satellites" = "midnightblue")


GMs0$viral.taxa4.MGE <- factor(GMs0$viral.taxa4.MGE, levels=rev(c("Unclassified Fragments", "Satellites", "Arfiviricetes", "Caudoviricetes", "Faserviricetes", "Malgrandaviricetes", "Tectiliviricetes")))

p2 <-ggplot(GMs0, aes(x = tSNE1, y = tSNE2, colour=host.family4)) + 
	scale_color_manual(values = col_family.host) + 
	theme_classic() + geom_point(size = 0.5, alpha=0.5) + 
	xlim(-43, 45) + ylim(-42, 40) + theme_classic() + mytheme + theme(legend.position='none')
ggsave("host_family.pdf", p2, limitsize=F, width = 5, height = 4.5, units = "in")


GMs0<- GMs0[rev(order(GMs0$viral.taxa4.MGE)),]
p1 <-ggplot(GMs0, aes(x = tSNE1, y = tSNE2, colour=viral.taxa4.MGE)) + 
	scale_color_manual(values = col_virus) + 
	theme_classic() + geom_point(size = 0.5, alpha=0.5) + 
	xlim(-43, 45) + ylim(-42, 40) + theme_classic() + mytheme + theme(legend.position='none')
ggsave("Class.pdf", p1, limitsize=F, width = 5, height = 4.5, units = "in")


