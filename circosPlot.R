library(circlize)
library(ComplexHeatmap)
library(gplots)



############################################################navyblue

X = read.table('vOTUs.new_tetra_inOTU-table_classified2.txt', header =T, sep = "\t")
pdf("figure 1C.pdf", width = 10, height = 10)
par(c(0,0,0,0))
circos.clear()
circos.par(start.degree = 90, gap.degree = 90)
col_lifestyle = c("virulent" = "darkgoldenrod1",  
				"temperate" =  "navyblue", 
				"zOther" = "white")

col_virus = c(
	"Arfiviricetes" = "sienna1",
	"Caudoviricetes" = "skyblue1",
	"Faserviricetes" = "cyan4",
	"Malgrandaviricetes" = "mediumvioletred",
	"Tectiliviricetes" = "olivedrab3",
	"zzzUnclassified Fragments" = "gray",
	"zSatellites" = "midnightblue")

col_family = c(
"Bifidobacteriaceae" = "orchid4",
"Bacteroidaceae" = "cornflowerblue",
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
"zOther" = "gray",
"Barnesiellaceae" = "mediumturquoise",
"Flavobacteriaceae" = "cadetblue",
"Odoribacteraceae" = "darkblue",
"Rikenellaceae" = "deepskyblue4"
)

col_phylum = c(
"Actinobacteria" = "purple4",
"Bacteroidetes" = "dodgerblue4",
"Eukarya" = "gold",
"Firmicutes" = "green4",
"Proteobacteria" = "red4",
"Verrucomicrobia" = "midnightblue",
"zOther" = "gray"
)

X <- X[order(X$host.phylum, X$host.family3, X$OTU.in),]

circos.heatmap(X$viral.taxa4.circos, col = col_virus  , track.height = 0.4, cluster = FALSE)
circos.heatmap(X$lifestyle.shiraz, col = col_lifestyle , track.height = 0.12, cluster = FALSE)
circos.heatmap(X$host.family3, col = col_family , track.height = 0.12, cluster = FALSE)
circos.heatmap(X$host.phylum, col = col_phylum , track.height = 0.12, cluster = FALSE)

dev.off()

