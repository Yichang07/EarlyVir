
###viromeClass

col_virus = c(
	"Arfiviricetes" = "sienna1",
	"Caudoviricetes" = "skyblue1",
	"Faserviricetes" = "cyan4",
	"Malgrandaviricetes" = "mediumvioletred",
	"Tectiliviricetes" = "olivedrab3",
	"Satellites" = "midnightblue",
	"Unclassified Fragments" = "gray")

OTU <- data.frame(Contig@otu_table@.Data)
OTU <- sweep(OTU,2,colSums(OTU),`/`)


OTU <- OTU[,order(OTU[2,],OTU[3,])]
OTU <- reshape2::melt(OTU)

OTU <- OTU  %>%
  dplyr::group_by(variable, order) %>%
  dplyr::summarise(n = sum(value)) %>%
  dplyr::mutate(percentage = n / sum(n))
OTU$order <- factor(OTU$order, levels=names(col_virus))

ggplot(OTU, aes(x=variable, y=percentage, fill=order)) + 
    geom_bar(stat="identity", width = 1) +
    scale_fill_manual(values=col_virus) + 
    theme_classic() + 
	labs(x="Samples", y="Fraction") +
    theme(legend.position="top") +
    scale_x_discrete(breaks=unlist(c(OTU[1,"variable"], OTU[1400,"variable"], OTU[2800,"variable"], OTU[4200,"variable"])), labels=c("0", "200", "400", "600"))

ggsave("viromeClass.pdf", width = 20, height = 8)




###hostfamily
###hostfamily
###hostfamily
col_family.host = c(
	"Acidaminococcaceae" = "gold",
	"Bifidobacteriaceae" = "orchid4",
	"Bacteroidaceae" = "cornflowerblue",
	"Prevotellaceae" = "dodgerblue3",
	"Tannerellaceae" = "deepskyblue",
	"Clostridiaceae" = "lawngreen",
	"Lachnospiraceae" = "olivedrab4",
	"Ruminococcaceae" = "mediumaquamarine",
	"Streptococcaceae" = "green",
	"Veillonellaceae" = "seagreen4",
	"Enterobacteriaceae" = "red3",
	"Sutterellaceae" = "tomato2",
	"<1% abun" = "white",
	"zOther" = "gray95")


Contig <- tax_glom(Contig, taxrank = "hostFamily")

OTU <- data.frame(Contig@otu_table@.Data)
row.names(OTU) <- Contig@tax_table@.Data[,"hostFamily"]
OTU <- sweep(OTU,2,colSums(OTU),`/`)
host_filter <- apply(OTU, 1, mean) < 0.01


OTU <- OTU[,order(OTU["Bacteroidaceae",],OTU["Ruminococcaceae",])]
OTU$hostFamily <- Contig@tax_table@.Data[,"hostFamily"]
OTU$hostFamily[host_filter] <- "<1% abun"

OTU <- reshape2::melt(OTU)
OTU <- OTU  %>%
  dplyr::group_by(variable, hostFamily) %>%
  dplyr::summarise(n = sum(value)) %>%
  dplyr::mutate(percentage = n / sum(n))

OTU$hostFamily <- factor(OTU$hostFamily, levels=names(col_family.host))
ggplot(OTU, aes(x=variable, y=percentage, fill=hostFamily)) + 
  geom_bar(stat="identity", width = 2) +
  scale_fill_manual(values=col_family.host) + 
    theme_classic() + 
	labs(x="Samples", y="Fraction") +
    mytheme + 
    theme(legend.position="top") +
    scale_x_discrete(breaks=unlist(c(OTU[1,"variable"], OTU[2800,"variable"], OTU[5600,"variable"], OTU[8400,"variable"])), labels=c("0", "200", "400", "600"))
ggsave("hostFamily2.pdf", width = 20, height = 8)



