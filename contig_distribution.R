col_family.host = c(
	"Bifidobacteriaceae" = "orchid4",
	"Bacteroidaceae" = "cornflowerblue",
	"Barnesiellaceae" = "mediumturquoise",
	"Flavobacteriaceae" = "cadetblue",
	"Odoribacteraceae" = "darkblue",
	"Rikenellaceae" = "deepskyblue4",
	"Prevotellaceae" = "dodgerblue3",
	"Tannerellaceae" = "deepskyblue",
	"Eukarya" = "golden",
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
	"Satellites" = "midnightblue",
	"Unclassified Fragments" = "gray")



Contig_10 = filter_taxa(Contig, function(x) sum(x > 0) > (0.1*length(x)), TRUE)
Contig_20 = filter_taxa(Contig, function(x) sum(x > 0) > (0.2*length(x)), TRUE)
Contig_30 = filter_taxa(Contig, function(x) sum(x > 0) > (0.3*length(x)), TRUE)
Contig_40 = filter_taxa(Contig, function(x) sum(x > 0) > (0.4*length(x)), TRUE)
Contig_50 = filter_taxa(Contig, function(x) sum(x > 0) > (0.5*length(x)), TRUE)




dataset <- c("Contig", "Contig_10", "Contig_20", "Contig_30", "Contig_40", "Contig_50")
hostFamily <- c()
for(i in dataset){
	tmp <- get(i)
	tmp_hostFamily <- data.frame(table(tmp@tax_table[,"hostFamily"]))
	row.names(tmp_hostFamily) <- tmp_hostFamily[,1]
	tmp_hostFamily <- tmp_hostFamily[-1]

	hostFamily <- merge(hostFamily, tmp_hostFamily, all=T, by="row.names")
	row.names(hostFamily) <- hostFamily[,1]
	hostFamily <- hostFamily[-1]
}
hostFamily[is.na(hostFamily)] <- 0



viromeOrder <- c()
for(i in dataset){
	tmp <- get(i)
	tmp_viromeOrder <- data.frame(table(tmp@tax_table[,"virus"]))
	row.names(tmp_viromeOrder) <- tmp_viromeOrder[,1]
	tmp_viromeOrder <- tmp_viromeOrder[-1]

	viromeOrder <- merge(viromeOrder, tmp_viromeOrder, all=T, by="row.names")
	row.names(viromeOrder) <- viromeOrder[,1]
	viromeOrder <- viromeOrder[-1]
}
viromeOrder[is.na(viromeOrder)] <- 0

contig_percent <- apply(hostFamily[-7], 2, sum)/16118

colnames(hostFamily) <- dataset
colnames(viromeOrder) <- dataset
names(contig_percent) <- dataset

hostFamily$hostFamily <- row.names(hostFamily)
viromeOrder$viromeOrder <- row.names(viromeOrder)

hostFamily <- melt(hostFamily)
viromeOrder <- melt(viromeOrder)

hostFamily$hostFamily <- factor(hostFamily$hostFamily, levels=names(col_family.host))
viromeOrder$viromeOrder <- factor(viromeOrder$viromeOrder, levels=names(col_virus))


#
contig_percent <- melt(contig_percent)
contig_percent$variable <- row.names(contig_percent)
ggplot(contig_percent, aes(y=value, x=variable)) + 
    geom_bar(stat="identity", width=0.7, color="black", fill="#999999") +
    theme_classic() + 
    labs(x="", y="Fraction of total vOTUs", title="") +
    mytheme + 
    theme(legend.position="none")
ggsave("contig_percent.pdf", width = 5, height = 5)

ggplot(hostFamily[hostFamily$hostFamily!="zOther",], aes(fill=hostFamily, y=value, x=variable)) + 
    geom_bar(position="fill", stat="identity", width=0.7) +
    scale_fill_manual(values=col_family.host) + 
    theme_classic() + 
    labs(x="", y="Fraction of selected vOTUs", title="Host family") +
    mytheme + 
    theme(legend.position="none")
ggsave("hostFamily.pdf", width = 5, height = 5)

ggplot(viromeOrder, aes(fill=viromeOrder, y=value, x=variable)) + 
    geom_bar(position="fill", stat="identity", width=0.7) +
    scale_fill_manual(values=col_virus) + 
    theme_classic() + 
	labs(x="", y="Fraction of selected vOTUs", title="Class") +
    mytheme + 
    theme(legend.position="none")
ggsave("viromeOrder.pdf", width = 5, height = 5)



