
Contig_10 = filter_taxa(Contig, function(x) sum(x > 0) > (0.1*length(x)), TRUE)
Contig_20 = filter_taxa(Contig, function(x) sum(x > 0) > (0.2*length(x)), TRUE)
Contig_30 = filter_taxa(Contig, function(x) sum(x > 0) > (0.3*length(x)), TRUE)
Contig_40 = filter_taxa(Contig, function(x) sum(x > 0) > (0.4*length(x)), TRUE)
Contig_50 = filter_taxa(Contig, function(x) sum(x > 0) > (0.5*length(x)), TRUE)

readscount <- sample_sums(Contig)
readscount_10 <- sample_sums(Contig_10)/readscount
readscount_20 <- sample_sums(Contig_20)/readscount
readscount_30 <- sample_sums(Contig_30)/readscount
readscount_40 <- sample_sums(Contig_40)/readscount
readscount_50 <- sample_sums(Contig_50)/readscount


dataset <- c("Contig_10", "Contig_20", "Contig_30", "Contig_40", "Contig_50")
read_percent <- cbind(readscount_10, readscount_20, readscount_30, readscount_40, readscount_50)
colnames(read_percent) <- dataset

#
read_percent <- melt(read_percent)
read_percent$variable <- row.names(read_percent)

ggplot(read_percent, aes(y=value, x=Var2)) + 
    geom_boxplot(width=0.7, color="black", fill="#ef8a62", outlier.shape = NA) +
    theme_classic() + 
    scale_y_continuous(position = "right") +
    scale_x_discrete(position = "top") +
    labs(x="", y="") +
    theme(legend.position="none", 
        #panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x=element_blank())
ggsave("contig_percent.pdf", width = 4.2, height = 2.5, bg = "transparent")

