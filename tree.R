
tree.newick <- read.newick("sel.vOTUs.fna.tree")

tip.label <- str_split(tree.newick$tip.label, "_", simplify=T)
tip.label <- paste(tip.label[,2],tip.label[,3],sep="_")
tree.newick$tip.label <- tip.label
vOTU <- vOTU[tip.label,]

circ <- ggtree(tree.newick, layout='circular') %<+% vOTU + geom_tiplab(aes(color=hostFamily)) +
  scale_fill_manual(values=cols, name="hostFamily") + 
  scale_color_manual(values=cols, name="hostFamily") + 
  theme(legend.position="none")
ggsave("tree1.pdf", circ, limitsize=F, width = 8, height = 8, units = "in")





