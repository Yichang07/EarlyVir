library(phyloseq)
library(ggplot2)
library(readxl)
library(dplyr)
library(vegan)
library(ggpubr)
library(gridExtra)
library(grid)
library(ape)
library(RColorBrewer)
library(reshape2)
library(lmerTest)
library(gtools)
library(viridis)




load("/mnt/raid5/zhangych/EarlyVir/Phyloseq_V4_1/bray_201216/OTU.RData")
res <- res[sort(row.names(res)),]


res$names <- c("AB usage before 6 months old","AB usage before 1 year old","AB usage during pregnancy","AB usage during birth",
"Mother AB usage during birth","Weight at birth",
"Birth induction","Still breast fed at sampling time","Cat or Dog","Cow milk in diet starting day",
"Daycare at sampling time","Delivery","Delivery method","Education",
"Egg in diet starting days","First antifungus","First AB usage","Fishoil during pregnancy",
"Fish in diet starting days","Gestational age","Hospitalized after birth","Income", "Low brith weight (< 2.5kg)",
"Mother BMI","Mother age","Older siblings","Preeclampsia",
"Smoking during pregnancy","Race","Type of living environment","Sex",
"Solely breast feeding days")


res$group <- c("Postnatal", "Postnatal", "Prenatal", "Perinatal",
	"Perinatal", "Perinatal",
	"Perinatal", "Postnatal", "Postnatal", "Postnatal",
	"Postnatal", "Perinatal", "Perinatal", "SIF",
	"Postnatal", "Postnatal", "Prenatal", "Prenatal",
	"Postnatal", "Perinatal", "Perinatal", "SIF", "Perinatal",
	"SIF", "SIF", "Postnatal", "Prenatal",
	"Prenatal", "SIF", "Postnatal", "Perinatal",
	"Postnatal")


res$group <- factor(res$group,levels = c("SIF", "Prenatal", "Perinatal", "Postnatal"))

res <- res[order(res$group, res$names),]

res$names <- factor(res$names,levels = res$names)


mytheme<- theme(plot.title = element_text(hjust=0.5, size=30),
				plot.margin = unit(c(0,0.5,0,4.1), "cm"),
                #legend.title = element_blank(),
                legend.position='none',
                legend.text = element_text(size = 28),
                legend.background = element_blank(),
                strip.background = element_blank(),
                strip.placement = "outside",
                strip.text = element_text(size = 28),
                axis.text.x = element_text(angle = 60, hjust = 1, size = 28),
                axis.text.y = element_text(angle = 90, hjust = 0.5, size = 28),
                axis.title.x = element_text(size = 28),
                axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size = 28),         
                axis.title = element_text(size = 28),
                axis.text = element_text(size = 28, color="black"),
                panel.grid = element_blank())

my_color <- c("#a6611a", '#dfc27d','#80cdc1', "#018571")


res$symbol[res$symbol == "."] = ""

###barplot bray
ggplot(data=res, aes(fill=group,y=names, x=R2*1e3)) + geom_bar(stat="identity", position=position_dodge())+ coord_flip() + 	scale_fill_manual(values = my_color) +
	labs(x="R2 ( × 1e-3)",y="",title="Bray-Curtis") + theme_classic() + mytheme
ggsave("bray_R2.pdf", limitsize=F, width = 26, height = 8.5, units = "in", bg = "transparent")


ggplot(data=res, aes(fill=group,y=names,x=-log10(P)))+geom_bar(stat="identity", position=position_dodge())+ coord_flip() +	geom_text(aes(y=names, x=0.07-log10(P),label=symbol), position = position_dodge(0.9), size=7)+ 
	geom_vline(xintercept = -log10(0.05), linetype = 2, color = "red") + 
	scale_fill_manual(values = my_color) +
	labs(x="-log10(P value)",y="",title="Bray-Curtis") + theme_classic() + mytheme
ggsave("bray_P.pdf", limitsize=F, width = 26, height = 8.5, units = "in", bg = "transparent")





###barplot sd
###barplot sd
###barplot sd
load("/mnt/raid5/zhangych/EarlyVir/Phyloseq_V4_1/sorensen_201214/OTU.RData")


mytheme<- theme(plot.title = element_text(hjust=0.5, size=30),
				plot.margin = unit(c(0,0.5,0,4.1), "cm"),
                #legend.title = element_blank(),
                legend.position='none',
                legend.text = element_text(size = 28),
                legend.background = element_blank(),
                strip.background = element_blank(),
                strip.placement = "outside",
                strip.text = element_text(size = 28),
                axis.text.x = element_text(angle = 60, hjust = 1, size = 28),
                axis.text.y = element_text(angle = 90, hjust = 0.5, size = 28),
                axis.title.x = element_text(size = 28),
                axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size = 28),         
                axis.title = element_text(size = 28),
                axis.text = element_text(size = 28, color="black"),
                panel.grid = element_blank())

my_color <- c("#a6611a", '#dfc27d','#80cdc1', "#018571")

res <- res[sort(row.names(res)),]

res$symbol[res$symbol == "."] = ""



#barplot
res$names <- c("AB usage before 6 months old","AB usage before 1 year old","AB usage during pregnancy","AB usage during birth",
"Mother AB usage during birth","Weight at birth",
"Birth induction","Still breast fed at sampling time","Cat or Dog","Cow milk in diet starting day",
"Daycare at sampling time","Delivery","Delivery method","Education",
"Egg in diet starting days","First antifungus","First AB usage","Fishoil during pregnancy",
"Fish in diet starting days","Gestational age","Hospitalized after birth","Income", "Low brith weight (< 2.5kg)",
"Mother BMI","Mother age","Older siblings","Preeclampsia",
"Smoking during pregnancy","Race","Type of living environment","Sex",
"Solely breast feeding days")


res$group <- c("Postnatal", "Postnatal", "Prenatal", "Perinatal",
	"Perinatal", "Perinatal",
	"Perinatal", "Postnatal", "Postnatal", "Postnatal",
	"Postnatal", "Perinatal", "Perinatal", "SIF",
	"Postnatal", "Postnatal", "Prenatal", "Prenatal",
	"Postnatal", "Perinatal", "Perinatal", "SIF", "Perinatal",
	"SIF", "SIF", "Postnatal", "Prenatal",
	"Prenatal", "SIF", "Postnatal", "Perinatal",
	"Postnatal")

res$group <- factor(res$group,levels = c("SIF", "Prenatal", "Perinatal", "Postnatal"))

res <- res[order(res$group, res$names),]

res$names <- factor(res$names,levels = res$names)


ggplot(data=res, aes(fill=group,y=names, x=R2*1e3)) + geom_bar(stat="identity", position=position_dodge())+ coord_flip() +
	scale_fill_manual(values = my_color) +
	labs(x="R2 ( × 1e-3)",y="",title="Sorensen-Dice") + theme_classic() + mytheme
ggsave("sd_R2.pdf", limitsize=F, width = 26, height = 8.5, units = "in", bg = "transparent")


ggplot(data=res, aes(fill=group,y=names,x=-log10(P)))+geom_bar(stat="identity", position=position_dodge())+ coord_flip() +
	geom_text(aes(y=names, x=0.07-log10(P),label=symbol), position = position_dodge(0.9), size=7)+ 
	geom_vline(xintercept = -log10(0.05), linetype = 2, color = "red") + 
	scale_fill_manual(values = my_color) +
	labs(x="-log10(P value)",y="",title="Sorensen-Dice") + theme_classic() + mytheme
ggsave("sd_P.pdf", limitsize=F, width = 26, height = 8.5, units = "in", bg = "transparent")




