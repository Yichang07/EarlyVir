library(phyloseq)
library(ggplot2)
library(readxl)
library(dplyr)
library(vegan)
library(usdm)
library(ggpubr)
library(gridExtra)
library(grid)
library(ape)
library(RColorBrewer)
library(reshape2)
library(lmerTest)


Alpha_Observed <- plot_richness(Contig, measures="Observed", x=var[3])$data
Alpha_Shannon <- plot_richness(Contig, measures="Shannon", x=var[3])$data


res <- matrix(data = NA, nrow = length(var), ncol = 4, byrow = FALSE, dimnames = NULL)
row.names(res) <- var




for(i in 1:length(var)){
	###fitted
	Observed <- lmer(value ~  get(var[i]) + (1| lane), REML = TRUE, data = Alpha_Observed)
	p <- anova(Observed)$`Pr(>F)`
	res[var[i],2] <- p
}

for(i in 1:length(var)){
	###fitted
	Observed <- lmer(value ~  get(var[i]) + (1| lane), REML = TRUE, data = Alpha_Shannon)
	Alpha_Shannon$fit<-predict(Observed)
	p <- anova(Observed)$`Pr(>F)`
	res[var[i],4] <- p
}




###barplot
res <- data.frame(res)
res <- cbind(row.names(res),res)
colnames(res) <- c("variable","observed_unfitted","observed_fitted","shannon_unfitted","shannon_fitted")


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


my_color <- c("#a6611a", '#dfc27d','#80cdc1', "#018571")

res$observed_symbol <- stars.pval(res$observed_fitted)
res$shannon_symbol <- stars.pval(res$shannon_fitted)

res$observed_symbol[res$observed_symbol=="."] = ""
res$shannon_symbol[res$shannon_symbol=="."] = ""

ggplot(data=res, aes(fill=group,y=names,x=-log10(observed_fitted)))+geom_bar(stat="identity", position=position_dodge())+ coord_flip() + 
	geom_text(aes(y=names, x=0.07-log10(observed_fitted),label=observed_symbol), position = position_dodge(0.9), size=7)+ 
	geom_vline(xintercept = -log10(0.05), linetype = 2, color = "red") + 
	scale_fill_manual(values = my_color) +
	scale_x_continuous(breaks=seq(0, 2, 1)) +
	labs(x="-log10(P value)",y="",title="Observed contigs") + theme_classic() + mytheme
ggsave("observed_fitted_barplot.pdf", limitsize=F, width = 26, height = 8.5, units = "in", bg = "transparent")


ggplot(data=res, aes(fill=group,y=names,x=-log10(shannon_fitted)))+geom_bar(stat="identity", position=position_dodge())+ coord_flip() + 
	geom_text(aes(y=names, x=0.07-log10(shannon_fitted),label=shannon_symbol), position = position_dodge(0.9), size=7)+ 
	geom_vline(xintercept = -log10(0.05), linetype = 2, color = "red") + 
	scale_fill_manual(values = my_color) +
	labs(x="-log10(P value)",y="",title="Shannon index") + theme_classic() + mytheme
ggsave("shannon_fitted_barplot.pdf", limitsize=F, width = 26, height = 8.5, units = "in", bg = "transparent")




