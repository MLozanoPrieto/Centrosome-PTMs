library(ggplot2)
library(ggrepel)

#FIGURE 1
#A

table1_psms <- read.table(file = "S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/graphs/txt_gráficas_R/fig1_visualizacion_psms.txt", sep="\t", header = TRUE)
table1_psms$type <- factor(table1_psms$type, levels = c("Total", "ScanFreq>1", "ScanFreq>1 peaks ", "ScanFreq>1 orphans ", "ScanFreq>1 known PTMs", "ScanFreq>1 unknown PTMs"))
p <- ggplot(data=table1_psms, aes(x=type, y=counts, fill = type)) +
  geom_bar(stat="identity")
p.labs <- p + labs(title = "Number of PSMs", x = "", y = "Counts")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_blank()) + 
  geom_text(aes(label=counts), vjust=-0.3, size=5) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text = element_text(size=15), axis.text.x = element_blank())

#B

table1_pdms <- read.table(file = "S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/graphs/txt_gráficas_R/fig1_visualizacion_pdms.txt", sep="\t", header = TRUE)
table1_pdms$type <- factor(table1_pdms$type, levels = c("Total", "ScanFreq>1", "ScanFreq>1 peaks ", "ScanFreq>1 orphans ", "ScanFreq>1 known PTMs", "ScanFreq>1 unknown PTMs"))
p <- ggplot(data=table1_pdms, aes(x=type, y=counts, fill = type)) +
  geom_bar(stat="identity")
p.labs <- p + labs(title = "Number of pdms", x = "", y = "Counts")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(legend.title = element_blank()) + 
  geom_text(aes(label=counts), vjust=-0.3, size=5) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + 
  theme(legend.text = element_text(size=15), axis.text.x = element_blank())

#FIGURE 2

table_known <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/conocidas/known_list_pdm_psm2.txt", sep="\t", header = TRUE)
table_known$modification <- factor(table_known$modification, levels = c("Methylation (+0-1 C13)", "Mono-oxidation (+0-3 C13)", "iTRAQ artefact (-1.00068775 Da)", "iTRAQ artefact (-2.003739175 Da)", "Dimethylation (+0-1 C13)", "iTRAQ label", "Formylation (artefact)","Kynurenin", "Dioxidation (+0-2 C13)", "iTRAQ artefact (-3.003143562)","UgiJoullieProGly", "Carbamidomethylation (artefact)", "Leu -> Met(ox)", "Carbamylation (artefact)", "Phosphorylation (+0-1 C13)", "Cys->Asp", "Arg addition by transpeptidation", "Iodination", "Trioxidation", "Asp->Met"))

#A

p <- ggplot(data=table_known, aes(x= modification, y=psm_count, fill = modification)) +
  geom_bar(stat="identity")
p.labs <- p + labs(title= "Number of PSMs of the most frequent known PTMs", x="", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) + 
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  theme(legend.text = element_text(size=15)) + 
  theme(axis.text.x = element_blank())

#B

p <- ggplot(data=table_known, aes(x= modification, y=pdm_count, fill = modification)) + 
  geom_bar(stat="identity")
p.labs <- p + labs(title= "Number of pdms of the most frequent known PTMs", x="", y="Number of pdms")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) + 
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  theme(legend.text = element_text(size=15)) + 
  theme(axis.text.x = element_blank())

#FIGURE 3 

table_aa_psm <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/conocidas/known_list_pdm_psm_aapsm_grouped_t.txt", sep="\t", header = TRUE)

p <- ggplot(data=table_aa_psm, aes(x=aa, y=Methylation...0.1.C13.)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Methylation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


p <- ggplot(data=table_aa_psm, aes(x=aa, y=Mono.oxidation...0.3.C13.)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Mono-oxidation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


p <- ggplot(data=table_aa_psm, aes(x=aa, y=Dimethylation...0.1.C13.)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Dimethylation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


p <- ggplot(data=table_aa_psm, aes(x=aa, y=Phosphorylation...0.1.C13.)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Phosphorylation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


p <- ggplot(data=table_aa_psm, aes(x=aa, y=Acetylation)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Acetylation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


p <- ggplot(data=table_aa_psm, aes(x=aa, y=Dioxidation...0.2.C13.)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Dioxidation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


p <- ggplot(data=table_aa_psm, aes(x=aa, y=Trioxidation)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Trioxidation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))

# SUPPLEMENTARY FIGURE 2

p <- ggplot(data=table_aa_psm, aes(x=aa, y=iTRAQ.label...0.2.C13.)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "iTRAQ label", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


p <- ggplot(data=table_aa_psm, aes(x=aa, y=Carbamylation..artefact.)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Carbamylation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))

p <- ggplot(data=table_aa_psm, aes(x=aa, y=Iodination)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Iodination", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))

p <- ggplot(data=table_aa_psm, aes(x=aa, y=Carbamidomethylation)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Carbamidomethylation", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))

p <- ggplot(data=table_aa_psm, aes(x=aa, y=Kynurenin)) +
  geom_bar(stat="identity", fill = "darkturquoise")
p.labs <- p + labs(title= "Kynurenin", x="Residue", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))


#FIGURE 4

table_permitted_psms <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/conocidas/known_list_permitted_psms_definitiva.txt", sep="\t", header = TRUE)

table_permitted_psms$Concept<- factor(table_permitted_psms$Concept, levels = c("Total number of PSMs (ScanFreq > 1)", "Permitted PSMs (ScanFreq > 1)", "Permitted RECOM assignations (ScanFreq > 1)"))
p <- ggplot(data=table_permitted_psms, aes(x=reorder(modification, -Counts), y = Counts, fill = Concept)) +
  geom_bar(stat="identity", position=position_dodge())
p.labs <- p + labs(title= "Number of PSMs per PTM", x="", y="Number of PSMs")
p.labs + theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  theme(legend.title = element_blank()) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  theme(legend.text = element_text(size=15)) + theme(axis.text.x = element_text(angle = 90))

# FIGURE 5

table_pie <-read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/graphs/Tabla_P0_CENTROSOMA_counts5.txt", sep="\t", header = TRUE)
table_pie3 <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/graphs/txt_gráficas_R/pie_3.txt", sep="\t", header = TRUE)

#A

table_pie3$Type<- factor(table_pie3$Type, levels = c("Unmodified", "Modified"))
p<-ggplot(data=table_pie3, aes(x="", y= Counts, fill=Type)) +
  geom_bar(stat="identity", width=2, color="black")+
  theme_void()+
  geom_label_repel(aes(label = table_pie3$X.psms), position = position_stack(vjust = 0.5), size=5, show.legend = F) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_polar("y", start=0)
p + labs(title= "Modified vs unmodified T cell centrosome proteome") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  theme(legend.text = element_text(size=15)) +
  theme(legend.title = element_blank()) 


#B

table_pie$Modification <- factor(table_pie$Modification, levels = c("Unknown", "Others", "Phosphorylation", "Dioxidation", "Mono-oxidation", "Dimethylation", "Methylation" ))
p<-ggplot(data=table_pie, aes(x="", y= psms, fill=Modification)) +
  geom_bar(stat="identity", width=2, color="black")+
  theme_void()+
  geom_label_repel(aes(label = table_pie$X.psms), position = position_stack(vjust = 0.5), size=5, show.legend = F) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_polar("y", start=0)
p + labs(title= "Percentage of PSMs of T cell centrosome PTMs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  theme(legend.text = element_text(size=15)) +
  theme(legend.title = element_blank()) 

#B

p<-ggplot(data=table_pie, aes(x="", y= pdms, fill=Modification)) +
  geom_bar(stat="identity", width=2, color="black")+
  theme_void()+
  geom_label_repel(aes(label = table_pie$X.pdms), position = position_stack(vjust = 0.5), size=5, show.legend = F) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_polar("y", start=0)
p + labs(title= "Percentage of pdms of T cell centrosome PTMs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) + 
  theme(legend.text = element_text(size=15)) + 
  theme(legend.title = element_blank()) 

# C 

table_distribution <-read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/graphs/Tabla_P0_CENTROSOMA_JV_counts_aa_distribution.txt", sep="\t", header = TRUE)

p <- ggplot(data=table_distribution, aes(x=Residue, y=Methylation)) +
  geom_bar(stat="identity", fill = "#FF61CC") +
  labs(title= "Methylation", x="Residue", y="Normalised abundance")+ 
  theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))
p


p <- ggplot(data=table_distribution, aes(x=Residue, y=Dimethylation)) +
  geom_bar(stat="identity", fill = "#C77CFF") +
  labs(title= "Dimethylation", x="Residue", y="Normalised abundance")+ 
  theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))
p


p <- ggplot(data=table_distribution, aes(x=Residue, y=Mono.oxidation)) +
  geom_bar(stat="identity", fill = "#00A9FF") +
  labs(title= "Mono-oxidation", x="Residue", y="Normalised abundance")+ 
  theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))
p


p <- ggplot(data=table_distribution, aes(x=Residue, y=Dioxidation)) +
  geom_bar(stat="identity", fill = "#00BFC4") +
  labs(title= "Dioxidation", x="Residue", y="Normalised abundance")+ 
  theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))
p


p <- ggplot(data=table_distribution, aes(x=Residue, y=Phosphorylation...0.2.C13.)) +
  geom_bar(stat="identity", fill = "#00BE67") +
  labs(title= "Phosphorylation", x="Residue", y="Normalised abundance")+ 
  theme(legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 30, face="bold")) +
  theme(axis.text=element_text(size=24), axis.title=element_text(size=24,face="bold"))
p

# FIGURE 7

table_GO <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/24_PeptideSiteList/test_hipergeometricos_ML/GOrilla/met_E_GOterms_process.txt", sep="\t", header = TRUE)

p <- ggplot(data=table_GO, aes(x=-log10(FDR_q_value), y=reorder(Description, -log10(FDR_q_value)), fill = Enrichment_score)) +
  geom_bar(stat="identity") +
  scale_fill_continuous(low="blue", high="red") +
  labs(title= "Process GO terms", x="-log10(FDR)", y="GO term", fill = "Enrichment score") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15,face="bold")) + 
  geom_vline(xintercept = -log10(0.05), linetype="dashed", color = "black", size=1)
p

#FIGURE 9

table_sig_total <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_total_met_E_cut.txt", sep="\t", header = TRUE)
table_cytoskeleton <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_cytoskeleton_cut.txt", sep="\t", header = TRUE)
table_large <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_large_cut.txt", sep="\t", header = TRUE)
table_small <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_small_cut.txt", sep="\t", header = TRUE)
table_moesin <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_moesin_cut.txt", sep="\t", header = TRUE)
table_myosin <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_myosin_cut.txt", sep="\t", header = TRUE)
table_septin <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_septin_cut.txt", sep="\t", header = TRUE)
table_tubulin <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_tubulin_cut.txt", sep="\t", header = TRUE)
table_actin <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_actin_cut.txt", sep="\t", header = TRUE)
table_vimentin <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_vimentin_cut.txt", sep="\t", header = TRUE)
table_chaperone <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_chaperone_cut.txt", sep="\t", header = TRUE)
table_glycolysis <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_glycolysis_cut.txt", sep="\t", header = TRUE)
table_histone <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_histone_cut.txt", sep="\t", header = TRUE)
table_ribosomal <- read.table(file="S:/U_Proteomica/LABS/LAB_FSM/Centrosome_PTMs/RECOM/23_mapping/Con_Recom/sigmoides/sigmoid_ribosomal_cut.txt", sep="\t", header = TRUE)


table_sig_total <- table_sig_total[order(table_sig_total$rank_total.N), ]


plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E  Methylation: total vs cytoskeleton proteins", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_cytoskeleton$ActRes_average, table_cytoskeleton$rank_cytoskeleton.N, col = "green", pch = 19)


plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs large ribosomal subunit proteins", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, type = "l", lwd = 5, xlim=c(-3,3))
points(table_large$ActRes_average, table_large$rank_large.N, col = "green", pch = 19)

plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs small ribosomal subunit proteins", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, type = "l", lwd = 5, xlim=c(-3,3))
points(table_small$ActRes_average, table_small$rank_small.N, col = "green", pch = 19)

plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs moesin", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_moesin$ActRes_average, table_moesin$rank_moesin.N, col = "green", pch = 19)

plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs myosin", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_myosin$ActRes_average, table_myosin$rank_myosin.N, col = "green", pch = 19)

plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs septin", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_septin$ActRes_average, table_septin$rank_septin.N, col = "green", pch = 19)

plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs tubulin", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type =  "l", lwd = 5, xlim=c(-3,3))
points(table_tubulin$ActRes_average, table_tubulin$rank_tubulin.N, col = "green", pch = 19)

plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs actin", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_actin$ActRes_average, table_actin$rank_actin.N, col = "green", pch =19)

plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs vimentin", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_vimentin$ActRes_average, table_vimentin$rank_vimentin.N, col = "green", pch = 19)


plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs chaperones", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_chaperone$ActRes_average, table_chaperone$rank_chaperone.N, col = "green", pch = 19)


plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs glycolysis proteins", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_glycolysis$ActRes_average, table_glycolysis$rank_glycolysis.N, col = "green", pch = 19)


plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs histones", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_histone$ActRes_average, table_histone$rank_histone.N, col = "green", pch = 19)


plot(table_sig_total$ActRes_average, table_sig_total$rank_total.N, main = "E Methylation: total vs ribosomal proteins", xlab = "Zpdm2pdmq ratio (Act/Res)", ylab = "Rank/N", cex.lab=1.5, cex.axis=1.5, cex.main=1.7, type = "l", lwd = 5, xlim=c(-3,3))
points(table_ribosomal$ActRes_average, table_ribosomal$rank_ribosomal.N, col = "green", pch = 19)
