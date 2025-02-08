library(dplyr)
library(ggplot2)
library(ggConvexHull)
library(irr)


# first evaluation of the Poch's system (60 photographs, 16 raters)
rep1 <- read.table("rep1_anon.txt", header = T)
# second evaluation of the Poch's system (60 photographs, 16 raters)
rep2 <- read.table("rep2_anon.txt", header = T)
# each photograph's frequencies (i.e. to which shape category these photos were assigned most frequently)
freq <- read.table("freq_anon.txt", header = T)
# pca data
pca <- read.table("PCA_anon.txt", header = T)


# FLEISS KAPPA inter-observer agreement ####
kappam.fleiss(rep1) 

# COHENS KAPPA intra-observer consistency ####
intrakappa <- list()
for (i in 1:dim(rep1)[2]) {
  intrakappa[[i]] <- kappa2(data.frame(uno = as.numeric(rep1[,i]),
                                       dos = as.numeric(rep2[,i]))
  )
  
}

intraK <- unlist(lapply(intrakappa, "[[", 5)) # kappa value
unlist(lapply(intrakappa, "[[", 7)) # z value
unlist(lapply(intrakappa, "[[", 8)) # p-value

# mean intra-observer agreement
mean(intraK)
# min and max intra-observer agreement
range(intraK)

# Category consistency #### 
# the most selected responses for each photo
DFfreq <- freq %>% group_by(Folio_num_anon) %>% top_n(1, Freq)
num_evaluations = 32 # Total number of evaluations (first round = 16)
# Ratio of the frequency of classification to the total number of evaluations
DFfreq$Freq/num_evaluations
# The same ratio but as a percentage 
range(DFfreq$Freq*100/num_evaluations)
# min = 15 percent of agreement among evaluations of a given photo 
# max = 81.25 percent of agreement among evaluations of a given photo


# REPRESENTATIVENESS ####
## FIGURE 4 MAIN TEXT
ggplot(pca, aes(x = PC1, y = PC2, colour = sample, fill = sample,
                label = Poch)) +  
  geom_point(shape = 19, size = 2) +
  scale_color_manual(values=c("#CCCCCC", "#6F7C12"))+
  geom_text(aes(label=ifelse(sample == "Poch",as.character(Poch),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 5)+
  geom_convexhull(alpha = 0.2, aes(fill = sample)) + 
  scale_fill_manual(values=c("#CCCCCC", "#6F7C12"))+
  theme(axis.text = element_text(size = 40))+
  theme_bw()+
  xlab("PC1 (31 %)") + ylab("PC2 (27 %)")

ggsave("Figure4.png")


## FIGURE 1SUPP SUPPLEMENTARY MATERIAL
# only female individuals
pca %>% filter(sample == "Poch" | Sexo == "F") %>%
  ggplot( aes(x = PC1, y = PC2, colour = sample, fill = sample,
              label = Poch)) +  
  geom_point(shape = 19, size = 2) +
  scale_color_manual(values=c("#CCCCCC", "#6F7C12"))+
  geom_text(aes(label=ifelse(sample == "Poch",as.character(Poch),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 4)+
  geom_convexhull(alpha = 0.2, aes(fill = sample)) + 
  scale_fill_manual(values=c("#CCCCCC", "#6F7C12"))+
  theme_bw()+
  xlab("PC1 (31 %)") + ylab("PC2 (27 %)")

ggsave("Figure1supp_left.png")

# only male individuals
ggplot(pca[pca$Sexo == "M", ], aes(x = PC1, y = PC2, colour = sample, fill = sample,
                                   label = Poch)) +  
  geom_point(shape = 19, size = 2) +
  scale_color_manual(values=c("#CCCCCC", "#6F7C12"))+
  geom_text(aes(label=ifelse(sample == "Poch",as.character(Poch),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 4)+
  geom_convexhull(alpha = 0.2, aes(fill = sample)) + 
  scale_fill_manual(values=c("#CCCCCC", "#6F7C12"))+
  theme_bw()+
  xlab("PC1 (31 %)") + ylab("PC2 (27 %)")

ggsave("Figure1supp_right.png")


# CONSISTENCY ####
## TABLE 1SUPP IN SUPPLEMENTARY MATERIAL
pca %>% filter(consistency != 0) %>% group_by(Poch) %>%
  reframe(mean = mean(consistency),
          min = min(consistency),
          max = max(consistency))

## FIGURE 5 MAIN TEXT
pca %>%  
  mutate(Poch2 = paste(Poch, consistency*100, "%",sep = " ")) %>%
  ggplot(aes(x=PC1, y=PC2, colour = sample, fill = sample,
             label = Poch)) +  
  # mexican individuals
  geom_point(pca[1:973, ], mapping=aes(x=PC1, y=PC2),
             alpha = 0.2, shape = 16, stroke = 1, color = "#CCCCCC")+
  geom_point(pca[1:973, ] %>% filter(consistency > 0 & consistency <= 0.6),mapping=aes(x=PC1, y=PC2),
             alpha = 0.9, shape = 16, stroke = 1, color = "#CCCCCC")+
  geom_point(pca[1:973, ] %>% filter(consistency > 0.6),mapping=aes(x=PC1, y=PC2),
             alpha = 0.9, shape = 16, stroke = 1, color = "#4C5760")+
  geom_text(aes(label=ifelse(sample != "Poch" & consistency > 0.6, as.character(Poch2),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 4, color = "#4C5760")+
  # Poch
  geom_point(pca[974:983, ] ,mapping=aes(x=PC1, y=PC2), 
             alpha = 0.9, shape = 16, stroke = 1, color = "#6F7C12")+
  geom_text(aes(label=ifelse(sample == "Poch", as.character(Poch),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 5, color = "#6F7C12")+
  theme_bw()+
  xlab("PC1 (31 %)") + ylab("PC2 (27 %)")

ggsave("Figure5.png")


## FIGURE 2SUPP IN SUPPLEMENTARY MATERIAL
ggplot(pca, aes(x=PC1, y=PC2, colour = sample, fill = sample,
                label = Poch)) +  
  # mexican individuals
  geom_point(pca[1:973, ], mapping=aes(x=PC1, y=PC2),
             alpha = 0.2, shape = 16, stroke = 1, color = "#CCCCCC")+
  
  geom_point(pca[1:973, ] %>% filter(consistency > 0 & consistency <= 0.6),mapping=aes(x=PC1, y=PC2),
             alpha = 0.9, shape = 16, stroke = 1, color = "#CCCCCC")+
  geom_text(aes(label=ifelse(sample != "Poch" & consistency > 0 & consistency <= 0.6, 
                             as.character(Poch),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 5, color = "#CCCCCC")+
  
  geom_point(pca[1:973, ] %>% filter(consistency > 0.6),mapping=aes(x=PC1, y=PC2),
             alpha = 0.9, shape = 16, stroke = 1, color = "#4C5760")+
  
  geom_text(aes(label=ifelse(sample != "Poch" & consistency > 0.6, as.character(Poch),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 5, color = "#4C5760")+
  # Poch
  geom_point(pca[974:983, ] ,mapping=aes(x=PC1, y=PC2), 
             alpha = 0.9, shape = 16, stroke = 1, color = "#6F7C12")+
  
  geom_text(aes(label=ifelse(sample == "Poch", as.character(Poch),'')),
            hjust = 0, nudge_x = 0.002, nudge_y = 0.002,size = 5, color = "#6F7C12")+
  theme_bw()+
  xlab("PC1 (31 %)") + ylab("PC2 (27 %)")

ggsave("Figure2supp.png")


# K-MEANS CLUSTERING ####
# assigned clusters for k = 4, k = 5, k = 6
pca$C4 <- as.factor(pca$C4)
pca$C5 <- as.factor(pca$C5)
pca$C6 <- as.factor(pca$C6)

## FIGURE 3SUPP IN SUPPLEMENTARY MATERIAL
pca %>% filter(sample == "Mexican") %>%
  # change Clusters in colour and fill accordingly
  ggplot(aes(x = PC1, y = PC2, colour = C5)) +
  geom_convexhull(alpha = 0.3, aes(fill = C5)) + 
  geom_point() + 
  scale_colour_manual(values = c("#f8786fff", "#ffc425", "#00bfc4ff", "#7cae00ff", "#c77cffff"))+
  scale_fill_manual(values = c("#f8786fff", "#ffc425", "#00bfc4ff", "#7cae00ff", "#c77cffff" ))+
  theme_bw() + labs(x = "PC1 (31 %)", y = "PC2 (27 %)")

ggsave("Figure3Asupp.png")

pca %>% filter(sample == "Mexican") %>%
  # change Clusters in colour and fill accordingly
  ggplot(aes(x = PC1, y = PC2, colour = C6)) +
  geom_convexhull(alpha = 0.3, aes(fill = C6)) + 
  geom_point() + 
  scale_colour_manual(values = c("#f8786fff", "#8c8c8c", "#00bfc4ff", "#7cae00ff", "#c77cffff","#ffc425"))+
  scale_fill_manual(values = c("#f8786fff", "#8c8c8c", "#00bfc4ff", "#7cae00ff", "#c77cffff","#ffc425"))+
  theme_bw() + labs(x = "PC1 (31 %)", y = "PC2 (27 %)")

ggsave("Figure3Bsupp.png")

