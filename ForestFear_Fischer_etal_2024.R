#### ANALYSIS OF DATA FOR MANUSCRIPT: Fischer et al., 2024 ########
#### Who is afraid of the woods? – An investigation of the implicit and explicit fear reactions of forests ####

## last update: Feb.2024 (d.fischer@uke.de; Djo Fischer)

#### Library
library(ggplot2)
library(sjPlot)
library(tidyr)
library(psych)
library(ggpubr)
library(see)
library(forcats)
library(lme4)
library(stats)
library(sjlabelled)
library(sjmisc)
library(performance)
library(emmeans)
library(ggeffects)
library(dplyr)
library(purrr)
library(Hmisc)
library(parameters)


rm(list= ls())

# read in data
setwd("G:/Forest_Fear/Paper/R_Skripts/") 
load('ForestFear_Fischer_etal_2024_data.Rdata')

setwd("G:/Wald-Angst-Studie/Analysen")
Daten <- read.csv("G:/Wald-Angst-Studie/Analysen/ForestFear_Feb_2024.csv", header = TRUE, sep = ";", dec = ",")


Daten$groupid[Daten$groupid == "1"] <- "park"
Daten$groupid[Daten$groupid == "2"] <- "house"
Daten$groupid[Daten$groupid == "3"] <- "house"
Daten$groupid[Daten$groupid == "4"] <- "house"
Daten$groupid[Daten$groupid == "5"] <- "house"
Daten$groupid <- as.factor(Daten$groupid)
Daten$Gender[Daten$Gender == "0"] <- "female"
Daten$Gender[Daten$Gender == "1"] <- "male"
Daten$Gender[Daten$Gender == "2"] <- "inter"
Daten$Gender <- as.factor(Daten$Gender)

# Transform the data into long format 
long <- reshape(data = Daten,
                varying = list(
                  c('AMP_forest_arousal_mean', 'AMP_park_arousal_mean', 'AMP_house_arousal_mean'),
                  c('AMP_forest_valence_mean', 'AMP_park_valence_mean', 'AMP_house_valence_mean'),
                  c('AMP_forest_dominance_mean', 'AMP_park_dominance_mean', 'AMP_house_dominance_mean'),
                  c('AMP_forest_arousal_mean_inverse', 'AMP_park_arousal_mean_inverse', 'AMP_house_arousal_mean_inverse'),
                  c('EX_forest_arousal_mean', 'EX_park_arousal_mean', 'EX_house_arousal_mean'),
                  c('EX_forest_valence_mean', 'EX_park_valence_mean', 'EX_house_valence_mean'),
                  c('EX_forest_dominance_mean', 'EX_park_dominance_mean', 'EX_house_dominance_mean'),
                  c('EX_forest_arousal_mean_inverse', 'EX_park_arousal_mean_inverse', 'EX_house_arousal_mean_inverse'),
                  c('Wald_Angst_sum', 'Park_Angst_sum', 'Haus_Angst_sum'),
                  c('Wald_Gefahr_sum', 'Park_Gefahr_sum', 'Haus_Gefahr_sum'),
                  c('AAT_diff_wald', 'AAT_diff_park', 'AAT_diff_haus'),
                  c('SPP_Wald_diff', 'SPP_Park_diff', 'SPP_Haus_diff')),
                direction = 'long',
                timevar = 'condition',
                times = c("forest", "park", "house"),
                v.names = c("AMP_fear","AMP_Valence", "AMP_Dominance", "AMP_fear_inverse",
                            "EX_fear","EX_Valence", "EX_Dominance", "EX_fear_inverse",
                            "fear_items","danger_items", 
                            "AAT_diff", "SPP_diff"))

long$group = long$groupid 
long$group[long$groupid == "park" ] <- "park"
long$group[long$groupid == "house" ] <- "house"
str(long$groupid)

long$ex_condition = long$condition
long$condition[long$groupid == "park" & long$condition == "forest"] <- "forest p"
long$condition[long$groupid == "house" & long$condition == "forest"] <- "forest h"

long$pic_h = long$condition
long$pic_h[long$condition == "park" ] <- NA
long$pic_p = long$condition
long$pic_p[long$condition == "house" ] <- NA


long$condition <- as.factor(long$condition)
long$ex_condition <- as.factor(long$ex_condition)
long$ID <- as.factor(long$ID)

############################# Detection of Outlier ###########################
# Identify the outliers with median absolute deviation (MAD) - method (Package: Routliers)
# with a MAD-threshold of three units; 1=Outlier, 0=no Outlier

library(Routliers)
#outliers_mad(long$AMP_Arousal, b = 1.4826,threshold = 3,na.rm = TRUE)

long$AMP_fear_isoutlier <- ifelse(long$AMP_fear %in% outliers_mad(long$AMP_fear, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$AMP_Dominance_isoutlier <- ifelse(long$AMP_Dominance %in% outliers_mad(long$AMP_Dominance, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$AMP_Valence_isoutlier <- ifelse(long$AMP_Valence %in% outliers_mad(long$AMP_Valence, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$AMP_fear_inverse_isoutlier <- ifelse(long$AMP_fear_inverse %in% outliers_mad(long$AMP_fear_inverse, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)

long$EX_fear_isoutlier <- ifelse(long$EX_fear %in% outliers_mad(long$EX_fear, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$EX_Dominance_isoutlier <- ifelse(long$EX_Dominance %in% outliers_mad(long$EX_Dominance, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$EX_Valence_isoutlier <- ifelse(long$EX_Valence %in% outliers_mad(long$EX_Valence, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$EX_fear_inverse_isoutlier <- ifelse(long$EX_fear_inverse %in% outliers_mad(long$EX_fear_inverse, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)

long$fear_items_isoutlier <- ifelse(long$fear_items %in% outliers_mad(long$fear_items, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$danger_items_isoutlier <- ifelse(long$danger_items %in% outliers_mad(long$danger_items, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)

long$AAT_diff_isoutlier <- ifelse(long$AAT_diff %in% outliers_mad(long$AAT_diff, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$AAT_RT_isoutlier <- ifelse(long$AAT_meanRT_overall %in% outliers_mad(long$AAT_meanRT_overall, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$SPP_diff_isoutlier <- ifelse(long$SPP_diff %in% outliers_mad(long$SPP_diff, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)
long$SPP_RT_isoutlier <- ifelse(long$ssp_meanRT %in% outliers_mad(long$ssp_meanRT, b = 1.4826,threshold = 3,na.rm = TRUE)$outliers, 1, 0)


#################### normal distribution - ####################
shapiro.test(long[long$EX_fear_isoutlier == 0, ]$EX_fear)
ggplot(long[long$EX_fear_isoutlier == 0, ], aes(x=EX_fear)) + geom_histogram()
shapiro.test(long[long$fear_items_isoutlier == 0, ]$fear_items)
ggplot(long[long$fear_items_isoutlier == 0, ], aes(x=fear_items)) + geom_histogram()
shapiro.test(long[long$danger_items_isoutlier == 0, ]$danger_items)
ggplot(long[long$danger_items_isoutlier == 0, ], aes(x=danger_items)) + geom_histogram()

shapiro.test(long[long$SPP_diff_isoutlier == 0,]$SPP_diff)
ggplot(long[long$SPP_diff_isoutlier == 0,], aes(x=SPP_diff)) + geom_histogram()
shapiro.test(long[long$AAT_diff_isoutlier == 0,]$AAT_diff)
ggplot(long[long$AAT_diff_isoutlier == 0,], aes(x=AAT_diff)) + geom_histogram()
shapiro.test(long[long$AMP_fear_isoutlier == 0,]$AMP_fear)
ggplot(long[long$AMP_fear_isoutlier == 0,], aes(x=AMP_fear)) + geom_histogram()

#### correlation between the independent variables
#### see Table Sx in the supplementary material
library("GGally")
library("Hmisc")
library("corrplot")
Cor<- subset(long[long$SPP_diff_isoutlier == 0 & long$AAT_diff_isoutlier == 0 & long$AMP_fear_isoutlier == 0 &
                           long$EX_fear_isoutlier == 0 & long$fear_items_isoutlier == 0 & long$danger_items_isoutlier == 0 ,], 
                    select = c(AMP_fear_inverse, EX_fear_inverse,
                               fear_items, danger_items, 
                               AAT_diff, SPP_diff))

colnames(Cor) <- c("AMP fear", "Ex fear", "fear items", "danger items", "AAT diff", "SPP diff")
frame_cor <- as.data.frame(Cor)

KM <- cor(frame_cor, method = "spearman", use = "complete.obs")

KM_p <- rcorr(as.matrix(frame_cor), type=c("spearman"))
KM_p

# Insignificant correlations are leaved blank
corrplot(KM_p$r, method = "square", type = "upper", order = "AOE", 
         p.mat = KM_p$P, sig.level = 0.05, insig = "blank", addCoef.col ='black', tl.col="black",)

ggcorr(Cor, method = c("pairwise", "spearman"),
       nbreaks = 6, label = TRUE, label_size = 3, color = "black")

###################################################################################################
### LMER models

#explizite tasks
long$X = long$EX_fear_inverse

EX_LMER_A <- lmer(X ~ ex_condition + EX_Valence + (1 | ID),
                  data=long[long$EX_fear_inverse_isoutlier == 0,] ,na.action = na.omit)
tab_model(EX_LMER_A, show.se = TRUE)
summary(EX_LMER_A)
anova(EX_LMER_A)

hist(residuals(EX_LMER_A))
qqnorm(residuals(EX_LMER_A)); qqline(residuals(EX_LMER_A))

tab_model(EX_LMER_A, show.se = TRUE, title = "Explicit picture rating", 
          dv.labels = c("Mean fear score"))

# Package lsmean: Post Hoc analysen - multiple comparison control: none!
emmeans(EX_LMER_A, list(pairwise ~ ex_condition), adjust = "none")

EX_Arousal_pic <- effects::effect(term= "ex_condition", mod= EX_LMER_A)
summary(EX_Arousal_pic)
x_EX_Arousal_pic <- as.data.frame(EX_Arousal_pic)

# Figure 4
# https://lmudge13.github.io/sample_code/mixed_effects.html
EX_A_plot <- ggplot() + 
  geom_violin(data=long[long$EX_fear_inverse_isoutlier == 0,], aes(ex_condition, EX_fear_inverse, fill=ex_condition)) + 
  scale_fill_manual(values=c("darkseagreen2", "azure3", "lemonchiffon"), labels = c('Forest','House','Park'))+
  geom_point(data=x_EX_Arousal_pic, aes(x=ex_condition, y=fit), size= 2, color="red") +
  geom_errorbar(data= x_EX_Arousal_pic, aes(x=ex_condition, ymin=lower, ymax=upper), size= 0.3, color="black") +
  theme_classic2()+
  labs(title = "Explicit picture rating - fear", subtitle = "
  
       
       ", 
       x="Condition", y="Mean fear score", fill = "Conditions") +
  scale_x_discrete(labels = c('Forest','House','Park'))+
  theme(axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        legend.title = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

EX_A_plot


# Angst Sum
long$X = long$fear_items

EX_LMER_fear <- lmer(X ~ ex_condition + (1 | ID),
                     data=long[long$fear_items_isoutlier == 0,] ,na.action = na.omit)
tab_model(EX_LMER_fear)
anova(EX_LMER_fear)

tab_model(EX_LMER_fear, show.se = TRUE, title = "Fear items", 
          dv.labels = c("Total score of the fear items"))

hist(residuals(EX_LMER_fear))
qqnorm(residuals(EX_LMER_fear)); qqline(residuals(EX_LMER_fear))
# Package lsmean: Post Hoc analysen - multiple comparison control:none
emmeans(EX_LMER_fear, list(pairwise ~ ex_condition), adjust = "none")

EX_Angst_pic <- effects::effect(term= "ex_condition", mod= EX_LMER_fear)
summary(EX_Angst_pic)
x_EX_Angst_pic <- as.data.frame(EX_Angst_pic)

# Figure 5
EX_Angst_plot <- ggplot() + 
  geom_violin(data=long[long$fear_items_isoutlier == 0,], aes(ex_condition, fear_items, fill=ex_condition)) + 
  scale_fill_manual(values=c("darkseagreen2", "azure3", "lemonchiffon"), labels = c('Forest','House','Park'))+
  geom_point(data=x_EX_Angst_pic, aes(x=ex_condition, y=fit), size= 2, color="red") +
  geom_errorbar(data= x_EX_Angst_pic, aes(x=ex_condition, ymin=lower, ymax=upper), size= 0.3, color="black") +
  theme_classic2()+
  labs(title = "Fear items", subtitle = "
  
       
       ", 
       x="Condition", y="Total score fear items", fill = "Conditions") + 
  scale_x_discrete(labels = c('Forest','House','Park'))+
  theme(axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        legend.title = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))



EX_Angst_plot


# Gefahr Sum
long$X = long$danger_items

EX_LMER_danger <- lmer(X ~ ex_condition + (1 | ID),
                       data=long[long$danger_items_isoutlier == 0,] ,na.action = na.omit)
tab_model(EX_LMER_danger, show.se = TRUE)
summary(EX_LMER_danger)
anova(EX_LMER_danger)

tab_model(EX_LMER_danger, show.se = TRUE, title = "Danger items", 
          dv.labels = c("Total score of the danger items"))

hist(residuals(EX_LMER_danger))
qqnorm(residuals(EX_LMER_danger)); qqline(residuals(EX_LMER_danger))

# Package lsmean: Post Hoc analysen - multiple comparison control: none!
emmeans(EX_LMER_danger, list(pairwise ~ ex_condition), adjust = "none")

# Figure 5
EX_Gefahr_pic <- effects::effect(term= "ex_condition", mod= EX_LMER_danger)
summary(EX_Gefahr_pic)
x_EX_Gefahr_pic <- as.data.frame(EX_Gefahr_pic)

EX_Gefahr_plot <- ggplot() + 
  geom_violin(data=long[long$danger_items_isoutlier == 0,], aes(ex_condition, danger_items, fill=ex_condition)) + 
  scale_fill_manual(values=c("darkseagreen2", "azure3", "lemonchiffon"), labels = c('Forest','House','Park'))+
  geom_point(data=x_EX_Gefahr_pic, aes(x=ex_condition, y=fit), size= 2, color="red") +
  geom_errorbar(data= x_EX_Gefahr_pic, aes(x=ex_condition, ymin=lower, ymax=upper), size= 0.3, color="black") +
  theme_classic2()+
  labs(title = "Danger items", subtitle = "
  
       
       ", 
       x="Condition", y="Total score danger items", fill = "Conditions") + 
  scale_x_discrete(labels = c('Forest','House','Park'))+
  theme(axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        legend.title = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        legend.text = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))



EX_Gefahr_plot

#####################################################
# implicit Tasks

# SPP_diff: 
long$X = long$SPP_diff

SPP_p_lmer <- lmer(X ~ condition + (1 | ID),
             data = long[long$SPP_RT_isoutlier == 0 & long$SPP_propcorrect >= "0.75" & long$SPP_diff_isoutlier == 0 &
                        long$display.refreshrate >= 45 &
                         long$group == "park",], na.action = na.omit)

tab_model(SPP_p_lmer, show.se = TRUE)
summary(SPP_p_lmer)
plot(check_distribution(SPP_p_lmer))

#Group == house
SPP_h_lmer <- lmer(X ~ condition + (1 | ID),
                   data = long[long$SPP_RT_isoutlier == 0 & long$SPP_propcorrect >= "0.75" & long$SPP_diff_isoutlier == 0 &
                              long$display.refreshrate >= 45 &
                                 long$group == "house",], na.action = na.omit)

tab_model(SPP_h_lmer, show.se = TRUE)
summary(SPP_h_lmer)
plot(check_distribution(SPP_h_lmer))

#tab Sx (supplementary material)
tab_model(SPP_p_lmer, SPP_h_lmer, dv.labels = c("(A) forest vs park", "(B) forest vs house"),
          title = "Subliminal Priming Procedure - different score", file = "plot_spp_models.html")

#library(webshot2)
#webshot("plot_spp_models.html", "plot_spp_models.png")

#plot_models(SPP_p_lmer, SPP_h_lmer)
#####################################################
# AMP_A: #inverse - die Skala einmal umgedreht
long$X = long$AMP_fear_inverse
#Group == park
AMP_p_inverse_lmer <- lmer(X ~ condition + EX_Valence + (1 | ID),
                   data = long[long$Chinesisch == 0 & long$AMP_fear_inverse_isoutlier == 0 &
                                 long$group == "park",], na.action = na.omit)

tab_model(AMP_p_inverse_lmer, show.se = TRUE, show.ci = 0.95)
summary(AMP_p_inverse_lmer)
plot(check_distribution(AMP_p_inverse_lmer))

#Group == house
AMP_h_inverse_lmer <- lmer(X ~ condition + EX_Valence + (1 | ID),
                   data = long[long$Chinesisch == 0 & long$AMP_fear_inverse_isoutlier == 0 &
                                 long$group == "house",], na.action = na.omit)

tab_model(AMP_h_inverse_lmer, show.se = TRUE)
summary(AMP_h_inverse_lmer)
plot(check_distribution(AMP_h_inverse_lmer))

#tab Sx (supplementary material)
tab_model(AMP_p_inverse_lmer, AMP_h_inverse_lmer, show.se = TRUE, dv.labels = c("(A) forest vs park", "(B) forest vs house"), 
          title = "Affect Misattribution Procedure - fear", file = "plot_amp_models.html")

## AAT-Diff

long$X = long$AAT_diff

AAT_p_lmer <- lmer(X ~ condition + EX_Valence + (1 | ID),
                   data = long[long$AAT_propcorrect_overall >= "0.75" & long$AAT_diff_isoutlier == 0 & long$AAT_RT_isoutlier == 0 &
                               long$group == "park",], na.action = na.omit)

tab_model(AAT_p_lmer, show.se = TRUE)
summary(AAT_p_lmer)
plot(check_distribution(AAT_p_lmer))

AAT_h_lmer <- lmer(X ~ condition + EX_Valence + (1 | ID),
                   data = long[long$AAT_propcorrect_overall >= "0.75" & long$AAT_diff_isoutlier == 0 & long$AAT_RT_isoutlier == 0 &
                                 long$group == "house",], na.action = na.omit)

tab_model(AAT_h_lmer, show.se = TRUE)
summary(AAT_h_lmer)
plot(check_distribution(AAT_h_lmer))

#tab Sx (supplementary material)
tab_model(AAT_p_lmer, AAT_h_lmer, dv.labels = c("(A) forest vs park", "(B) forest vs house"), 
          title = "Approach-Avoidance Task - different score", file = "plot_aat_models.html")


##############Correction for multiple comparison ###################################################
#Benjamini Hochberg - FDR

implicit_p <- c(0.008, 0.221, 0.017, 0.337, 0.392, 0.003)
p.adjust(implicit_p, method = "BH")

explicit_p <- c(0.9298, .0001, .0001,.0001,.0001,.0001,.0001,0.0002, 0.1797)
p.adjust(explicit_p, method = "BH")


#########################################
# Supplement Analyses

#SECURITY
# implicit Model: SECURITY
long$X = long$AMP_Dominance

AMP_p_security_lmer <- lmer(X ~ condition + (1 | ID),
                           data = long[long$Chinesisch == 0 & long$AMP_Dominance_isoutlier == 0 &
                                         long$group == "park",], na.action = na.omit)

AMP_h_security_lmer <- lmer(X ~ condition + (1 | ID),
                           data = long[long$Chinesisch == 0 & long$AMP_Dominance_isoutlier == 0 &
                                         long$group == "house",], na.action = na.omit)

#tab Sx (supplementary material)
tab_model(AMP_p_security_lmer, AMP_h_security_lmer, show.se = TRUE, dv.labels = c("(A) forest vs park", "(B) forest vs house"), 
          title = "Affect Misattribution Procedure - security")

#Explisit Dominance
long$X = long$EX_Dominance

EX_LMER_D <- lmer(X ~ ex_condition + (1 | ID),
                  data=long[long$EX_Dominance_isoutlier == 0,] ,na.action = na.omit)

tab_model(EX_LMER_D, show.se = TRUE, title = "Explicit picture rating", 
          dv.labels = c("Mean security score"))

# Package lsmean: Post Hoc analysen - multiple comparison control: none!
emmeans(EX_LMER_D, list(pairwise ~ ex_condition), adjust = "none")


# VALENCE
# implicit Model: VALENCE
long$X = long$AMP_Valence

AMP_p_valence_lmer <- lmer(X ~ condition + (1 | ID),
                            data = long[long$Chinesisch == 0 & long$AMP_Valence_isoutlier == 0 &
                                          long$group == "park",], na.action = na.omit)


AMP_h_valence_lmer <- lmer(X ~ condition + (1 | ID),
                            data = long[long$Chinesisch == 0 & long$AMP_Valence_isoutlier == 0 &
                                          long$group == "house",], na.action = na.omit)

#tab Sx (supplementary material)
tab_model(AMP_p_valence_lmer, AMP_h_valence_lmer, show.se = TRUE, dv.labels = c("(A) forest vs park", "(B) forest vs house"), 
          title = "Affect Misattribution Procedure - valence")

#Explisit Dominance
long$X = long$EX_Valence

EX_LMER_V <- lmer(X ~ ex_condition + (1 | ID),
                  data=long[long$EX_Valence_isoutlier == 0,] ,na.action = na.omit)

tab_model(EX_LMER_V, show.se = TRUE, title = "Explicit picture rating", 
          dv.labels = c("Mean valence score"))

# Package lsmean: Post Hoc analysen - multiple comparison control: none!
emmeans(EX_LMER_V, list(pairwise ~ ex_condition), adjust = "none")


# Deskriptive Stichprobenbeschreibung
Sample <- subset(long, select = c(group, 
                Alter, Sex, Bildungsjahre,  
                AMP_Valence, AMP_Dominance, AMP_fear_inverse,
                EX_Valence, EX_Dominance, EX_fear_inverse,
                fear_items,danger_items, 
                AAT_diff, SPP_diff))

Tab <- describeBy(Sample, Sample$group,  na.rm = TRUE) 
tab_dfs(Tab, title= c("House", "Park"), digits = 3, show.rownames = TRUE)
