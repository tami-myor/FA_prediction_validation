
library(dplyr)
library(ggplot2)
library(reshape2)

auc <- read.csv('C:\\Users\\User\\Desktop\\FA\\Validation\\auc_bootstrap.csv')
auc <- auc[,-1]

auc <- auc %>% rename('Full Model 6Y' = FULL,
                    'EMR Model 6Y' = No.SES,
                    'Compact Model 6Y' = No.prg,
                   'Parental Atopy' = Parental.Atopic,
                   'Infant AD' = Infant.AD,
                   'Full Model 1Y' = X1Y.FULL,
                   'Compact Model 1Y' = X1Y.NoPrg,
                   'Parental Atopy IFPS'= ParentalAtopic_val,
                   'Infant AD IFPS'= InfantAD_val,
                   'Compact Validation 1Y' = X1Y_val,
                   'Compact Validation 6Y'  = X6Y_val)


df <- melt(auc)
df$end_year <- ifelse(grepl("1Y", df$variable),"1Y","6Y")
df$Type <- ifelse(grepl("Validation", df$variable),"Validation",
                  ifelse((grepl("Parental", df$variable)|grepl("Infant", df$variable)),"SOC","Algorithm"))
df$end_year <- as.factor(df$end_year)
df$validation <- as.factor(df$validation)


ggplot(df, aes(x=value, y=variable, fill=Type)) +
  geom_violin(trim=FALSE) +
  coord_flip() +
  geom_boxplot(width=0.2) +
  facet_grid(. ~ end_year, scale="free") +
  ggtitle("AUC results with bootstrapping") +
  xlab("AUC Score") + ylab("Algorithm") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette="Blues") + # Pastel1 +
  geom_vline(xintercept=0.5, linetype="dashed", color = "black")

### Leumit results only: ###
leumit_alg <- df[df$variable %in% c("Full Model 6Y","EMR Model 6Y","Compact Model 6Y","Parental Atopy","Infant AD","Full Model 1Y","Compact Model 1Y"),]


ggplot(leumit_alg, aes(x=value, 
                       y=factor(variable, levels = c("Full Model 6Y","EMR Model 6Y","Compact Model 6Y","Full Model 1Y","Compact Model 1Y","Parental Atopy","Infant AD")),
                         fill=Type)) +
  geom_violin(trim=FALSE) +
  coord_flip() +
  geom_boxplot(width=0.2) +
  ggtitle("MyorCare FA Algorithms", subtitle = "AUC results with bootstrapping") +
  xlab("AUC Score") + ylab("Algorithm") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette="Blues") + # Pastel1 +
  geom_vline(xintercept=0.5, linetype="dashed", color = "black") 

## Graph with manual colors
manual_colors <- c("royalblue4","cornflowerblue","slategray2","thistle","plum","forestgreen","seagreen3")
ggplot(leumit_alg, aes(x=value, 
                       y=factor(variable, levels = c("Full Model 6Y","EMR Model 6Y","Compact Model 6Y",
                                                     "Full Model 1Y","Compact Model 1Y","Parental Atopy","Infant AD")),
                       fill=variable
                       ))+
  geom_violin(trim=FALSE) +
  coord_flip() +
  geom_boxplot(width=0.2) +
  scale_fill_manual(values = manual_colors) +
  ggtitle("MyorCare FA Algorithms", subtitle = "AUC results with bootstrapping") +
  xlab("AUC Score") + ylab("Algorithm") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none") +
  geom_vline(xintercept=0.5, linetype="dashed", color = "black") 




### 6Y results only: ###
auc_6y <- df[df$variable %in% c("Compact Model 6Y","Compact Validation 6Y", #"Parental Atopy","Infant AD",
                                'Parental Atopy IFPS','Infant AD IFPS'),]

ggplot(auc_6y, aes(x=value, 
                   y=factor(variable, levels = c("Compact Model 6Y","Compact Validation 6Y",
                                                 'Parental Atopy IFPS','Infant AD IFPS')),
                   fill=Type)) +
  geom_violin(trim=FALSE) +
  coord_flip() +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  ggtitle("MyorCare FA 6Y Algorithm & IFPS Validation", subtitle = "AUC results with bootstrapping") +
  xlab("AUC Score") + ylab("Algorithm") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette="Blues") + # Pastel1 +
  geom_vline(xintercept=0.5, linetype="dashed", color = "black") 


manual_colors2 <- c("cornflowerblue","thistle","plum","forestgreen")


ggplot(auc_6y, aes(x=value, 
                   y=factor(variable, levels = c("Compact Model 6Y","Compact Validation 6Y",
                                                 'Parental Atopy IFPS','Infant AD IFPS')),
                   fill=variable)) +
  geom_violin(trim=FALSE) +
  coord_flip() +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  scale_fill_manual(values = manual_colors2) +
  ggtitle("MyorCare FA 6Y Algorithm & IFPS Validation", subtitle = "AUC results with bootstrapping") +
  xlab("AUC Score") + ylab("Algorithm") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none") +
  geom_vline(xintercept=0.5, linetype="dashed", color = "black") 


# 1Y validation + soc :

auc_1y <- df[df$variable %in% c("Compact Model 1Y","Compact Validation 1Y", "Parental Atopy","Infant AD"),]
                              #  'Parental Atopy IFPS','Infant AD IFPS'),]

ggplot(auc_1y, aes(x=value, 
                   y=factor(variable, levels = c("Compact Model 1Y","Compact Validation 1Y",
                                                 "Parental Atopy","Infant AD")),
                   fill=variable)) +
  geom_violin(trim=FALSE) +
  coord_flip() +
  geom_boxplot(width=0.1, outlier.shape = NA) +
  scale_fill_manual(values = manual_colors2) +
  ggtitle("MyorCare FA 1Y Algorithm & IFPS Validation", subtitle = "AUC results with bootstrapping") +
  xlab("AUC Score") + ylab("Algorithm") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none") +
  geom_vline(xintercept=0.5, linetype="dashed", color = "black") 


############################################ 
### Statistical test fro results comparison:

df$Model <- ifelse(df$variable %in% c("Compact Model 6Y","Compact Validation 6Y"), "Compact 6Y",
                     ifelse(df$variable %in% c("Compact Model 1Y","Compact Validation 1Y"), "Compact 1Y", 
                            ifelse(df$variable %in% c("Parental Atopy","Infant AD","Parental Atopy IFPS","Infant AD IFPS"), 
                                   "SOC", "else")))

df$Dataset <- ifelse(df$variable %in% c("Compact Validation 6Y","Compact Validation 1Y","Parental Atopy IFPS","Infant AD IFPS"),
                     "External Validation","Test")

df$Model <- as.factor(df$Model)
df$Dataset <- as.factor(df$Dataset)



# Repeated Measures ANOVA
library(ez)  

anova_DF <- df[df$Model!="else",]

ggplot(anova_DF, aes(x=value, fill=Model)) +
  geom_density()

ggplot(anova_DF, aes(x=value, fill=Dataset)) +
  geom_density()

ggplot(anova_DF, aes(x=value, fill=variable)) +
  geom_density()

anova_DF <- anova_DF[anova_DF$Model!="SOC",]


# Perform ANOVA
anova_results <- ezANOVA(
  data = anova_DF,
  dv = value,          # Dependent variable (AUC)
  wid = Model,       # Within-subject variable (Model)
  within = Dataset   # Repeated-measures factor (Test vs. External Validation)
)

anova_results


pairwise.t.test(anova_DF$value, anova_DF$Dataset, paired = TRUE, p.adjust.method = "bonferroni")
#pairwise.t.test(anova_DF$value, anova_DF$Model, paired = TRUE, p.adjust.method = "bonferroni")

# AUC trends for each model across the test and external validation datasets
ggplot(anova_DF, aes(x = Dataset, y = value, group = Model, color = Model)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "AUC Comparison Across Models and Datasets",
       x = "Dataset", y = "AUC") +
  theme_minimal() +
  stat_compare_means(paired = TRUE)

#  stat_compare_means(aes(label = ..p.signif..),paired = TRUE)
  

library(ggpubr)
compare_means(len ~ supp, data = ToothGrowth, paired = TRUE)
stat_compare_means(paired = TRUE)

compare_means(value ~ Model,  data = anova_DF, method = "anova")


# Friedman test for non parametric data:

anova_DF %>% group_by(Model, Dataset) %>% summarise(AUC = mean(value))


### Delong test:
library(pROC)

# AUCs from two models
model_1y <- df[df$variable=="Compact Model 1Y","value"]
model_6y <- df[df$variable=="Compact Model 6Y","value"]
val_1y <- df[df$variable=="Compact Validation 1Y","value"]
val_6y <- df[df$variable=="Compact Validation 6Y","value"]


## paired t-test :
t_test <- t.test(df[df$variable=="Compact Model 6Y","value"],df[df$variable=="Compact Model 1Y","value"], paired = TRUE)
print(t_test) #  p-value < 2.2e-16
t_test <- t.test(df[df$variable=="Compact Validation 1Y","value"],df[df$variable=="Compact Validation 6Y","value"], paired = TRUE)
print(t_test) #  p-value < 2.2e-16
t_test <- t.test(df[df$variable=="Compact Model 6Y","value"],df[df$variable=="Compact Validation 6Y","value"], paired = F)
print(t_test) #  p-value < 2.2e-16
t_test <- t.test(df[df$variable=="Compact Validation 1Y","value"],df[df$variable=="Compact Model 1Y","value"], paired = F)
print(t_test) #  p-value < 2.2e-16

## Wilcoxon Signed-Rank Test
wilcoxon_test <- wilcox.test(model_1y, model_6y, paired = TRUE)
print(wilcoxon_test)
wilcoxon_test <- wilcox.test(val_1y, val_6y, paired = TRUE)
print(wilcoxon_test)
wilcoxon_test <- wilcox.test(model_1y, val_1y, paired = TRUE)
print(wilcoxon_test)
wilcoxon_test <- wilcox.test(model_6y, val_6y, paired = TRUE)
print(wilcoxon_test)


wilcox.test(0.8, auc$`Full Model 6Y`, paired = F, alternative ="greater") #p-value = 0.08388
wilcox.test(0.8, auc$`Compact Model 6Y` , paired = F, alternative ="less") #p-value = 0.7358
wilcox.test(0.8, auc$`Compact Model 1Y` , paired = F, alternative ="less") #p-value = 0.09915
wilcox.test(0.8, auc$`Compact Validation 1Y`, paired = F, alternative ="less") #p-value = 0.08388
wilcox.test(0.8, auc$`Compact Validation 6Y`, paired = F, alternative ="less") #p-value = 0.0845

# Normailty tests:
x <- rnorm(1000, 0.8)
ks.test(x, 'pnorm')
shapiro.test(x)
qqnorm(x)
hist(x)

ks.test(auc$`Compact Model 6Y`, 'pnorm')
shapiro.test(auc$`Compact Model 6Y`)
qqnorm(auc$`Compact Model 6Y`)
hist(auc$`Compact Model 6Y`)
ks.test(auc$`Compact Model 1Y`, 'pnorm') 
shapiro.test(auc$`Compact Model 1Y`)
ks.test(auc$`Compact Validation 1Y`, 'pnorm') 
shapiro.test(auc$`Compact Validation 1Y`)
ks.test(auc$`Compact Validation 6Y`, 'pnorm') 
shapiro.test(auc$`Compact Validation 6Y`)
qqnorm(auc$`Compact Validation 6Y`)
hist(auc$`Compact Validation 6Y`)
# Kolmogorov–Smirnov test is used for n ≥50

#one-sample t-test
t_6y <- t.test(auc$`Compact Model 6Y`, mu=0.8) 
t_1y <- t.test(auc$`Compact Model 1Y`, mu=0.78) 
t_val6y <- t.test(auc$`Compact Validation 6Y`, mu=0.8) 
t_val1y <- t.test(auc$`Compact Validation 1Y`, mu=0.78) 

options(scipen = 999)
t_6y$p.value
t_6y$estimate
t_6y$conf.int
t_6y$conf.int[1]
t_6y$conf.int[2]

t_val1y$p.value
t_val1y$estimate
t_val1y$conf.int

t.test(auc$`Compact Model 6Y`, auc$`Compact Validation 6Y`)
t.test(auc$`Compact Model 1Y`, auc$`Compact Validation 1Y`)
t.test(auc$`Compact Model 6Y`, auc$`Compact Model 1Y`)


library(pROC)

# AUCs from two models
model1_auc <- c(0.85, 0.88, 0.86, 0.89, 0.87)  # Example AUCs for Model 1
model2_auc <- c(0.80, 0.84, 0.82, 0.85, 0.83)  # Example AUCs for Model 2

# Perform DeLong Test
roc_model1 <- roc(response = c(0,1,1,0,0,0), predictor = c(0,0,1,0,0,0))
roc_model2 <- roc(response = c(0,1,1,0,0,0), predictor = c(0,0,1,0,0,1))

delong_test <- roc.test(roc_model1, roc_model2, method = "delong")
print(delong_test)
