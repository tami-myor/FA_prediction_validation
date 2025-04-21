library(dplyr)
library(tableone)

############
### IFPS ###
############

df <- read.csv("C:\\Users\\User\\Desktop\\FA\\Validation\\ifps_pred.csv")

table(df$FA_6y)


## Table 1 ##
df$parent_atopic <- ifelse(df$Maternal.atopic==1 | df$Paternal.atopic==1,1,0)
df$sibling_atpoic <- ifelse(df$prec_sibling>0,1,0)



var_list <- c("Gender","Urban", "RACE", 
              "Autumn","Spring","Summer","Winter", 
              "Infant.Atopic.Dermatitis","Infant.Systemic.Antibiotics","Smoking",          
              "Sibling.FA","prec_sibling","sibling_atpoic","siblings",
              "First.Born","Parent.atopic.condition"        
              )

fac_list <- c("Gender","Urban", "RACE", 
              "Autumn","Spring","Summer","Winter", 
              "Infant.Atopic.Dermatitis","Infant.Systemic.Antibiotics","Smoking",          
              "Sibling.FA","sibling_atpoic",  
              "First.Born"        
)


cont_list <- c("prec_sibling","Parent.atopic.condition","siblings")


tab_c <- CreateTableOne(vars = var_list, 
                        factorVars=fac_list, 
                        strata ="FA_6y", 
                        data = df)

kableone(tab_c)
print(tab_c, nonnormal = cont_list)

t.test(FA6Y$Antibiotics_During_Pregnancy, FA6Y$Cohort, p.adjust.methods="bonfferoni")
t.test(FA6Y$Parent_Atopic_Condition, FA6Y$Cohort, p.adjust.methods="bonfferoni")
t.test(FA6Y$Percent_of_Siblings_with_Atopic~FA6Y$Cohort, p.adjust.methods="bonferroni")

p1 <- FA6Y[FA6Y$Cohort==1,"Percent_of_Siblings_with_Atopic"]
p2 <- FA6Y[FA6Y$Cohort==0,"Percent_of_Siblings_with_Atopic"]
p.unadj <- c(p1,p2)
p.bonf <- p.adjust(p.unadj, method = "bonferroni")
round(p.bonf, 3)

t.test(FA6Y$Antibiotics_During_Pregnancy~FA6Y$Cohort, p.adjust.methods="bonferroni")
t.test(FA6Y$Parent_Atopic_Condition~FA6Y$Cohort, p.adjust.methods="bonferroni")

wilcox.test(FA6Y$Percent_of_Siblings_with_Atopic~FA6Y$Cohort, pairwise.wilcox.test=FALSE)
wilcox.test(FA6Y$Number.of.Siblings~FA6Y$Cohort)

## Effect size ##
library(effectsize)
df$Cohort <- as.factor(df$FA_6y)

chisq.test(FA6Y$First.Born,FA6Y$Cohort)[3]
cramers_v(FA6Y$First.Born,FA6Y$Cohort)

t.test(FA6Y$Percent.atopic.sblng~FA6Y$Cohort)
wilcox.test(FA6Y$Percent.atopic.sblng~FA6Y$Cohort)
cohens_d(FA6Y$Percent.atopic.sblng~FA6Y$Cohort)


# p-value & effect size for cat. vars: chi-square & cramers_v
res_list <- c()
CI_low <- c()
CI_high <- c()
p_value <- c()

for (i in fac_list) {
  res_list[i] <- c(cramers_v(df[[i]],df$Cohort)$Cramers_v_adjusted)
  CI_low[i] <- c(cramers_v(df[[i]],df$Cohort)$CI_low)
  CI_high[i] <- c(cramers_v(df[[i]],df$Cohort)$CI_high) 
  p_value[i] <- c(chisq.test(df[[i]],df$Cohort)$p.value)
  res_table <- cbind(res_list[-1], CI_low[-1], CI_high[-1], p_value)
}


# p-value & effect size for cont. vars: wilcox & cohens_d

res_listc <- c()
CI_lowc<- c()
CI_highc<- c()
p_valuec<- c()

for (i in cont_list) {
  res_listc[i] <- c(cohens_d(df[[i]],df$Cohort)$Cohens_d)
  CI_lowc[i] <- c(cohens_d(df[[i]],df$Cohort)$CI_low)
  CI_highc[i] <- c(cohens_d(df[[i]],df$Cohort)$CI_high) 
  
  p_valuec[i] <- c(wilcox.test(df[[i]]~df$Cohort)$p.value)
  res_table <- cbind(res_listc, CI_lowc, CI_highc, p_valuec)
}
res_table
