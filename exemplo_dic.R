# Análise de variância (ANOVA) para um experimento completamente casualizado (DIC)

# usando o pacote ExpDes (recomendado)
install.packages("ExpDes")
library("ExpDes")
trat <- rep(c("A", "B", "C", "D"), each = 5)
resp <- c(25, 26, 20, 23, 21,
          31, 25, 28, 27, 24,
          22, 26, 28, 25, 29,
          33, 29, 31, 34, 28)
crd(trat, resp)
qf(0.95, 3, 16)

# ------------------------------------------------------------------------
#   Analysis of Variance Table
# ------------------------------------------------------------------------
#   DF     SS     MS     Fc     Pr>Fc
# Treatament  3 163.75 54.583 7.7976 0.0019756
# Residuals  16 112.00  7.000                 
# Total      19 275.75                        
# ------------------------------------------------------------------------

# > qf(0.95, 3, 16)
# [1] 3.238872


# outra forma de realizar a ANOVA usando a funcao aov
anova_exemplo <- aov(resp ~ trat)
summary(anova_exemplo)

# usando a funcao lm e anova
mod_lm <- lm(resp ~ trat)
anova(mod_lm)


# usando a funcao oneway.test
oneway.test(resp ~ trat, var.equal = TRUE)

# usando o pacote easyanova
install.packages("easyanova")
library(easyanova)

# para DIC simples, a 1a coluna deve ser o tratamento
# e a última coluna a resposta
dados <- data.frame(trat, resp)
res_easy <- ea1(dados, design = 1)
res_easy

# Tarefa: procurar outras funções e pacotes para realizar a ANOVA em R.