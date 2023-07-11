
setwd("C:/Users/Nathan/Downloads/livros economia/estatistica/puc/econometria 4")
library(stats)
library(readxl)
#a
returns<-read_excel("returns.xlsx")
pca<-returns[,-(1:17)]
pca_pad<-scale(returns[,-(1:17)])
pca_result <- prcomp(pca)
pca_pad_result <- prcomp(pca_pad)
plot(pca_result$x[,1], pca_result$x[,2], xlab = "PC1", ylab = "PC2")

#otimo
#Informal Way

prop_var <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
prop_var_acum <- cumsum(prop_var)
plot(prop_var_acum, type = "b", xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada")

prop_pad_var <- (pca_pad_result$sdev^2) / sum(pca_pad_result$sdev^2)
prop_pad_var_acum <- cumsum(prop_pad_var)
plot(prop_pad_var_acum, type = "b", xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada")

######Rule of Thumb

for(i in 2:length(prop_var_acum)){
  if(prop_var_acum[i] -prop_var_acum[i-1]<0.03){
    thumb<-i-1
    stop()
  }

}
thumb
for(i in 2:length(prop_var_acum)){
  if(prop_pad_var_acum[i] -prop_pad_var_acum[i-1]<0.03){
    thumb_pad<-i-1
    stop()
  }
  
}
thumb_pad

#########Biggest Drop
autovalores<-pca_result$sdev
autovalores1<<-c(autovalores[-1],1)
biggest_drop<-which.max(autovalores/autovalores1)
biggest_drop

autovalores_pad<-pca_pad_result$sdev
autovalores1_pad<<-c(autovalores_pad[-1],1)
biggest_drop_pad<-which.max(autovalores_pad/autovalores1_pad)
biggest_drop_pad


#### By the Rule of Thumb we select the 2 first PCs. By the PCA of the standardized variables the 2 first PCs explain:
print(summary_pca$importance["Cumulative Proportion","PC2"])
##### 0.09503 of the variance.

#### b)
# Regressing PC1 on the 16 anomaly factors:
PC1 <- summary_pca$x[,'PC1']
PC1_factors <- cbind(PC1, returns[,2:17])

rgrss_pc1_factors <- lm(PC1 ~ ., data = PC1_factors)
summary(rgrss_pc1_factors)

#Regressing PC2 on the 16 factors:
PC2 <- summary_pca$x[,'PC2']
PC2_factors <- cbind(PC2, returns[,2:17])

rgrss_pc2_factors <- lm(PC2 ~ ., data = PC2_factors)
summary(rgrss_pc2_factors)
# How they relate? The R² and adj. R² of the PC1 regression is 0.954 and 0.9516, and for 
# the PC2 regression they are 0.6353 and 0.6164. So the anomaly factors contain almost all
# of the information of the first PC. The R²s are lower on the PC2 regression, but still are considerably high.
# The only anomaly factor that has a p-value lower than 0.001 in both regressions is the Market (MKT) factor.
# This can be interpreted buy the fact that the principal compenents are a linear combination of the returns
# of all firms, so makes sense the stock market return providing a good fit.
