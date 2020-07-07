install.packages('rpart')
install.packages('rpart.plot')
install.packages('RWeka')
install.packages('e1071')
install.packages('xgboost')
install.packages('readr')
install.packages('stringr')
install.packages('caret')
install.packages('car')

#Obter os conjuntos

library('RJDBC')
library('rpart')
library('rpart.plot')
library('caret')
library('RWeka')
library('e1071')
library('xgboost')
library('readr')
library('stringr')
library('caret')
library('car')

setwd("C:/Users/ctcca/Documents/USP/POS_BigData/MOD4/Monografia/CodigoFonte")

#Conectar à base de dados
drv <- JDBC("oracle.jdbc.OracleDriver", classPath="ojdbc6.jar"," ")
con <- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:xe", "CARTOLAFC","CARTOLAFC")

conjunto1 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA1")
conjunto2 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA2")
conjunto3 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA3")
conjunto4 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA4")
conjunto5 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA5")

conjunto1$ID_POSICAO <- as.factor(conjunto1$ID_POSICAO)
conjunto1$ULT_RESULT_CLUBE <- as.factor(conjunto1$ULT_RESULT_CLUBE)
conjunto1$ULT_RESULT_ADV <- as.factor(conjunto1$ULT_RESULT_ADV)
conjunto1$ENTRE_MELHORES <- as.factor(conjunto1$ENTRE_MELHORES)
conjunto1$QUINTIL <- as.factor(conjunto1$QUINTIL)

conjunto4$ENTRE_MELHORES <- as.factor(conjunto4$ENTRE_MELHORES)
conjunto4$QUINTIL <- as.factor(conjunto4$QUINTIL)

rodadasTeste <- c(6, 8, 12, 16, 20, 24, 26, 28, 32, 36)

'%notin%' <- Negate('%in%')

#Separar conjuntos 1
treino1 <- conjunto1[conjunto1$RODADA %notin% rodadasTeste,]
teste1 <- conjunto1[conjunto1$RODADA %in% rodadasTeste,]

rows_sample <- rownames(treino1[treino1$ENTRE_MELHORES=="1",])
rows_sample <- union(rows_sample, sample(rownames(treino1[treino1$ENTRE_MELHORES=="0",]), size=1519, replace=F))
treino1_bal <- subset(conjunto1,rownames(conjunto1) %in% rows_sample)

#Separar conjuntos 2
treino2 <- conjunto2[conjunto2$RODADA %notin% rodadasTeste,]
teste2 <- conjunto2[conjunto2$RODADA %in% rodadasTeste,]

#Separar conjuntos 3
treino3 <- conjunto3[conjunto3$RODADA %notin% rodadasTeste,]
teste3 <- conjunto3[conjunto3$RODADA %in% rodadasTeste,]

#Separar conjuntos 4
treino4 <- conjunto4[conjunto4$RODADA %notin% rodadasTeste,]
teste4 <- conjunto4[conjunto4$RODADA %in% rodadasTeste,]

rows_sample4 <- rownames(treino4[treino4$ENTRE_MELHORES=="1",])
rows_sample4 <- union(rows_sample4, sample(rownames(treino4[treino4$ENTRE_MELHORES=="0",]), size=1519, replace=F))
treino4_bal <- subset(conjunto4,rownames(conjunto4) %in% rows_sample4)

#Separar conjuntos 5
treino5 <- conjunto5[conjunto5$RODADA %notin% rodadasTeste,]
teste5 <- conjunto5[conjunto5$RODADA %in% rodadasTeste,]

###################### Árvore de decisão ######################
#Para árvore de decisão serão usados os conjuntos C1 e C4

set.seed(42)

### Classes desbalanceadas
modelo_c1 <- rpart(
  ENTRE_MELHORES ~ ID_POSICAO + RODADA + PRECO_INI_RODADA + MEDIA_ULT3_VARI + MEDIA_INI_RODADA + ULT_PONTUACAO + MEDIA_ULT3_PONT + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
  data = treino1,
  method = "class",
  control = rpart.control(minsplit=6, minbucket=1, cp=-1),
  parms = list(split = "Information"))

pred_c1 <- predict(modelo_c1, teste1, "vector")
pred_c1 <- pred_c1-1
pred_c1 <- as.data.frame(pred_c1)
pred_c1$pred_c1 <- as.factor(pred_c1$pred_c1)

#Matriz de Confusao
confusionMatrix(pred_c1$pred_c1, teste1$ENTRE_MELHORES)


### Melhor Modelo até agora -> Usando os quintis
modelo_c1_a <- rpart(
  QUINTIL ~ ID_POSICAO + RODADA + PRECO_INI_RODADA + MEDIA_ULT3_VARI + MEDIA_INI_RODADA + ULT_PONTUACAO + MEDIA_ULT3_PONT + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
  data = treino1,
  method = "class",
  control = rpart.control(minsplit=5, minbucket=1, cp=0.001),
  parms = list(split = "Information"))

pred_c1_a <- predict(modelo_c1_a, teste1, "vector")
pred_c1_a <- as.data.frame(pred_c1_a)
pred_c1_a$pred_c1_a <- as.factor(pred_c1_a$pred_c1_a)

#Matriz de Confusao
confusionMatrix(pred_c1_a$pred_c1_a, teste1$QUINTIL)

### Usar classificação binária balanceada
modelo_c1_b <- rpart(
  ENTRE_MELHORES ~ ID_POSICAO + RODADA + PRECO_INI_RODADA + MEDIA_ULT3_VARI + MEDIA_INI_RODADA + ULT_PONTUACAO + MEDIA_ULT3_PONT + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
  data = treino1_bal,
  method = "class",
  control = rpart.control(minsplit=5, minbucket=2, cp=0.1),
  parms = list(split = "Information"))

pred_c1_b <- predict(modelo_c1_b, teste1, "vector")
pred_c1_b <- pred_c1_b-1
pred_c1_b <- as.data.frame(pred_c1_b)
pred_c1_b$pred_c1_b <- as.factor(pred_c1_b$pred_c1_b)

#Matriz de Confusao
confusionMatrix(pred_c1_b$pred_c1_b, teste1$ENTRE_MELHORES)

### Modelo com PCA
modelo_c4 <- rpart(
  ENTRE_MELHORES ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
  data = treino4,
  method = "class",
  control = rpart.control(minsplit=6, minbucket=1, cp=-1),
  parms = list(split="Information"))

pred_c4 <- predict(modelo_c4, teste4, "vector")
pred_c4 <- pred_c4-1
pred_c4 <- as.data.frame(pred_c4)
pred_c4$pred_c4 <- as.factor(pred_c4$pred_c4)

#Matriz de Confusao
confusionMatrix(pred_c4$pred_c4, teste4$ENTRE_MELHORES)

### Modelo com PCA usando os quintis
modelo_c4_a <- rpart(
  QUINTIL ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
  data = treino4,
  method = "class",
  control = rpart.control(minsplit=5, minbucket=1, cp=0.00085),
  parms = list(split = "Information"))
  
pred_c4_a <- predict(modelo_c4_a, teste4, "vector")
pred_c4_a <- as.data.frame(pred_c4_a)
pred_c4_a$pred_c4_a <- as.factor(pred_c4_a$pred_c4_a)

#Matriz de Confusao
confusionMatrix(pred_c4_a$pred_c4_a, teste4$QUINTIL)

### Classificação Binária Balanceada
modelo_c4_b <- rpart(
  ENTRE_MELHORES ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
  data = treino4_bal,
  method = "class",
  control = rpart.control(minsplit=6, minbucket=1, cp=-1),
  parms = list(split="Information"))

pred_c4_b <- predict(modelo_c4_b, teste4, "vector")
pred_c4_b <- pred_c4_b-1
pred_c4_b <- as.data.frame(pred_c4_b)
pred_c4_b$pred_c4_b <- as.factor(pred_c4_b$pred_c4_b)

#Matriz de Confusao
confusionMatrix(pred_c4_b$pred_c4_b, teste4$ENTRE_MELHORES)

################### Árvore de decisão RWeka ###################

wekamod_c1 <- J48(
  ENTRE_MELHORES ~ ID_POSICAO + RODADA + PRECO_INI_RODADA + MEDIA_ULT3_VARI + MEDIA_INI_RODADA + ULT_PONTUACAO + MEDIA_ULT3_PONT + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
  treino1,
  control = Weka_control(R = TRUE, N = 10, B = FALSE, M = 1),
  options = NULL
)

summary(wekamod_c1)

#Predição
wekapred_c1 <- as.data.frame(predict(wekamod_c1, teste1))
summary(wekapred_c1)
confusionMatrix(wekapred_c1[,1], teste1$ENTRE_MELHORES)

### Usando os quintis para classificacao
wekamod_c1_b <- J48(
  QUINTIL ~ ID_POSICAO + RODADA + PRECO_INI_RODADA + MEDIA_ULT3_VARI + MEDIA_INI_RODADA + ULT_PONTUACAO + MEDIA_ULT3_PONT + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
  treino1,
  control = Weka_control(R = TRUE, N = 5, B = FALSE, M = 2),
  options = NULL
)

summary(wekamod_c1_b)

#Predição
wekapred_c1_b <- as.data.frame(predict(wekamod_c1_b, teste1))
summary(wekapred_c1_b)
confusionMatrix(wekapred_c1_b[,1], teste1$QUINTIL)

### Classificando com a distribuição balanceada
wekamod_c1_c <- J48(
  ENTRE_MELHORES ~ ID_POSICAO + RODADA + PRECO_INI_RODADA + MEDIA_ULT3_VARI + MEDIA_INI_RODADA + ULT_PONTUACAO + MEDIA_ULT3_PONT + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
  treino1_bal,
  control = Weka_control(R = TRUE),
  options = NULL
)

summary(wekamod_c1_c)

#Predição
wekapred_c1_C <- as.data.frame(predict(wekamod_c1_c, teste1))
summary(wekapred_c1_C)
confusionMatrix(wekapred_c1_C[,1], teste1$ENTRE_MELHORES)

### Modelar usando o resultado da PCA
wekamod_c4 <- J48(
  ENTRE_MELHORES ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
  treino4,
  control = Weka_control(R = FALSE, M = 1, C = 0.65),
  options = NULL
)

summary(wekamod_c4)

#Predição
wekapred_c4 <- as.data.frame(predict(wekamod_c4, teste4))
summary(wekapred_c4)
confusionMatrix(wekapred_c4[,1], teste4$ENTRE_MELHORES)

### Usando os quintis para classificacao
wekamod_c4_b <- J48(
  QUINTIL ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
  treino4,
  control = Weka_control(R = TRUE, N = 3, B = FALSE, M = 1),
  options = NULL
)

summary(wekamod_c4_b)

#Predição
wekapred_c4_b <- as.data.frame(predict(wekamod_c4_b, teste4))
summary(wekapred_c4_b)
confusionMatrix(wekapred_c4_b[,1], teste4$QUINTIL)

### Classificando com a distribuição balanceada
wekamod_c4_c <- J48(
  ENTRE_MELHORES ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
  treino4_bal,
  control = Weka_control(R = TRUE),
  options = NULL
)

summary(wekamod_c4_c)

#Predição
wekapred_c4_C <- as.data.frame(predict(wekamod_c4_c, teste4))
summary(wekapred_c4_C)
confusionMatrix(wekapred_c4_C[,1], teste4$ENTRE_MELHORES)

######################### Naive Bayes #########################

### Conjunto 1 -> Entre Melhores
nbmod_c1_em <- naiveBayes(treino1[,c(4:21)],treino1$ENTRE_MELHORES)

#predição
teste1$MANDO_CAMPO <- as.factor(teste1$MANDO_CAMPO)
nbpred_c1_em <- as.data.frame(predict(nbmod_c1_em, teste1[,c(4:21)], "class"))
confusionMatrix(nbpred_c1_em[,1], teste1$ENTRE_MELHORES)

### Conjunto 1 -> Quintil

nbmod_c1_qu <- naiveBayes(treino1[,c(4:21)], treino1$QUINTIL)

#predição
nbpred_c1_qu <- as.data.frame(predict(nbmod_c1_qu, teste1[,c(4:21)], "vector"))
confusionMatrix(nbpred_c1_qu[,1], teste1$QUINTIL)

### Conjunto 1B -> ENTRE_MELHORES

treino1_bal$MANDO_CAMPO <- as.factor(treino1_bal$MANDO_CAMPO)
nbmod_c1b_em <- naiveBayes(treino1_bal[,c(4:21)], treino1_bal$ENTRE_MELHORES)

#predição
nbpred_c1b_em <- as.data.frame(predict(nbmod_c1b_em, teste1[,c(4:21)], "class"))
confusionMatrix(nbpred_c1b_em[,1], teste1$ENTRE_MELHORES)

### Conjunto 4 -> ENTRE_MELHORES
nbmod_c4_em <- naiveBayes(treino4[,c(5:19)], treino4$ENTRE_MELHORES)

#predição
nbpred_c4_em <- as.data.frame(predict(nbmod_c4_em, teste4[,c(5:19)]), "class")
confusionMatrix(nbpred_c4_em[,1], teste4$ENTRE_MELHORES)

### COnjunto 4 -> QUINTIL
nbmod_c4_qu <- naiveBayes(treino4[,c(5:19)], treino4$QUINTIL)

#Prediçao
nbpred_c4_qu <- as.data.frame(predict(nbmod_c4_qu, teste4[,c(5:19)]), "class")
confusionMatrix(nbpred_c4_qu[,1], teste4$QUINTIL)

### Conjunto 4B -> ENTRE_MELHORES
nbmod_c4b_em <- naiveBayes(treino4_bal[,c(5:19)], treino4_bal$ENTRE_MELHORES)

#Predição
nbpred_c4b_em <- as.data.frame(predict(nbmod_c4b_em, teste4[,c(5:19)]), "class")
confusionMatrix(nbpred_c4b_em[,1], teste4$ENTRE_MELHORES)

#Transformar o conjunto 5 para Naive Bayes
treino5_f <- treino5
treino5_f$ID_POSICAO_ATA <- as.factor(treino5_f$ID_POSICAO_ATA)
treino5_f$ID_POSICAO_GOL <- as.factor(treino5_f$ID_POSICAO_GOL)
treino5_f$ID_POSICAO_LAT <- as.factor(treino5_f$ID_POSICAO_LAT)
treino5_f$ID_POSICAO_MEI <- as.factor(treino5_f$ID_POSICAO_MEI)
treino5_f$ID_POSICAO_ZAG <- as.factor(treino5_f$ID_POSICAO_ZAG)
treino5_f$ID_POSICAO_TEC <- as.factor(treino5_f$ID_POSICAO_TEC)
treino5_f$ULT_RESULT_CLUBE_E <- as.factor(treino5_f$ULT_RESULT_CLUBE_E)
treino5_f$ULT_RESULT_CLUBE_V <- as.factor(treino5_f$ULT_RESULT_CLUBE_V)
treino5_f$MANDO_CAMPO <- as.factor(treino5_f$MANDO_CAMPO)
treino5_f$ULT_RESULT_ADV_E <- as.factor(treino5_f$ULT_RESULT_ADV_E)
treino5_f$ULT_RESULT_ADV_V <- as.factor(treino5_f$ULT_RESULT_ADV_V)
treino5_f$ENTRE_MELHORES <- as.factor(treino5_f$ENTRE_MELHORES)
treino5_f$QUINTIL <- as.factor(treino5_f$QUINTIL)

teste5_f <- teste5
teste5_f$ID_POSICAO_ATA <- as.factor(teste5_f$ID_POSICAO_ATA)
teste5_f$ID_POSICAO_GOL <- as.factor(teste5_f$ID_POSICAO_GOL)
teste5_f$ID_POSICAO_LAT <- as.factor(teste5_f$ID_POSICAO_LAT)
teste5_f$ID_POSICAO_MEI <- as.factor(teste5_f$ID_POSICAO_MEI)
teste5_f$ID_POSICAO_ZAG <- as.factor(teste5_f$ID_POSICAO_ZAG)
teste5_f$ID_POSICAO_TEC <- as.factor(teste5_f$ID_POSICAO_TEC)
teste5_f$ULT_RESULT_CLUBE_E <- as.factor(teste5_f$ULT_RESULT_CLUBE_E)
teste5_f$ULT_RESULT_CLUBE_V <- as.factor(teste5_f$ULT_RESULT_CLUBE_V)
teste5_f$MANDO_CAMPO <- as.factor(teste5_f$MANDO_CAMPO)
teste5_f$ULT_RESULT_ADV_E <- as.factor(teste5_f$ULT_RESULT_ADV_E)
teste5_f$ULT_RESULT_ADV_V <- as.factor(teste5_f$ULT_RESULT_ADV_V)
teste5_f$ENTRE_MELHORES <- as.factor(teste5_f$ENTRE_MELHORES)
teste5_f$QUINTIL <- as.factor(teste5_f$QUINTIL)

### Conjunto 5 -> ENTRE_MELHORES
nbmod_c5_em <- naiveBayes(treino5_f[,c(5:27)], treino5_f$ENTRE_MELHORES)

#Predição
nbpred_c5_em <- as.data.frame(predict(nbmod_c5_em, teste5_f[,c(5:27)], "class"))
confusionMatrix(nbpred_c5_em[,1], teste5_f$ENTRE_MELHORES)

### Conjunto 5 -> QUINTIL
nbmod_c5_qu <- naiveBayes(treino5_f[,c(5:27)], treino5_f$QUINTIL)

#Predição
nbpred_c5_qu <- as.data.frame(predict(nbmod_c5_qu, teste5_f[,c(5:27)], "class"))
confusionMatrix(nbpred_c5_qu[,1], teste5_f$QUINTIL)

#################### Extreme Gradient Boost ###################
treino2$QUINTIL <- as.factor(treino2$QUINTIL)
treino2$ENTRE_MELHORES <- as.factor(treino2$ENTRE_MELHORES)
teste2$QUINTIL <- as.factor(teste2$QUINTIL)
teste2$ENTRE_MELHORES <- as.factor(teste2$ENTRE_MELHORES)

num_levels <- length(levels(teste2$ENTRE_MELHORES))

set.seed(42)
### Conjunto 2 -> Quintil
xgbmod_c2_qu <- xgboost(
  data = data.matrix(treino2[,c(4:30)]),
  label = treino2$QUINTIL,
  eta = 0.009,
  max_depth = 14,
  nround = 1000,
  subsample = 0.5,
  colsample_bytree = 1,
  seed = 42,
  eval_metric = "merror",
  objective = "multi:softprob",
  num_class = 6,
  nthread = 3,
  early_stopping_rounds = 20,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c2_qu <- predict(xgbmod_c2_qu, data.matrix(teste2[,c(4:30)]), reshape=T)
xgbpred_c2_qu <- as.data.frame(xgbpred_c2_qu)
xgbpred_c2_qu <- xgbpred_c2_qu[,-1]
colnames(xgbpred_c2_qu) <- levels(treino2$QUINTIL)
xgbpred_c2_qu <- apply(xgbpred_c2_qu,1,function(x) colnames(xgbpred_c2_qu)[which.max(x)])
xgbpred_c2_qu <- as.data.frame(xgbpred_c2_qu)

#Matriz de confusão
confusionMatrix(xgbpred_c2_qu[,1], teste2$QUINTIL)

treino3$QUINTIL <- as.factor(treino3$QUINTIL)
treino3$ENTRE_MELHORES <- as.factor(treino3$ENTRE_MELHORES)
teste3$QUINTIL <- as.factor(teste3$QUINTIL)
teste3$ENTRE_MELHORES <- as.factor(teste3$ENTRE_MELHORES)

### Conjunto 3 -> Quintil
xgbmod_c3_qu <- xgboost(
  data = data.matrix(treino3[,c(4:26)]),
  label = treino3$QUINTIL,
  eta = 0.009,
  max_depth = 14,
  nround = 1000,
  subsample = 0.5,
  colsample_bytree = 1,
  seed = 42,
  eval_metric = "merror",
  objective = "multi:softprob",
  num_class = 6,
  nthread = 3,
  early_stopping_rounds = 10,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c3_qu <- predict(xgbmod_c3_qu, data.matrix(teste3[,c(4:26)]), reshape=T)
xgbpred_c3_qu <- as.data.frame(xgbpred_c3_qu)
xgbpred_c3_qu <- xgbpred_c3_qu[,-1]
colnames(xgbpred_c3_qu) <- levels(treino3$QUINTIL)
xgbpred_c3_qu <- apply(xgbpred_c3_qu,1,function(x) colnames(xgbpred_c3_qu)[which.max(x)])
xgbpred_c3_qu <- as.data.frame(xgbpred_c3_qu)

#Matriz de confusão
confusionMatrix(xgbpred_c3_qu[,1], teste3$QUINTIL)

### Conjunto 4 -> Quintil
xgbmod_c4_qu <- xgboost(
  data = data.matrix(treino4[,c(5:19)]),
  label = treino4$QUINTIL,
  eta = 0.009,
  max_depth = 14,
  nround = 1000,
  subsample = 0.5,
  colsample_bytree = 1,
  seed = 42,
  eval_metric = "merror",
  objective = "multi:softprob",
  num_class = 6,
  nthread = 3,
  early_stopping_rounds = 10,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c4_qu <- predict(xgbmod_c4_qu, data.matrix(teste4[,c(5:19)]), reshape=T)
xgbpred_c4_qu <- as.data.frame(xgbpred_c4_qu)
xgbpred_c4_qu <- xgbpred_c4_qu[,-1]
colnames(xgbpred_c4_qu) <- levels(treino4$QUINTIL)
xgbpred_c4_qu <- apply(xgbpred_c4_qu,1,function(x) colnames(xgbpred_c4_qu)[which.max(x)])
xgbpred_c4_qu <- as.data.frame(xgbpred_c4_qu)

#Matriz de confusão
confusionMatrix(xgbpred_c4_qu[,1], teste4$QUINTIL)

treino5$QUINTIL <- as.factor(treino5$QUINTIL)
treino5$ENTRE_MELHORES <- as.factor(treino5$ENTRE_MELHORES)
teste5$QUINTIL <- as.factor(teste5$QUINTIL)
teste5$ENTRE_MELHORES <- as.factor(teste5$ENTRE_MELHORES)

set.seed(42)
### Conjunto 5 -> Quintil
xgbmod_c5_qu <- xgboost(
  data = data.matrix(treino5[,c(4:27)]),
  label = treino5$QUINTIL,
  eta = 0.01,
  max_depth = 15,
  nround = 1000,
  subsample = 0.5,
  colsample_bytree = 1,
  seed = 42,
  eval_metric = "merror",
  objective = "multi:softprob",
  num_class = 6,
  nthread = 3,
  early_stopping_rounds = 10,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c5_qu <- predict(xgbmod_c5_qu, data.matrix(teste5[,c(4:27)]), reshape=T)
xgbpred_c5_qu <- as.data.frame(xgbpred_c5_qu)
xgbpred_c5_qu <- xgbpred_c5_qu[,-1]
colnames(xgbpred_c5_qu) <- levels(treino5$QUINTIL)
xgbpred_c5_qu <- apply(xgbpred_c5_qu,1,function(x) colnames(xgbpred_c5_qu)[which.max(x)])
xgbpred_c5_qu <- as.data.frame(xgbpred_c5_qu)

#Matriz de confusão
confusionMatrix(xgbpred_c5_qu[,1], teste5$QUINTIL)

### Conjunto 2 -> Entre Melhores
treino2$ENTRE_MELHORES <- as.numeric(treino2$ENTRE_MELHORES)
treino2$ENTRE_MELHORES <- treino2$ENTRE_MELHORES - 1

set.seed(42)
xgbmod_c2_em <- xgboost(
  data = data.matrix(treino2[,c(4:30)]),
  label = treino2$ENTRE_MELHORES,
  eta = 0.05,
  max_depth = 10,
  nround = 1000,
  subsample = 0.8,
  colsample_bytree = 0.5,
  seed = 42,
  eval_metric = "logloss",
  objective = "reg:logistic",
  nthread = 3,
  early_stopping_rounds = 10,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c2_em <- predict(xgbmod_c2_em, data.matrix(teste2[,c(4:30)]), reshape=T)
xgbpred_c2_em <- as.data.frame(xgbpred_c2_em)
xgbpred_c2_em <- apply(xgbpred_c2_em,1,function(x) ifelse(x<=0.10,0,1))
xgbpred_c2_em <- as.data.frame(xgbpred_c2_em)

#Matriz de confusão
xgbpred_c2_em[,1] <- as.factor(xgbpred_c2_em[,1])
confusionMatrix(xgbpred_c2_em[,1], teste2$ENTRE_MELHORES)

### Conjunto 3 -> Entre Melhores
treino3$ENTRE_MELHORES <- as.numeric(treino3$ENTRE_MELHORES)
treino3$ENTRE_MELHORES <- treino3$ENTRE_MELHORES - 1

set.seed(42)
xgbmod_c3_em <- xgboost(
  data = data.matrix(treino3[,c(4:26)]),
  label = treino3$ENTRE_MELHORES,
  eta = 0.05,
  max_depth = 10,
  nround = 1000,
  subsample = 0.8,
  colsample_bytree = 0.5,
  seed = 42,
  eval_metric = "logloss",
  objective = "reg:logistic",
  nthread = 3,
  early_stopping_rounds = 10,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c3_em <- predict(xgbmod_c3_em, data.matrix(teste3[,c(4:26)]), reshape=T)
xgbpred_c3_em <- as.data.frame(xgbpred_c3_em)
xgbpred_c3_em <- apply(xgbpred_c3_em,1,function(x) ifelse(x<=0.11,0,1))
xgbpred_c3_em <- as.data.frame(xgbpred_c3_em)

#Matriz de confusão
xgbpred_c3_em[,1] <- as.factor(xgbpred_c3_em[,1])
confusionMatrix(xgbpred_c3_em[,1], teste3$ENTRE_MELHORES)

### Conjunto 4 -> Entre Melhores
treino4$ENTRE_MELHORES <- as.numeric(treino4$ENTRE_MELHORES)
treino4$ENTRE_MELHORES <- treino4$ENTRE_MELHORES - 1

set.seed(42)
xgbmod_c4_em <- xgboost(
  data = data.matrix(treino4[,c(5:19)]),
  label = treino4$ENTRE_MELHORES,
  eta = 0.05,
  max_depth = 10,
  nround = 1000,
  subsample = 0.8,
  colsample_bytree = 0.5,
  seed = 42,
  eval_metric = "logloss",
  objective = "reg:logistic",
  nthread = 3,
  early_stopping_rounds = 10,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c4_em <- predict(xgbmod_c4_em, data.matrix(teste4[,c(5:19)]), reshape=T)
xgbpred_c4_em <- as.data.frame(xgbpred_c4_em)
xgbpred_c4_em <- apply(xgbpred_c4_em,1,function(x) ifelse(x<=0.09,0,1))
xgbpred_c4_em <- as.data.frame(xgbpred_c4_em)

#Matriz de confusão
xgbpred_c4_em[,1] <- as.factor(xgbpred_c4_em[,1])
confusionMatrix(xgbpred_c4_em[,1], teste4$ENTRE_MELHORES)

### Conjunto 5 -> Entre Melhores
treino5$ENTRE_MELHORES <- as.numeric(treino5$ENTRE_MELHORES)
treino5$ENTRE_MELHORES <- treino5$ENTRE_MELHORES - 1

set.seed(42)
xgbmod_c5_em <- xgboost(
  data = data.matrix(treino5[,c(5:27)]),
  label = treino5$ENTRE_MELHORES,
  eta = 0.05,
  max_depth = 10,
  nround = 1000,
  subsample = 0.8,
  colsample_bytree = 0.5,
  seed = 42,
  eval_metric = "logloss",
  objective = "reg:logistic",
  nthread = 3,
  early_stopping_rounds = 10,
  booster = "gbtree"
)

#Fazer as predições
xgbpred_c5_em <- predict(xgbmod_c5_em, data.matrix(teste5[,c(5:27)]), reshape=T)
xgbpred_c5_em <- as.data.frame(xgbpred_c5_em)
xgbpred_c5_em <- apply(xgbpred_c5_em,1,function(x) ifelse(x<=0.14,0,1))
xgbpred_c5_em <- as.data.frame(xgbpred_c5_em)

#Matriz de confusão
xgbpred_c5_em[,1] <- as.factor(xgbpred_c5_em[,1])
confusionMatrix(xgbpred_c5_em[,1], teste5$ENTRE_MELHORES)
