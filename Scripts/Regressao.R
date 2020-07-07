install.packages('gtools')
install.packages('sm')
install.packages('data.table')
install.packages('RSNNS')

library('RJDBC')
library('gtools')
library('sm')
library('data.table')
library('RSNNS')

#Regressão só trabalha com dados numéricos
#Só é necessário carregar os conjuntos 2, 3, 4 e 5

drv <- JDBC("oracle.jdbc.OracleDriver", classPath="ojdbc6.jar"," ")
con <- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:xe", "CARTOLAFC","CARTOLAFC")

conjunto2 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA2")
conjunto3 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA3")
conjunto4 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA4")
conjunto5 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA5")

rodadasTeste <- c(6, 8, 12, 16, 20, 24, 26, 28, 32, 36)

'%notin%' <- Negate('%in%')

#Separar conjuntos 2
treino2 <- conjunto2[conjunto2$RODADA %notin% rodadasTeste,]
teste2 <- conjunto2[conjunto2$RODADA %in% rodadasTeste,]

#Separar conjuntos 3
treino3 <- conjunto3[conjunto3$RODADA %notin% rodadasTeste,]
teste3 <- conjunto3[conjunto3$RODADA %in% rodadasTeste,]

#Separar conjuntos 4
treino4 <- conjunto4[conjunto4$RODADA %notin% rodadasTeste,]
teste4 <- conjunto4[conjunto4$RODADA %in% rodadasTeste,]

normalize <- function(df)
{
  for(col in 1:dim(df)[2])
  {
    df[[col]] <- ((df[[col]]-min(df[[col]]))/(max(df[[col]])-min(df[[col]])))
  }
  return(df)
}

#Separar conjuntos 5
conj5_pontosNorm <- conjunto5$PONTOS
conj5_pontosNorm <- as.data.frame(conj5_pontosNorm)
conj5_pontosNorm <- normalize(conj5_pontosNorm)
conjunto5$PONTOS_NORM <- conj5_pontosNorm$conj5_pontosNorm

treino5 <- conjunto5[conjunto5$RODADA %notin% rodadasTeste,]
teste5 <- conjunto5[conjunto5$RODADA %in% rodadasTeste,]

#Função para calcular as estatísticas
pred.stats <- function(y.pred, df_real, norm = FALSE)
{
  
  if(norm==FALSE)
    y.real <- df_real$PONTOS
  else
    y.real <- df_real$PONTOS_NORM
  
  residuals <- y.real - y.pred
  
  sq_residuals <- residuals^2
  
  ss_tot <- y.real - mean(y.real)
  ss_tot <- ss_tot^2
  sum_ss_tot <- sum(ss_tot)
  
  erro_abs_quad <- sum(sq_residuals)
  erro_med_quad <- mean(sq_residuals)
  
  r_quadrado <- 1 - (erro_abs_quad/sum_ss_tot)
  
  ### Calculo de posicao relativa
  
  df_real2 <- cbind(df_real, y.pred)
  
  df_real2_dt <- data.table(df_real2)
  
  if(norm==FALSE)
    df_real2_dt <- df_real2_dt[, POSICAO_RELATIVA_REAL := frank(-PONTOS), by = list(ANO,RODADA)]
  else
    df_real2_dt <- df_real2_dt[, POSICAO_RELATIVA_REAL := frank(-PONTOS_NORM), by = list(ANO,RODADA)]
  
  df_real2_dt <- df_real2_dt[, POSICAO_RELATIVA_PRED := frank(-y.pred), by = list(ANO,RODADA)]
  
  df_real2_dt$DIF_POS <- abs(df_real2_dt$POSICAO_RELATIVA_REAL - df_real2_dt$POSICAO_RELATIVA_PRED)
  
  dif_abs_pos <- sum(df_real2_dt$DIF_POS)
  dif_med_pos <- mean(df_real2_dt$DIF_POS)
  
  df_real2_dt2 <- df_real2_dt$DIF_POS[df_real2_dt$POSICAO_RELATIVA_REAL<=50]
  
  dif_abs_melhores <- sum(df_real2_dt2)
  dif_med_melhores <- mean(df_real2_dt2)
  
  return(cbind(erro_abs_quad, erro_med_quad, r_quadrado, dif_abs_pos, dif_med_pos, dif_abs_melhores, dif_med_melhores))
}

############ Plotar Curvas de Treinamento e Teste ############
plot(density(conjunto2$PONTOS), ylab="Densidade", main="", xlab = "Pontos", lwd=2)
lines(density(treino2$PONTOS), col='blue', lwd=2)
lines(density(teste2$PONTOS), col='green', lwd=2)
legend("topright",legend=c("Conjunto Completo","Conjunto de Treinamento", "Conjunto de Teste"), col=c("black","blue","green"),lty=c(1,1,1), ncol=1, lwd=c(2,2,2))

summary(conjunto2$PONTOS)
sd(conjunto2$PONTOS)
summary(treino2$PONTOS)
sd(treino2$PONTOS)
summary(teste2$PONTOS)
sd(teste2$PONTOS)

conjunto2$QUINTIL <- as.factor(conjunto2$QUINTIL)
treino2$QUINTIL <- as.factor(treino2$QUINTIL)
teste2$QUINTIL <- as.factor(teste2$QUINTIL)

summary(conjunto2$QUINTIL)
summary(treino2$QUINTIL)
summary(teste2$QUINTIL)

conjunto2$ENTRE_MELHORES <- as.factor(conjunto2$ENTRE_MELHORES)
treino2$ENTRE_MELHORES <- as.factor(treino2$ENTRE_MELHORES)
teste2$ENTRE_MELHORES <- as.factor(teste2$ENTRE_MELHORES)

summary(conjunto2$ENTRE_MELHORES)
summary(treino2$ENTRE_MELHORES)
summary(teste2$ENTRE_MELHORES)

###################### Regressão Linear ######################

### Conjunto 2 - Regressão Linear
lm_c2_po <- lm(PONTOS ~ RODADA + ID_POSICAO_ATA + ID_POSICAO_GOL + ID_POSICAO_LAT + ID_POSICAO_MEI + ID_POSICAO_TEC + ID_POSICAO_ZAG + PRECO_INI_RODADA + MEDIA_ULT3_VARI + MEDIA_INI_RODADA + ULT_PONTUACAO + MEDIA_ULT3_PONT + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE_D + ULT_RESULT_CLUBE_E + ULT_RESULT_CLUBE_V + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV_D + ULT_RESULT_ADV_E + ULT_RESULT_ADV_V + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV
               , data = treino2)

pred_lm_c2_po <- predict(lm_c2_po, teste2)

plot(density(teste2$PONTOS), ylim = c(0,0.4), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução LM_C2_PO")
lines(density(pred_lm_c2_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_lm_c2_po, teste2)

View(pred_lm_c2_po)

### Conjunto 3 - Regressão Linear

lm_c3_po <- lm(PONTOS ~ RODADA + ID_POSICAO_ATA + ID_POSICAO_GOL + ID_POSICAO_LAT + ID_POSICAO_MEI + ID_POSICAO_TEC + ID_POSICAO_ZAG + PRECO_INI_RODADA + MEDIA_ULT3_VARI + ULT_PONTUACAO + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE_E + ULT_RESULT_CLUBE_V + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV_E + ULT_RESULT_ADV_V + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
               data = treino3)

pred_lm_c3_po <- predict(lm_c3_po, teste3)

plot(density(teste3$PONTOS), ylim = c(0,0.4), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução LM_C3_PO")
lines(density(pred_lm_c3_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_lm_c3_po, teste3)

### Conjunto 4 - Regressão Linear

lm_c4_po <- lm(PONTOS ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15,
               data = treino4)

pred_lm_c4_po <- predict(lm_c4_po, teste4)

plot(density(teste4$PONTOS), ylim = c(0,0.4), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução LM_C4_PO")
lines(density(pred_lm_c4_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_lm_c4_po, teste4)

### Conjunto 5 - Regressão Linear

lm_c5_po <- lm(PONTOS ~ RODADA_NORM + ID_POSICAO_ATA + ID_POSICAO_GOL + ID_POSICAO_LAT + ID_POSICAO_MEI + ID_POSICAO_TEC + ID_POSICAO_ZAG + PRECO_INI_RODADA + MEDIA_ULT3_VARI + ULT_PONTUACAO + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE_E + ULT_RESULT_CLUBE_V + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV_E + ULT_RESULT_ADV_V + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
               data = treino5)

pred_lm_c5_po <- predict(lm_c5_po, teste5)

plot(density(teste5$PONTOS), ylim = c(0,0.4), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução LM_C5_PO")
lines(density(pred_lm_c5_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_lm_c5_po, teste5)

### Conjunto 5 - Pontuação Normalizada - Regressão Linear

lm_c5_poN <- lm(PONTOS_NORM ~ RODADA_NORM + ID_POSICAO_ATA + ID_POSICAO_GOL + ID_POSICAO_LAT + ID_POSICAO_MEI + ID_POSICAO_TEC + ID_POSICAO_ZAG + PRECO_INI_RODADA + MEDIA_ULT3_VARI + ULT_PONTUACAO + G_ULT_ROD + G_ULT3_ROD + POS_ATUAL_CLUBE + ULT_RESULT_CLUBE_E + ULT_RESULT_CLUBE_V + MANDO_CAMPO + GOLS_PRO_ULT3 + GOLS_CON_ULT3 + POS_ATUAL_ADV + ULT_RESULT_ADV_E + ULT_RESULT_ADV_V + GOLS_PRO_ULT3_ADV + GOLS_CON_ULT3_ADV,
               data = treino5)

pred_lm_c5_poN <- predict(lm_c5_poN, teste5)

plot(density(teste5$PONTOS_NORM), ylim = c(0,18), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução LM_C5_PON")
lines(density(pred_lm_c5_poN), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_lm_c5_poN, teste5, norm=TRUE)

####################### Redes Neurais Artificiais #######################

### Conjunto 2 - Pontuação - Redes Neurais Artificiais

treino2_mat <- as.matrix(treino2[,c(4:30)])
treino2_mat_y <- as.matrix(treino2[,31])
teste2_mat <- as.matrix(teste2[, c(4:30)])
teste2_mat_y <- as.matrix(teste2[,31])

set.seed(42)
mlp_c2_po <- mlp(
  x = treino2_mat,
  y = treino2_mat_y,
  size = c(26,27),
  maxit = 118,
  initFunc= "Randomize_Weights",
  hiddenActFunc = "Act_IdentityPlusBias",
  learnFunc = "Rprop",
  #learnFuncParams=c(0.1,0),
  linOut=TRUE,
  inputsTest = teste2_mat,
  targetsTest = teste2_mat_y
)

pred_mlp_c2_po <- predict(mlp_c2_po, teste2_mat)

plot(density(teste2$PONTOS), ylim = c(0,0.5), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução MLP_C2_PO")
lines(density(pred_mlp_c2_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_mlp_c2_po, teste2)

### Conjunto 3 - Pontuação - Redes Neurais Artificiais

treino3_mat <- as.matrix(treino3[,c(4:26)])
treino3_mat_y <- as.matrix(treino3[,27])
teste3_mat <- as.matrix(teste3[, c(4:26)])
teste3_mat_y <- as.matrix(teste3[,27])

set.seed(42)
mlp_c3_po <- mlp(
  x = treino3_mat,
  y = treino3_mat_y,
  size = c(15,35),
  maxit = 75,
  initFunc= "Randomize_Weights",
  hiddenActFunc = "Act_IdentityPlusBias",
  learnFunc = "Rprop",
  #learnFuncParams=c(0.1,0),
  linOut=TRUE,
  inputsTest = teste3_mat,
  targetsTest = teste3_mat_y
)

pred_mlp_c3_po <- predict(mlp_c3_po, teste3_mat)

plot(density(teste3$PONTOS), ylim = c(0,1), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução MLP_C3_PON")
lines(density(pred_mlp_c3_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_mlp_c3_po, teste3)

### Conjunto 4 - Pontuação - Redes Neurais Artificiais

treino4_mat <- as.matrix(treino4[,c(5:19)])
treino4_mat_y <- as.matrix(treino4[,20])
teste4_mat <- as.matrix(teste4[, c(5:19)])
teste4_mat_y <- as.matrix(teste4[,20])

set.seed(42)
mlp_c4_po <- mlp(
  x = treino4_mat,
  y = treino4_mat_y,
  size = c(33,37),
  maxit = 130,
  initFunc= "Randomize_Weights",
  hiddenActFunc = "Act_IdentityPlusBias",
  learnFunc = "Rprop",
  #learnFuncParams=c(0.1,0),
  linOut=TRUE,
  inputsTest = teste4_mat,
  targetsTest = teste4_mat_y
)

pred_mlp_c4_po <- predict(mlp_c4_po, teste4_mat)

plot(density(teste4$PONTOS), ylim = c(0,1), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução MLP_C4_PON")
lines(density(pred_mlp_c4_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_mlp_c4_po, teste4)

### Conjunto 5 - Pontuação - Redes Neurais Artificiais

treino5_mat <- as.matrix(treino5[,c(5:27)])
treino5_mat_y <- as.matrix(treino5[,28])
teste5_mat <- as.matrix(teste5[, c(5:27)])
teste5_mat_y <- as.matrix(teste5[,28])

set.seed(42)
mlp_c5_po <- mlp(
  x = treino5_mat,
  y = treino5_mat_y,
  size = c(26,26),
  maxit = 57,
  initFunc= "Randomize_Weights",
  hiddenActFunc = "Act_IdentityPlusBias",
  learnFunc = "Rprop",
  #learnFuncParams=c(0.1,0),
  linOut=TRUE,
  inputsTest = teste5_mat,
  targetsTest = teste5_mat_y
)

pred_mlp_c5_po <- predict(mlp_c5_po, teste5_mat)

plot(density(teste5$PONTOS), ylim = c(0,0.5), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução MLP_C5_PO")
lines(density(pred_mlp_c5_po), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_mlp_c5_po, teste5)

### Conjunto 5 - Pontuação Normalizada - Redes Neurais Artificiais

treino5_mat <- as.matrix(treino5[,c(5:27)])
treino5_mat_y <- as.matrix(treino5[,31])
teste5_mat <- as.matrix(teste5[, c(5:27)])
teste5_mat_y <- as.matrix(teste5[,31])

set.seed(42)
mlp_c5_poN <- mlp(
  x = treino5_mat,
  y = treino5_mat_y,
  size = c(8,40),
  maxit = 222,
  initFunc= "Randomize_Weights",
  hiddenActFunc = "Act_IdentityPlusBias",
  learnFunc = "Rprop",
  #learnFuncParams=c(0.1,0),
  linOut=TRUE,
  inputsTest = teste5_mat,
  targetsTest = teste5_mat_y
)

pred_mlp_c5_poN <- predict(mlp_c5_poN, teste5_mat)

plot(density(teste5$PONTOS_NORM), ylim = c(0,18), col = 'blue', xlab = "", ylab = "Densidade", main = "Execução MLP_C5_PON")
lines(density(pred_mlp_c5_poN), col = 'red')
legend("topright",legend=c("Pontuação original","Pontuação predita"), col=c("blue","red"),lty=c(1,1), ncol=1)

pred.stats(pred_mlp_c5_poN, teste5, norm=TRUE)