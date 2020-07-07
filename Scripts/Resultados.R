setwd("C:/Users/ctcca/Documents/USP/POS_BigData/MOD4/Monografia/CodigoFonte")

###### Laboratório de Resultados ######

### As predições vêm dos modelos dos scripts anteriores

form <- c("4-4-2","4-3-3","5-3-2","3-5-2","3-4-3","4-5-1","5-4-1")
GOL <- c(1,1,1,1,1,1,1)
ZAG <- c(2,2,3,3,3,2,3)
LAT <- c(2,2,2,0,0,2,2)
MEI <- c(4,3,3,5,4,5,4)
ATA <- c(2,3,2,2,3,1,1)
TEC <- c(1,1,1,1,1,1,1)

dfForm <- data.frame(form, GOL, ZAG, LAT, MEI, ATA, TEC)

anosTeste = c("2018", "2019")

######### Naive Bayes #########
nbpred_c1_qu_prob <- as.data.frame(predict(nbmod_c1_qu, teste1[,c(4:21)], "raw"))
nb1 <- cbind(teste1[,c(1:5,22)], nbpred_c1_qu_prob, nbpred_c1_qu)
names(nb1)[12] <- "Classe"

dfResultados <- data.frame("2018", 1, "1-1-1", "POS", "BRA", "Apelido", 2)
names(dfResultados) = c("ANO", "RODADA", "FORMACAO", "POSICAO", "CLUBE", "APELIDO", "PONTOS")

for(ano in anosTeste)
{
  for(rodada in rodadasTeste)
  {
    for(formacao in dfForm$form)
    {
      for(idPos in 2:7)
      {
        num <- dfForm[dfForm$form==formacao,idPos]
        
        if(num!=0)
        {
        
          posicao <- names(dfForm)[idPos]
          best <- nb1[nb1$RODADA==rodada & nb1$ANO==ano & nb1$ID_POSICAO==posicao & nb1$Classe=="5",]
          best2 <- best[order(-best$`5`),]
          
          cFormacao <- rep(formacao, num)
          cPosicao <- rep(posicao, num)
          dfResultTemp <- data.frame(cbind(best2$ANO[1:num], best2$RODADA[1:num], cFormacao, cPosicao, best2$SIGLA[1:num], best2$APELIDO[1:num], best2$PONTOS[1:num]))
          names(dfResultTemp) <- names(dfResultados)
          dfResultados <- rbind(dfResultados, dfResultTemp)
        
        }
      }
    }
  }
}

#Escrever resultados
write.csv(dfResultados,file="Resultados_NB_C1_QU.csv",row.names = F)

#Pegar o terceiro melhor zagueiro da rodada 36 de 2019
best <- nb1[nb1$RODADA==36 & nb1$ANO=="2019" & nb1$ID_POSICAO=="ZAG" & nb1$Classe=="4",]
View(best)

######### Extreme Gradient Boosting #########

xgbpred_c2_qu_prob <- predict(xgbmod_c2_qu, data.matrix(teste2[,c(4:30)]), reshape=T)
xgbpred_c2_qu_prob <- as.data.frame(xgbpred_c2_qu_prob)
xgbpred_c2_qu_prob <- xgbpred_c2_qu_prob[,-1]
colnames(xgbpred_c2_qu_prob) <- levels(treino2$QUINTIL)

xgbpred_c2_qu_prob <- cbind(xgbpred_c2_qu_prob, xgbpred_c2_qu)
xgb1 <- cbind(teste2[,1:10], xgbpred_c2_qu_prob, teste2$PONTOS)
names(xgb1)[5:10] <- c("ATA","GOL","LAT","MEI","TEC","ZAG")
names(xgb1)[16:17] <- c("Classe", "PONTOS")

cIdPos <- apply(xgb1[,5:10],1,function(x) colnames(xgb1[,5:10])[which.max(x)])
xgb1 <- cbind(xgb1, cIdPos)
names(xgb1)[18] <- "ID_POSICAO" 

dfResultados <- data.frame("2018", 1, "1-1-1", "POS", "BRA", "Apelido", 2)
names(dfResultados) = c("ANO", "RODADA", "FORMACAO", "POSICAO", "CLUBE", "APELIDO", "PONTOS")

for(ano in anosTeste)
{
  for(rodada in rodadasTeste)
  {
    for(formacao in dfForm$form)
    {
      for(idPos in 2:7)
      {
        num <- dfForm[dfForm$form==formacao,idPos]
        
        if(num!=0)
        {
          
          posicao <- names(dfForm)[idPos]
          best <- xgb1[xgb1$RODADA==rodada & xgb1$ANO==ano & xgb1$ID_POSICAO==posicao & xgb1$Classe=="5",]
          best2 <- best[order(-best$`5`),]
          
          cFormacao <- rep(formacao, num)
          cPosicao <- rep(posicao, num)
          dfResultTemp <- data.frame(cbind(best2$ANO[1:num], best2$RODADA[1:num], cFormacao, cPosicao, best2$SIGLA[1:num], best2$APELIDO[1:num], best2$PONTOS[1:num]))
          names(dfResultTemp) <- names(dfResultados)
          dfResultados <- rbind(dfResultados, dfResultTemp)
          
        }
      }
    }
  }
}

write.csv(dfResultados,file="Resultados_XGB_C2_QU.csv",row.names = F)

######### Regressão linear #########
lm1 <- cbind(teste2[,1:11], pred_lm_c2_po, teste2$PONTOS)
names(lm1)[12:13] <- c("PONTOS_PRED", "PONTOS")
names(lm1)[5:10] <- c("ATA","GOL","LAT","MEI","TEC","ZAG")

cIdPos <- apply(lm1[,5:10],1,function(x) colnames(lm1[,5:10])[which.max(x)])
View(cIdPos)
lm1 <- cbind(lm1, cIdPos)
names(lm1)[14] <- "ID_POSICAO"

dfResultados <- data.frame("2018", 1, "1-1-1", "POS", "BRA", "Apelido", 2)
names(dfResultados) = c("ANO", "RODADA", "FORMACAO", "POSICAO", "CLUBE", "APELIDO", "PONTOS")

for(ano in anosTeste)
{
  for(rodada in rodadasTeste)
  {
    for(formacao in dfForm$form)
    {
      for(idPos in 2:7)
      {
        num <- dfForm[dfForm$form==formacao,idPos]
        
        if(num!=0)
        {
          
          posicao <- names(dfForm)[idPos]
          best <- lm1[lm1$RODADA==rodada & lm1$ANO==ano & lm1$ID_POSICAO==posicao,]
          best2 <- best[order(-best$PONTOS_PRED),]
          
          cFormacao <- rep(formacao, num)
          cPosicao <- rep(posicao, num)
          dfResultTemp <- data.frame(cbind(best2$ANO[1:num], best2$RODADA[1:num], cFormacao, cPosicao, best2$SIGLA[1:num], best2$APELIDO[1:num], best2$PONTOS[1:num]))
          names(dfResultTemp) <- names(dfResultados)
          dfResultados <- rbind(dfResultados, dfResultTemp)
          
        }
      }
    }
  }
}

View(dfResultados)

write.csv(dfResultados,file="Resultados_LM_C2_PO.csv",row.names = F)

######### Redes Neurais #########
mlp1 <- cbind(teste2[,1:11], pred_mlp_c2_po, teste2$PONTOS)
names(mlp1)[12:13] <- c("PONTOS_PRED", "PONTOS")
names(mlp1)[5:10] <- c("ATA","GOL","LAT","MEI","TEC","ZAG")

cIdPos <- apply(mlp1[,5:10],1,function(x) colnames(mlp1[,5:10])[which.max(x)])
View(cIdPos)
mlp1 <- cbind(mlp1, cIdPos)
names(mlp1)[14] <- "ID_POSICAO"

View(mlp1)

dfResultados <- data.frame("2018", 1, "1-1-1", "POS", "BRA", "Apelido", 2)
names(dfResultados) = c("ANO", "RODADA", "FORMACAO", "POSICAO", "CLUBE", "APELIDO", "PONTOS")

for(ano in anosTeste)
{
  for(rodada in rodadasTeste)
  {
    for(formacao in dfForm$form)
    {
      for(idPos in 2:7)
      {
        num <- dfForm[dfForm$form==formacao,idPos]
        
        if(num!=0)
        {
          
          posicao <- names(dfForm)[idPos]
          best <- mlp1[mlp1$RODADA==rodada & mlp1$ANO==ano & mlp1$ID_POSICAO==posicao,]
          best2 <- best[order(-best$PONTOS_PRED),]
          
          cFormacao <- rep(formacao, num)
          cPosicao <- rep(posicao, num)
          dfResultTemp <- data.frame(cbind(best2$ANO[1:num], best2$RODADA[1:num], cFormacao, cPosicao, best2$SIGLA[1:num], best2$APELIDO[1:num], best2$PONTOS[1:num]))
          names(dfResultTemp) <- names(dfResultados)
          dfResultados <- rbind(dfResultados, dfResultTemp)
          
        }
      }
    }
  }
}

View(dfResultados)

write.csv(dfResultados,file="Resultados_MLP_C2_PO.csv",row.names = F)
