install.packages('RJDBC')
install.packages('mltools')
install.packages('data.table')
install.packages('corrplot')
install.packages('mlbench')
install.packages('caret')

library(data.table)
library(mltools)
library('RJDBC')
library(corrplot)
library(mlbench)
library(caret)

setwd("C:/Users/ctcca/Documents/USP/POS_BigData/MOD4/Monografia/CodigoFonte")

#################### Conectar à base de dados #####################
drv <- JDBC("oracle.jdbc.OracleDriver", classPath="ojdbc6.jar"," ")
con <- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:xe", "CARTOLAFC","CARTOLAFC")

#################### Padronizar os arquivos de partidas #####################

partidas_2018 <- read.csv("Dados_Entrada/Partidas/2018_partidas.csv",header=T,sep=',',encoding='UTF-8')
partidas_2019 <- read.csv("Dados_Entrada/Partidas/2019_partidas.csv",header=T,sep=',',encoding='UTF-8')

depara_2018 <- read.csv("Dados_Entrada/DadosPreparados/DePara_Times_2018.csv",header=T,sep=';',encoding='UTF-8')
depara_2019 <- read.csv("Dados_Entrada/DadosPreparados/DePara_Times_2019.csv",header=T,sep=';',encoding='UTF-8')

dbExecute(con, "DROP TABLE PARTIDAS_2018_BRUTO")
dbExecute(con, "DROP TABLE PARTIDAS_2019_BRUTO")
dbExecute(con, "DROP TABLE DEPARA_2018")
dbExecute(con, "DROP TABLE DEPARA_2019")
dbCommit(con)

names(depara_2018)[1] <- "clube"
names(depara_2019)[1] <- "id"
  
dbWriteTable(con, "DEPARA_2018", depara_2018)
dbWriteTable(con, "DEPARA_2019", depara_2019)
dbCommit(con)

partidas_2018_trab <- cbind(partidas_2018[,1:2], partidas_2018[,4:6])
dbWriteTable(con, "PARTIDAS_2018_BRUTO", partidas_2018_trab)

partidas_2019_trab <- partidas_2019[,2:6]
dbWriteTable(con, "PARTIDAS_2019_BRUTO", partidas_2019_trab)

dbCommit(con)

#Consolidar resultados de partidas
dbExecute(con, "DROP TABLE PARTIDAS_CONSOLIDADO")

dbExecute(con,
          "CREATE TABLE PARTIDAS_CONSOLIDADO AS SELECT '2018' ANO
          , A.ROUND
          , ROW_NUMBER() OVER (PARTITION BY A.ROUND ORDER BY A.GAME) NUM_JOGO
          , B.SIGLA HOME_TEAM
          , TO_NUMBER(SUBSTR(A.SCORE, 1, INSTR(SCORE,' x ')-1)) HOME_SCORE
          , TO_NUMBER(SUBSTR(A.SCORE, INSTR(SCORE, 'x')+2, 2)) AWAY_SCORE
          , C.SIGLA AWAY_TEAM
          FROM PARTIDAS_2018_BRUTO A, DEPARA_2018 B, DEPARA_2018 C
          WHERE B.CLUBE = A.HOME_TEAM
          AND C.CLUBE = A.AWAY_TEAM
          UNION
          SELECT '2019' ANO
          , A.ROUND
          , ROW_NUMBER() OVER (PARTITION BY A.ROUND ORDER BY A.HOME_TEAM) NUM_JOGO
          , B.SIGLA HOME_TEAM
          , A.HOME_SCORE
          , A.AWAY_SCORE
          , C.SIGLA AWAY_TEAM
          FROM PARTIDAS_2019_BRUTO A, DEPARA_2019 B, DEPARA_2019 C
          WHERE B.ID = A.HOME_TEAM
          AND C.ID = A.AWAY_TEAM")

dbCommit(con)

#################### Montar a tabela rodada por rodada ####################

dbExecute(con, "DROP TABLE PARTIDAS_CONSOLIDADO2")

dbExecute(con, 
"CREATE TABLE PARTIDAS_CONSOLIDADO2 AS 
SELECT ANO, ROUND, NUM_JOGO, HOME_TEAM AS TIME, HOME_SCORE GOLS_PRO, AWAY_SCORE GOLS_CONTRA, 1 MANDO_CAMPO, AWAY_TEAM ADVERSARIO, ' ' RESULTADO FROM PARTIDAS_CONSOLIDADO
UNION
SELECT ANO, ROUND, NUM_JOGO, AWAY_TEAM AS TIME, AWAY_SCORE GOLS_PRO, HOME_SCORE GOLS_CONTRA, 0 MANDO_CAMPO, HOME_TEAM ADVERSARIO, ' ' RESULTADO FROM PARTIDAS_CONSOLIDADO")

dbExecute(con, "CREATE INDEX PARTIDAS_CONS2_I ON PARTIDAS_CONSOLIDADO2(ANO, ROUND, TIME)")

dbExecute(con,
          "UPDATE PARTIDAS_CONSOLIDADO2 SET RESULTADO = (CASE WHEN GOLS_PRO > GOLS_CONTRA THEN 'V' ELSE (CASE WHEN GOLS_PRO = GOLS_CONTRA THEN 'E' ELSE 'D' END) END)")

dbExecute(con, 
          "CREATE TABLE TABELAS_CLASSIF AS SELECT SEL2.ANO
          , SEL2.RODADA
          , ROW_NUMBER() OVER(PARTITION BY SEL2.ANO, SEL2.RODADA ORDER BY SEL2.PONTOS DESC, SEL2.VITORIAS DESC, SEL2.SALDO_GOLS DESC, SEL2.GOLS_PRO DESC) POSICAO
          , SEL2.TIME
          , SEL2.PONTOS
          , SEL2.JOGOS
          , SEL2.VITORIAS
          , SEL2.EMPATES
          , SEL2.DERROTAS
          , SEL2.GOLS_PRO
          , SEL2.GOLS_CONTRA
          , SEL2.SALDO_GOLS
          FROM
          (
            SELECT SEL1.ANO
            , ROD.ROUND RODADA
            , SEL1.TIME
            , SUM(SEL1.VITORIA) * 3 + SUM(SEL1.EMPATE) PONTOS
            , SUM(SEL1.JOGO) JOGOS
            , SUM(SEL1.VITORIA) VITORIAS
            , SUM(SEL1.EMPATE) EMPATES
            , SUM(SEL1.DERROTA) DERROTAS 
            , SUM(SEL1.GOLS_PRO) GOLS_PRO
            , SUM(SEL1.GOLS_CONTRA) GOLS_CONTRA
            , SUM(SEL1.SALDO_GOLS) SALDO_GOLS
            FROM
            (
              SELECT SEL.*
                , 1 JOGO
              , CASE WHEN GOLS_PRO > GOLS_CONTRA THEN 1 ELSE 0 END VITORIA
              , CASE WHEN GOLS_PRO = GOLS_CONTRA THEN 1 ELSE 0 END EMPATE
              , CASE WHEN GOLS_PRO < GOLS_CONTRA THEN 1 ELSE 0 END DERROTA 
              , GOLS_PRO - GOLS_CONTRA SALDO_GOLS
              FROM PARTIDAS_CONSOLIDADO2 SEL
            ) SEL1, (SELECT DISTINCT ANO, ROUND FROM PARTIDAS_CONSOLIDADO) ROD
            WHERE ROD.ANO = SEL1.ANO
            AND SEL1.ROUND <= ROD.ROUND
            GROUP BY SEL1.ANO, ROD.ROUND, SEL1.TIME
          ) SEL2
          ORDER BY SEL2.ANO ASC, SEL2.RODADA ASC, SEL2.PONTOS DESC, SEL2.VITORIAS DESC, SEL2.SALDO_GOLS DESC, SEL2.GOLS_PRO DESC")

dbCommit(con)

dbExecute(con, "CREATE INDEX TABELAS_CLASSIF1 ON TABELAS_CLASSIF(ANO, RODADA, TIME)")

####################### Montar os dados de entrada ####################

dbExecute(con, "DROP TABLE SCORES_2018")
dbExecute(con, "DROP TABLE SCORES_2019")

carregar_diretorio <- function(ano, diretorio, cnx){
  
  tabela <- paste("SCORES_", ano, sep="")
  
  for(i in 1:38) {
    dir <- paste(diretorio,'/',ano,'/rodada-',i,'.csv',sep="")
    print(dir)
    resultado <- read.csv(dir, header=T,sep=',',encoding='UTF-8')
    resultado$G[is.na(resultado$G)] <- 0
    resultado <- cbind(resultado[,1:15],resultado$G, resultado$G)
    names(resultado) <- cbind("seq","nome","slug","apelido","foto","id_atleta","rodada","sigla_clube","id_posicao","id_status","pontos","preco","variacao","media","clube","G_Acum","G_Rodada")
    if(i==1){
      dbWriteTable(cnx,tabela,resultado)
    }
    else{
      dbWriteTable(cnx,tabela,resultado,append=T,overwrite=F)
    }
    
  }
  
  #Índice para facilitar alterações
  dbExecute(con, paste("CREATE INDEX ", tabela, "1 ON ", tabela, "(RODADA, ID_ATLETA)", sep=""))

}

carregar_diretorio('2018','Dados_Entrada/Scores', con)
carregar_diretorio('2019','Dados_Entrada/Scores', con)
dbCommit(con)

dbExecute(con, "UPDATE SCORES_2018 A SET A.G_RODADA = (A.G_ACUM - NVL((SELECT B.G_ACUM FROM SCORES_2018 B WHERE B.ID_ATLETA = A.ID_ATLETA AND B.RODADA = (A.RODADA-1)), A.G_ACUM))")
dbExecute(con, "UPDATE SCORES_2019 A SET A.G_RODADA = (A.G_ACUM - NVL((SELECT B.G_ACUM FROM SCORES_2019 B WHERE B.ID_ATLETA = A.ID_ATLETA AND B.RODADA = (A.RODADA-1)), A.G_ACUM))")
dbCommit(con)

#Índice para facilitar consultas
dbExecute(con, "CREATE INDEX SCORES_20182 ON SCORES_2018(RODADA, ID_POSICAO)")
dbExecute(con, "CREATE INDEX SCORES_20192 ON SCORES_2019(RODADA, ID_POSICAO)")

#Drop Table
dbExecute(con, "DROP TABLE DADOS_CONSOLIDADOS_ENTRADA1")

#Consolidar os dados de entrada dos algoritmos
dbExecute(con, "
          CREATE TABLE DADOS_CONSOLIDADOS_ENTRADA1 AS
          SELECT '2019' ANO
, B.SIGLA
, A.APELIDO
, A.RODADA
, UPPER(A.ID_POSICAO) ID_POSICAO
, (A.PRECO+(A.VARIACAO*-1)) PRECO_INI_RODADA
, NVL((SELECT ROUND(AVG(X.VARIACAO),3) FROM SCORES_2019 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA BETWEEN (A.RODADA-3) AND (A.RODADA-1)),0) MEDIA_ULT3_VARI
, NVL((SELECT X.MEDIA FROM SCORES_2019 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA = (A.RODADA-1)),0) MEDIA_INI_RODADA
, NVL((SELECT X.PONTOS FROM SCORES_2019 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA = (A.RODADA-1)),0) ULT_PONTUACAO
, NVL((SELECT ROUND(AVG(X.PONTOS),3) FROM SCORES_2019 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA BETWEEN (A.RODADA-3) AND (A.RODADA-1)),0) MEDIA_ULT3_PONT
, NVL((SELECT X.G_RODADA FROM SCORES_2019 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA = (A.RODADA-1)),0) G_ULT_ROD
, NVL((SELECT SUM(X.G_RODADA) FROM SCORES_2019 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA BETWEEN (A.RODADA-3) AND (A.RODADA-1)),0) G_ULT3_ROD

, C.POSICAO POS_ATUAL_CLUBE
, (SELECT X.RESULTADO FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND = (A.RODADA-1) AND X.TIME = B.SIGLA) ULT_RESULT_CLUBE
, D.MANDO_CAMPO
, (SELECT SUM(X.GOLS_PRO) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = B.SIGLA) GOLS_PRO_ULT3
, (SELECT SUM(X.GOLS_CONTRA) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = B.SIGLA) GOLS_CON_ULT3
, (SELECT X.POSICAO FROM TABELAS_CLASSIF X WHERE X.ANO = C.ANO AND X.RODADA = (A.RODADA-1) AND X.TIME = D.ADVERSARIO) POS_ATUAL_ADV
, (SELECT X.RESULTADO FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND = (A.RODADA-1) AND X.TIME = D.ADVERSARIO) ULT_RESULT_ADV
, (SELECT SUM(X.GOLS_PRO) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = D.ADVERSARIO) GOLS_PRO_ULT3_ADV
, (SELECT SUM(X.GOLS_CONTRA) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = D.ADVERSARIO) GOLS_CON_ULT3_ADV

, A.PONTOS
, CASE WHEN (SELECT COUNT(1) FROM SCORES_2019 X WHERE X.RODADA = A.RODADA AND X.ID_POSICAO = A.ID_POSICAO AND X.PONTOS > A.PONTOS) < 5 THEN 1 ELSE 0 END ENTRE_MELHORES

FROM SCORES_2019 A, DEPARA_2019 B, TABELAS_CLASSIF C, PARTIDAS_CONSOLIDADO2 D
WHERE (A.PONTOS <> 0 AND A.VARIACAO <> 0)
AND A.RODADA >= 4
AND B.ID = A.SIGLA_CLUBE
AND C.ANO = '2019'
AND C.RODADA = (A.RODADA-1)
AND C.TIME = B.SIGLA
AND D.ANO = C.ANO
AND D.ROUND = A.RODADA
AND D.TIME = B.SIGLA

UNION ALL

SELECT '2018' ANO
, B.SIGLA
, A.APELIDO
, A.RODADA
, UPPER(A.ID_POSICAO) ID_POSICAO
, (A.PRECO+(A.VARIACAO*-1)) PRECO_INI_RODADA
, NVL((SELECT ROUND(AVG(X.VARIACAO),3) FROM SCORES_2018 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA BETWEEN (A.RODADA-3) AND (A.RODADA-1)),0) MEDIA_ULT3_VARI
, NVL((SELECT X.MEDIA FROM SCORES_2018 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA = (A.RODADA-1)),0) MEDIA_INI_RODADA
, NVL((SELECT X.PONTOS FROM SCORES_2018 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA = (A.RODADA-1)),0) ULT_PONTUACAO
, NVL((SELECT ROUND(AVG(X.PONTOS),3) FROM SCORES_2018 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA BETWEEN (A.RODADA-3) AND (A.RODADA-1)),0) MEDIA_ULT3_PONT
, NVL((SELECT X.G_RODADA FROM SCORES_2018 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA = (A.RODADA-1)),0) G_ULT_ROD
, NVL((SELECT SUM(X.G_RODADA) FROM SCORES_2018 X WHERE X.ID_ATLETA = A.ID_ATLETA AND X.RODADA BETWEEN (A.RODADA-3) AND (A.RODADA-1)),0) G_ULT3_ROD

, C.POSICAO POS_ATUAL_CLUBE
, (SELECT X.RESULTADO FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND = (A.RODADA-1) AND X.TIME = B.SIGLA) ULT_RESULT_CLUBE
, D.MANDO_CAMPO
, (SELECT SUM(X.GOLS_PRO) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = B.SIGLA) GOLS_PRO_ULT3
, (SELECT SUM(X.GOLS_CONTRA) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = B.SIGLA) GOLS_CON_ULT3
, (SELECT X.POSICAO FROM TABELAS_CLASSIF X WHERE X.ANO = C.ANO AND X.RODADA = (A.RODADA-1) AND X.TIME = D.ADVERSARIO) POS_ATUAL_ADV
, (SELECT X.RESULTADO FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND = (A.RODADA-1) AND X.TIME = D.ADVERSARIO) ULT_RESULT_ADV
, (SELECT SUM(X.GOLS_PRO) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = D.ADVERSARIO) GOLS_PRO_ULT3_ADV
, (SELECT SUM(X.GOLS_CONTRA) FROM PARTIDAS_CONSOLIDADO2 X WHERE X.ANO = C.ANO AND X.ROUND BETWEEN (A.RODADA-3) AND (A.RODADA-1) AND X.TIME = D.ADVERSARIO) GOLS_CON_ULT3_ADV

, A.PONTOS
, CASE WHEN (SELECT COUNT(1) FROM SCORES_2018 X WHERE X.RODADA = A.RODADA AND X.ID_POSICAO = A.ID_POSICAO AND X.PONTOS > A.PONTOS) < 5 THEN 1 ELSE 0 END ENTRE_MELHORES

FROM SCORES_2018 A, DEPARA_2018 B, TABELAS_CLASSIF C, PARTIDAS_CONSOLIDADO2 D
WHERE (A.PONTOS <> 0 AND A.VARIACAO <> 0)
AND A.RODADA >= 4
AND B.CLUBE_ARQ2 = A.CLUBE
AND C.ANO = '2018'
AND C.RODADA = (A.RODADA-1)
AND C.TIME = B.SIGLA
AND D.ANO = C.ANO
AND D.ROUND = A.RODADA
AND D.TIME = B.SIGLA")

dbCommit(con)

################# Redução de Dimensionalidade ##################

#Ler do banco de dados
dadosEntrada1 <- dbReadTable(con, "DADOS_CONSOLIDADOS_ENTRADA1")

dadosEntrada1$ID_POSICAO <- as.factor(dadosEntrada1$ID_POSICAO)
dadosEntrada1$ULT_RESULT_CLUBE <- as.factor(dadosEntrada1$ULT_RESULT_CLUBE)
dadosEntrada1$ULT_RESULT_ADV <- as.factor(dadosEntrada1$ULT_RESULT_ADV)

dadosEntrada1 <- one_hot(as.data.table(dadosEntrada1))
dadosEntrada2 <- dadosEntrada1[,c(4:30)]

#Fazer a eliminação de variáveis
set.seed(7)

#Matriz de correlação
MCor <- cor(dadosEntrada2)

#Fonte: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}

#1 Examinar as correlações
correlacoes <- flattenCorrMatrix(MCor)
View(correlacoes)

#Análise de Componentes Principais
pca <- prcomp(x = dadosEntrada2, scale = TRUE)
pca
summary(pca)
plot(pca, type="l", pch=16, main ="Scree Plot")
screeplot(pca, type="l", pch=16, main = "Scree Plot", npcs = 27)

################### Montagem dos Conjuntos de Entrada ###################

#Conjunto 1 -> Já está no banco de dados -> Relacao DADOS_CONSOLIDADOS_ENTRADA1

#Conjunto 2 -> Aplicação de OneHotEncoding -> Relacao DADOS_CONSOLIDADOS_ENTRADA2
dbWriteTable(con, "DADOS_CONSOLIDADOS_ENTRADA2", dadosEntrada1)

#Conjunto 3 -> Remoção de variáveis após análise de correlação -> Relacao DADOS_CONSOLIDADOS_ENTRADA3
dbWriteTable(con, "DADOS_CONSOLIDADOS_ENTRADA3", dadosEntrada1[,c(-13,-15,-19,-26)])

#Conjunto 4 -> Após aplicação da PCA
rot <- as.matrix(pca$rotation)
dadosEntrada4 <- (as.matrix(dadosEntrada2) %*% rot)
dadosEntrada4 <- as.data.frame(dadosEntrada4)
dadosEntrada4 <- dadosEntrada4[,c(1:15)]
dadosEntrada4 <- cbind(dadosEntrada1[,c(1:4)], dadosEntrada4, dadosEntrada1[,c(31:32)])
dbWriteTable(con, "DADOS_CONSOLIDADOS_ENTRADA4", dadosEntrada4)

#Conjunto 5 -> Conjunto 3 normalizado 0-1
dadosEntrada5 <- dadosEntrada2

normalize <- function(df)
{
  for(col in 1:dim(df)[2])
  {
    df[[col]] <- ((df[[col]]-min(df[[col]]))/(max(df[[col]])-min(df[[col]])))
  }
  return(df)
}

dadosEntrada5 <- (normalize(dadosEntrada5))
dadosEntrada5 <- cbind(dadosEntrada1[,c(1:4)],dadosEntrada5,dadosEntrada1[,c(31:32)])
names(dadosEntrada5)[5] <- "RODADA_NORM"
dadosEntrada5 <- dadosEntrada5[,c(-14,-16,-20,-27)]
dbWriteTable(con, "DADOS_CONSOLIDADOS_ENTRADA5", dadosEntrada5)

#################### Alteração do ENTRE_MELHORES ####################
dbExecute(con, "ALTER TABLE DADOS_CONSOLIDADOS_ENTRADA1 ADD QUINTIL VARCHAR(1)")
dbExecute(con, "ALTER TABLE DADOS_CONSOLIDADOS_ENTRADA2 ADD QUINTIL VARCHAR(1)")
dbExecute(con, "ALTER TABLE DADOS_CONSOLIDADOS_ENTRADA3 ADD QUINTIL VARCHAR(1)")
dbExecute(con, "ALTER TABLE DADOS_CONSOLIDADOS_ENTRADA4 ADD QUINTIL VARCHAR(1)")
dbExecute(con, "ALTER TABLE DADOS_CONSOLIDADOS_ENTRADA5 ADD QUINTIL VARCHAR(1)")

#Quintil da Entrada1
dbExecute(con, "UPDATE DADOS_CONSOLIDADOS_ENTRADA1 X SET X.QUINTIL = 
(SELECT SEL.QUINTIL FROM
(SELECT A.ANO, A.SIGLA, A.APELIDO, A.RODADA, A.ID_POSICAO, NTILE(5) OVER (PARTITION BY ANO, RODADA, ID_POSICAO ORDER BY PONTOS ASC) QUINTIL
FROM DADOS_CONSOLIDADOS_ENTRADA1 A) SEL
WHERE SEL.ANO = X.ANO
AND SEL.SIGLA = X.SIGLA
AND SEL.APELIDO = X.APELIDO
AND SEL.RODADA = X.RODADA)")
dbCommit(con);

dbExecute(con, "UPDATE DADOS_CONSOLIDADOS_ENTRADA2 X SET X.QUINTIL = 
          (SELECT A.QUINTIL FROM DADOS_CONSOLIDADOS_ENTRADA1 A
          WHERE A.ANO = X.ANO
          AND A.RODADA = X.RODADA
          AND A.SIGLA = X.SIGLA
          AND A.APELIDO = X.APELIDO)");
dbCommit(con);

dbExecute(con, "UPDATE DADOS_CONSOLIDADOS_ENTRADA3 X SET X.QUINTIL = 
          (SELECT A.QUINTIL FROM DADOS_CONSOLIDADOS_ENTRADA1 A
          WHERE A.ANO = X.ANO
          AND A.RODADA = X.RODADA
          AND A.SIGLA = X.SIGLA
          AND A.APELIDO = X.APELIDO)");
dbCommit(con);

dbExecute(con, "UPDATE DADOS_CONSOLIDADOS_ENTRADA4 X SET X.QUINTIL = 
          (SELECT A.QUINTIL FROM DADOS_CONSOLIDADOS_ENTRADA1 A
          WHERE A.ANO = X.ANO
          AND A.RODADA = X.RODADA
          AND A.SIGLA = X.SIGLA
          AND A.APELIDO = X.APELIDO)");
dbCommit(con);

dbExecute(con, "UPDATE DADOS_CONSOLIDADOS_ENTRADA5 X SET X.QUINTIL = 
          (SELECT A.QUINTIL FROM DADOS_CONSOLIDADOS_ENTRADA1 A
          WHERE A.ANO = X.ANO
          AND A.RODADA = X.RODADA
          AND A.SIGLA = X.SIGLA
          AND A.APELIDO = X.APELIDO)");
dbCommit(con);
