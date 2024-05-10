# Feito por Claudionor Ferreira da Silva Junior


# Importando pacotes necessários

library(corrplot)
library(dplyr)
library(GGally)
library(ggplot2)
library(caret)
library(factoextra)

# Importando base
tumor <- read.csv("wdbc.data", header = FALSE)
colnames(tumor) <- c("ID", "Diagnosis", "Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity", "Concave_points", "Symmetry", "Fractal_dimension", "Radius_SE", "Texture_SE", "Perimeter_SE", "Area_SE", "Smoothness_SE", "Compactness_SE", "Concavity_SE", "Concave_points_SE", "Symmetry_SE", "Fractal_dimension_SE", "Radius_W", "Texture_W", "Perimeter_W", "Area_W", "Smoothness_W", "Compactness_W", "Concavity_W", "Concave_points_W", "Symmetry_W", "Fractal_dimension_W")

# Separando a base em treino e teste

set.seed(340) # Escolhendo uma seed para fixarmos a amostra treino e teste

train <- createDataPartition(tumor$Diagnosis, p = 0.75, list = F)

tumor_treino <- tumor[train,]
tumor_teste <- tumor[-train,]

# Fazendo o tratamento na base treino

# Vamos retirar a colunas desnecessárias para a análise (coluna ID)
tumor_treino <- tumor_treino[,-1]

# Vamos verificar se há valores faltantes
sum(is.na(tumor_treino)) # Sem valores faltantes

# Verificando correlação entre as variaveis
correlacao <- cor(tumor_treino[,-1])
# Definir o tamanho do dispositivo gráfico
corrplot(correlacao, method = "circle")

# Através da matriz de correlação, é notável que existem variáveis altamente correlacionadas.
# Vamos tentar reduzir a dimensionalidade do banco através de uma análise de componentes principais
# A fim de reduzir a multicolinearidade

tumor_treino$Diagnosis <- as.factor(tumor_treino$Diagnosis) # Transformando a variável resposta em fator

# Utilizando o comando pca
pca <- prcomp(tumor_treino[,-1], scale. = T)  # Implantando o algoritmo de componentes principais

fviz_eig(pca, addlabels = T)

# Através do Scree Plot podemos perceber que a variável os 3 primeiros PC's
# ditam 80,3% da variabilidade do conjunto de dados

tumor_treino_pca <- predict(pca, newdata = tumor_treino) # Acrescentando os novos componentes principais no banco

# Alguns tratamentos dos dados treino
Diagnosis <- tumor_treino$Diagnosis
tumor_treino_pca <- as.data.frame(tumor_treino_pca)

dados_com_pcs <- cbind(tumor_treino_pca, Diagnosis) # Devolvendo a variável respostando ao banco

dados_com_pcs_tratado <- dados_com_pcs[,c(1,2,3,31)] # Deixando apenas as 3 primeiras componentes principais que representam 80% da variabilidade do conjunto de dados


# Fazendo tratamento na base teste (o mesmo que fizemos na base treino)

tumor_teste$Diagnosis <- as.factor(tumor_teste$Diagnosis)

tumor_teste <- tumor_teste[,-1]
tumor_teste_pca <- predict(pca, newdata = tumor_teste)

tumor_teste_pca <- as.data.frame(tumor_teste_pca)

Diagnosis <- tumor_teste$Diagnosis

unique(Diagos_t)

tumor_teste_pcs <- cbind(tumor_teste_pca, Diagnosis)
dados_teste <- tumor_teste_pcs[,c(1,2,3,31)]


# Treinando o modelo

controle <- trainControl(method = "cv", number = 10) # Aqui utilizamos o método k-fold para treinarmos o modelo

modelo_glm <- train(Diagnosis~., data = dados_com_pcs_tratado, method = "glm", trControl = controle) # O modelo usado será o de Regressão Logística


# Resultados do modelo

predicao <- predict(modelo_glm, dados_teste) # Implantando a base teste para testarmos o modelo treinado

confusionMatrix(predicao, dados_teste[,4]) # Conclui-se uma Ácuracia de 92%; Sensibilidade de 93% e Especificidade de 92%


# Dessa forma, foi encontrado um modelo que se saiu bem para valores fora da amostra original do dados,
# indicando que o modelo consegue designar bem se o tumor de uma pessoa é Benigno ou Maligno;


