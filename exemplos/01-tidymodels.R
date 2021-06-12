# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(skimr)
library(tidymodels)
library(rpart.plot)

# Dados -------------------------------------------------------------------
data("diamonds")

# EAD ---------------------------------------------------------------------
# Exploratory Data Analysis

# Explora as colunas e seus respectivos tipos
glimpse(diamonds)


# Explora as colunas e dados, fornece informações diferentes do glimpse,
# pode ser considerado como uma evolução do summary
# bom pra identificar outliers com o "hist"
skim(diamonds)

# Faz correlações diversas entre os dados criando pares, o sample_n é para gerar 
# uma amostra menor dos dados, assim não será necessário fazer para todas as linhas
GGally::ggpairs(diamonds %>% sample_n(2000))


# Verifica uma possível relação entre o preço e a variável "x"
qplot(x, price, data = diamonds)



# Precisamos passar pro R:
# 1. A f que queremos usar
# 2. Ajustar a f para um conjunto de dados

# ----------------------------------------------------------------------
# Passo 1: Especificações de 
# a) a f (a hipótese) com seus respectivos hiperparâmetros; 
# b) o pacote 'motor' (engine);
# c) a tarefa/modo ("regression" ou "classification").

especificacao_modelo <- 
  
  # árvore, o número é pra definir a complexidade, nesse caso, muito baixa
  decision_tree(cost_complexity = 0.001) %>%
  
  # qual pacote irá ajustar a árvore
  set_engine("rpart") %>%
  
  # essa vai ser uma árvore de "regressão"
  set_mode("regression")


# Outros exemplos...

# especificacao_modelo <- linear_reg() %>%
# set_engine("lm") %>%
# set_mode("regression")

# especificacao_modelo <- rand_forest() %>% 
# set_engine("ranger") %>% 
# set_mode("regression)


# --------------------------------------------------------------------
# Passo 2: Ajuste do modelo

modelo <- 
  especificacao_modelo %>% 
  fit(price ~ x, data = diamonds)

print(modelo)

# plota a árvore
rpart.plot::prp(modelo$fit)


# --------------------------------------------------------------------
# Passo 3: Analisar as previsões
library(dplyr)

diamonds_com_previsao <- diamonds %>% 
  mutate(
    price_pred = predict(modelo, new_data = diamonds)$.pred
  )


# Pontos observados + curva da f
diamonds_com_previsao %>%
  filter(x > 0) %>%
  ggplot() +
  geom_point(aes(x, price), alpha = 0.3) +
  geom_step(aes(x, price_pred), color = 'red', size = 1) +
  theme_bw()


# Observado vs Esperado
diamonds_com_previsao %>%
  filter(x > 0) %>%
  ggplot() +
  geom_point(aes(price_pred, price)) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw()

# Como quantificar a qualidade de um modelo?

library(yardstick)


# Métricas de erro: Erro quadrático médio (RMSE)
diamonds_com_previsao %>% rmse(truth = price, estimate = price_pred)

# residuo = truth-estimate
diamonds_com_previsao %>% mae(truth = price, estimate = price_pred)

# r quadrado
diamonds_com_previsao %>% rsq(truth = price, estimate = price_pred)


