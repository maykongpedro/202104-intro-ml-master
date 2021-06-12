
# Pacotes -----------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(ggplot2)

# Dados -------------------------------------------------------------------

data("diamonds")


# pegando uma parte dos dados
set.seed(1)
base_inicial <- diamonds %>% 
  sample_n(10000)



# Separar treino e teste --------------------------------------------------

# aqui eu uso as funcoes initial_split + training e test

# Definindo 75% da base como treino
quebra_em_treino_e_teste <- initial_split(base_inicial, prop = .75)

base_treino <- training(quebra_em_treino_e_teste)
base_teste <- testing(quebra_em_treino_e_teste)


# Definir o esqueleto do modelo -------------------------------------------

especificacao_arvore <- decision_tree(
  
  # Aqui colocamos os hiperparâmetros:
  cost_complexity = 0.001, # definindo a complexidade como baixa
  min_n = 5,
  tree_depth = tune() # a função tune manda o R procurar esse hiperparâmetro, nesse caso,
                      # a profundidade da árvore
) %>% 
  
  set_mode("regression") %>% 
  set_engine("rpart")



# Quebrar a base em treino e validacao ------------------------------------

quebra_treino_validacao <- validation_split(base_treino, prop = .75)

# Ajustar vários valores do hiperparametro na base treino e calcul --------

resultado_varios_testes <- tune_grid(
  especificacao_arvore,     # sempre definir primeiro a especificação
  price ~ x,                # fórmula
  quebra_treino_validacao,  # quebra construida
  grid = tibble(tree_depth = 1:20), # quantos pontos queremos que ele calcule, definindo
                                    # dessa forma obrigamos ele a calcular todos
  
  
  #grid = 20,                # quantos pontos queremos que ele calcule, se definido dessa forma,
                            # o R para quando achar a melhor alternativa possível
  metrics = metric_set(rmse, rsq, mae)  # quais métricas queremos usar
)

resultado_varios_testes

# verificando resultados do tune_grid
collect_metrics(resultado_varios_testes) %>% 
  arrange(tree_depth) %>% 
  count(tree_depth)


# Plota as três métricas que escolhemos para várias profundidades de árvores
autoplot(resultado_varios_testes)

# Retorna o melhor resultado segundo a métrica requerida
show_best(resultado_varios_testes, metric = "rmse", n = 3)
show_best(resultado_varios_testes, metric = "rsq", n = 3)

# especificacao do modelo final -------------------------------------------

especificacao_modelo_final <-
  tune::finalize_model(especificacao_arvore,
                       tune::select_best(resultado_varios_testes, 
                                         "rmse"))


especificacao_modelo_final


# ajuste final ------------------------------------------------------------

modelo_final <- fit(especificacao_modelo_final,
    y ~ x,
    data = base_treino)

library(rpart.plot)

prp(modelo_final$fit)

# calculo final do erro ---------------------------------------------------

base_teste_com_pred <- base_teste %>% 
  mutate(
    price_pred = predict(modelo_final, base_teste)$.pred
  )

base_teste_com_pred %>% 
  rmse(truth = price, estimate = price_pred)

base_teste_com_pred %>% 
  rsq(truth = price, estimate = price_pred)

base_teste_com_pred %>% 
  mae(truth = price, estimate = price_pred)

