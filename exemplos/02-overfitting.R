# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(skimr)
library(tidymodels)


# Dados -------------------------------------------------------------------
data("diamonds")

set.seed(8)
diamondsinho <- diamonds %>%
  filter(x > 0) %>% 
  group_by(x) %>%
  sample_n(1) %>%
  ungroup()

# definicao do modelo -----------------------------------------------------

# árvore mais travada, os hiperparâmetros limitam o crescimento dela
especificacao_modelo1 <-
  
  # precisa ter no mínimo 5 observações em cada folha e só pode crescer 10x
  # se "cost_complexity" for zero, a árvore pode crescer e cortar o quanto quiser,
  # o que vai acaar gerando o overfitting
  decision_tree(cost_complexity = 0.004, min_n = 5, tree_depth = 10) %>%
  set_engine("rpart") %>%
  set_mode("regression")


# árvore mais flexível, pois pode crescer bastante, eventualmente vai dar overfitting
especificacao_modelo2 <- 
  decision_tree(cost_complexity = 0, min_n = 2, tree_depth = 20) %>%
  set_engine("rpart") %>%
  set_mode("regression")


# ajuste do modelo --------------------------------------------------------
ajuste_modelo1 <- especificacao_modelo1 %>% fit(price ~ x, data = diamondsinho)
ajuste_modelo2 <- especificacao_modelo2 %>% fit(price ~ x, data = diamondsinho)


# predicoes ---------------------------------------------------------------
diamondsinho_com_previsao <- diamondsinho %>% 
  mutate(
    price_pred1 = predict(ajuste_modelo1, new_data = diamondsinho)$.pred,
    price_pred2 = predict(ajuste_modelo2, new_data = diamondsinho)$.pred
  )

# qualidade dos ajustes e graficos ----------------------------------------
# Métricas de erro
diamondsinho_com_previsao_longo <- diamondsinho_com_previsao %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) 


# olhando as métricas, conseguimos ver que o modelo 2 erra apenas 35 dólares (unidade do ajuste)
# erra pouco pq overfitou
diamondsinho_com_previsao_longo %>%
  group_by(modelo) %>%
  rmse(truth = price, estimate = price_pred)



# r quadrado, modelo 2 com r² = 1
diamondsinho_com_previsao_longo %>%
  group_by(modelo) %>%
  rsq(truth = price, estimate = price_pred)


# Pontos observados + curva da f
diamondsinho_com_previsao_g1 <- diamondsinho_com_previsao %>%
  ggplot() +
  geom_point(aes(x, price), size = 3) +
  geom_step(aes(x, price_pred2, color = 'modelo2'), size = 1) +
  geom_step(aes(x, price_pred1, color = 'modelo1'), size = 1) +
  theme_bw()
diamondsinho_com_previsao_g1


# Observado vs Esperado
diamondsinho_com_previsao_g2 <- diamondsinho_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price, colour = modelo), size = 3) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw()
diamondsinho_com_previsao_g2


# resíduos vs Esperado
diamondsinho_com_previsao_g3 <- diamondsinho_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price - price_pred, colour = modelo), size = 3) +
  geom_abline(slope = 0, intercept = 0, colour = "purple", size = 1) +
  ylim(c(-10000,10000)) +
  labs(y = "resíduo (y - y_chapeu)") +
  theme_bw()
diamondsinho_com_previsao_g3

############################################################################
############################################################################
############################################################################
# Agora vamos fingir que estamos em produção! (pontos vermelhos do slide)

set.seed(3)
# "dados novos chegaram..."
diamondsinho_novos <- 
  diamonds %>%
  filter(x > 0) %>% 
  sample_n(100)


# predicoes ---------------------------------------------------------------
diamondsinho_novos_com_previsao <- 
  diamondsinho_novos %>%
  mutate(
    # quando usamos esse "new_data" estamos fazendo o ajuste pra dados novos
    price_pred1 = predict(ajuste_modelo1, new_data = diamondsinho_novos)$.pred,
    price_pred2 = predict(ajuste_modelo2, new_data = diamondsinho_novos)$.pred
  )

diamondsinho_novos_com_previsao


# qualidade dos ajustes e graficos ----------------------------------------
# Métricas de erro
diamondsinho_novos_com_previsao_longo <- diamondsinho_novos_com_previsao %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) 

diamondsinho_novos_com_previsao_longo

# Podemos observar que o erro gerado pelo modelo de overfitting foi muito maior,
# porque ele não conseguiu se adaptar aos dados novos
diamondsinho_novos_com_previsao_longo %>%
  group_by(modelo) %>%
  rmse(truth = price, estimate = price_pred)

# r quadrado também baixou  
diamondsinho_novos_com_previsao_longo %>%
  group_by(modelo) %>%
  rsq(truth = price, estimate = price_pred)


# Pontos observados + curva da f
diamondsinho_novos_com_previsao_g1 <- 
  diamondsinho_novos_com_previsao %>%
  ggplot() +
  geom_point(aes(x, price), size = 3) +
  geom_step(aes(x, price_pred2, color = 'modelo2'), size = 1) +
  geom_step(aes(x, price_pred1, color = 'modelo1'), size = 1) +
  theme_bw()

# vendo o gráfico do ajuste e o gráfico com os dados novos juntos usando o 
# pacote "pacthwork"
diamondsinho_com_previsao_g1 / diamondsinho_novos_com_previsao_g1



# Observado vs Esperado
diamondsinho_novos_com_previsao_g2 <- diamondsinho_novos_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price, colour = modelo), size = 3) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw()

# vendo o gráfico do ajuste e o gráfico com os dados novos juntos usando o 
# pacote "pacthwork"
diamondsinho_com_previsao_g2 / diamondsinho_novos_com_previsao_g2



# resíduos vs Esperado
diamondsinho_novos_com_previsao_g3 <- 
  diamondsinho_novos_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price - price_pred, colour = modelo), size = 3) +
  geom_abline(slope = 0, intercept = 0, colour = "purple", size = 1) +
  ylim(c(-10000,10000)) +
  labs(y = "resíduo (y - y_chapeu)") +
  theme_bw()

# vendo o gráfico do ajuste e o gráfico com os dados novos juntos usando o 
# pacote "pacthwork"
diamondsinho_com_previsao_g3 / diamondsinho_novos_com_previsao_g3
