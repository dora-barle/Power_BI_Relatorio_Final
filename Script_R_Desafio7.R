# Crie o boxplot com ggplot2
library(ggplot2)
ggplot(dataset, aes(x = Country, y = Sales, fill= Country)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Total Vendas x Produto",
    x = "País",
    y = "Vendas"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", family = "Trebuchet MS", hjust = 0.5),  
    axis.title.x = element_text(size = 14, family = "Trebuchet MS"), 
    axis.title.y = element_text(size = 14, family = "Trebuchet MS"), 
    axis.text.x = element_text(size = 12, family = "Trebuchet MS"), 
    axis.text.y = element_text(size = 12, family = "Trebuchet MS")  
  )



#######################################################################
library(ggplot2)
# Criação do modelo de regressão linear
modelo <- lm(Units_Sold ~ Discounts, data = dataset)

# Gráfico de dispersão com linha de regressão
ggplot(dataset, aes(x = Discounts, y = Units_Sold)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +  # Adiciona a linha de regressão
  labs(title = "Regressão entre Descontos e Unidades Vendidas",
       x = "Descontos (%)",
       y = "Unidades Vendidas") +
       theme(
    plot.title = element_text(size = 20, face = "bold", family = "Trebuchet MS", hjust = 0.5),  
    axis.title.x = element_text(size = 14, family = "Trebuchet MS"),  
    axis.title.y = element_text(size = 14, family = "Trebuchet MS"), 
    axis.text.x = element_text(size = 12, family = "Trebuchet MS"),  
    axis.text.y = element_text(size = 12, family = "Trebuchet MS")  
    )

########################################################################
# Modelo de Previsao de Vendas 2015
crescimento_medio <- (dataset$Sales[2] - dataset$Sales[1]) / dataset$Sales[1]

# Definir o desvio padrão para adicionar variabilidade à simulação
desvio_padrao <- 0.02  # 2% de variabilidade

# Simular as vendas para cada mês de 2015 com o crescimento médio e a variabilidade
set.seed(42)  # Para reprodutibilidade
sales_2015_simuladas <- dataset$Sales[2] * (1 + crescimento_medio + rnorm(12, mean = 0, sd = desvio_padrao))

# Criar um dataframe com os resultados simulados para cada mês de 2015
meses_2015 <- seq.Date(from = as.Date("2015-01-01"), by = "month", length.out = 12)
dataset_2015 <- data.frame(
  Mes = meses_2015,
  Sales = sales_2015_simuladas
)

library(ggplot2)

ggplot(dataset_2015, aes(x = Mes, y = Sales)) +
  geom_line(color = "purple", size = 1.5) +
  labs(title = "Projeção de Vendas para 2015",
       x = "Mês",
       y = "Vendas") +
        theme(
    plot.title = element_text(size = 20, face = "bold", family = "Trebuchet MS", hjust = 0.5),  
    axis.title.x = element_text(size = 14, family = "Trebuchet MS"),  
    axis.title.y = element_text(size = 14, family = "Trebuchet MS"), 
    axis.text.x = element_text(size = 12, family = "Trebuchet MS"),  
    axis.text.y = element_text(size = 12, family = "Trebuchet MS")  
    )

