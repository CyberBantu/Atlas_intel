# -----------------
# Importando a base dos dados e bibliotecas
library(readr)
library(readxl)
library(janitor)
library(ggplot2)
library(tibble)
library(lubridate)
library(tidyr)
library(dplyr)
rm(list = ls())
# Importando a base para a primeira analise - Sendo a tabela para a primeira questão 
# O objetivo é replicar parte das analise feitas no Pyhon, porem vai haver algumas diferenças

"""
Parte 1 --> Evolução Primeira Tabela

1. Faça um gráfico que represente a evolução das das respostas durante este ano
2. O que você pode concluir sobre a aprovação (e respectivamente, deseproação) desse candidato?
Parte 2 --> Respondentes

1. O que você pode concluir sobre as variaveis de Renda Mensal e Idade dos respondentes ( dica: Calcular os diferentes momentos da variavel, mediana, média, quartis)
2. Qual a porcentagem de Homen entre os respondentes? QUal a porcentagem de Mulheres?
3. Qual a porcentagem de solteiros entre os respondem?
"""
# ----------------
# Importando a base
df_temporal= read_excel('tarefa_atlas_intel.xlsx', sheet = 2)

# Transpondo
df_temporal_t = t(df_temporal)

# convertando
df_temporal_t = as.data.frame(df_temporal_t)

# Transformando o indice em uma nova coluna renomeado
df_temporal_t = rownames_to_column(df_temporal_t, "Data")

colnames(df_temporal_t) = df_temporal_t[1,]

df_temporal_t = df_temporal_t[-1,]

# aqui eu tive um problema com o nome mesmo depois de todo o tratamento, vou ter que renomear a a ntiga coluna de indice
colnames(df_temporal_t)

# modificando
colnames(df_temporal_t)[1] = "Data"

# Tratando os dados --------------------------

str(df_temporal_t)

# mudando os tipos das colunas
df_temporal_t$Data = as.Date(df_temporal_t$Data)
df_temporal_t$Approve = as.double(df_temporal_t$Approve)
df_temporal_t$Disapprove = as.double(df_temporal_t$Disapprove)
df_temporal_t$`Don't know` = as.double(df_temporal_t$`Don't know`)

# verificando
str(df_temporal_t)

# mudandooo nome das colunas
colnames(df_temporal_t)[2] = "aprovacao"
colnames(df_temporal_t)[3] = "desaprovacao"
colnames(df_temporal_t)[4] = "nao_sabe"

str(df_temporal_t)

""" 
Parte 1 --> Evolução Primeira Tabela

1. Faça um gráfico que represente a evolução das das respostas durante este ano
"""
# Plotando

ggplot(data = df_temporal_t, aes(x = Data))+
  geom_line(aes(y = aprovacao, color = "Aprovação"))+
  geom_line(aes(y = desaprovacao, color = "Desaprovação"))+
  geom_line(aes(y = nao_sabe, color = "Não sabe"))+
  theme_classic()+
  scale_color_manual(values = c("#6bb38e", "#c90a00", "#ffc501"), 
                     labels = c("Aprovação", "Desaprovação", "Não sabe"))+
  labs(x = "Data", y = "Percentual (%)", 
       title = "Índice de aprovação do Candidato por Dia", 
       color = "Respostas")

# Tratando os dados novamente para poder extrair a semana, dia, mes e trimestre

df_temporal_t = df_temporal_t %>% mutate(dia = day(Data),
                                         mes = month(Data),
                                         semana = week(Data),
                                         trimestre = quarter(Data))



# Mês--------------
# Aprovação por Mes
# Plot
grup_mes = df_temporal_t %>% group_by(mes) %>% summarize(mes_aprovacao= mean(aprovacao),
                                                         mes_desaprovacao= mean(desaprovacao),
                                                         mes_nao_sabe = mean(nao_sabe))


ggplot(data = grup_mes, aes(x = mes))+
  geom_col(aes(y = mes_desaprovacao, fill = "Desaprovação"), position = position_dodge(width = 0.7))+
  geom_col(aes(y = mes_aprovacao, fill = "Aprovação"), position = position_dodge(width = 0.7))+
  geom_col(aes(y = mes_nao_sabe, fill = "Não sabe"), position = position_dodge(width = 0.7))+
  geom_text(aes(y = mes_aprovacao, label = paste0(round(mes_aprovacao,2), "%"), size = 10)
            , position = position_dodge(width = 0.7),
            vjust = -0.5
            , size = 4)+
  geom_text(aes(y = mes_desaprovacao, label = paste0(round(mes_desaprovacao,2), "%"))
            , position = position_dodge(width = 0.7)
            , vjust = -0.5
            , size = 4)+
  geom_text(aes(y = mes_nao_sabe, label = paste0(round(mes_nao_sabe,2), "%"))
            , position = position_dodge(width = 0.7)
            , vjust = -0.5
            , size = 4)+
  scale_fill_manual(values = c("#6bb38e", "#c90a00", "#ffc501"), 
                    labels = c("Aprovação", "Desaprovação", "Não sabe"))+
  theme_classic()+
  labs(x = "Mês", y = "Percentual (%)", 
       title = "Índice de aprovação Média do Candidato por Mês", 
       fill = "Respostas")

# Semana -------------
grup_semana = df_temporal_t %>% group_by(semana) %>% summarize(semana_aprovacao= mean(aprovacao),
                                                         semana_desaprovacao= mean(desaprovacao),
                                                         semana_nao_sabe = mean(nao_sabe))


ggplot(data = grup_semana, aes(x = semana))+
  geom_col(aes(y = semana_desaprovacao , fill = "Desaprovação"), position = position_dodge(width = 0.7))+
  geom_col(aes(y = semana_aprovacao , fill = "Aprovação"), position = position_dodge(width = 0.7))+
  geom_col(aes(y = semana_nao_sabe , fill = "Não sabe"), position = position_dodge(width = 0.7))+
  scale_fill_manual(values = c("#6bb38e", "#c90a00", "#ffc501"), 
                    labels = c("Aprovação", "Desaprovação", "Não sabe"))+
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  theme_classic()+
  labs(x = "Semana", y = "Percentual (%)", 
       title = "Índice de aprovação Média do Candidato por Semana", 
       fill = "Respostas")


# Trimestre -----
# Agrupando
grup_trimestre = df_temporal_t %>% group_by(trimestre) %>% summarize(trimestre_aprovacao= mean(aprovacao),
                                                                     trimestre_desaprovacao= mean(desaprovacao),
                                                                     trimestre_nao_sabe = mean(nao_sabe))

# plort
ggplot(data = grup_trimestre, aes(x = trimestre))+
  geom_col(aes(y = trimestre_desaprovacao, fill = "Desaprovação"), position = position_dodge(width = 0.7))+
  geom_col(aes(y = trimestre_aprovacao, fill = "Aprovação"), position = position_dodge(width = 0.7))+
  geom_col(aes(y = trimestre_nao_sabe, fill = "Não sabe"), position = position_dodge(width = 0.7))+
  scale_fill_manual(values = c("#6bb38e", "#c90a00", "#ffc501"), 
                    labels = c("Aprovação", "Desaprovação", "Não sabe"))+
  geom_text(aes(y = trimestre_aprovacao, label = paste0(round(trimestre_aprovacao,2), "%"), size = 10)
            , position = position_dodge(width = 0.7),
            vjust = -0.5
            , size = 4)+
  geom_text(aes(y = trimestre_desaprovacao, label = paste0(round(trimestre_desaprovacao,2), "%"))
            , position = position_dodge(width = 0.7)
            , vjust = -0.5
            , size = 4)+
  geom_text(aes(y = trimestre_nao_sabe, label = paste0(round(trimestre_nao_sabe,2), "%"))
            , position = position_dodge(width = 0.7)
            , vjust = -0.5
            , size = 4)+
  theme_classic()+
  labs(x = "Semana", y = "Percentual (%)", 
       title = "Índice de aprovação Média do Candidato por Trimestre", 
       fill = "Respostas")


"""
Conclusão Tabela 1
Alguns pontos são observado nos dados e nos dão um panorama sobre o indice de aprovação do candidato - Observação dos dados por mês, semana e trimestre

2. Tendência de crescimento no índice de aprovação do candidato
3. Diminuição da taxa de desaprovação
4. Maior diminuição na taxa de desaprovação no mês de Julho
5.Uma diminuição no ultimo trimeste da taxa de Não sabe que pode refletir no aumento da aprovação do canditado no mesmo período
  Tendência de crescimento a partir da 25ª semana até o final do ano.
"""

# Parte 2 -------
"""
Parte 2 --> Respondentes

1. O que você pode concluir sobre as variaveis de Renda Mensal e Idade dos respondentes ( dica: Calcular os diferentes momentos da variavel, mediana, média, quartis)
2. Qual a porcentagem de Homen entre os respondentes? QUal a porcentagem de Mulheres?
3. Qual a porcentagem de solteiros entre os respondem?
"""

# Importando dados dos respondentes
df_respondentes = read_excel('tarefa_atlas_intel.xlsx', sheet = 3)

# olhando os tipos
str(df_respondentes)

# Capturando informações de maneira sumarizada
summary(df_respondentes)

"""
    Idade        Renda Mensal          
  Min.   :18.00   Min.   : 1231         
  1st Qu.:29.00   1st Qu.: 4004     
  Mediana : 6964   Mediana : 6964   
  Media   :39.39   Media   : 6698                                        
  3rd Qu.:51.50   3rd Qu.: 8930                                        
  Max.   :64.00   Max.   :12485                                        


 Acima temos a representaão informações de Minima de idade, Valor do Primeiro Quartil, Mediana, Média , 3 Quartil e Maximo

"""


"""
Sobre a Idade Pode-se concluir que:

1. As idades estão entre 18 e 64 anos
2. 50 % dos respondentes de até 38 anos
3. A média Geral é de 39 anos
4. Desvio Padrão de 13
"""
# BOxplot - Com isso é possível ter uma ideia de como a distribuição esta sendo feita para idade, cruzando com fatores de Raça e Gênero ------
# Plotando o grafico Box Plot de idade

ggplot()+
  geom_boxplot(data = df_respondentes, aes(x = Idade), fill ='#26979f')+
  labs(title = 'BoxPlot de Idade dos respondentes da pesquisa')+
  theme_minimal()


# Infos de Boxplot crusado por raça
ggplot()+
  geom_boxplot(data = df_respondentes, aes(x = Idade, y = Raça), fill ='#26979f')+
  labs(title = 'BoxPlot de Idade e Raça dos respondentes da pesquisa')+
  theme_minimal()

# Infos de Boxplot crusado por Genero
ggplot()+
  geom_boxplot(data = df_respondentes, aes(x = Idade, y = Gênero), fill ='#26979f')+
  labs(title = 'BoxPlot de Idade e Gênero dos respondentes da pesquisa')+
  theme_minimal()

# Questão 2 Porcentagem de Homens e Mulheres------

agrup_genero = df_respondentes %>% group_by(Gênero) %>% summarise(respondentes = n()) %>% 
  mutate(porcentagem = round(respondentes / sum(respondentes) * 100 , 2 )
         )

"""
A porcentagem de Homens é de 62.63% e de Mulheres é de 37.37 %
58 % dos respondentes são homens são brancos
73 % das mulheres são brancas
"""
# Colocando nomes padronzado
df_respondentes = df_respondentes %>% clean_names()

# Plotando boxplot de renda por raça
ggplot(data = df_respondentes)+
  geom_boxplot(aes(x = renda_mensal, y = raca), fill = '#26979f')+
  labs(title = 'Boxplot da Faixa de Renda por Cor dos Respondentes',
       caption = 'Christian Basilio')+
  theme_minimal()

"""
1. Obsevando a raça e renda é possivel ver que pretos tem a faixa de renda mais deslocada ao topo, mesmo com um outlier com valor baixo
2. A renda dos bracos e pardos possuem uma grande dispesar, sendo a mediana de pardos a menor
"""

# 3. Qual a porcentagem de solteiros entre os respondem? --------

agp_estado_civil = df_respondentes %>% group_by(estado_civil) %>% summarise(respondentes = n()) %>% 
  mutate(porcentagem = round(respondentes / sum(respondentes) * 100 , 2 )
  )

# plot do grafico
ggplot(data = agp_estado_civil) +
  geom_col(aes(x = estado_civil, y = respondentes, fill = estado_civil)) +
  geom_text(aes(x = estado_civil, y = respondentes, label = respondentes), vjust = -0.5) +
  labs(title = "Distribuição dos respondentes por estado civil", fill = "Estado civil", x = "Estado civil", y = "Número de respondentes") +
  theme(plot.title = element_text(size = 15, hjust = 0.5), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14))




# Solteiros são 13,3 % dos casos      

