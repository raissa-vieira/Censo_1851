usethis::use_github()
library(tidverse)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(forcats)
#carregando a base:
censo_1851<-read_excel("data-raw/censo1851-profissoes-mulheres.xlsx")
#tirando a linha com o total:
censo_menos_total<- subset(censo_1851, !Occupations=="Total of Females")
#gráfico das profissões das mulheres com menos de 20 anos:
grafico_menos_20<-censo_menos_total%>%
  arrange(desc(censo_menos_total$`Under 20 years`))%>%slice_max(`Under 20 years`,n=10)%>%
  mutate(Occupations=forcats::fct_reorder(Occupations,`Under 20 years`))%>%
  ggplot()+
  geom_col(aes(x = Occupations, y = `Under 20 years`))+
  scale_y_continuous(labels = scales::unit_format(unit = "milhões", scale = 1e-6)) +
  coord_flip() +
  labs(x = "Ocupação", y = "Quantidade") +
  theme_economist()
grafico_menos_20
#gráfico das profissões das mulheres com mais de 20 anos:
grafico_mais_20<-censo_menos_total%>%
  arrange(desc(censo_menos_total$`20 years & upwards`))%>%
 slice_max(`20 years & upwards`,n=10)%>%
  mutate(Occupations=forcats::fct_reorder(Occupations,`20 years & upwards`))%>%
  mutate(Occupations =forcats::lvls_revalue(Occupations, rev(c("Esposa", "Empregada Doméstica", "Filha, neta, irmã, sobrinha",
                                                           "Viúva", "Produtora de vestido, chapeleira", "Esposa de fazendeiro ou criador de gado", "Manufatura de algodão",
  "Lavadeira de roupa", "Pensionista", "Esposa de Sapateiro"))))%>%
ggplot()+
  geom_col(aes(x = Occupations, y = `20 years & upwards`))+
  scale_y_continuous(labels = scales::unit_format(unit = "milhões", scale = 1e-6)) +
  coord_flip() +
  labs(x = "Ocupação", y = "Quantidade") +
  theme_economist()
grafico_mais_20

#gráfico com as profissões de todas as mulheres:
grafico_total<-censo_menos_total%>%
  arrange(desc(censo_menos_total$`All ages`))%>%
  slice_max(`All ages`,n=10)%>%
  mutate(Occupations=forcats::fct_reorder(Occupations,`All ages`))%>%
  ggplot()+
  geom_col(aes(x = Occupations, y = `All ages`))+
  scale_y_continuous(labels = scales::unit_format(unit = "milhões", scale = 1e-6)) +
  coord_flip() +
  labs(x = "Ocupação", y = "Quantidade") +
  theme_economist()
grafico_total


