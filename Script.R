usethis::use_github()
library(tidverse)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)

censo_1851<-read_excel("data-raw/censo1851-profissoes-mulheres.xlsx")

censo_menos_total<- subset(censo_1851, !Occupations=="Total of Females")



grafico_todas_idades<-censo_todas_idades%>%
 arrange(desc(censo_todas_idades$`All ages`))%>%slice_max(`All ages`,n=10)%>%
  mutate(Occupations=forcats::fct_reorder(Occupations,`All ages`))%>%
  ggplot()+
  geom_col(aes(x = Occupations, y = `All ages`))+
scale_y_continuous(labels = scales::unit_format(unit = "milhões", scale = 1e-6)) +
  coord_flip() +
  labs(x = "Ocupação", y = "Quantidade") +
  theme_economist()
grafico_todas_idades


grafico_menos_20<-censo_todas_idades%>%
  arrange(desc(censo_todas_idades$`Under 20 years`))%>%slice_max(`Under 20 years`,n=10)%>%
  mutate(Occupations=forcats::fct_reorder(Occupations,`Under 20 years`))%>%
  ggplot()+
  geom_col(aes(x = Occupations, y = `Under 20 years`))+
  scale_y_continuous(labels = scales::unit_format(unit = "milhões", scale = 1e-6)) +
  coord_flip() +
  labs(x = "Ocupação", y = "Quantidade") +
  theme_economist()
grafico_menos_20

grafico_mais_20<-censo_todas_idades%>%
  arrange(desc(censo_todas_idades$`20 years & upwards`))%>%slice_max(`20 years & upwards`,n=10)%>%
  mutate(Occupations=forcats::fct_reorder(Occupations,`20 years & upwards`))%>%
  ggplot()+
  geom_col(aes(x = Occupations, y = `20 years & upwards`))+
  scale_y_continuous(labels = scales::unit_format(unit = "milhões", scale = 1e-6)) +
  coord_flip() +
  labs(x = "Ocupação", y = "Quantidade") +
  theme_economist()
grafico_mais_20

grafico_menos_20+grafico_mais_20
