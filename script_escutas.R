##SCRIPT ESCUTAS PNAB##

if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(lubridate) == F) install.packages("lubridate"); require(lubridate)
if(require(ggalt) == F) install.packages("ggalt"); require(ggalt)
if(require(gridExtra) == F) install.packages("gridExtra"); require(gridExtra)
if(require(VennDiagram) == F) install.packages("VennDiagram"); require(VennDiagram)
if(require(eulerr) == F) install.packages("eulerr"); require(eulerr)
if(require(ggrepel) == F) install.packages("ggrepel"); require(ggrepel)
if(require(magrittr) == F) install.packages("magrittr"); require(magrittr)
if(require(here) == F) install.packages("here"); require(here)
if(require(janitor) == F) install.packages("janitor"); require(janitor)
if(require(rio) == F) install.packages("rio"); require(rio)
if(require(stringr) == F) install.packages("stringr"); require(stringr)
if(require(splitstackshape) == F) install.packages("splitstackshape"); require(splitstackshape)
if(require(here) == F) install.packages("here"); require(here)
if(require(sf) == F) install.packages("sf"); require(sf)
theme_set(theme_bw())


# bancos
setwd(here("Dados"))

banco_geral <- read_xlsx("banco_geral.xlsx", sheet = "CONSOLIDADE X TEMA")
banco_mapa <- read_xlsx("banco_mapa.xlsx")
banco_escutas <- read_xlsx("banco_geral.xlsx", sheet = "ESCUTAS")


# GERAL---------

a <- banco_geral %>% group_by(OITIVA, TEMA) %>% summarise(n = n())

write.xlsx(a, "tabela1.xlsx")

## Temas

temas<- banco_geral %>% count(TEMA)

theme_set(theme_bw())
gtema <- ggplot(temas, aes (reorder (TEMA, n), n))+
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#716CA7") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =-0.1) +
  expand_limits(y = c(0, 350))+
  labs(y = "Número de contribuições", x = "Tema")+
  coord_flip()

gtema

ggsave ("gtema.png", height = 6, width = 8)


## Subtema

subtema <- banco_geral %>% count(TEMA, SUBTEMA) %>% 
  mutate(SUBTEMA= ifelse(SUBTEMA== "REMANEJAMENTO DE RECURSOS E RENDIMENTOS", "REMANEJAMENTO\nE RENDIMENTOS",
                         ifelse(SUBTEMA== "TRANSPARÊNCIA E FISCALIZAÇÃO", "TRANSPARÊNCIA\nE FISCALIZAÇÃO",
                                ifelse(SUBTEMA== "INSCRIÇÕES E IMPEDIMENTOS", "INSCRIÇÕES\nE IMPEDIMENTOS",
                                       SUBTEMA))))


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

gsubtema <- ggplot(subtema %>% filter(TEMA!= "EDITAIS"), aes(x= reorder_within(SUBTEMA,n,TEMA), y = n))+
  geom_bar(stat="identity", width=.6, aes(fill = TEMA))+
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(.~TEMA, scales = "free", ncol = 2)+ theme(legend.position = "none")+
  geom_text(aes(label=paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")), size = 4.5, hjust = -.1) +
  expand_limits(y = c(0, 180))+theme(legend.position = "none") +
  theme(panel.grid = element_blank(), axis.title = element_text(face = "bold"), 
        axis.text = element_text(size= 12),
        strip.text = element_text(size = 20))+
  labs(y = "Número de contribuições", x = "Subtemas")


gsubtema

ggsave ("gsubtema.png", height = 10, width = 12)

# Editais

gsubtema <- ggplot(subtema %>% filter(TEMA == "EDITAIS") %>% 
                     mutate(SUBTEMA = ifelse(SUBTEMA == "LINGUAGEM TÉCNICOS", "Técnicos",
                                   str_to_title(SUBTEMA))), 
                   aes(x= reorder(SUBTEMA,n), y = n)) +
  geom_segment(aes(xend=SUBTEMA, yend = 0), color="grey80", lty="dashed")+
  geom_point(size=2,color="#623E3E") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = - 0.1, size=3) +
  expand_limits(y = c(0, 40)) +
  labs(y = "Número de Contribuições", x = "Subtemas")

gsubtema

ggsave ("editais.png", height = 6, width = 8)

# CONTRIBUICOES POR LINGUAGENS

linguagem <- rbind(
  banco_escutas %>% select(CODE, LINGUAGEM),
  banco_mapa %>% select(CODE, LINGUAGEM)
) %>% unique()

banco_geral <- banco_geral %>% 
  left_join(linguagem, by = "CODE")


unique(banco_geral$LINGUAGEM)

banco_geral <- banco_geral %>% 
  mutate(LINGUAGEM = str_to_upper(LINGUAGEM))

banco_geral <- banco_geral %>% 
  mutate(LINGUAGEM = ifelse(LINGUAGEM == "GESTORES MUNICIPAIS DE CULTURA", "GESTÃO",
                            ifelse(LINGUAGEM == "ARTES DA DANÇA", "DANÇA",
                                   ifelse(LINGUAGEM == "TÉCNICO", "TÉCNICOS",
                                          ifelse(LINGUAGEM == "ARTES DO TEATRO", "TEATRO",
                                                 ifelse(LINGUAGEM == "MODA", "DESIGN E MODA",
                                                        ifelse(LINGUAGEM == "DESIGN", "DESIGN E MODA",
                                                               ifelse(LINGUAGEM == "MUSEUS", "PATRIMÔNIO",
                                                                      ifelse(LINGUAGEM == "OUTRO SEGMENTO CULTURAL", "OUTRA",
                                                                             LINGUAGEM)))))))))


linguagem <- banco_geral %>% count(LINGUAGEM) %>% 
  mutate(LINGUAGEM = str_to_title(LINGUAGEM))
names(banco_geral)

#
linguagem_plot <- ggplot(linguagem, aes(x= reorder(LINGUAGEM,n), y = n)) +
  geom_segment(aes(xend=LINGUAGEM, yend = 0), color="grey80", lty="dashed")+
  geom_point(size=2,color="#623E3E") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n), accuracy = 0.01), ")")),
            hjust = - 0.1, size=3) +
  expand_limits(y = c(0, 250)) +
  labs(y = "Número de Contribuições", x = "LINGUAGEM")

linguagem_plot

ggsave ("linguagem_escuta2.png", height = 5, width = 7)

# SUBTEMAS POR REGIAO
setwd(here("Dados"))
RD_Municipio <- read_xlsx("RD_Municipio.xlsx")
pernambuco <- st_read("PE_Municipios_2022.shp")

linguagem <- rbind(
  banco_escutas %>% select(CODE, `PRINCIPAL MUNICÍPIO DE ATUAÇÃO`) %>% rename(MUNICIPIO = 2),
  banco_mapa %>% select(CODE, MUNICIPIO)
) %>% mutate(MUNICIPIO = str_to_upper(MUNICIPIO)) %>% 
  unique()

banco_geral <- banco_geral %>% 
  left_join(linguagem, by = "CODE")



regiao <- banco_geral %>% 
  group_by(MUNICIPIO) %>% summarise(n = n()) %>% 
  mutate(MUNICIPIO = ifelse(MUNICIPIO == "LAGOA DO ITAENGA", "LAGOA DE ITAENGA",
                            MUNICIPIO))

pernambuco <- pernambuco %>% mutate(NM_MUN = toupper(NM_MUN)) %>% 
  left_join(regiao, by = c("NM_MUN" = "MUNICIPIO")) %>% 
  left_join(RD_Municipio %>% 
              mutate(municipio = toupper(municipio)), by = c("NM_MUN" = "municipio"))

pernambuco <- pernambuco %>% filter(CD_MUN != 2605459)


macrorregiao <- pernambuco %>% ungroup() %>% filter(is.na(n) == F) %>% 
  group_by(macrorreg_new) %>% 
  summarise(y = sum(n)) %>% as.data.frame() %>% select(macrorreg_new, y)

pernambuco <- pernambuco %>% left_join(macrorregiao)

g5 <- ggplot() +
    geom_sf(data = pernambuco, aes(fill = n), color = "black") +
    labs(title = "",
         fill = "Inscrições",
         caption = "*Fernando de Noronha contribuiu com 3 manifestações.\nSertão 416 (37,24%)\nRMR 336 (30,08%)\n Zona da Mata 144 (12,89%)\nAgreste 113 (10,12%)\nNão Informado 108 (9,67%)") +
    theme_minimal() +
    theme(plot.caption = element_text(size = 9))
plot(g5)


ggsave("mapa.png")

banco_geral <- banco_geral %>% 
  left_join(RD_Municipio %>% 
              mutate(municipio = str_to_upper(municipio)), by = c("MUNICIPIO" = "municipio"))

municipios <- banco_geral %>% 
  group_by(MUNICIPIO) %>% summarise(n = n()) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01))

write.xlsx(municipios, "municipio data.xlsx")

macrorregiao <- banco_geral %>% 
  group_by(macrorreg_new) %>% summarise(n = n()) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
         macrorreg_new = ifelse(is.na(macrorreg_new) == T, "Não Informado",
                                macrorreg_new))




regiao_subtema <- banco_geral %>% 
  group_by(macrorreg_new, SUBTEMA, TEMA) %>% summarise(n = n()) %>% 
  arrange(macrorreg_new, -n) %>% mutate(count = 1) %>% 
  group_by(macrorreg_new) %>% mutate(rank = cumsum(count)) %>% 
  filter(rank <= 5)


write.xlsx(regiao_subtema, "regiao subtema.xlsx")

tipo <- banco_geral %>% group_by(TEMA, TIPO) %>% 
  summarise(n = n()) %>% group_by(TEMA) %>% mutate(prop = n/sum(n),
                                                   TEMA = str_to_title(TEMA))

tipo %>% group_by(TIPO) %>% summarise(n = sum(n))



g <- ggplot(tipo, aes(x = TEMA, y = prop, fill = TIPO)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.01)), position = position_stack(vjust = .5), 
            size = 4, fontface = "bold") +
  coord_flip() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        axis.text = element_text(size = 12)) +
  scale_fill_manual(breaks = c("QUESTIONAMENTO", "PROPOSTA"),
                    values = c("PROPOSTA" = "#D8BFD8",
                               "QUESTIONAMENTO" = "darkorchid1")) +
  labs(x = "Tema", y = "Proporção de Manifestação", fill = "Tipo") 
g

ggsave("propostas e questionamentos.png")

# SUBTEMA POR LINGUAGEM

ling_subtema <- banco_geral %>% 
  group_by(LINGUAGEM, SUBTEMA) %>% summarise(n = n()) %>% 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.01)) %>% 
  arrange(LINGUAGEM, -n)


write.xlsx(ling_subtema, "subtema por linguagem.xlsx")

# MAPA----------


setwd(here("Dados"))
mapa <- read_xlsx("banco_mapa.xlsx") 

## aniversario, nascimento e data:

a <- as.Date("2024-03-20")

GASTRO <- mapa %>% mutate(NASCIMENTO = as.Date(NASCIMENTO),
                            idade = round(time_length(difftime(a, mapa$NASCIMENTO), unit = "years"),0))

## mesorregião: 
GASTRO <- GASTRO %>% mutate(MACRORREGIAO = ifelse(MACRORREGIAO == "Região Metropolitana",
                                                  "RMR", MACRORREGIAO))

## pcd (trabalhar com lógicas)
GASTRO <- GASTRO %>% mutate(PCD = ifelse(PCD == "Sim", 1, 0),
                            PCD = ifelse(is.na(PCD) == T, 0, PCD))

#### Análise Gráfica (DataViz) + Análise Exploratória ####
## produzir graficos: idade, tempo de contribuicao, linguagem, macrorregiao, dados sensiveis

## idade - GASTRO

GASTRO <- GASTRO %>% filter(idade >= 18 & idade <= 100)

theme_set(theme_bw())

g1 <- ggplot(GASTRO, aes(idade)) +
  geom_histogram(breaks = seq(from = 18, to = max(GASTRO$idade)+2, by = 2), 
                 color = "black", fill = "lightgray") + 
  scale_x_continuous(breaks = seq(from = 18, to = max(GASTRO$idade)+2, by = 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid = element_blank()) +
  labs(x = "Idade",
       y = "Qtd. Inscritos")

g1


setwd(here("Dados"))

ggsave("graf 1 idade GASTRO.png")

min(GASTRO$idade)
max(GASTRO$idade)
mean(GASTRO$idade)
sd(GASTRO$idade)
scales::percent(nrow(GASTRO %>% filter(idade >= 60))/nrow(GASTRO), accuracy = 0.01L)


#### linguagem

table(GASTRO$LINGUAGEM)

linguagens_GASTRO <- GASTRO %>% 
  group_by(LINGUAGEM) %>%
  summarise(inscricoes = n()) %>%
  mutate(proporcao = scales::percent(inscricoes/nrow(GASTRO), accuracy = 0.01L))

# Lollipop ordered chart
g3 <- ggplot(linguagens_GASTRO, aes(x= reorder(LINGUAGEM, inscricoes), y=inscricoes)) + 
  geom_point(aes(), size = 3) + 
  geom_segment(aes(x=LINGUAGEM, 
                   xend=LINGUAGEM, 
                   y=0, 
                   yend=inscricoes), linetype = 2) +
  geom_text(aes(label=paste0(inscricoes, " (", proporcao, ")"), y = inscricoes+3.5), 
            position=position_dodge(width=0.9), 
            color = "black", size = 3, hjust = .5) +
  labs(x = "",
       y = "Qtd. Inscritos",
       caption = "") + 
  theme(legend.position = "NULL",
        panel.grid = element_blank(),
        axis.text = element_text(size = 12)) + 
  coord_flip() +
  expand_limits(y = c(0, 30))


g3

ggsave("Grafico 3 Linguagem GASTRO.png")


#### macrorregiao/estadualizacao
mg5 <- GASTRO %>% group_by(MACRORREGIAO) %>% 
  summarise(inscricoes = n()) %>% 
  mutate(interior = ifelse(MACRORREGIAO == "RMR", "RMR", "Interior do Estado"))

g4 <- ggplot(mg5, aes(x= interior, y= inscricoes))+
  geom_bar(stat = "identity", position = "stack", aes(fill = MACRORREGIAO), 
           width = .7)+
  geom_text(aes(label = paste0(MACRORREGIAO, ", ", 
                               inscricoes, "\n(", 
                               scales::percent(inscricoes/sum(inscricoes), 
                                               accuracy = 0.01L),")"), 
                group = MACRORREGIAO), 
            position = position_stack(vjust = .4), size = 3.5)+
  geom_text(aes(label = paste0(interior, "=", inscricoes, " (", 
                               scales::percent(inscricoes/sum(mg5$inscricoes), 
                                               accuracy = 0.01L)," )")), 
            size = 3.5, vjust = -.5,
            data = mg5 %>% filter(MACRORREGIAO !="RMR") %>% 
              group_by(interior) %>% 
              summarise(inscricoes = sum(inscricoes)))+
  theme(legend.position = "none",
        panel.grid = element_blank())+
  scale_fill_manual(values = c("RMR" = "grey80", 
                               "Agreste" = "orchid4", 
                               "Sertão" = "tan2", 
                               "Zona da Mata" = "chartreuse4"))


g4

ggsave("Grafico Regionalizacao Design.png")

#### Dados Sensiveis 

sensiveis_ad <- GASTRO %>% gather(key = "sensivel", value = "subgrupo", GENERO:COMUNIDADE) %>% 
  group_by(sensivel, subgrupo) %>% summarise(inscricoes = n()) %>% mutate(sensivel = str_to_title(sensivel),
                                                                          subgrupo = str_to_title(subgrupo))

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

g5 <- ggplot(sensiveis_ad, aes(x= reorder_within(subgrupo, inscricoes, sensivel), y = inscricoes))+
  geom_bar(stat="identity", width=.6, aes(fill = sensivel))+
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(.~sensivel, scales = "free_y", ncol = 1)+
  theme_bw()+
  theme(panel.grid = element_blank()) +
  labs(x = "",
                  y = "Qtd. Inscritos")+
  geom_text(aes(label=paste0(inscricoes, " (", scales::percent(inscricoes/nrow(GASTRO), accuracy = 0.01L), ")")), size = 2.5, hjust = -.1) +
  expand_limits(y = c(0, 130))+theme(legend.position = "none")


g5

ggsave("Grafico Dados Sensiveis Escutas.png")

### PCD

mapa_propostas <- mapa %>% 
  gather(key = "EDITAL", value = "NOTA", FORMACAO:DIVERSIDADE) %>% 
  group_by(EDITAL) %>% summarise(NOTA = mean(NOTA)) %>% 
  arrange(-NOTA)

write.xlsx(mapa_propostas, "mapa propostas.xlsx")

###TESTE
sensiveis_ad <- GASTRO %>%
  gather(key = "sensivel", value = "subgrupo", GENERO:COMUNIDADE, RACA) %>%
  group_by(sensivel, subgrupo) %>%
  summarise(inscricoes = n()) %>%
  mutate(sensivel = str_to_title(sensivel),
         subgrupo = str_to_title(subgrupo), sensivel = ifelse(sensivel == "Raca", "Étnico-racial",
                                                              ifelse(sensivel == "Genero", "Gênero", sensivel)))

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

g5 <- ggplot(sensiveis_ad, aes(x = reorder_within(subgrupo, inscricoes, sensivel), y = inscricoes)) +
  geom_bar(stat = "identity", width = 0.6, aes(fill = sensivel)) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(. ~ sensivel, scales = "free_y", ncol = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "Qtd. Inscritos") +
  geom_text(aes(label = paste0(inscricoes, " (", scales::percent(inscricoes / nrow(GASTRO), accuracy = 0.01L), ")")), size = 2.5, hjust = -0.1) +
  expand_limits(y = c(0, 130)) +
  theme(legend.position = "none")
g5

ggsave("Grafico_Dados_Sensiveis_Escutas.png", g5)

###Temas Noronha

Noronha_temas <- CICLO_DE_ESCUTAS_PNAB_2024_consolidado %>% 
  filter(!is.na(TEMA)) %>% 
  count(TEMA)

theme_set(theme_bw())
Noronha_gtema <- ggplot(Noronha_temas, aes (reorder (TEMA, n), n))+
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#716CA7") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =-0.1) +
  expand_limits(y = c(0, 30))+
  labs(y = "Número de contribuições", x = "Tema")+
  coord_flip()

Noronha_gtema

ggsave ("Noronha_gtema.png", height = 6, width = 8)

#incluindo noronha no geral (tema)
banco_geral_N <- CICLO_DE_ESCUTAS_PNAB_2024_consolidadO_N

temas_c_noronha <- banco_geral_N %>% count(TEMA)

theme_set(theme_bw())
gtema <- ggplot(temas_c_noronha, aes (reorder (TEMA, n), n))+
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#716CA7") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =-0.1) +
  expand_limits(y = c(0, 350))+
  labs(y = "Número de contribuições", x = "Tema")+
  coord_flip()

gtema

ggsave ("gtema_c_noronha.png", height = 6, width = 8)

#incluindo noronha subtema
subtema_c_noronha <- banco_geral_N %>% count(TEMA, SUBTEMA) %>% 
  mutate(SUBTEMA= ifelse(SUBTEMA== "REMANEJAMENTO DE RECURSOS E RENDIMENTOS", "REMANEJAMENTO\nE RENDIMENTOS",
                         ifelse(SUBTEMA== "TRANSPARÊNCIA E FISCALIZAÇÃO", "TRANSPARÊNCIA\nE FISCALIZAÇÃO",
                                ifelse(SUBTEMA== "INSCRIÇÕES E IMPEDIMENTOS", "INSCRIÇÕES\nE IMPEDIMENTOS",
                                       SUBTEMA))))


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

gsubtema <- ggplot(subtema_c_noronha %>% filter(TEMA!= "EDITAIS"), aes(x= reorder_within(SUBTEMA,n,TEMA), y = n))+
  geom_bar(stat="identity", width=.6, aes(fill = TEMA))+
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(.~TEMA, scales = "free", ncol = 2)+ theme(legend.position = "none")+
  geom_text(aes(label=paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")), size = 4.5, hjust = -.1) +
  expand_limits(y = c(0, 180))+theme(legend.position = "none") +
  theme(panel.grid = element_blank(), axis.title = element_text(face = "bold"), 
        axis.text = element_text(size= 12),
        strip.text = element_text(size = 20))+
  labs(y = "Número de contribuições", x = "Subtemas")


gsubtema

ggsave ("gsubtema_c_noronha.png", height = 10, width = 12)
