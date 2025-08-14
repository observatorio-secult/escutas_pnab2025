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


