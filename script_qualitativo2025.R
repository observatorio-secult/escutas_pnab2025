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
setwd(here("dados"))

escutas <- read_xlsx("escuta_pnab2025.xlsx", sheet = "Qualitativo")
mapa <- read_xlsx("escuta_pnab2025.xlsx", sheet = "Mapa")


a <- escutas %>% group_by(tema, subtema) %>% summarise(n = n()) %>% 
  arrange(tema, -n)

setwd(here())

write.xlsx(a, "quantidade de proposicoes por tema e subtema.xlsx")


mapa %>% group_by(pnab_1) %>% summarise(n = n())


write.xlsx(mapa %>% filter(pnab_1 == "Não") %>% select(id, pnab_1, pnab_1_n),
           "razões não participação pnab1.xlsx")


# GERAL---------

## Temas

temas<- escutas %>% count(tema)


gtema <- ggplot(temas, aes (reorder (tema, n), n))+
  geom_bar(stat = "identity", width = .6, color = "black", fill = "#01cf01") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust =-0.1) +
  expand_limits(y = c(0, 350))+
  labs(y = "Número de contribuições", x = "Tema")+
  coord_flip() +
  expand_limits(y = c(0, 450))

gtema

ggsave ("gtema.png", height = 6, width = 8)

## Notas dos editais

names(mapa)

opiniao <- mapa %>% select(id, op_formacao:op_diversidade) %>% 
  gather(key = "edital", value = "nota", op_formacao:op_diversidade) %>% 
  mutate(nota = as.numeric(gsub("\\..*", "", nota)))

opiniao %>% group_by(edital) %>% summarise(nota = mean(nota)) %>% arrange(-nota)


## Subtema

subtema <- escutas %>% count(tema, subtema) %>% 
  mutate(subtema = case_when(subtema == "Transparência e Fiscalização" ~ "Transparência e\nFiscalização",
                             subtema == "Inscrições e Impedimentos" ~ "Inscrições e\nImpedimentos",
                             subtema == "Remanejamento de Recursos e Rendimentos" ~ "Remanejamento de\nRecursos",
                             tema == "Inválida" ~ "Inválida",
                             TRUE ~ subtema)) %>% filter(tema != "Inválida")


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

gsubtema <- ggplot(subtema %>% filter(tema != "Editais"), aes(x= reorder_within(subtema,n,tema), y = n))+
  geom_bar(stat="identity", width=.6, aes(fill = tema))+
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(.~tema, scales = "free_y", ncol = 2)+ theme(legend.position = "none")+
  geom_text(aes(label=paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")), 
            size = 4, hjust = -.1, data = subtema %>% filter(tema != "Editais") %>% filter(tema != "Inválida")) +
  geom_text(aes(label=paste0(n, " (", scales::percent(n/sum(n), accuracy = 0.01), ")")), 
            size = 4, hjust = 1.1, data = subtema %>% filter(tema != "Editais") %>% filter(tema == "Inválida")) +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank(), 
        axis.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", size = 11))+
  expand_limits(y = c(0, 180)) +
  labs(y = "Número de contribuições", x = "Subtemas")


gsubtema

ggsave ("gsubtema.png")

# Editais

gsubtema <- ggplot(subtema %>% filter(tema == "Editais"), 
                   aes(x= reorder(subtema,n), y = n)) +
  geom_segment(aes(xend=subtema, yend = 0), color="grey80", lty="dashed")+
  geom_point(size=2,color="#183fff") +
  coord_flip()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(n, " (",
                               scales::percent(n/sum(n)), ")")),
            hjust = - 0.1, size=3) +
  expand_limits(y = c(0, 200)) +
  labs(y = "Número de Contribuições", x = "Subtemas")

gsubtema

ggsave ("editais.png")

# CONTRIBUICOES POR LINGUAGENS

linguagem <- escutas %>% left_join(mapa %>% select(id, linguagem)) %>% 
  count(linguagem)


# SUBTEMAS POR REGIAO

setwd(here("Dados"))
RD_Municipio <- read_xlsx("RD_Municipio.xlsx") %>% mutate(municipio = ifelse(municipio == "Lagoa do Itaenga", "Lagoa de Itaenga", municipio))
pernambuco <- st_read("PE_Municipios_2022.shp")


regiao <- mapa %>% 
  mutate(municipio = ifelse(municipio == "São Caetano", "São Caitano", municipio)) %>% 
  left_join(RD_Municipio) %>% 
  group_by(municipio) %>% summarise(n = n())

pernambuco <- pernambuco %>% mutate(municipio = toupper(NM_MUN)) %>% 
  left_join(regiao %>% mutate(municipio = toupper(municipio))) %>% 
  left_join(RD_Municipio %>% 
              mutate(municipio = toupper(municipio)))

pernambuco <- pernambuco %>% filter(CD_MUN != 2605459)


macrorregiao <- pernambuco %>% ungroup() %>% filter(is.na(n) == F) %>% 
  group_by(macrorreg_new) %>% 
  summarise(y = sum(n)) %>% as.data.frame() %>% select(macrorreg_new, y)

pernambuco <- pernambuco %>% left_join(macrorregiao)

g5 <- ggplot() +
  geom_sf(data = pernambuco, aes(fill = n), color = "black") +
  labs(title = "",
       fill = "Inscrições") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9))
plot(g5)


ggsave("mapa.png")

mapa <- mapa %>% mutate(municipio = ifelse(municipio == "São Caetano", "São Caitano", municipio)) %>% 
  left_join(RD_Municipio)

mapa %>% count(municipio) %>% arrange(-n) %>% head(10)

# banco_geral <- banco_geral %>% 
#   left_join(RD_Municipio %>% 
#               mutate(municipio = str_to_upper(municipio)), by = c("MUNICIPIO" = "municipio"))
# 
# municipios <- banco_geral %>% 
#   group_by(MUNICIPIO) %>% summarise(n = n()) %>% 
#   mutate(prop = scales::percent(n/sum(n), accuracy = 0.01))
# 
# write.xlsx(municipios, "municipio data.xlsx")
# 
# macrorregiao <- banco_geral %>% 
#   group_by(macrorreg_new) %>% summarise(n = n()) %>% 
#   mutate(prop = scales::percent(n/sum(n), accuracy = 0.01),
#          macrorreg_new = ifelse(is.na(macrorreg_new) == T, "Não Informado",
#                                 macrorreg_new))
# 
# 
# 
# 
# regiao_subtema <- banco_geral %>% 
#   group_by(macrorreg_new, SUBTEMA, TEMA) %>% summarise(n = n()) %>% 
#   arrange(macrorreg_new, -n) %>% mutate(count = 1) %>% 
#   group_by(macrorreg_new) %>% mutate(rank = cumsum(count)) %>% 
#   filter(rank <= 5)
# 
# 
# write.xlsx(regiao_subtema, "regiao subtema.xlsx")
# 
# tipo <- banco_geral %>% group_by(TEMA, TIPO) %>% 
#   summarise(n = n()) %>% group_by(TEMA) %>% mutate(prop = n/sum(n),
#                                                    TEMA = str_to_title(TEMA))
# 
# tipo %>% group_by(TIPO) %>% summarise(n = sum(n))
# 
# 
# 
# g <- ggplot(tipo, aes(x = TEMA, y = prop, fill = TIPO)) +
#   geom_bar(stat = "identity", position = "stack", color = "black") +
#   geom_text(aes(label = scales::percent(prop, accuracy = 0.01)), position = position_stack(vjust = .5), 
#             size = 4, fontface = "bold") +
#   coord_flip() +
#   theme(panel.grid = element_blank(),
#         legend.position = "top",
#         axis.text = element_text(size = 12)) +
#   scale_fill_manual(breaks = c("QUESTIONAMENTO", "PROPOSTA"),
#                     values = c("PROPOSTA" = "#D8BFD8",
#                                "QUESTIONAMENTO" = "darkorchid1")) +
#   labs(x = "Tema", y = "Proporção de Manifestação", fill = "Tipo") 
# g
# 
# ggsave("propostas e questionamentos.png")
# 
# # SUBTEMA POR LINGUAGEM
# 
# ling_subtema <- banco_geral %>% 
#   group_by(LINGUAGEM, SUBTEMA) %>% summarise(n = n()) %>% 
#   mutate(prop = scales::percent(n/sum(n), accuracy = 0.01)) %>% 
#   arrange(LINGUAGEM, -n)
# 
# 
# write.xlsx(ling_subtema, "subtema por linguagem.xlsx")
# 
# 
