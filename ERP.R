# Setup -------------------------------------------------------------------
rm(list = ls())
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Data load ---------------------------------------------------------------

ERP_TESTE_HP_BBG <- read_excel("P:/Teo/ERP_TESTE_HP_BBG.xlsx",
                               range = cell_limits(ul= c(6, 1), lr = c(NA, NA)), 
                               na = c("#N/A N/A"))



Dicionario_1 <- read_excel("./Dicionario.xlsx", 
                           range = cell_limits(ul = c(1, 1), lr = c(NA, 2)),
                           na = c("#N/A N/A", "#N/A Invalid Security"))

Dicionario_2 <- read_excel("./Dicionario.xlsx", 
                           range = cell_limits(ul = c(1, 8), lr = c(NA, 9)))


tbl_dic <- left_join(Dicionario_1, Dicionario_2, by = c("Cod" = "Cod")) %>% 
  mutate(Ticker = paste(Ticker,  "BS Equity"))

rm(list = c("Dicionario_1", "Dicionario_2"))

# Data manipulation -------------------------------------------------------

# Ajusta a coluna Data
ERP_TESTE_HP_BBG$Date <- as.Date(ERP_TESTE_HP_BBG$Date)

#  Transforma no formato longo
tbl <- tidyr::pivot_longer(data = ERP_TESTE_HP_BBG, cols = -Date)

# Inicializa as colunas
tbl$Ticker <- as.character(NA)
tbl$Field <- as.character(NA)

# Popula as colunas
tbl[ , c("Ticker", "Field")] <- stringr::str_split(string = tbl$name, pattern = "\\|", simplify = TRUE)

# Apaga a coluna
tbl[ , "name"] <- NULL

# Transforma em dados wide.
tbl_wide <- tidyr::pivot_wider(data = tbl,
                               id_cols = c("Date", "Ticker"),
                               names_from = "Field", 
                               values_from = "value")


# Calcula as ROE, Payout, g e k
tbl_wide <- tbl_wide %>% 
  mutate(ROE = TRAIL_12M_EPS/BOOK_VAL_PER_SH,
         Payout = DVD_SH_12M/TRAIL_12M_EPS,
         g = ROE *(1-Payout), 
         DIV_1 = DVD_SH_12M * (1+g),
         k = (DIV_1/PX_LAST) + g)

# Juncao com os dados de setor das empresas
tbl_wide <- left_join(tbl_wide, tbl_dic, by = c("Ticker"))


# Exclui a????es dos setores "Financas e Seguros", "Fundos"
# tbl_wide <- tbl_wide %>% filter(!(Cod %in% c(4010, 4020, 4030)))

# 1. S??o exclu??das as a????es que n??o apresentaram cota????o de fechamento no m??s.
tbl_wide <- tbl_wide %>% filter(!(is.na(PX_LAST)))

# 2. S??o exclu??das as a????es de empresas para as quais houve preju??zo no
# per??odo m??vel de 12 meses (ou seja, lucro l??quido por a????o ?? negativo),
# bem como aquelas para as quais o valor patrimonial da a????o ?? negativo.
tbl_wide <- tbl_wide %>% filter(TRAIL_12M_EPS >= 0)

# 3. S??o exclu??das as a????es para as quais o payout calculado ?? superior a 100%.
tbl_wide <- tbl_wide %>% filter(Payout <= 1)

# 4. S??o exclu??das as a????es para as quais DPA ou LPA ou VPA n??o foram informados
tbl_wide <- tbl_wide %>% filter(!(is.na(TRAIL_12M_EPS) | is.na(BOOK_VAL_PER_SH) | is.na(DVD_SH_12M)))

# 5. S??o exclu??das as a????es em que o VPA = 0
tbl_wide <- tbl_wide %>% filter(BOOK_VAL_PER_SH !=0)


T10_Bond <- read_excel("./T10_Bond.xlsx", 
                       range = cell_limits(ul=c(7, 1), lr = c(NA, 3)),
                       col_names = c("Date", "T10", "CDI"),
                       # col_types = c("date", "numeric", "numeric")
)

T10_Bond$Date <- as.Date(T10_Bond$Date)


tbl_wide %>%
  group_by(Date) %>% 
  summarise(K = mean(k, trim = 0.1), Eg = mean(g, trim = 0.1)) %>% 
  left_join(T10_Bond, by = c("Date" = "Date")) %>% 
  mutate(ERP = K-CDI/100) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = ERP)) + 
  geom_line(aes(x = Date, y = CDI/100), colour = "blue") + 
  geom_line(aes(x = Date, y = ERP - Eg), colour = "red") + 
  # geom_vline(xintercept = seq( from = as.Date("2010-01-01"), to = as.Date("2023-01-01"), by = "quarter"), colour = "red", linetype = "dashed") + 
  labs()


tbl_wide %>% 
  filter(!(Cod %in% c(5020,
    # 3020, 
    6010,
    3010, 
    # 2520,
    4510,
    2010, 2530,3030,2030,3520,
    2550))) %>% 
  mutate(fCod = factor(x = Cod, levels = Cod, labels = Desc)) %>% 
  # pull(ddd)
  group_by(Date, fCod) %>% 
  summarise(K = mean(k, trim = 0.1),
            Eg = mean(g, trim = 0.1),
            DIV_Yield = mean(DIV_1/PX_LAST, trim = 0.1)) %>% 
  ggplot() +
  # geom_line(aes(x = Date, y = K, colour = Desc)) + 
  geom_line(aes(x = Date, y = K,
                colour = fCod)) + 
  # geom_text(aes(x = as.Date("2023-01-26"), y = DIV_Yield, label = fCod)) + 
  labs()

tbl_wide %>% 
  filter(Ticker %in% c("BBDC4 BS Equity",
                       "BBAS3 BS Equity",
                       "VALE3 BS Equity",
                       # "BRAP4 BS Equity",
                       # "SUZB3 BS Equity",
                       "TAEE11 BS Equity",
                       "ENBR3 BS Equity",
                       "CPLE6 BS Equity",
                       "CMIG4 BS Equity"
                       )) %>% 
  mutate(fCod = factor(x = Ticker)) %>% 
  # pull(ddd)
  group_by(Date, fCod) %>% 
  summarise(K = mean(k, trim = 0.1),
            Eg = mean(g, trim = 0.1),
            DIV_Yield = mean(DIV_1/PX_LAST, trim = 0.1)) %>% 
  ggplot() +
  # geom_line(aes(x = Date, y = K, colour = Desc)) + 
  # geom_line(aes(x = Date, y = K,
  #               colour = fCod)) + 
  geom_line(aes(x = Date,
                # y = K,
                y = DIV_Yield,
                colour = fCod)) + 
  # geom_text(aes(x = as.Date("2023-01-26"), y = DIV_Yield, label = fCod)) + 
  facet_wrap(~fCod) +
  labs(title = "Dividend Yeild", y = "DIV_1/P0", X = NULL) + 
  theme(legend.position = "none")


