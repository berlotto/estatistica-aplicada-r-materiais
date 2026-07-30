## ============================================================
## Gerador de Datasets Sintéticos — Projeto Estatística Aplicada em R
## Domínio: Varejo (fictício, sem relação com dados reais de nenhuma empresa)
##
## Objetivo: gerar dois datasets com estrutura estatística CONHECIDA e
## CONTROLADA, para servir de complemento aos exercícios com dados abertos
## reais ao longo dos módulos 1-9. Como a estrutura é desenhada por nós,
## dá para conferir se a interpretação de cada teste está correta.
##
## Reprodutibilidade: seed fixa (set.seed(2026)) — rodar este script sempre
## gera exatamente os mesmos dois arquivos CSV.
## ============================================================

set.seed(2026)

## ------------------------------------------------------------
## DATASET 1 — varejo_sintetico.csv
## Cross-sectional: uma linha por loja. Cobre módulos 1 a 7.
## ------------------------------------------------------------

n_lojas <- 1300

regiao <- factor(sample(c("Norte", "Sul", "Leste", "Oeste", "Central"),
                         n_lojas, replace = TRUE,
                         prob = c(0.18, 0.22, 0.20, 0.18, 0.22)))

tipo_loja <- factor(sample(c("Rua", "Shopping", "Quiosque"),
                            n_lojas, replace = TRUE,
                            prob = c(0.45, 0.35, 0.20)))

# porte é a variável latente que gera os 3 perfis de loja (módulo 5: cluster/PCA)
porte <- factor(sample(c("Pequena", "Media", "Grande"),
                        n_lojas, replace = TRUE,
                        prob = c(0.45, 0.35, 0.20)),
                 levels = c("Pequena", "Media", "Grande"))

# Parâmetros-base por porte (perfil latente) -------------------
base_area      <- c(Pequena = 80,   Media = 220,  Grande = 480)
sd_area        <- c(Pequena = 15,   Media = 35,   Grande = 60)
base_func      <- c(Pequena = 4,    Media = 11,   Grande = 28)
sd_func        <- c(Pequena = 1.2,  Media = 2.5,  Grande = 5)
base_fluxo     <- c(Pequena = 2200, Media = 6500, Grande = 15000)
sd_fluxo       <- c(Pequena = 400,  Media = 900,  Grande = 2200)
base_ticket    <- c(Pequena = 45,   Media = 65,   Grande = 95)
sd_ticket      <- c(Pequena = 8,    Media = 10,   Grande = 15)
base_marketing <- c(Pequena = 1500, Media = 4500, Grande = 12000)

area_m2 <- pmax(20, rnorm(n_lojas, base_area[porte], sd_area[porte]))
num_funcionarios <- pmax(1, round(rnorm(n_lojas, base_func[porte], sd_func[porte])))

# tipo_loja modula levemente o fluxo (shopping atrai mais fluxo por m2)
mult_fluxo_tipo <- c(Rua = 1.0, Shopping = 1.15, Quiosque = 0.75)[as.character(tipo_loja)]
fluxo_clientes_mensal <- pmax(200, rnorm(n_lojas, base_fluxo[porte] * mult_fluxo_tipo, sd_fluxo[porte]))

ticket_medio <- pmax(10, rnorm(n_lojas, base_ticket[porte], sd_ticket[porte]))

# gasto_marketing: log-normal (assimétrica à direita "de fábrica") + correlacionada
# com fluxo_clientes_mensal por construção (mesmo porte impulsiona os dois) —
# isso é a multicolinearidade que o módulo 3 precisa para a regressão múltipla.
gasto_marketing <- rlnorm(n_lojas,
                           meanlog = log(base_marketing[porte]) - 0.15,
                           sdlog = 0.35)
# outliers propositais: ~1% das lojas rodou campanha atípica de alto investimento
idx_outlier_mkt <- sample(seq_len(n_lojas), size = round(0.01 * n_lojas))
gasto_marketing[idx_outlier_mkt] <- gasto_marketing[idx_outlier_mkt] * runif(length(idx_outlier_mkt), 4, 7)

# receita_mensal: variável-alvo para regressão múltipla.
# Depende de fluxo, ticket, marketing (colinear com fluxo) e área.
receita_mensal <- 500 +
  0.9   * fluxo_clientes_mensal * (ticket_medio / 60) +
  0.35  * gasto_marketing +
  1.2   * area_m2 +
  rnorm(n_lojas, 0, 1800)
receita_mensal <- pmax(500, receita_mensal)

# taxa_conversao: relação NÃO-LINEAR e saturante com fluxo_clientes_mensal
# (regressão polinomial — e risco de extrapolação além do range observado)
taxa_conversao <- pmin(0.42,
  0.05 + 0.30 * (1 - exp(-fluxo_clientes_mensal / 5000)) + rnorm(n_lojas, 0, 0.02))
taxa_conversao <- pmax(0.01, taxa_conversao)

# proporção conhecida (~7%) — IC para proporção, distribuição binomial
teve_reclamacao_grave <- rbinom(n_lojas, 1, 0.07)

# contagem tipo Poisson — distribuições discretas (módulo 6)
num_reclamacoes_mes <- rpois(n_lojas, lambda = 1.3)

# --- Subconjunto pareado: campanha de marketing (módulo 4 / teste t pareado) ---
participou_campanha <- rbinom(n_lojas, 1, 0.19)  # ~250 lojas
efeito_campanha <- rnorm(n_lojas, mean = 0.06, sd = 0.05)  # maioria melhora, algumas pioram
receita_pre_campanha  <- ifelse(participou_campanha == 1, receita_mensal, NA)
receita_pos_campanha  <- ifelse(participou_campanha == 1,
                                 pmax(500, receita_mensal * (1 + efeito_campanha) + rnorm(n_lojas, 0, 400)),
                                 NA)

# --- Subconjunto teste A/B: programa de fidelidade (módulo 4 / Mann-Whitney) ---
participou_teste_fidelidade <- rbinom(n_lojas, 1, 0.23)  # ~300 lojas
grupo_teste_fidelidade <- rep(NA_character_, n_lojas)
idx_teste <- which(participou_teste_fidelidade == 1)
grupo_teste_fidelidade[idx_teste] <- sample(c("Controle", "Tratamento"),
                                             length(idx_teste), replace = TRUE)

vendas_incrementais_fidelidade <- rep(NA_real_, n_lojas)
idx_controle <- which(grupo_teste_fidelidade == "Controle")
idx_tratamento <- which(grupo_teste_fidelidade == "Tratamento")
# Controle: aproximadamente normal, efeito nulo
vendas_incrementais_fidelidade[idx_controle] <- rnorm(length(idx_controle), mean = 0, sd = 300)
# Tratamento: DELIBERADAMENTE não-normal — maioria com ganho moderado,
# poucas lojas "estrela" com ganho muito alto (cauda longa) -> força via Mann-Whitney
vendas_incrementais_fidelidade[idx_tratamento] <- rgamma(length(idx_tratamento), shape = 1.6, scale = 350) - 100

varejo <- data.frame(
  loja_id = sprintf("LJ%04d", seq_len(n_lojas)),
  regiao, tipo_loja, porte,
  area_m2 = round(area_m2, 1),
  num_funcionarios,
  fluxo_clientes_mensal = round(fluxo_clientes_mensal, 0),
  ticket_medio = round(ticket_medio, 2),
  gasto_marketing = round(gasto_marketing, 2),
  receita_mensal = round(receita_mensal, 2),
  taxa_conversao = round(taxa_conversao, 4),
  teve_reclamacao_grave,
  num_reclamacoes_mes,
  participou_campanha,
  receita_pre_campanha = round(receita_pre_campanha, 2),
  receita_pos_campanha = round(receita_pos_campanha, 2),
  participou_teste_fidelidade,
  grupo_teste_fidelidade,
  vendas_incrementais_fidelidade = round(vendas_incrementais_fidelidade, 2)
)

write.csv(varejo, "/mnt/user-data/outputs/varejo_sintetico.csv", row.names = FALSE)

## ------------------------------------------------------------
## DATASET 2 — painel_regional_varejo.csv
## Painel cidade x mês + snapshot espacial. Cobre módulos 8 e 9.
## ------------------------------------------------------------

n_cidades <- 50
n_meses <- 84  # 7 anos

# 5 polos regionais (macro-regiões) com centro espacial próprio.
# Coordenadas em km, sobre uma grade sintética de 100x100 — NÃO correspondem
# a locais reais, é só uma grade fictícia com estrutura espacial embutida.
polos <- data.frame(
  polo = paste0("Polo_", 1:5),
  centro_x = c(15, 40, 65, 85, 30),
  centro_y = c(20, 70, 35, 80, 50)
)

cidade_polo <- sample(polos$polo, n_cidades, replace = TRUE)
coord_x <- polos$centro_x[match(cidade_polo, polos$polo)] + rnorm(n_cidades, 0, 6)
coord_y <- polos$centro_y[match(cidade_polo, polos$polo)] + rnorm(n_cidades, 0, 6)
coord_x <- pmin(100, pmax(0, coord_x))
coord_y <- pmin(100, pmax(0, coord_y))

nomes_cidades <- c(
  "Porto Verde","Vale do Sol","Serra Alta","Campo Novo","Rio Claro",
  "Monte Bello","Boa Esperanca","Sao Gabriel","Agua Fria","Colina Azul",
  "Vista Alegre","Barra Longa","Pinhal Grande","Estrela do Norte","Lagoa Bonita",
  "Cerro Verde","Vale Feliz","Porto Seguro Fic","Campo Alegre","Serra Bonita",
  "Riacho Doce","Bela Vista Fic","Morro Azul","Fonte Nova","Planalto Verde",
  "Vale Dourado","Boa Vista Fic","Sao Martinho","Terra Nova Fic","Alto Bonito",
  "Rio das Pedras","Campina Grande Fic","Vale Encantado","Serra Doce","Porto Alto",
  "Monte Claro","Praia Verde","Campo Dourado","Rio Bonito","Vale das Flores",
  "Cachoeira Alta","Colina Verde","Estancia Nova","Lago Azul","Serra Negra Fic",
  "Vista Bonita","Barra Nova","Pedra Branca","Fazenda Velha","Vale Real"
)

polo_efeito_renda <- setNames(rnorm(5, 0, 1), polos$polo)

indice_renda_media <- 3.0 +
  1.1 * polo_efeito_renda[cidade_polo] +
  rnorm(n_cidades, 0, 0.35)

# ticket_medio_regional compartilha o MESMO fator latente regional (0.7 do peso)
# + ruído independente -> correlação bruta com renda é PARCIALMENTE espúria
# (efeito regional compartilhado), exatamente o ponto do módulo 9.
ticket_medio_regional <- 55 +
  9 * (0.7 * polo_efeito_renda[cidade_polo] + 0.3 * rnorm(n_cidades, 0, 1)) +
  rnorm(n_cidades, 0, 3)

# placebo espacial: ruído puro, sem estrutura de polo -> Moran's I ~ 0 esperado
var_placebo_espacial <- rnorm(n_cidades, 100, 15)

populacao_mil <- pmax(8, round(rlnorm(n_cidades, meanlog = log(45), sdlog = 0.6)))

cidades <- data.frame(
  cidade_id = sprintf("CID%02d", seq_len(n_cidades)),
  cidade_nome = nomes_cidades[seq_len(n_cidades)],
  polo = cidade_polo,
  coord_x_km = round(coord_x, 2),
  coord_y_km = round(coord_y, 2),
  populacao_mil,
  indice_renda_media = round(indice_renda_media, 3),
  ticket_medio_regional = round(ticket_medio_regional, 2),
  var_placebo_espacial = round(var_placebo_espacial, 2)
)

# --- Painel mensal (módulo 8: séries temporais) ---
datas <- seq(as.Date("2019-01-01"), by = "month", length.out = n_meses)
t_idx <- seq_len(n_meses)

# série nacional de confiança do consumidor (mesma base para todas as cidades,
# com pequeno ruído local) — sazonalidade + leve tendência
confianca_nacional <- 100 + 0.08 * t_idx +
  6 * sin(2 * pi * t_idx / 12 + 0.4) +
  rnorm(n_meses, 0, 2.5)

nivel_base_cidade <- setNames(
  200 * (populacao_mil / mean(populacao_mil))^0.6 * exp(rnorm(n_cidades, 0, 0.15)),
  cidades$cidade_id
)

painel_list <- vector("list", n_cidades)
for (i in seq_len(n_cidades)) {
  cid <- cidades$cidade_id[i]
  base <- nivel_base_cidade[[cid]]
  fase_local <- rnorm(1, 0, 0.3)
  tendencia_local <- rnorm(1, 0.0015, 0.0006)  # crescimento mensal ~0.15%
  # sazonalidade anual (pico nov/dez, característico de varejo) + tendência + ruído
  indice_vendas <- base * (1 + tendencia_local * t_idx) *
    (1 + 0.18 * sin(2 * pi * t_idx / 12 - 1.2 + fase_local)) +
    rnorm(n_meses, 0, base * 0.04)
  indice_vendas <- pmax(base * 0.3, indice_vendas)

  painel_list[[i]] <- data.frame(
    cidade_id = cid,
    mes_ano = datas,
    indice_vendas = round(indice_vendas, 1),
    indice_confianca_consumidor = round(confianca_nacional + rnorm(n_meses, 0, 1.5), 1)
  )
}
painel_mensal <- do.call(rbind, painel_list)

painel_regional <- merge(painel_mensal, cidades, by = "cidade_id")
painel_regional <- painel_regional[order(painel_regional$cidade_id, painel_regional$mes_ano), ]

write.csv(painel_regional, "/mnt/user-data/outputs/painel_regional_varejo.csv", row.names = FALSE)

## ------------------------------------------------------------
## Conferência rápida (sai no console, não vai pro CSV)
## ------------------------------------------------------------
cat("=== varejo_sintetico.csv ===\n")
cat("Linhas:", nrow(varejo), " Colunas:", ncol(varejo), "\n")
cat("Lojas na campanha:", sum(varejo$participou_campanha), "\n")
cat("Lojas no teste de fidelidade:", sum(varejo$participou_teste_fidelidade), "\n\n")

cat("=== painel_regional_varejo.csv ===\n")
cat("Linhas:", nrow(painel_regional), " Colunas:", ncol(painel_regional), "\n")
cat("Cidades:", n_cidades, " Meses por cidade:", n_meses, "\n")
cat("Correlação bruta renda x ticket (nível cidade):",
    round(cor(cidades$indice_renda_media, cidades$ticket_medio_regional), 3), "\n")
