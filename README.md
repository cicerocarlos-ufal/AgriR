# AgriR <img src="https://img.shields.io/badge/R-%3E%3D%204.0-blue" alt="R >= 4.0"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/cicerocarlos-ufal/AgriR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cicerocarlos-ufal/AgriR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**AgriR** reúne ferramentas para o ensino e a análise de experimentos
agrícolas: sorteio e croqui de delineamentos, análise de variância,
testes de comparação múltipla, regressão polinomial, estatística descritiva
e simulações didáticas — também disponíveis em um aplicativo interativo
(Shiny).

Desenvolvido na Universidade Federal de Alagoas (UFAL), Campus Arapiraca.

## Instalação

```r
# install.packages("remotes")
remotes::install_github("cicerocarlos-ufal/AgriR")
```

## O que o pacote faz

| Tema | Funções |
|---|---|
| Delineamentos (sorteio, croqui e caderno de campo) | `des.crd()`, `des.rbd()`, `des.fat2.crd()`, `des.fat2.rbd()`, `des.sp.crd()`, `des.sp.rbd()`, `des.fat3.crd()`, `des.fat3.rbd()`, `des.ssp.crd()`, `des.ssp.rbd()` |
| ANOVA de um fator | `anova_crd()` (DIC), `anova_rbd()` (DBC), `latsd()` (quadrado latino) |
| Fatorial duplo | `anova_fat2_crd()`, `anova_fat2_rbd()` — com desdobramento da interação |
| Parcelas subdivididas | `anova_sp2_crd()`, `anova_sp2_rbd()` — erro (a), erro (b) e erro combinado (Satterthwaite) |
| Fatorial triplo | `anova_fat3_crd()`, `anova_fat3_rbd()` — desdobramento das interações duplas e tripla |
| Parcelas subsubdivididas | `anova_ssp_crd()`, `anova_ssp_rbd()` — erros (a), (b) e (c) e erros combinados |
| Comparações múltiplas | `tukey()`, `duncan()`, `lsd()`, `snk()`, `scottknott()`, `scheffe()` |
| Regressão (fatores quantitativos) | `reg.poly()` — linear, quadrática e cúbica, com escolha do melhor modelo |
| Gráficos | `barplotb()`, `boxplotb()` (base), `barplotg()`, `pointplotg()` (ggplot2) |
| Descritiva | `desc()`, `freqtable()`, `histg()`, `ari_mean()`, `geo_mean()`, `har_mean()`, `pon_mean()` |
| Simulações didáticas | `aproximate()`, `int.confidance()`, `means.test()`, `par.test()` |
| Aplicativo | `run_app()` |

## Exemplos

```r
library(AgriR)

# Sorteio de um DBC com 5 tratamentos e 4 blocos
set.seed(123)
d <- des.rbd(trt = 5, rep = 4)
d$Book

# ANOVA em DIC com teste de Tukey
crd <- read.csv(system.file("extdata", "crd.csv", package = "AgriR"))
res <- anova_crd(crd$trat, crd$resp, mcomp = "tukey",
                 xlab = "Tratamentos", ylab = "Produtividade")
res$Anova
res$Mcomp

# Doses (fator quantitativo): regressão polinomial
dose <- read.csv(system.file("extdata", "dose.csv", package = "AgriR"))
anova_crd(dose$dose, dose$resp, quali = FALSE, xlab = "Dose (kg/ha)")

# Fatorial em DBC, desdobrando o fator 2 dentro do fator 1, Scott-Knott
fat <- read.csv(system.file("extdata", "fat2.csv", package = "AgriR"))
anova_fat2_rbd(fat$cultivar, fat$adubo, fat$bloco, fat$resp,
               fac.names = c("Cultivar", "Adubo"), fac.fix = "fac.B",
               mcomp = "sk", graph.type = "barplotg")

# Parcelas subdivididas em DBC
sp <- read.csv(system.file("extdata", "split.csv", package = "AgriR"))
anova_sp2_rbd(sp$irrigacao, sp$cultivar, sp$bloco, sp$resp,
              fac.names = c("Irrigação", "Cultivar"))

# Fatorial triplo em DBC
f3 <- read.csv(system.file("extdata", "fat3.csv", package = "AgriR"))
anova_fat3_rbd(f3$cultivar, f3$adubo, f3$espac, f3$bloco, f3$resp,
               fac.names = c("Cultivar", "Adubo", "Espaçamento"))

# Parcelas subsubdivididas em DBC
ssp <- read.csv(system.file("extdata", "ssp.csv", package = "AgriR"))
anova_ssp_rbd(ssp$irrigacao, ssp$cultivar, ssp$epoca, ssp$bloco, ssp$resp,
              fac.names = c("Irrigação", "Cultivar", "Época"))

# Aplicativo interativo
run_app()
```

Os arquivos em `inst/extdata` (`crd.csv`, `rbd.csv`, `dose.csv`, `fat2.csv`,
`split.csv`, `latsq.csv`, `fat3.csv`, `ssp.csv`) são **dados simulados**, criados apenas para os
exemplos.

## Arquivos CSV no aplicativo

O aplicativo lê arquivos CSV com cabeçalho. O padrão é separador de colunas
`;` e decimal `,` (formato usado pelo Excel em português); ambos podem ser
alterados na tela.

## Créditos

Parte do código foi adaptada, sob a licença GPL, dos pacotes
[ExpDes](https://CRAN.R-project.org/package=ExpDes) (E. B. Ferreira,
D. A. Nogueira, P. P. Cavalcanti) e
[agricolae](https://CRAN.R-project.org/package=agricolae)
(F. de Mendiburu), e da implementação do teste de Scott-Knott de
L. J. da Silva.

## Licença

GPL (>= 2).
