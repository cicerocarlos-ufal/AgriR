# AgriR 1.0.0

## Novidades: experimentos com três fatores
* `des.fat3.crd()`, `des.fat3.rbd()`: sorteio, croqui e caderno de campo do
  fatorial triplo em DIC e DBC.
* `des.ssp.crd()`, `des.ssp.rbd()`: sorteio, croqui e caderno de campo de
  parcelas subsubdivididas em DIC e DBC.
* `anova_fat3_crd()`, `anova_fat3_rbd()`: ANOVA do fatorial triplo, com
  desdobramento da interação tripla (cada fator dentro das combinações dos
  outros dois) ou das interações duplas significativas (nos dois sentidos) e
  efeitos principais dos fatores sem interação significativa.
* `anova_ssp_crd()`, `anova_ssp_rbd()`: ANOVA de parcelas subsubdivididas
  com erros (a), (b) e (c), três CVs e desdobramentos com o erro adequado a
  cada comparação (erros combinados com graus de liberdade de
  Satterthwaite).
* Aplicativo: opção "Three" nas abas de delineamento e ANOVA.
* Dados de exemplo simulados `fat3.csv` e `ssp.csv`.

Primeira versão organizada como pacote R (DESCRIPTION, NAMESPACE,
documentação em `man/`, testes e integração contínua).

## Correções de bugs

### Testes de comparação múltipla
* As letras eram associadas aos tratamentos por ordem alfabética
  (`order(rownames())`), enquanto as médias seguiam a ordem dos níveis do
  fator. Com 10 ou mais tratamentos numéricos ("1", "10", "2", ...) ou com
  níveis em ordem definida pelo usuário, **as letras ficavam trocadas**. Agora
  as letras são associadas pelo nome do tratamento.
* `scottknott()`: gravava e apagava um arquivo `skresult` no diretório de
  trabalho do usuário; continuava subdividindo grupos mesmo quando a divisão
  anterior não era significativa; usava `rep[1]` (supunha delineamento
  balanceado). Reescrito sem arquivos temporários, seguindo Scott & Knott
  (1974): a subdivisão só continua dentro de grupos cuja divisão foi
  significativa.
* `scheffe()`: recebia o **quadrado médio** do erro, mas as ANOVAs fatoriais e
  em parcelas subdivididas passavam a **soma de quadrados**; o resultado ficava
  errado. Agora recebe `SSerror`, como os demais testes.
* `tukey()`, `duncan()`, `lsd()`, `snk()`: as médias e desvios do gráfico
  incluíam `NA`.
* Valores padrão recursivos (`xlab = xlab`) causavam erro quando o argumento
  não era informado.

### ANOVA
* `anova_fat2_crd()`/`anova_fat2_rbd()`, desdobramento `fac.fix = "fac.B"`:
  Scott-Knott e SNK usavam o fator e os níveis errados; Scheffé usava `==`
  (comparação) no lugar de `=` (o resultado era descartado).
* `anova_fat2_rbd()`: objeto `cont` inexistente (erro com Scheffé) e
  `tab[[1]][[1]][5, 1]` no SNK.
* `anova_sp2_rbd()`: no desdobramento usava a linha de **blocos** como erro (a)
  e a linha da **interação** como erro (b), invalidando os testes F, o erro
  combinado e os graus de liberdade de Satterthwaite; na regressão sem
  interação usava blocos como erro (a).
* `anova_sp2_crd()`/`anova_sp2_rbd()`: `sigT` inexistente (erro) e argumentos
  gráficos ausentes no desdobramento `fac.B`; resultados do SNK e da
  regressão não eram guardados; Scheffé ausente no desdobramento.
* `latsd()`: chamava testes inexistentes (`lsdb`, `ccboot`, `ccF`) e chamava
  os testes sem os argumentos gráficos obrigatórios (erro).
* A linha "Total" das tabelas mostrava a soma dos valores de F e de p.
* Teste de Shapiro-Wilk protegido contra erros (resíduos constantes, n < 3).
* `mcomp` inválido (por exemplo `"skottknott"`, usado no app) fazia a função
  falhar com "object 'test' not found"; agora há mensagem clara e os nomes
  `"sk"`/`"skottknott"` são aceitos como sinônimos de `"scottknott"`.

### Regressão polinomial (`reg.poly()`)
* Com `graph = FALSE` dava erro (`object 'xm1' not found`) — isso quebrava a
  aba "ANOVA Table" do app para fatores quantitativos.
* O modelo quadrático só era ajustado com 4 ou mais níveis e o cúbico com 5
  ou mais (havia código morto para os casos de 3 e 4 níveis). Agora:
  quadrático com >= 3 níveis e cúbico com >= 4.
* Os níveis vinham dos nomes de linha (texto); agora são convertidos para
  número, com mensagem clara se não forem numéricos (fator ou texto).

### Delineamentos (`des.*`)
* `des.fat2.crd()`, `des.sp.crd()`, `des.sp.rbd()`: o "Book" (caderno de
  campo) combinava níveis dos fatores de forma errada (por exemplo, 3 x 2
  gerava combinações repetidas e outras ausentes).
* `des.sp.crd()`/`des.sp.rbd()`: rótulos e cores eram extraídos por posição
  fixa de caracteres, o que quebrava com 10 ou mais níveis do fator 1.
* `des.fat2.rbd()`: objeto `densi` inexistente com `graph.col = FALSE`.
* `des.fat2.rbd()`/`des.sp.rbd()`: `text(2, bl)` passava o texto como
  coordenada y.
* Os desenhos alteravam `par()` sem restaurar.

### Gráficos
* `boxplotb()`: `text()` recebia as letras como coordenada y (erro).
* `barplotg()`: o tema `theme_bw()` era descartado; `require(ggplot2)` dentro
  das funções; uso do argumento `size` (obsoleto) em `geom_errorbar()`.

### Estatística descritiva e médias
* `ari_mean()`, `geo_mean()`, `har_mean()`: `na.rm` era ignorado ou dividia
  pelo total incluindo `NA`.
* `freqtable()`: frequência acumulada calculada a partir de valores
  arredondados (podia não terminar em 1); nomes de colunas com erros de
  digitação; exemplo da documentação não rodava (`bins` indefinido).
* `histg()`: `scores` inválido gerava erro obscuro; `NA` afetava o número de
  classes; argumento `main` ignorado.
* `desc()`: rótulo "Confidence interval - 95%" mostrava apenas a margem de
  erro; agora mostra os limites inferior e superior.
* `aproximate()`: eixo x fixo em 0-200.

### Aplicativo Shiny
* Unificado em `inst/shiny/app.R` (havia duas versões; a `menu.shiny.v1.R`
  estava incompleta) e iniciado com `run_app()`.
* Separador de colunas e separador decimal padrão eram ambos vírgula
  (leitura do CSV falhava); agora `;` e `,`.
* Na aba "Experimental Design", com dois fatores e parcela subdividida eram
  desenhados dois delineamentos.
* "Graph Output" falhava em DIC (buscava a coluna de bloco vazia).
* A tabela da ANOVA fatorial não repassava `mcomp` e `fac.names`.

## Mudanças de comportamento
* **Funções renomeadas** (exigência do CRAN: nomes como `anova.crd` são
  interpretados como métodos S3 da função genérica `anova()`):
  `anova.crd` -> `anova_crd`, `anova.rbd` -> `anova_rbd`,
  `anova.fat2.crd` -> `anova_fat2_crd`, `anova.fat2.rbd` -> `anova_fat2_rbd`,
  `anova.sp2.crd` -> `anova_sp2_crd`, `anova.sp2.rbd` -> `anova_sp2_rbd`.
  Scripts antigos precisam ser atualizados.
* As funções de ANOVA e de comparação múltipla retornam o resultado de forma
  invisível (a saída já é impressa no console; antes ela aparecia duas
  vezes). Use `res <- anova_crd(...)` para guardar o resultado.
* As ANOVAs fatoriais e em parcelas subdivididas passam a retornar também a
  tabela da ANOVA, o CV, o teste de normalidade e a tabela do desdobramento
  (`Unfolding`).
* `scheffe()` passa a ter a mesma assinatura dos demais testes
  (`y, trt, DFerror, SSerror, ...`).
* As funções de delineamento retornam `Design` (e `Desing`, mantido por
  compatibilidade) e um `Book` com a posição de cada parcela.
* Mensagens do console padronizadas em inglês (sem acentos no código).
* `int.confidance()`, `means.test()`, `par.test()` e `aproximate()` retornam
  (invisivelmente) os valores calculados.
