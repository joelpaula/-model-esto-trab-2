---
title: "Modelação Estocástica - Trabalho 1"
subtitle: 'Ciência de Dados - PL - 3º ano| Professora: Catarina Marques'
author: 
- Catarina Castanheira, 92478
- João Martins, 93259
- Joel Paula, 93392
date: "15/10/2021"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
header-includes:
  - \usepackage[sfdefault]{roboto}
  - \renewcommand{\familydefault}{\sfdefault}
  - \usepackage{titling}
  - \pretitle{\par\vspace{50mm}\begin{center}
  - \posttitle{\par\vspace{100mm}\end{center}}
    \includegraphics[width=2in,height=2in]{rgb_iscte_pt_horizontal_positive.png}\LARGE\\}
editor_options: 
  markdown: 
    wrap: 120
---

<!-- get the required files from 3rd party sources -->

<link href='http://fonts.googleapis.com/css?family=Roboto' rel='stylesheet' type='text/css'> <!-- use the font -->

```{=html}
<style>
  body {
    font-family: 'Roboto', sans-serif;
  }
</style>
```
\newpage



# Problema:

Pretende-se gerar 2000 números de uma distribuição t de Student com 4 graus de liberdade via um passeio aleatório.
Considere como distribuição candidata a distribuição normal padrão, assim como, a possibilidade de admitir diferentes
valores para o seu desvio-padrão, como por exemplo 0.5 e 2. Considere, para cada uma das situações anteriores, o valor
inicial igual a 0.

Implemente o algoritmo em R e compare, para cada uma das situações, a distribuição empírica dos valores gerados com a
distribuição teórica. Decida por uma distribuição candidata.

Para a distribuição candidata escolhida, avalie diferentes porções da cadeia e deite fora as p primeiras observações do
passeio aleatório que lhe pareçam que não possuem a distribuição alvo.

Interprete todos os resultados obtidos.

# Resolução:

Começamos por fazer uma representação das curvas teóricas que no fundo nos servirão de base para a análise da eficácia
dos cenários de simulação mais abaixo.

No gráfico, podemos perceber que a curva candidata que mais se aproxima da nossa curva alvo, a t-Student com 4 graus de
liberdade, é a Normal padrão. Se esta será de facto a melhor distribuição candidata para gerar NPA com uma distribuição
T (4) ou não usando o algoritmo Metropolis Random Walk, será analisado mais à frente.


```r
# distribuição alvo t-student com 4 graus de liberdade, T(v=4):
f <- function(x, v = 4) {
    dt(x, df = v)
} # distribuição alvo; 

# colors
cols <- c("gray40", "dodgerblue3", "firebrick2", "darkgoldenrod2")
# Gráfico com as curvas de densidade sobrepostas:
curve(f, -5, 5, ylim =c(0,0.8), col=cols[1], lwd=1)
curve(dnorm(x, sd=0.5), -5, 5, add=T, col=cols[2], lwd=1)
curve(dnorm(x), -5, 5, add=T, col=cols[3], lwd=1)
curve(dnorm(x, sd=2), -5, 5, add=T, col=cols[4], lwd=1)
legend("topright", 
       legend=c("Alvo", "Candidata N(0,0.5)", "Candidata N(0,1)", "Candidata N(0,2)"), 
       col=cols,
       lty = 1, lwd=2, bty = "n")
```

![](Trabalho-2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Na tradução do algoritmo *Metropolis Random Walk* para o código R vamos replicar os seguintes passos essenciais:

1.  definição de um vetor em que armazenaremos os valores de x gerados para a cadeia;

2.  estabelecer o primeiro valor da cadeia como sendo zero, de acordo com o solicitado no enunciado;

3.  Ciclo com o número de iterações igual ao número de NPA que queremos gerar menos 1, que já está definido como zero,
    composto por:

    1.  a definição de um valor para o incremento $z$ (que segue uma distribuição Normal com uma média de zero e um
        desvio-padrão parametrizado em cada cenário);
    2.  a definição de um valor de $y$ candidato (a partir da fórmula $y = x_{[i-1]} + z)$;
    3.  e a definição do critério de aceitação/rejeição do $y$ gerado (geramos um NPA com distribuição $U (0,1)$; se
        este NPA for inferior ou igual ao critério $\alpha$ definido $min(\frac{f(y,v)}{f(x_{[i - 1]},v)},1)$, o próximo
        valor de $x$ na cadeia será o valor de $y$ gerado, caso contrário corresponderá ao $x$ anterior;
    4.  por fim incluímos no algoritmo uma fórmula para o cálculo da taxa de aceitação dos NPA gerados, que servirá para
        análise de eficiência da função candidata.

Por uma questão prática, decidimos englobar o algoritmo numa função em R, permitindo-nos facilmente escolher os
parâmetros em cada cenário (dimensão da cadeia, o parâmetro da distribuição alvo - graus de liberdade -, os parâmetros
da distribuição candidata - média e desvio-padrão ‑, e o valor inicial da cadeia).

Para percebermos quais os valores do desvio-padrão da distribuição candidata se poderão adequar mais à geração de NPA
para a distribuição t-Student, serão simulados cenários em que este parâmetro será diferente. E para cada um deles será
importante perceber: qual a taxa de aceitação dos valores de $y$ gerados; quanto tempo a cadeia demora a convergir para
a distribuição estacionária (no fundo, qual a dimensão da fase de *burn-in*); quanto tempo a cadeia demora a percorrer
toda a amplitude dos valores de $x$ possíveis; qual os "níveis" de autocorrelação existentes entre os valores da cadeia
gerada; e qual a semelhança da distribuição dos valores gerados com a distribuição teórica.

Apesar de o número de NPA desejado ser de 2000, para cada um dos amostradores iremos definir um $N$ (NPA gerados)
superior, no sentido de dar "espaço de manobra" para *o burn-in*/aquecimento. Os NPA considerados deverão ser os últimos
2000 obtidos com cada amostrador.


```r
# Para o problema em questão, o seguinte é o conhecido:

# N <- 2000 # pretende-se gerar 2000 NPA
# a geração dos NPA terá de ser feita via um passeio aleatório


# distribuição candidata: Normal-padrão, i.e. N(0,1); outras distribuições candidatas para considerar: N(0, sigma), com sigma a assumir valores por nós designados # nolint
#g <- function(x, mean_g = 0, sigma_g = 1) {
#    dnorm(x, mean = mean_g, sd = sigma_g) # nolint
#} # distribuição candidata: Normal com média fixa em zero e desvio-padrão editável # nolint
#sigma <- 1 # começamos com uma Normal-padrão, valor editável # nolint

# para todas as situações, o valor inicial será zero

tst_random_walk <- function(N = 3000, v=4, mean_g = 0, sigma_g = 1, start = 0) {
    set.seed(42)
    x <- numeric(N) # vector de zeros, de dimensão N
    x[1] <- start # definimos valor inicial no vector; valor fixo
    k <- 0
    for (i in 2:N) {
        # geração de um NPA de acordo com a distribuição candidata # nolint
        z <- rnorm(n = 1, mean_g, sigma_g) 
        y <- x[i - 1] + z
        alpha <- min(f(y, v) / f(x[i - 1], v), 1)
        u <- runif(1)
        if (u <= alpha) { # cálculo da aceitação dos valores gerados
            x[i] <- y
            k <- k + 1     # contagem dos aceites
        } else {
            x[i] <- x[i - 1]
        }
    }
    
    par(mfrow = c(1, 1))

    acf(x)

    refline <- qt(c(.01, .99), df = v)

    
    plot(x[1:500], type = "l")
    abline(h = refline, lty=2, col="red")
    plot(c(501:1000), x[501:1000], type = "l")
    abline(h = refline, lty=2, col="red")

    plot(x, type = "l")
    abline(h = refline, lty=2, col="red")
    
    # manter só os npa gerados depois do Burn in
    from <- length(x)-2000
    to <- length(x)
    x <- x[from:to]
    
    plot(x, type = "l")
    abline(h = refline, lty=2, col="red")

    plot(c(1501:2000), x[1501:2000], type = "l")
    abline(h = refline, lty=2, col="red")
    

    print("Taxa de aceitação: ")
    print(k / N)
    par(mfrow = c(1, 3), mar= c(5, 4, 12, 1))
    plot(ecdf(x), main = bquote(sigma==.(sigma_g)))
    curve(pt(x, v), add = TRUE, col = "red")
    
    
    qqplot(qt(x, v), x, main="", xlab=bquote("Quantis T-"~.(v)), ylab="Quantis Amostrais")
    abline(0, 1,col="red")
    
    hist(x, main="", xlab="", ylab="Densidade", freq=FALSE, ylim=c(0,0.5))
    curve(f, -10, 10, add = T, col="red")
}
```

## $N (0, 0.5)$

Comecemos então com o primeiro cenário, em que a distribuição candidata é uma Normal $N (0, 0.5)$.


```r
tst_random_walk()
```

![](Trabalho-2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-3-2.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-3-3.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-3-4.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-3-5.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-3-6.png)<!-- -->

```
## [1] "Taxa de aceitação: "
## [1] 0.7356667
```

```
## Warning in qt(x, v): NaNs produced
```

![](Trabalho-2_files/figure-html/unnamed-chunk-3-7.png)<!-- -->

-   Taxa de aceitação: 0.8586667

-   Tempo até convergência: TODO

-   Tempo até percorrer toda a amplitude de $x$: TODO

-   Até que *lag* existem elevados níveis para autocorrelação: *lag* 20

-   Semelhança dos valores gerados com a distribuição teórica: TODO

## $N (0, 1)$

Analisemos agora o cenário em que temos como distribuição candidata uma Normal $N (0, 1)$.


```r
tst_random_walk(sigma_g = 0.5)
```

![](Trabalho-2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-4-2.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-4-3.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-4-4.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-4-5.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-4-6.png)<!-- -->

```
## [1] "Taxa de aceitação: "
## [1] 0.8586667
```

```
## Warning in qt(x, v): NaNs produced
```

![](Trabalho-2_files/figure-html/unnamed-chunk-4-7.png)<!-- -->

-   Taxa de aceitação: 0.7356667

-   Tempo até convergência: TODO

-   Tempo até percorrer toda a amplitude de $x$: TODO

-   Até que *lag* existem elevados níveis para autocorrelação: *lag* 15

-   Semelhança dos valores gerados com a distribuição teórica: TODO

## $N (0, 2)$

No terceiro cenário, temos como distribuição candidata uma Normal $N(0, 2)$.


```r
tst_random_walk(sigma_g = 2)
```

![](Trabalho-2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-5-2.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-5-3.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-5-4.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-5-5.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-5-6.png)<!-- -->

```
## [1] "Taxa de aceitação: "
## [1] 0.5313333
```

```
## Warning in qt(x, v): NaNs produced
```

![](Trabalho-2_files/figure-html/unnamed-chunk-5-7.png)<!-- -->

-   Taxa de aceitação: 0.5313333

-   Tempo até convergência: TODO

-   Tempo até percorrer toda a amplitude de $x$: TODO

-   Até que lag existem elevados níveis para autocorrelação: lag 13

-   Semelhança dos valores gerados com a distribuição teórica: TODO

## $N (0, 20)$

Por fim, temos o caso em que o amostrador tem como distribuição candidata uma Normal com desvio padrão bastante superior
àqueles observados nos cenários anteriores, $N(0,20)$.


```r
tst_random_walk(sigma_g = 20)
```

![](Trabalho-2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-6-2.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-6-3.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-6-4.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-6-5.png)<!-- -->![](Trabalho-2_files/figure-html/unnamed-chunk-6-6.png)<!-- -->

```
## [1] "Taxa de aceitação: "
## [1] 0.072
```

```
## Warning in qt(x, v): NaNs produced
```

![](Trabalho-2_files/figure-html/unnamed-chunk-6-7.png)<!-- -->

-   Taxa de aceitação: 0.072

-   Tempo até convergência: TODO

-   Tempo até percorrer toda a amplitude de $x$: TODO

-   Até que *lag* existem elevados níveis para autocorrelação : *lag* 16

-   Semelhança dos valores gerados com a distribuição teórica: TODO

## Conclusões: TODO

- qual o melhor amostrador e porquê;

- retirar os primeiros valores p que não parecem fazer parte da distribuição alvo

- o que acontece à taxa de aceitação quando o desvio padrão é aumentado;

- o que acontece ao tempo até à convergência quando o desvio padrão é aumentado;

- o que acontece ao tempo até percorrer toda a amplitude de x quando o desvio padrão é aumentado;

- (?) o que acontece ao número de lags a partir do qual deixa de existir autocorrelação quando o desvio padrão é aumentado;

- o que acontece à semelhança com a distribuição teórica quando o desvio padrão é aumentado;

- propriedades das cadeias de Markov que são verificáveis

- cadeias de Markov convergem para o valor esperado (neste caso, a média das Normais candidatas)
