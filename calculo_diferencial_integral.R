#### validar questões descritivas e validar se as func que tem / foram feitas certas

# 1) Calcule o ângulo θ entre a e b. Sua resposta deve ser um único número com até três casas decimais (se necessário).

# Vetores dados
a <- c(8, 5, 8, 10, 8)
b <- c(14, 7, 16, 9, 8)

# Cálculo do produto escalar
produto_escalar <- sum(a * b)

# Cálculo das normas (magnitudes) dos vetores
norma_a <- sqrt(sum(a^2))
norma_b <- sqrt(sum(b^2))

# Cálculo do cosseno do ângulo
cos_theta <- produto_escalar / (norma_a * norma_b)

# Cálculo do ângulo em radianos e conversão para graus
theta_rad <- acos(cos_theta)
theta_deg <- theta_rad * (180 / pi)

# Arredondamento para 3 casas decimais
theta_deg_arredondado <- round(theta_deg, 3)

# Exibição do resultado
theta_deg_arredondado

# 2) Considere a decomposição em autovalores e autovetores de uma matriz A como apresentado abaixo. Obtenha o determinante de A.

# Autovalores da matriz A
autovalores <- c(22.2, 0.6, 0.6, 0.6)

# Cálculo do determinante (produto dos autovalores)
determinante <- prod(autovalores)

# Resultado
determinante

# 3) Dê o valor (caso exista) que a função deveria ter no ponto dado para ser contínua neste ponto.

# Definindo a função f(x)
f <- function(x) {
  ifelse(x < 1, x, ifelse(x > 1, 1/x, NA))  # NA em x = 1 (ainda não definido)
}

# Cálculo dos limites laterais em x = 1
limite_esquerdo <- 1  # lim x→1⁻ f(x) = lim x→1⁻ x = 1
limite_direito <- 1    # lim x→1⁺ f(x) = lim x→1⁺ 1/x = 1

# Verificação se os limites laterais são iguais
if (limite_esquerdo == limite_direito) {
  valor_continuidade <- limite_esquerdo  # f(1) deve ser esse valor
  cat("Para que f(x) seja contínua em x = 1, defina f(1) =", valor_continuidade, "\n")
} else {
  cat("Os limites laterais não coincidem. Não é possível tornar f(x) contínua em x = 1.\n")
}

# 4) Selecione a frase que melhor descreve o funcionamento de uma função.
# b. Existe um conjunto de entradas, essas entradas são submetidas a algum processamento e ao final existe uma saída.

# 5) O que busca-se em um problema de otimização?
# b. Busca-se determinar os valores extremos de uma função, ou seja, o máximo ou o mínimo que uma função pode assumir em um dado intervalo.

# 6) Calcule a subtração de a−b=c. Para incluir sua resposta faça a soma de c. A resposta deve ser um único valor numérico com até três casas decimais (se necessário).

# Definindo os vetores
a <- c(7, 6, 7, 11, 14)
b <- c(8, 7, 9, 9, 9)

# Calculando a subtração a - b
c <- a - b

# Calculando a soma dos elementos de c
soma_c <- sum(c)

# Arredondando para 3 casas decimais (se necessário)
resultado_final <- round(soma_c, 3)

# Mostrando o resultado
print(resultado_final)

# 7) Qual das alternativas abaixo corresponde a uma das principais utilidades práticas das derivadas?
# b. Encontrar valores de máximo ou mínimo de funções.

# 8) Calcule a matriz C=A+B. Para incluir a sua resposta some todos os elementos da matriz C. Sua resposta deve ser um único número. Use três casas decimais (se necessário).

# Definindo as matrizes A e B
A <- matrix(c(11, 8, 13, 11, 11,
              9, 10, 10, 8, 13,
              7, 4, 11, 9, 11,
              8, 12, 11, 13, 8,
              14, 13, 6, 7, 5), nrow = 5, byrow = TRUE)

B <- matrix(c(4, 9, 8, 5, 15,
              8, 5, 10, 16, 14,
              10, 18, 10, 7, 16,
              14, 11, 10, 5, 14,
              8, 15, 13, 11, 17), nrow = 5, byrow = TRUE)

# Calculando a soma das matrizes
C <- A + B

# Somando todos os elementos de C
soma_total <- sum(C)

# Arredondando para 3 casas decimais (se necessário)
resultado_final <- round(soma_total, 3)

# Mostrando o resultado
print(resultado_final)

# 9) Determine a raiz a direita do ponto crítico da equação:
#f(μ;yi)=∑i=1n2{yiμ−1+log(μyi)}−3.84
#usando o método de Newton. A escolha de valor inicial é parte da questão. Como critério para parada utilize o erro estimado, ou seja, |xi+1−xi|<ϵ, onde ϵ
#é o erro tolerado. Use tolerância de 0.0001. A sua resposta é o valor da raiz use três casas decimais se necessário.
#Considere os seguintes valores para y
#y
##  [1] 0.182 1.696 1.621 2.448 0.879 2.667 3.886 0.964 0.683 0.033

# Dados fornecidos
y <- c(0.182, 1.696, 1.621, 2.448, 0.879, 2.667, 3.886, 0.964, 0.683, 0.033)
n <- length(y)
epsilon <- 0.0001

# Função objetivo
f <- function(mu) {
  if (any(mu <= 0) || any(y <= 0)) return(NA)
  sum(2 * (y/mu - 1 + log(mu/y))) - 3.84
}

# Derivada da função
f_prime <- function(mu) {
  if (mu <= 0) return(NA)
  sum(2 * (-y/(mu^2) + 1/mu))
}

# Método de Newton corrigido
newton_method <- function(f, f_prime, x0, epsilon, max_iter = 100) {
  x <- x0
  for (i in 1:max_iter) {
    f_val <- f(x)
    f_deriv <- f_prime(x)
    
    # Verifica se f_val ou f_deriv são NA/NaN
    if (is.na(f_val) || is.na(f_deriv)) {
      stop("Valor inválido (NA/NaN) encontrado durante a iteração")
    }
    
    x_new <- x - f_val/f_deriv
    
    # Garante que x_new permaneça positivo
    if (x_new <= 0) {
      x_new <- x / 2
    }
    
    if (abs(x_new - x) < epsilon) {
      return(round(x_new, 3))
    }
    x <- x_new
  }
  warning("O método não convergiu após ", max_iter, " iterações")
  return(round(x, 3))
}

# Escolha do valor inicial (média geométrica para melhor estabilidade)
initial_guess <- exp(mean(log(y[y > 0]))) # Calcula média geométrica
                    
# Encontrando a raiz
raiz <- newton_method(f, f_prime, initial_guess, epsilon)
                     
# Resultado
print(raiz)

# 10) Calcule a derivada de f(x)=4

# Pacote para cálculo simbólico de derivadas
if (!require(Deriv)) install.packages("Deriv")
library(Deriv)

# Definindo a função constante
f <- function(x) 4

# Calculando a derivada
f_derivada <- Deriv(f)

# Resultado
print(f_derivada)

# 11) Assinale a opção que corresponde a uma função polinomial.
# a.y=3^x

# 12) Considere matrizes A, Be C compatíveis é incorreto afirmar:
# a.(AB)^⊤=B^⊤A^⊤.

# 13) Como é a estrutura de uma matriz triangular superior?
# e.Os elementos abaixo da diagonal são todos iguais a zero.

# 14) Como é chamada uma matriz quadrada de posto completo?
# b.Não singular.

# 15) Calcule a derivada primeira e segunda da seguinte equação 7x3+5x2+12 e avalie no ponto x=2. A resposta são dois valores numéricos usando três casas decimais.
# Carregando o pacote para derivadas simbólicas
if (!require(Deriv)) install.packages("Deriv")
library(Deriv)

# Definindo a função
f <- function(x) 7*x^3 + 5*x^2 + 12

# Calculando a derivada primeira
f_prime <- Deriv(f)

# Calculando a derivada segunda
f_double_prime <- Deriv(f_prime)

# Avaliando no ponto x = 2
x <- 2
derivada_primeira <- f_prime(x)
derivada_segunda <- f_double_prime(x)

# Arredondando para 3 casas decimais
resultados <- round(c(derivada_primeira, derivada_segunda), 3)

# Mostrando os resultados
cat("Derivada primeira em x=2:", resultados[1], "\n")
cat("Derivada segunda em x=2:", resultados[2])

# 16) Calcule a matriz o produto entre as matrizes A e B. Coloque o resultado em uma nova matriz chamada de C. 
#Para incluir a sua resposta some todos os elementos da matriz C. Sua resposta deve ser um único número. Use três casas decimais (se necessário).

# Definindo as matrizes A e B
A <- matrix(c(16, 8, 11, 18, 15,
              15, 11, 9, 10, 14,
              8, 10, 12, 10, 11,
              11, 13, 5, 10, 7,
              9, 11, 8, 12, 14), nrow = 5, byrow = TRUE)

B <- matrix(c(4, 14, 12, 10, 13,
              11, 15, 9, 10, 7,
              9, 15, 13, 10, 16,
              9, 9, 11, 18, 13,
              16, 13, 10, 11, 13), nrow = 5, byrow = TRUE)

# Calculando o produto matricial C = A %*% B
C <- A %*% B

# Somando todos os elementos de C
soma_total <- sum(C)

# Arredondando para 3 casas decimais (se necessário)
resultado_final <- round(soma_total, 3)

# Mostrando o resultado
print(resultado_final)

# 17) Sobre a Quadratura de Gauss-Hermite adaptativa, assinale a alternativa correta.

# d.Contornando as limitações da quadratura de Gauss-Hermite, a ideia é simplesmente espalhar os pontos de forma mais inteligente 
#para diminuir o número de pontos necessários para aproximar a integral.

# 18) Dado (7x+9)2 calcule a derivada.

# Carregar o pacote (instalar se necessário)
if (!require(Deriv)) install.packages("Deriv")
library(Deriv)

# Definir a função
f <- function(x) (7*x + 9)^2

# Calcular a derivada
f_derivada <- Deriv(f)

# Mostrar o resultado
print(f_derivada)

# 19) Calcule o ângulo θ entre a e b. Sua resposta deve ser um único número com até três casas decimais (se necessário). 
# Definir os vetores a e b
a <- c(11, 8, 10, 9, 7)
b <- c(4, 15, 13, 9, 10)

# Calcular o produto escalar
produto_escalar <- sum(a * b)

# Calcular as normas (comprimentos) dos vetores
norma_a <- sqrt(sum(a^2))
norma_b <- sqrt(sum(b^2))

# Calcular o cosseno do ângulo
cos_theta <- produto_escalar / (norma_a * norma_b)

# Calcular o ângulo θ em radianos e converter para graus
theta_rad <- acos(cos_theta)
theta_deg <- theta_rad * (180 / pi)

# Arredondar para 3 casas decimais
theta_deg_arredondado <- round(theta_deg, 3)

# Resultado final
print(theta_deg_arredondado)

#20) Considerando um dado ponto crítico de uma função de 2 variáveis, o que define se este ponto é de máximo ou de mínimo?
# b.As derivadas parciais de segunda ordem no ponto crítico devem ter o mesmo sinal. Se forem positivas, o ponto é de mínimo. Se forem negativas, o ponto é de máximo.

#21) Resolva a seguinte integral usando algum método da classe dos métodos baseados em simulação. A escolha do método adequado é parte da questão. Use 985 pontos de integração. Na equação o valor de κ
#é igual a 0.5. Como este método é baseado em simulação antes de usá-lo fixe a semente em 123 usando o comando set.seed(123).

set.seed(123)  # Fixar semente para reprodutibilidade
kappa <- 0.5
n <- 985       # Número de pontos de integração

# Gerar amostras da distribuição de proposta (normal padrão)
x_amostras <- rnorm(n)

# Função integranda
integranda <- function(x) {
  (1 / (kappa + 1/kappa)) * exp(-abs(x)/kappa)
}

# Densidade da proposta (normal padrão)
q_x <- function(x) {
  dnorm(x)
}

# Estimador Monte Carlo com Importance Sampling
estimativa <- mean(integranda(x_amostras) / q_x(x_amostras))

# Resultado
round(estimativa, 3)

# 22) Resolva a seguinte integral usando algum método da classe dos métodos baseados em quadratura Gaussiana. A escolha do método adequado é parte da questão. Use 20 pontos de integração. Use λ
#igual a 7988. Sua resposta é um número. Use três casas decimais.
if (!require(statmod)) install.packages("statmod")
library(statmod)  # Para quadratura Gauss-Laguerre

lambda <- 7988
n <- 20  # Número de pontos de integração

# Obter nós e pesos para Gauss-Laguerre
gl <- gauss.quad(n, kind = "laguerre")

# Definir a função f(u)
f <- function(u) {
  (1 / 0.12) * (lambda^3 - (u / 0.12))
}

# Aplicar a quadratura
integral <- sum(gl$weights * f(gl$nodes))

# Resultado com 3 casas decimais
round(integral, 3)

# 23) Calcule a derivada de y=(8x−5)2/(7x+4) e avalie no ponto x=2
if (!require(numDeriv)) install.packages("numDeriv")
# Definir a função
y <- function(x) (8*x - 5)^2 / (7*x + 4)

# Calcular a derivada numericamente usando pacote 'numDeriv'
library(numDeriv)
resultado <- grad(y, x = 2)

# Arredondar para 3 casas decimais
round(resultado, 3)

# 24) A Figura abaixo ilustra o gráfico de uma função bidimensional usando duas formas de visualização: o gráfico de perspectiva e o de isolinhas. Assinale entre as funções qual gera o gráfico apresentado. Para desenhar os gráficos considere uma malha retangular entre [−2,2]
#para as duas variáveis independentes.
# b.0.5x2−0.5y2

# 25) Calcule a derivada primeira e segunda da seguinte equação y=ax e avalie no ponto x=2 e a=2
#. A resposta são dois valores numéricos usando três casas decimais.
if (!require(Deriv)) install.packages("Deriv")
library(Deriv)

# Definir a função
y <- function(a, x) a^x

# Derivada primeira (y')
y_prime <- Deriv(y, "x")

# Derivada segunda (y'') 
y_double_prime <- Deriv(y_prime, "x")

a <- 2
x <- 2

# Calcular os valores
valor_y_prime <- y_prime(a, x)
valor_y_double_prime <- y_double_prime(a, x)

# Arredondar para 3 casas decimais
resultados <- round(c(valor_y_prime, valor_y_double_prime), 3)

# Exibir os resultados
cat("Derivada primeira em x=2:", resultados[1], "\n")
cat("Derivada segunda em x=2:", resultados[2])

#26) Resolva a seguinte integral usando o método de Gauss-Legendre. Use 23 pontos de integração e considere o limite inferior de integração (a=0.1) e o limite superior de integração (b 11.1). Na Equação considere os valores de α
#igual a 1 e β igual a 1. Sua resposta é um número. Use três casas decimais.
library(statmod)  # Para obter nós e pesos de Gauss-Legendre

# Parâmetros
alpha <- 1
beta <- 1
a <- 0.1
b <- 11.1
n <- 23  # Número de pontos de integração

# Obter nós e pesos
gauss <- gauss.quad(n, kind = "legendre")

# Transformação para o intervalo [a, b]
y <- function(t) (b - a)/2 * t + (a + b)/2
dy_dt <- (b - a)/2

# Função integranda (simplificada para alpha=1, beta=1)
integrand <- function(y) exp(-y)

# Aproximação da integral
integral <- dy_dt * sum(gauss$weights * integrand(y(gauss$nodes)))

# Resultado com 3 casas decimais
round(integral, 3)

#27) Obtenha os autovalores da matriz A. Sua resposta deve ser quatro valores. Use três casas decimais (se necessário).

# Definir a matriz A
A <- matrix(c(2.0, 1.6, 1.6, 1.6,
              1.6, 2.0, 1.6, 1.6,
              1.6, 1.6, 2.0, 1.6,
              1.6, 1.6, 1.6, 2.0), 
            nrow = 4, byrow = TRUE)

# Calcular os autovalores
autovalores <- eigen(A)$values

# Arredondar para 3 casas decimais
autovalores_arredondados <- round(autovalores, 3)

# Mostrar os resultados
print(autovalores_arredondados)

#28) Calcule a derivada primeira e segunda da seguinte equação y=(5−x)4 e avalie no ponto x=2. A resposta são dois valores numéricos usando três casas decimais.
# Definir a função
y <- function(x) (5 - x)^4

# Derivada primeira
y_prime <- function(x) -4 * (5 - x)^3

# Derivada segunda
y_double_prime <- function(x) 12 * (5 - x)^2

# Avaliar em x = 2
x <- 2
cat("Derivada primeira em x=2:", round(y_prime(x), 3), "\n")
cat("Derivada segunda em x=2:", round(y_double_prime(x), 3))

#29) Qual é a principal diferença entre a decomposição em valores singulares e a decomposição em autovalores e autovetores?
#a. A decomposição em valores singulares é definida para qualquer matriz, enquanto que a decomposição em autovalores e autovetores é definida apenas para matrizes simétricas.

#30) Calcule as derivadas parciais de primeira e segunda ordem da seguinte função de duas variáveis: u=ln(7x+2y. Avalie cada uma das derivadas no ponto x=2e y=1. 
#A resposta são seis valores numéricos na seguinte ordem: ux;uy;uxx;uyy;uxye uyx. Use arredondamento com três casas decimais para a sua resposta.

# Definir a função
u <- function(x, y) log(7*x + 2*y)

# Derivadas parciais de primeira ordem
u_x <- function(x, y) 7 / (7*x + 2*y)
u_y <- function(x, y) 2 / (7*x + 2*y)

# Derivadas parciais de segunda ordem
u_xx <- function(x, y) -49 / (7*x + 2*y)^2
u_yy <- function(x, y) -4 / (7*x + 2*y)^2
u_xy <- function(x, y) -14 / (7*x + 2*y)^2

# Avaliar em x=2, y=1
x <- 2; y <- 1
resultados <- c(
  u_x(x, y), u_y(x, y),
  u_xx(x, y), u_yy(x, y),
  u_xy(x, y), u_xy(x, y)  # u_{yx} = u_{xy}
)

# Arredondar para 3 casas decimais
round(resultados, 3)