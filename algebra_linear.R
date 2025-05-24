# 1) Calcule a matriz C=A−B. Para incluir a sua resposta some todos os elementos da matriz C. Sua resposta deve ser um único número. Use três casas decimais (se necessário).

# Definindo as matrizes A e B
A <- matrix(c(12, 8, 9, 5, 9,
              9, 11, 10, 15, 7,
              10, 13, 13, 12, 10,
              12, 12, 14, 10, 10,
              14, 16, 8, 8, 11), 
            nrow = 5, byrow = FALSE)

B <- matrix(c(15, 9, 11, 9, 5,
              13, 12, 8, 8, 14,
              11, 6, 8, 8, 10,
              16, 13, 8, 6, 15,
              5, 5, 8, 15, 10), 
            nrow = 5, byrow = FALSE)

# Calculando a matriz C = A - B
C <- A - B

# Somando todos os elementos de C
soma_C <- sum(C)

# Arredondando para 3 casas decimais (se necessário)
soma_C_arredondado <- round(soma_C, 3)

# Resultado
soma_C_arredondado

#2) Calcule o produto interno de apor b, ou seja c=a⋅b. Para incluir sua resposta faça a soma de c. A resposta deve ser um único valor numérico com até três casas decimais (se necessário).

# Definindo os vetores a e b
a <- c(11, 10, 8, 8, 13)
b <- c(12, 9, 13, 10, 6)

# Calculando o produto elemento a elemento (vetor c)
c <- a * b

# Somando os elementos de c (produto interno)
soma_c <- sum(c)

# Arredondando para 3 casas decimais (se necessário)
soma_c_arredondado <- round(soma_c, 3)

# Resultado
soma_c_arredondado

# 3) Use o método de Gauss-Seidel para resolver o sistema Ax=b onde A e b são dados abaixo. Quantas iterações foram necessárias para o método convergir? Use como valores iniciais um vetor de zeros. Sua resposta deve ser apenas um número inteiro.

# Definindo a matriz A e o vetor b
A <- matrix(c(2.5, 0.8, 0.8, 0.7,
              0.8, 2.5, 0.7, 0.8,
              0.8, 0.7, 2.5, 0.8,
              0.7, 0.8, 0.8, 2.5), 
            nrow = 4, byrow = TRUE)
b <- c(10, 8, 14, 17)

# Parâmetros do método
tol <- 1e-6       # Tolerância para convergência
max_iter <- 1000  # Número máximo de iterações
n <- length(b)
x <- rep(0, n)    # Vetor inicial (zeros)
iter <- 0         # Contador de iterações
convergiu <- FALSE

# Método de Gauss-Seidel
while (!convergiu && iter < max_iter) {
  x_antigo <- x
  for (i in 1:n) {
    soma <- 0
    for (j in 1:n) {
      if (j != i) {
        soma <- soma + A[i, j] * x[j]
      }
    }
    x[i] <- (b[i] - soma) / A[i, i]
  }
  iter <- iter + 1
  # Verifica convergência (norma da diferença)
  if (sqrt(sum((x - x_antigo)^2)) < tol) {
    convergiu <- TRUE
  }
}

# Número de iterações necessárias
iter

# 4) Use o método de Jacobi para resolver o sistema Ax=b onde A e b são dados abaixo. Quantas iterações foram necessárias para o método convergir? 
#Use como valores iniciais um vetor de zeros. Sua resposta deve ser apenas um número inteiro.

# Definindo a matriz A e o vetor b
A <- matrix(c(2.5, 0.8, 0.8, 0.7,
              0.8, 2.5, 0.7, 0.8,
              0.8, 0.7, 2.5, 0.8,
              0.7, 0.8, 0.8, 2.5), 
            nrow = 4, byrow = TRUE)
b <- c(9, 11, 4, 10)

# Parâmetros do método
tol <- 1e-6       # Tolerância para convergência
max_iter <- 1000   # Número máximo de iterações
n <- length(b)
x <- rep(0, n)     # Vetor inicial (zeros)
iter <- 0          # Contador de iterações
convergiu <- FALSE

# Método de Jacobi
while (!convergiu && iter < max_iter) {
  x_antigo <- x
  x_novo <- numeric(n)
  
  for (i in 1:n) {
    soma <- 0
    for (j in 1:n) {
      if (j != i) {
        soma <- soma + A[i, j] * x_antigo[j]
      }
    }
    x_novo[i] <- (b[i] - soma) / A[i, i]
  }
  
  x <- x_novo
  iter <- iter + 1
  
  # Verifica convergência (norma da diferença)
  if (sqrt(sum((x - x_antigo)^2)) < tol) {
    convergiu <- TRUE
  }
}

# Número de iterações necessárias
iter


# 5) Considere o sistema linear definido por Ax=b onde A e b são dados abaixo. 
#Encontre a solução deste sistema pelo método de eliminação de Gauss sem pivotação Use três casas decimais (quando necessário) para incluir a sua resposta. 
#A sua resposta terá 4 posições com a solução na ordem x1, x2, x3 e x4. A tolerância para esta questão é de 0,1. Note que os valores entre colchetes não fazem parte da matriz A.

# Definindo a matriz A e o vetor b
A <- matrix(c(1.0, 0.4, 0.4, 0.3,
              0.4, 1.0, 0.3, 0.4,
              0.4, 0.3, 1.0, 0.4,
              0.3, 0.4, 0.4, 1.0), 
            nrow = 4, byrow = TRUE)
b <- c(11, 7, 14, 10)

# Tolerância
tol <- 0.1

# Eliminação de Gauss sem pivotação
n <- nrow(A)

# Fase de eliminação
for (k in 1:(n-1)) {
  for (i in (k+1):n) {
    fator <- A[i, k] / A[k, k]
    A[i, k:n] <- A[i, k:n] - fator * A[k, k:n]
    b[i] <- b[i] - fator * b[k]
  }
}

# Substituição retroativa
x <- numeric(n)
x[n] <- b[n] / A[n, n]
for (i in (n-1):1) {
  soma <- sum(A[i, (i+1):n] * x[(i+1):n])
  x[i] <- (b[i] - soma) / A[i, i]
}

# Arredondando para 3 casas decimais
x <- round(x, 3)

# Resultado
x

# 6) Obtenha os autovalores da matriz A. Sua resposta deve ser quatro valores. Use três casas decimais (se necessário).

# Definindo a matriz A
A <- matrix(c(2.0, 1.8, 1.8, 1.8,
              1.8, 2.0, 1.8, 1.8,
              1.8, 1.8, 2.0, 1.8,
              1.8, 1.8, 1.8, 2.0), 
            nrow = 4, byrow = TRUE)

# Calculando os autovalores
autovalores <- eigen(A)$values

# Arredondando para 3 casas decimais
autovalores_arredondados <- round(autovalores, 3)

# Resultado
autovalores_arredondados

# 7) Considere uma matriz A cuja decomposição em autovalores e autovetores é dada abaixo. Obtenha o log do determinante da inversa de A.

# Extrair os autovalores da decomposição
autovalores <- c(27.2, 1.6, 1.6, 1.6)

# Calcular o log do determinante da inversa de A
log_det_inv_A <- -sum(log(autovalores))

# Resultado
log_det_inv_A

# 8) Sobre soluções de sistemas de equações lineares é correto afirmar.
# d.Se o número de linhas for maior que o número de colunas, o sistema tipicamente não terá solução.

# 9) Calcule a multiplicação de b=αa. Para incluir sua resposta faça a soma de b. A resposta deve ser um único valor numérico com até três casas decimais (se necessário).

# Definindo o vetor a e o escalar alpha
a <- c(8, 7, 8, 12, 4)
alpha <- 6

# Multiplicação do vetor a pelo escalar alpha (b = αa)
b <- alpha * a

# Soma dos elementos de b
soma_b <- sum(b)

# Arredondando para 3 casas decimais (se necessário)
soma_b_arredondado <- round(soma_b, 3)

# Resultado
soma_b_arredondado

# 10) Autovalores e autovetores tem papel importante em técnicas de redução de dimensionalidade. Qual é o objetivo deste tipo de técnica?
# b. Reduzir o número de variáveis a serem analisadas facilitando a interpretação

# 11) Use o método de Gauss-Seidel para resolver o sistema Ax=b onde A e b são dados abaixo. 
# Quantas iterações foram necessárias para o método convergir? Use como valores iniciais um vetor de zeros. Sua resposta deve ser apenas um número inteiro.

A <- matrix(c(2.5, 0.8, 0.8, 0.7,
              0.8, 2.5, 0.7, 0.8,
              0.8, 0.7, 2.5, 0.8,
              0.7, 0.8, 0.8, 2.5), 
            nrow = 4, byrow = TRUE)
b <- c(10, 9, 8, 17)

# Parâmetros
tol <- 1e-6       # Tolerância
max_iter <- 1000  # Máximo de iterações
n <- length(b)
x <- rep(0, n)    # Vetor inicial
iter <- 0
convergiu <- FALSE

# Método de Gauss-Seidel
while (!convergiu && iter < max_iter) {
  x_antigo <- x
  for (i in 1:n) {
    soma <- sum(A[i, -i] * x[-i])
    x[i] <- (b[i] - soma) / A[i, i]
  }
  iter <- iter + 1
  if (sqrt(sum((x - x_antigo)^2)) < tol) convergiu <- TRUE
}

iter

# 12) Obtenha a exponencial matricial da matriz A abaixo. Para inserir a sua resposta calcule o traço da matriz resultante.

# Carregar a biblioteca necessária
if (!require(expm)) install.packages("expm")
library(expm)

# Definir a matriz A
A <- matrix(c(1.0, 0.8, 0.8, 0.7,
              0.8, 1.0, 0.7, 0.8,
              0.8, 0.7, 1.0, 0.8,
              0.7, 0.8, 0.8, 1.0), 
            nrow = 4, byrow = TRUE)

# Calcular a exponencial matricial
exp_A <- expm(A)

# Calcular o traço da matriz resultante
traco <- sum(diag(exp_A))

# Resultado com 6 casas decimais
round(traco, 6)

# 13) Em que consiste a operação de transposição de uma matriz?
# c. Rearranjar a matriz de forma que suas linhas são transformadas em colunas e vice-versa.

# 14) No contexto de métodos iterativos para solução de sistemas lineares. Em qual situação o método para?
# a. Quando algum critério de parada é atingido.

# 15) Em que consiste o método de Eliminação de Gauss-Jordan?
#d. Transformar o sistema original em um sistema diagonal usando operações com linhas.

# 16) Considere uma matriz A cuja decomposição em autovalores e autovetores é dada abaixo. Obtenha o log do determinante da inversa de A.

autovalores <- c(33, 3, 3, 1)
log_det_inv <- -sum(log(autovalores))
log_det_inv

# 17) Qual é a principal diferença entre a decomposição em valores singulares e a decomposição em autovalores e autovetores?
#a. A decomposição em valores singulares é definida para qualquer matriz, enquanto que a decomposição em autovalores e autovetores é definida apenas para matrizes simétricas.

# 18) A decomposição em autovalores e autovetores pode ser usada para obter que tipo especial de matriz?
# d. Diagonal.

# 19) Obtenha os autovalores da matriz A. Sua resposta deve ser quatro valores. Use três casas decimais (se necessário).

# Definindo a matriz A
A <- matrix(c(1.0, 0.8, 0.8, 0.7,
              0.8, 1.0, 0.7, 0.8,
              0.8, 0.7, 1.0, 0.8,
              0.7, 0.8, 0.8, 1.0), 
            nrow = 4, byrow = TRUE)

# Calculando os autovalores
autovalores <- eigen(A)$values

# Arredondando para 3 casas decimais
autovalores_arredondados <- round(autovalores, 3)

# Resultado
autovalores_arredondados

# 20) Que condição deve ser satisfeita para uma matriz A ser considerada simétrica?
# d.A⊤=A

# 21) Calcule o ângulo θ entre a e b. Sua resposta deve ser um único número com até três casas decimais (se necessário).

# Definir os vetores a e b
a <- c(9, 9, 6, 8, 11)
b <- c(4, 8, 18, 6, 14)

# Calcular o produto interno
produto_interno <- sum(a * b)

# Calcular as normas
norma_a <- sqrt(sum(a^2))
norma_b <- sqrt(sum(b^2))

# Calcular cos(theta)
cos_theta <- produto_interno / (norma_a * norma_b)

# Calcular theta em radianos e graus
theta_rad <- acos(cos_theta)
theta_graus <- theta_rad * (180 / pi)

# Arredondar para 3 casas decimais
theta_arredondado <- round(theta_rad, 3)

# Resultado
theta_arredondado

# 22) Obtenha os autovalores da matriz A. Sua resposta deve ser quatro valores. Use três casas decimais (se necessário).

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

# Exibir os autovalores
print(autovalores_arredondados)

# 23) Calcule a matriz C=αA. Para incluir a sua resposta some todos os elementos da matriz C. Sua resposta deve ser um único número. Use três casas decimais (se necessário).

# Definir a matriz A
A <- matrix(c(13, 9, 12, 2, 5,
              6, 8, 6, 19, 5,
              13, 9, 11, 7, 9,
              13, 15, 11, 10, 10,
              13, 5, 11, 8, 13),
            nrow = 5, byrow = TRUE)

# Definir alpha
alpha <- 3

# Calcular C = alpha * A
C <- alpha * A

# Somar todos os elementos de C
soma_total <- sum(C)

# Arredondar para 3 casas decimais (se necessário)
soma_total_arredondada <- round(soma_total, 3)

# Resultado
print(soma_total_arredondada)

# 24) Qual é a condição que permite que duas matrizes possam ser somadas ou subtraídas?

#d. As matrizes precisam ter o mesmo número de linhas e colunas.

# 25) Obtenha os valores singulares da matriz A abaixo. Sua resposta deve ser quatro valores. Use três casas decimais (se necessário).

# Definir a matriz A
A <- matrix(c(11, 9, 9, 12, 8,
              8, 8, 12, 6, 11,
              6, 11, 8, 10, 7,
              12, 11, 11, 12, 15),
            nrow = 4, byrow = TRUE)

# Calcular a decomposição em valores singulares (SVD)
svd_result <- svd(A)

# Extrair os valores singulares
valores_singulares <- svd_result$d

# Arredondar para 3 casas decimais
valores_singulares_arredondados <- round(valores_singulares, 3)

# Mostrar os valores singulares
print(valores_singulares_arredondados)

# 26) Obtenha a exponencial matricial da matriz A abaixo. Para inserir a sua resposta calcule o traço da matriz resultante.

# Carregar a biblioteca necessária
library(expm)

# Definir a matriz A
A <- matrix(c(1.0, 0.8, 0.8, 0.7,
              0.8, 1.0, 0.7, 0.8,
              0.8, 0.7, 1.0, 0.8,
              0.7, 0.8, 0.8, 1.0),
            nrow = 4, byrow = TRUE)

# Calcular a exponencial matricial
exp_A <- expm(A)

# Calcular o traço (soma da diagonal)
traco <- sum(diag(exp_A))

# Arredondar para 3 casas decimais (se necessário)
traco_arredondado <- round(traco, 3)

# Resultado
print(traco_arredondado)

# 27) Considere matrizes A, B e C compatíveis e α e β escalares. As seguintes propriedades são verdadeiras, exceto:

#d.(α+β)A=αA+βB.

# 28) Calcule a matriz o produto de Hadamard entre as matrizes A e B. Coloque o resultado em uma nova matriz chamada de C. 
#Para incluir a sua resposta some todos os elementos da matriz C. Sua resposta deve ser um único número. Use três casas decimais (se necessário).

# Definir a matriz A
A <- matrix(c(9, 7, 13, 10, 18,
              14, 13, 19, 11, 12,
              7, 13, 12, 7, 9,
              10, 12, 12, 6, 11,
              10, 8, 10, 9, 7),
            nrow = 5, byrow = TRUE)

# Definir a matriz B
B <- matrix(c(4, 12, 8, 10, 7,
              15, 8, 8, 10, 5,
              11, 9, 18, 7, 8,
              11, 5, 6, 10, 6,
              10, 12, 13, 6, 11),
            nrow = 5, byrow = TRUE)

# Calcular o produto de Hadamard (elemento a elemento)
C <- A * B

# Somar todos os elementos de C
soma_total <- sum(C)

# Arredondar para 3 casas decimais (se necessário)
soma_total_arredondada <- round(soma_total, 3)

# Resultado
print(soma_total_arredondada)

# 29) Como é feita a atualização dos valores no método de Jacobi?
# b. Os valores das incógnitas são atualizados todos de uma vez no final de cada iteração.

# 30) Calcule a soma de a+b=c. Para incluir sua resposta faça a soma de c. A resposta deve ser um único valor numérico com até três casas decimais (se necessário).

# Definir os vetores a e b
a <- c(12, 10, 11, 7, 7)
b <- c(5, 9, 10, 12, 9)

# Calcular c = a + b (soma elemento a elemento)
c <- a + b

# Somar todos os elementos de c
soma_total <- sum(c)

# Arredondar para 3 casas decimais (se necessário)
soma_total_arredondada <- round(soma_total, 3)

# Resultado
print(soma_total_arredondada)
