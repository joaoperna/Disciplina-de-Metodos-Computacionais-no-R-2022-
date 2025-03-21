#Limpando a memória Global
rm(list=ls())
#Setando o wd manualmente eo R_site para evitar msg
R_LIBS_SITE="C:/Program Files/R/R-4.2.0/R repositório"
setwd("C:/Users/João Perna/Desktop/Caixa de Entrada/Metodos Computacionais atividades no R")
##################################################
#Exercise 1
#1.2. On the command line, type demo(graphics). Follow prompts. You might prefer to re-size the windows.
demo(graphics)
#1.3.  On the command line, type demo(image). This demonstration is concerned with representations of 3D data.
demo(image)
#1.4. Get help on the function q, by typing ?q
?q
#1.5. Quit R , by typing q().
q()
#Exercise 2
#2.0 Baixar o arquivo
url <- "https://www.ma.imperial.ac.uk/~das01/RCourse/hills.txt"
download.file(url, "hills.txt")
#2.1. Read the file, assigning the result to the object hills
hills <- read.table("hills.txt")
#2.2. Examine the object. Note column and row names
hills
#2.3. Construct a scatterplot matrix
pairs(hills)
#2.4. Make the columns of the hills object available by name
attach(hills)
#2.5. Construct a scatter plot. The function call in this way means the first argument is the horizontal axis
plot(dist,time)
#2.6. Interact with the plot to label points - right click to finish
#identify(dist,time,row.names(hills))
#2.7. Compute a linear regression of time against distance
lm(time~dist)
#2.8. Obtain more information about the regression
summary(lm(time ~ dist))
#2.9. Add the least squares regression line - note anonymous function call
abline(lm(time~dist))
#2.10. Obtain some diagnostics plots - note the different arguments to the plot function. Be aware of the prompt in the Console.
plot(lm(time~dist))
#2.11. there are many pre-defined system objects. Display the value of pi - note that this is a reserved word
pi
#2.12. List objects in current working space
ls()
#2.13. Display the object ls
ls
#2.14. Create a copy of the hills object
hill.cp <- hills
#2.15. List objects in current working space
ls()
#2.16. Delete the copy. Note that there is no undelete functionality.
rm(hill.cp)
#Exercise 3
#3.1. Create a vector of coefficients for a quadratic equation, using the sample function. Here, we draw a sample of size 3 from −20, −19, . . . , 19, 20 with replacement
coeffs <- sample(-20:20,3,replace=T)
#3.2. Determine the class of the object coeffs.
class(coeffs)
#3.3. Determine the length of the object coeffs
length(coeffs)
#3.4. Determine the names associated with the vector
names(coeffs)
#3.5. Assign some names. Note the function call occurring on the right hand of the assignment operator.
names(coeffs) <- c("a","b","c")
#3.6. Prepare to plot the equation, by constructing a regularly spaced vector for the horizontal axis
x <- seq(-3,3,length=200)
x
#3.7. Evaluate the quadratic at each point in vector x
y <- coeffs[1]*x^2+coeffs[2]*x+coeffs[3]
y
#3.8. Construct the plot
plot(x,y)
plot(y=y,x=x)
#3.9. 9. Does the equation have real roots? Compute the discriminant
coeffs[2]^2-4*coeffs[1]*coeffs[3]
#3.10. Oops, we didn’t retain the value! R stores the last unassigned object in the system object.Last.value
disc <- .Last.value
#3.11. Create a vector of type character, and display the second elemen
chr.vec <- sample(letters,5); chr.vec[2]
#3.Problems
#3.1*. Compute the real roots of the quadratic equation
##Criando uma função para resolução 
Bhaskara <- function(a,b,c){
  if(a == 0)
    print("A equação não é do segundo grau")
  else{
    delta = (b*b)-(4*a*c)
  if(delta<0)
    print("Não existe raiz real")
  if(delta>0){
    print("Existem duas raizes reais e diferentes")
    x1 = (-b+sqrt(delta))/(2*a)
    x2 = (-b-sqrt(delta))/(2*a) 
    return(c(x1,x2))}
  if(delta == 0){
    print("Existem duas raizes reais e iguais")
    x1 = (-b+sqrt(delta))/(2*a)
    return(x1)}}}
Bhaskara(6,7,8)
#3.2*
##Without using R , determine the result of the following computation
x <- c(1,2,3)
x[1]/x[2]^3-1+2*x[3]-x[2-1]
#3.3*
## Generate a regular grid between -50 and 50. Construct separate plots of log(x), exp(x), sin(x),sin(2x), sin(x)/cos(x). Examine the cumulative sum of the final function. Experiment with the argument type of the plot function.
##Duvidas Marcleiton
#Exercise 4
#4.1. Create a logical vector
x <- seq(-3,3,length=200) > 0
x
#4.2. negate this vector
!x
#4.3. Compute the truth table for logical AND
c(T,T,F,F) & c(T,F,F,T)
#4.4. Explore arithmetic with logical and numeric
1:3 + c(T,F,T)
#4.5. Compute the intersection of {1, 2, . . . , 10} and {5, 6, . . . , 15}
intersect(1:10,5:15)
#4.6. Create a factor
drinks <- factor(c("beer","beer","wine","water"))
drinks
#4.7. Examine the representation of the factor
unclass(drinks)
#4.Problems
#4.1* Compute the truth table for logical OR. The function R computes the logical EXCLUSIVE-OR. What is the difference between the two?
##Material de suporte https://www.educative.io/answers/what-is-the-xor-function-in-r e https://mathcenter.oxford.emory.edu/site/math117/logicalValuesInR/#:~:text=There%20are%20only%20two%20logical,T%20and%20FALSE%20with%20F%20.
## Tabela lógica
c(TRUE,TRUE,FALSE,FALSE) | c(TRUE,FALSE,FALSE,TRUE)
xor(x,y)

x <- c(T,T,F,F)
y <- c(F,F,T,T)
xor(x,y)
c(TRUE,TRUE,FALSE,FALSE) || c(TRUE,FALSE,FALSE,TRUE)
xor(x,y)
#4.2* Consider the vector 1:K, where K is a positive integer. Write an R command that determines how many elements in the vector are exactly divisible by 3.
##Material de referencia: https://www.tutorialspoint.com/how-to-find-numbers-that-are-divisible-by-a-certain-number-for-a-range-of-values-in-r#:~:text=In%20R%2C%20the%20divisibility%20of,for%20loop%20will%20be%20used.
for(t in 1:14){if(t%%3==0){print(t)}}
#4.3* . Write an R command to evaluate the proportion of beer in the drinks factor object.
##Material de referencia: https://stackoverflow.com/questions/24901061/in-r-how-do-i-compute-factors-percentage-given-on-different-variable
table(drinks)/sum(table(drinks))
#Exercise 5
##5.1. Examine the row and column names of the hills object
row.names(hills)
names(hills)
##5.2. Create data frames. Note row and column names
x1.df <- data.frame(1:10,I(letters[1:10]),factor(letters[1:10]))
x2.df <- data.frame(X1=1:10,X2=I(letters[1:10]),X3=factor(letters[1:10]))
##5.3.Compute the mean of column X1
mean(x2.df$X1)
##5.4. Create a matrix
x.mat <-matrix(1:12,nrow=3,ncol=4)
##5.5. Examine the default dimension names of the matrix
dimnames(x.mat)
##5.6. Assign some dimnames to the matrix
dimnames(x.mat) <- list(letters[1:3],letters[1:4])
##5.7. Combine matrices
xx <- cbind(x.mat,x.mat)
xxx <- rbind(x.mat,x.mat)
rbind(xx,xxx)
##5.8. Explore indexing
x <- 1:10
names(x) <- letters[x]
x[1:3]
x[c(1,10)]
x[c(-1,-2)]
x[ x > 5]
x[c("a","d")]
x[]
jj1 <- matrix(1:100,ncol=10)
jj1[1:5,]
jj1[1:4,x[x <3]]
##5.9. Compute sums of the columns of the hills object
lapply(hills,sum)
sapply(hills,sum)
##5.10. Create a list, and examine elements
x.lis <- list(a=1:10,b=letters[1:3],b=matrix(1:10,ncol=2))
library(tidyverse)
glimpse(x.lis)      #### Não existe x.lis$1 como cita o exercício, logo foi utilizado x.list$a
x.lis$a 
x.lis[[2]]
##5.11. Element-wise arithmetic with matrices
x.mat <- matrix(1:10,ncol=2)
x.mat+1
x.mat + x.mat
##5.12. Matrix multiplication
x.mat %*% t(x.mat)
##5.13. Compute row and column sums of a matrix
apply(x.mat,1,sum)
apply(x.mat,2,sum)
#5.Problems
#5.1.* Construct a 2×2 data frame, X say. Experiment with X^(1:K), where K takes values 1:4. How does the recycling rule behave? What happens if you remove the brackets from the command?
K <- 1:4
Ats <- data.frame(x^(1:K))
Ats1 <- data.frame(x^1:K)
##5.2. The function system.time returns timings for R operations. Examine the help system about thisfunction. For a 107 ×2 matrix, X, and vector y of length 107/2 compute (a number of times) Xty using matrix multiplication and the function crossproduct. Which is quicker?
?system.time 
system.time(matrix(data = 10^7*2, nrow = 100, ncol = 2))
x <- matrix(data = 1, nrow = 10^7, ncol = 2)
y <- 10^7/2
system.time(t(x)%%y)
system.time(crossprod(x, y = NULL))
#Resposta a função crossprod é mais rapida
#Exercise 6
##6.1. Determine what objects are in the current workspace
ls()
objects()
##6.2. Create and edit a new data frame
a.df <- data.frame()
fix(a.df)
##6.3. Create, then delete some objects. Note the multiple assignment
jj1 <- jj2 <- jj3 <- a.df
rm(a.df)
rm(list = objects(pattern= "jj*"))
##6.4.  Examine the search path
search()
attach(hills)
search()
##6.5. Write the hills object to a file
write.table(hills,"test.dat")
##6.6. Get some help
?mean
help.start()
##6.7. Do a silly large computation.
sin(matrix(0, nrow = 5000, ncol = 5000))
#6.Problems
##6.1* Generate a matrix of size n × p. Use the function as.data.frame to coerce the matrix to a data frame. Which object requires more storage space?
matrix_6 <- matrix(1:30, nrow = 6, ncol = 5)
matrix_6.1 <- data.frame(matrix_6)
?object.size
object.size(matrix_6)
object.size(matrix_6.1)
#Resposta o data frame ocupa mais memória
#Exercise 7
##7.1  Generate a sample of random normal deviates, and a sample of random exponential deviates.
x <- rnorm(50)
x
y <- rnorm(50,0,1)
y
##7.2 Compute some summaries
mean(x)
sqrt(var(x))
cor(x,y)
cor(cbind(x,y))
##7.3.  Try the summary function
summary(x)
summary(cbind(x,y))
##7.4. Let X ∼ N(0, 1) and Y ∼ Exp(2). Compute P(X > 1.644) and find q such that P(Y < q) = 0.75.
1-pnorm(1.644)
qexp(0.75,2)
## 7.5. Use the sample function to obtain a random sample of 10 realisations in a biased coin experiment
sample(c("Head","Tail"), 10, prob=c(0.3,0.7),replace=T)
## 7.6.  Load the package MASS and examine the help
help(package="MASS")
## 7.7. Experiment with set.seed
set.seed(1)
runif(10)
set.seed(1)
runif(10)
runif(10)
## 7.8. Test to see if sample x is consistent with an Exp(1) distribution using a QQ plot
plot(qexp(ppoints(x),1),sort(x))
abline(0,1)
### Examine the help for the function qqnorm.
## 7.9.  Compare the two samples with a QQ plot
qqplot(x,y)
abline(0,1)
## 7.10. Compare the two samples with box plots
boxplot(x,y)
## 7.11. A simple alternative to display the two samples
plot(c(x,y),rep(0:1,c(length(x),length(y))),xlab="",ylab="")
## 7.12. Plot a histogram of x and a box plot of y, in the same figure.
par(mfrow=c(2,1))
hist(x)
boxplot(y)
## 7.13.  Consider the Pima Indians data: a collection of variables observed on a particular group of native American Indians who are either healthy or diabetic. This data includes measurements of tricep, skinfold and blood glucose level. First, download and read in the data
### https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database
#### Dúvida Marcleiton
# 7.Problems
##Examine the built in ChickWeight data (the help gives background about the data). The function split will prove useful to do the following (as will a script) 
##a - Método 1
dura <- ChickWeight
attach(dura)
n <- 413
df2 <- dura[row.names(dura) %in% (n+1):nrow(dura), ]
df3 <- df2[row.names(df2) %in% (414+10):nrow(df2), ]
plot(time,weight, data = df3)
##b Método 2
dura1 <- filter(dura,Diet==4)
##c
dura2 <- dura %>% mutate(Chick = as.numeric(Chick)) %>% filter(Chick < 14)
?mean
mean(dura2$weight)
ggplot(data = dura2)+ aes(Time,weight)+
  geom_smooth()
###Duvida Marcleiton
#Exercise 8
##8.1.Write an R expression to determine if two sets, A and B, represented as integer vectors are disjoint. If they are disjoint, display elements of set A otherwise display elements of set B. (Examine thehelp for functions print and cat)
A <- 1:100
print(A)
B <- 101:200
print(B)

funcao8 <- function(number) {
  ifelse(any(number<100),
         print("Entre 1 e 100 logo pertence a A"),print("Entre 101 e 200 logo pertence a B"))
}

funcao8(2)
##8.2.Write R codes that takes the coefficients of a quadratic equation, and outputs an appropriate message for the cases of (i). two distinct roots (b² − 4ac > 0) (ii) coincident roots (b² = 4ac) or (iii). complex roots (b² < 4ac)
### Output appropriate message based on discriminant
numero2funcao <- function(a,b,c){
  diferenciador <- b^2 - 4*a*c
    if (diferenciador > 0) {
  raiz1 <- (-b + sqrt(diferenciador))/(2*a)
  raiz2 <- (-b - sqrt(diferenciador))/(2*a)
  cat("The quadratic equation has two distinct roots: ", raiz1, " and ", raiz2, ".\n")
} else if (diferenciador == 0) {
  raiz <- -b/(2*a)
  cat("The quadratic equation has coincident roots: ", raiz, ".\n")
} else {
  real_part <- -b/(2*a)
  imaginary_part <- sqrt(-diferenciador)/(2*a)
  cat("The quadratic equation has complex roots: ", real_part, "+", imaginary_part, "i and ", real_part, "-", imaginary_part, "i.\n")
}}
numero2funcao(1,4,4)
##8.3. Let vector y be the logarithm of a random sample from a standard normal distribution, N(0, 1). Use the ifelse function to replace missing values with the value 9999.
set.seed(1234)
t <- runif(10, min = 0, max = 1)   
t[2:3] <- NA   
t <- ifelse(is.na(t), 9999, t)  
t
##8.4. Let n be a large integer. Compute a vector x containing n random uniform deviates. Embed the following code in the system.time function for (i in 1:n) y[i] <- sin mes. Now, time the call y <- sin(x) Which is faster?
n <- 1000000
x <- runif(n)
y <- numeric(n)
system.time(for (i in 1:n) y[i] <- sin(x[i]))
system.time(y <- sin(x))
###Reposta o calculo diretemente sem o loop, ou seja, a segunda
##8.5. Compound interest can be computed using the formula A = P × (1 + R/100)n where P is the original money lent, A is what it amounts to in n years at R percent per year interest. Write R code to calculate the amount of money owed after n years, where n changes from 1 to 15 in yearly increments, if the money lent originally is 5000 pounds and the interest rate remains constant throughout the period at 11.5%.
# Volume inicial emprestado
inicial <- 5000
# Taxa de Juros
juros <- 0.115
anos
anos <- 1:15            # number of years, from 1 to 15

# calculate the amount owed for each year
valor_total <- inicial * (1 + juros) ^ anos

# print the results
for (i in 1:length(valor_total)) {
  cat("Depois", anos[i], "anos, o Valor Total é:", round(valor_total[i], 2), "pounds\n")
}
##8.6. Write a loop structure to scan through an integer vector to determine the index of the maximum value. The loop should terminate as soon as the index is obtained. (Don’t worry about ties). Of course, this is not a good way to do this! Examine the help for the rank, sort and order functions.
# create a vector of integers
x <- c(59, 82, 3111, 211111, 101, 62, 45)
# initialize variables
maior_valor <- x[i]
valor_index <- i
# loop through the vector to find the maximum value and its index
for (i in 1:length(x)) {
  if (x[i] > maior_valor) {
    maior_valor <- x[i]
    valor_index <- i
    break  # terminate the loop as soon as the index is obtained
  }
}
# print the result
cat("A posição do maior valor é:",valor_index, "\n")

# Problemas exercícios Microeconometrics with R
## Chapter 1
##Create the following simulated data.
##1.1.N = 100   (100 data points)
##1.2.x ~ U[0,1]   (uniformly distributed between 0 and 1)
##1.3.u ~ N(0,2)  (normally distributed with a variance of 2)
##1.4.y = a + b*x + u, where a = -2, b = 3.
##2.Run ordinary least squares on the data created in (1).  Present estimates for a and b.  Discuss why they are or are not close to the true values.
#Criando um banco de dados
set.seed(123)
#Linhas de 1 a 100
N <- 100
#y = a + bx + v supondo que a=2 e b=3 sejam os parametros da populacao
a <- -2
b <- 3
#Amostra aleatoria dos valores de x
x <- runif(N)
#Vetor aleatorio do termo de erro v
u <- rnorm(N)
#Criando o vetor y
y <- a + b*x + u
head(y)
##3.Repeat (2) with N = 1000.  Discuss how the estimates of a and b are different (or the same) from (2).
#Linhas de 1 a 1000
N <- 1000
#y = a + bx + v supondo que a=2 e b=3 sejam os parametros da populacao
a <- -2
b <- 3
#Amostra aleatoria dos valores de x
x <- runif(N)
#Vetor aleatorio do termo de erro v
v <- rnorm(N)
#Criando o vetor y
y <- a + b*x + v
head(y)
#Comparando as medias de y e x
mean(y[x>0.95]) - mean(y[x<0.05])
##4.Download the data for Using Geographic Variation in College Proximity to Estimate Returns to Schooling by David Card (http://www.nber.org/papers/w4483).  The data is available here: http://davidcard.berkeley.edu/data_sets.html.  Also available here: Google Sheets
data <- readxl::read_xlsx("nls.xlsx")
##4.1.Plot log wages in 1976 on number of years of education in 1976.
### Tratando os dados
data$wage76 <- as.numeric(data$wage76)
data$ed76 <- as.numeric(data$ed76)
data$lwage76 <- log(data$wage76)
dados <- data[is.na(data$lwage76)==0,]
###Plotando
plot(dados$ed76, dados$lwage76, data = dados, xlab = "Anos de Educação", ylab = "Log do salário")
##4.2.Run OLS of log wages in 1976 on number of years of education in 1976.
regcap1 <- lm(data = dados, lwage76 ~ ed76)
summary(regcap1)
##4.3.Plot log wages in 1976 on work experience (measured as age less years of education plus 6 years).
### Tratando os dados
dados$wexp76 = dados$age76 - dados$ed76 + 6
###Plotando
plot(dados$wexp76 , dados$lwage76, data = dados, xlab = "Anos de experiência", ylab = "Log do salário")
ggplot(data = dados)+
         aes(wexp76, lwage76)+
         geom_point()+
         xlab("Anos de experiência")+ ylab("Log do salário")
 ##4.4.Run OLS of log wages in 1976 on experience in 1976.
regcap12 <- lm(data = dados, lwage76 ~ wexp76)
summary(regcap12)
##4.5.If we are trying to understand how much years of education affects wages, should we worry that some people have more or less work experience?
 ### Sim pois a através da regressão podemos determinar que a experiencia de trabalho tem relação positiva com o (log do) salário dos indivíduos.
## Chapter 2
##1.Create the following simulated data.
##1.1.x ~ U[0,1] (uniformly distributed)
set.seed(123)
N <- 50  
x <- round(runif(N))  
x  
##1.2.w ~ U[0,1] + d*x, where d = 3  (uniformly distributed between 0 and 1 + the effect of x)
x <- round(runif(N)) 
a <- 1
b <- 2
c <- -3
d <- 3
w_u <- runif(N)
w_u
w <- d*x + w_u
w
##1.3.e ~ N(0,1)  (normally distributed with a variance of 1)
e <- rnorm(N)
e
##1.4.y = a + b*x + c*w + e, where a = 1, b = 0, c = -3
y = a + b*x + c*w + e
y
##1.5.N = 50 (50 data points)
##1.6.Run ordinary least squares on the relationship between y and x (y as a function of x).  Present the estimates of b.  Discuss why it is or is not close to the true value.
shortreg <- lm(y ~ x)
summary(shortreg)
##1.7.Draw the causal diagrams representing this data generating process.

##1.8.Run ordinary least squares on the relationship between y and all of x and w.  Present the estimates of b.  Discuss why the it is or is not close to the true value.
###Obtendo manualmente os coeficiente de cada relacao
e_hat <- shortreg$coefficients[2] 
c_hat <- lm(y ~ w)$coef[2]
d_hat <- lm(w ~ x)$coef[2]

###A estimativa de b
e_hat - c_hat*d_hat
e_hat
##1.9.Can you show that b must be equal to zero?  Show your test. 
sum(b)
summary(b)
##2.Download the data for Mortgage Lending in Boston.  Available here: HMDA data (Google Sheets) 
x <- readxl::read_xlsx("hmda_aer.xlsx")
##2.1.Create variables for mortgage denials, race, income, credit worthiness.
##2.3.Using ordinary least squares can you show that there is evidence that banks are illegally denying mortgages based on race?  Or that there is evidence that banks are behaving legally?  Explain your method.  Explain the concerns with your analysis.
library(readxl)
x <- read_xlsx("hmda_aer.xlsx")
x$deny <- ifelse(x$s7==3,1,ifelse(x$s7==1 | x$s7==2,0,NA))
x$black <- as.numeric(x$s13==3)

#Salarios
x$lwage <- NA
x$wage <- ifelse(x$s31a == 0, NA, x$s31a)
x$lwage <- log(x$wage)

#Pefil de pagamento no sistema bancario
x$mhist <- x$s42

#Perfil pagamento do consumidor
x$chist <- x$s43

#Pefil de pagamento de emprestimos publico
x$phist <- x$s44

#Tempo empregado
x$emp <- x$s25a
x$emp <- ifelse(x$emp > 1000,NA,x$emp)

#Definindo as matrizes
Y1 <- x$deny
X1 <- cbind(1,x$black)
W1 <- cbind(1, x$lwage, x$chist, 
            x$mhist, x$phist, x$emp)

#Excluindo as linhas com NAs
index <- is.na(rowSums(cbind(Y1,X1,W1)))==0

#Redefinindo as matrizes sem NAs
X2 <- X1[index,]
W2 <- W1[index,]
Y2 <- Y1[index]

#Estimando as matrizes dos coeficientes
beta_tilde_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%Y2
delta_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%W2
gamma_hat <- solve(t(W2)%*%W2)%*%t(W2)%*%Y2
beta_hat <- beta_tilde_hat - delta_hat%*%gamma_hat

#Incluindo mais variaveis
x$married <- as.numeric(x$s23a == "M")

#Participacao das despesas na renda
x$dr <- ifelse(x$s45 > 999999, NA, x$s45)

#Linhas de credito
x$clines <- ifelse(x$s41 > 999999, NA, x$s41)

#Sexo
x$male <- as.numeric(x$s15 == 1)

#(S11) County
x$suff <- ifelse(x$s11 > 999999, NA, x$s11)

#(S35) Liquid assets (in thousands)
x$assets <- ifelse(x$s35 > 999999, NA, x$s35)

#(S6) Loan amount (in thousands)
x$s6 <- ifelse(x$s6 > 999999, NA, x$s6)

#(S50) Appraised value (in thousands)
x$s50 <- ifelse(x$s50 > 999999, NA, x$s50)

#(S33) Purchase price (in thousands)
x$s33 <- ifelse(x$s33 > 999999, NA, x$s33)

#(S6) Loan amount (in thousands)
x$lr <- x$s6/x$s50
x$pr <- x$s33/x$s50

#(S16) Co-applicant sex	
x$coap <- x$s16==4
x$school <- ifelse(x$school > 999999, NA, x$school)

#Times application was reviewed by underwriter
x$s57 <- ifelse(x$s57 > 999999, NA, x$s57)

#(S48) Term of loan (months)
x$s48 <- ifelse(x$s48 > 999999, NA, x$s48)

#(S39) Number of commercial credit reports
x$s39 <- ifelse(x$s39 > 999999, NA, x$s39)

#(chvalc) Change in median value of
#property in a given tract, 1980-1990
x$chval <- ifelse(x$chval > 999999, NA, x$chval)

#(S20) Number of units in property purchased
x$s20 <- ifelse(x$s20 > 999999, NA, x$s20)
x$lwage_coap <- NA

#(S31C) Total monthly income of coapplicant ($)
x[x$s31c > 0 & x$s31c < 999999,]$lwage_coap <-
  log(x[x$s31c > 0 & x$s31c < 999999,]$s31c)
x$lwage_coap2 <- ifelse(x$coap==1,x$lwage_coap,0)
x$male_coap <- x$s16==1

#Redefinindo nosso W1
W1 <- cbind(1,x$lwage,x$chist,x$mhist,x$phist,x$emp,
            x$emp^2,x$married,x$dr,x$clines,x$male,
            x$suff,x$assets,x$lr,x$pr,x$coap,x$s20,
            x$s24a,x$s27a,x$s39,x$s48,x$s53,x$s55,x$s56,
            x$s57,x$chval,x$school,x$bd,x$mi,x$old,
            x$vr,x$uria,x$netw,x$dnotown,x$dprop,
            x$lwage_coap2,x$lr^2,x$pr^2,x$clines^2,x$rtdum)

#Eliminando NAs
index <- is.na(rowSums(cbind(Y1,X1,W1)))==0
X2 <- X1[index,]
W2 <- W1[index,]
Y2 <- Y1[index]

#Estimando as matrizes dos coeficientes
beta_tilde_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%Y2
delta_hat <- solve(t(X2)%*%X2)%*%t(X2)%*%W2
gamma_hat <- solve(t(W2)%*%W2)%*%t(W2)%*%Y2
beta_hat <- beta_tilde_hat - delta_hat%*%gamma_hat
##2.2.Run ordinary least squares on the relationship between mortgage denials and race.  Does this provide evidence that banks are acting illegally?
#Bootstrap
set.seed(123456)
K <- 1000
bs_mat <- matrix(NA,K,2)

for (k in 1:K) {
  index_k <- round(runif(length(Y2),min = 1, max = length(Y2)))
  Y3 <- Y2[index_k]
  X3 <- X2[index_k,]
  W3 <- W2[index_k,]
  beta_tilde_hat <- solve(t(X3)%*%X3)%*%t(X3)%*%Y3
  delta_hat <- solve(t(X3)%*%X3)%*%t(X3)%*%W3
  gamma_hat <- solve(t(W3)%*%W3)%*%t(W3)%*%Y3
  bs_mat[k,] <- beta_tilde_hat - delta_hat%*%gamma_hat
}

#Inserindo valores na tabela
tab_res <- matrix(NA,2,4)
tab_res[,1] <- colMeans(bs_mat)
tab_res[,2] <- apply(bs_mat,2,sd)
tab_res[1,3:4] <- quantile(bs_mat[,1], c(0.025,0.975))
tab_res[2,3:4] <- quantile(bs_mat[,2], c(0.025,0.975))
colnames(tab_res) <- c("Estimate",'SD', "2.5%","97.5%")
tab_res

assas <- lm(lwage,chist,mhist,phist,emp,
  emp^2,married,dr,clines,male,
  suff,assets,lr,pr,coap,s20,
  s24a,s27a,s39,s48,s53,xs55,s56,
  s57,chval,school,bd,mi,old,
  vr,uria,netw,dnotown,dprop,
  lwage_coap2,lr^2,pr^2,clines^2,rtdum , data = x)
summary(assas)
##3.Download the data for Using Geographic Variation in College Proximity to Estimate Returns to Schooling by David Card (http://www.nber.org/papers/w4483).  The data is available here: http://davidcard.berkeley.edu/data_sets.html.  Also available here: Google Sheets
library(readxl)
nls <- read_xlsx("nls.xlsx")
##3.1.Replicate Table 2 of the paper (to the extent you can with the data).  Note that you need to read the paper carefully to determine the definitions of the variables.
#Lendo variaveis como numericas
nls$lwage76 <- as.numeric(nls$lwage76)
nls1 <- nls[is.na(nls$lwage76)==0,]

#Criando variaveis
nls1$exp <- nls1$age76 - nls1$ed66 - 6
nls1$exp2 <- (nls1$exp^2)/100

#Estimativa OLS
regca22 <- lm(lwage76 ~ ed76 + exp + exp2 + black +reg76r + smsa76r + smsa66r
          + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668
          + reg669, data = nls1)
summary(regca22)
##3.2.How should we interpret the estimates for row (1) Education and (2) Experience?
### Ambas têm graus de relevância superiores a 0.001%, ou seja, existe uma chance muito pequena de se incorrer em erro do tipo 1. Ao mesmo experiencia parece ser mais relevante para determinar o nível de salário dos indivíduos (sem levar em consideração o efeito negativo de exp^2 é claro).

##3.3.What concerns should we have about these estimates for determining the value of a policy that subsidizes college?
### É necessário tanto se ater tanto a disparidade racial (evidenciada em black), as questões regionais (de localização) e a escolaridade dos pais, objetivando diminuir as disparidades.

## Chapter 3
##1.Create the following simulated data.
##1.1.x ~ a binary variable, where Prob(x=1) = 0.2  (20% are 1s)
##1.2.w ~ U[0,1] + d*x, where d = 3  (uniformly distributed between 0 and 1 + the effect of x)
##1.3.z ~ U[0,1] + f*x, where f = 2
##1.4.e ~ N(0,1)  (normally distributed with a variance of 1)
set.seed(123)
N <- 50
x <- rbinom(N, 1, 0.2)
a <- 1
b <- 0
c <- -3
d <- 3
e <- rnorm(N, mean = 0, sd = 1)
f <- 2
g <- -2
w <- runif(N) + d*x
z <- runif(N) + f*x
##1.5.y = a + b*x + c*w + g*z + e, where a = 1, b = 0, c = -3, g = -2
##1.6.N = 50 (50 data points)
y = a + b*x + c*w + g*z + e
head(y)
##1.7.Run ordinary least squares on the relationship between y and x (y as a function of x).  Present the estimates of b.  Discuss why it is or is not close to the true value.
regcap3 <- lm(y ~ x)
summary(regcap3)
##1.8.Draw the causal diagrams representing this data generating process.
##1.9.Run ordinary least squares on the relationship between y and all of x, w and z.  Present the estimates of b.  Discuss why the it is or is not close to the true value.
regcap32 <- lm(y ~ x + w)
head(y)
##2.Create the following simulated data.
##2.1.z ~ U[0,1] (uniformly distributed between 0 and 1)
##2.2.u ~ N(0,2)  (normally distributed with a variance of 2)
##2.3.e ~ N(0,1) 
##2.4.x = c + d*u + f*z +  e, where c = -4, d = -2, f = 2
##2.5.y = a + b*x + u, where a = 3, b = -2.
##2.6.N = 1000   (1000 data points)
library(LaplacesDemon)
set.seed(123)
N <- 1000
a <-  3 
b <-  -2
c <-  -4
d <-  -2
e <-  rnorm(N, mean = 0, sd = 1)
f <-  2
u <-  rnormv(N, mean = 0, var = 2)
z <-  - runif(N, min = 0, max = 2)
x <-  c + d*u *f*z + e
y <-  a + b*x + u
head(y)
##2.8.Run ordinary least squares on the relationship between y and x (y as a function of x).  Present estimates for a and b.  Discuss why they are or are not close to the true values.
regcap33 <- lm(y ~ x)
summary(regcap33)
##2.9.Run OLS on the relationship between x and z, present estimates for c and f.  
regcap34 <- lm(x ~ z)
summary(regcap34)
##2.10.Run OLS on the relationship between y and z.  How should the coefficient estimate on z be interpreted?
regcap35 <- lm(y ~ z)
summary(regcap35)
##2.11.Using the results in (h) and (i) to determine the IV estimate of b.  Discuss why this estimate is or is not close to the true value of b.
##3.Using matrix algebra, derive the IV estimate.
lm_iv <- function(y, X_in, Z_in=X_in, Reps = 100, 
                  min_in = 0.05, max_in = 0.95) {
  set.seed(123456)
  X <- cbind(1, X_in)
  Z <- cbind(1,Z_in)
  
  bs_mat <- matrix(NA,Reps, dim(X)[2])
  N <- length(y)
  for (r in 1:Reps) {
    index_bs <- round(runif(N, min = 1, max = N))
    y_bs <- y[index_bs]
    X_bs <- X[index_bs,]
    Z_bs <- Z[index_bs,]
    bs_mat[r,] <- solve(t(Z_bs)%*%X_bs)%*%t(Z_bs)%*%y_bs
  }
  
  tab_res <- matrix(NA, dim(X)[2],4)
  tab_res[,1] <- colMeans(bs_mat)
  for (j in 1:dim(X)[2]) {
    tab_res[j,2] <- sd(bs_mat[,j])
    tab_res[j,3] <- quantile(bs_mat[,j],min_in)
    tab_res[j,4] <- quantile(bs_mat[,j],max_in)
  }
  colnames(tab_res) <- c("coef", "sd", 
                         as.character(min_in), as.character(max_in))
  return(tab_res)
}

print(lm_iv(y,x), digits = 3) #OLS
print(lm_iv(y,x,z), digits = 3) #IV
##4.Create the following simulated data.
##4.1.x ~ a binary variable, where Prob(x=1) = 0.2  (20% are 1s)
#w ~ U[0,1] + d*x, where d = 3  (uniformly distributed between 0 and 1 + the effect of x)
#z ~ U[0,1] + f*x, where f = 2
#e ~ N(0,1)  (normally distributed with a variance of 1)
#y = a + b*x + c*w + g*z + e, where a = 1, b = 0, c = -3, g = -2
#N = 1000 
set.seed(123)
N <- 1000
a <- 1
b <- 0
c <- -3
d <- 3
e <- rnormv(N, mean = 0, var = 1)
f <- 2
g <- -2
x <- rbinom(N, 1, 0.2)
w <- runif(N, min = 0, max = 1) + d*x 
z <- runif(N) + d*f
y = a + b*x + c*w + g*z + e
head(y)
##4.2.Calculate the OLS estimate based on the matrix algebra in R.
regcap36 <- lm(y ~ x)
summary(regcap36)
##4.3.Calculate the IV estimate based on the matrix algebra in R. 
bd_hat <- lm(y ~ z)$coef[2]
d_hat <- lm(x ~ z)$coef[2]
bd_hat/d_hat
##5.Download the data for Using Geographic Variation in College Proximity to Estimate Returns to Schooling by David Card (http://www.nber.org/papers/w4483).  The data is available here: Data (Google Sheets)
database <- read.dta()
##5.1.Replicate Table 3 A (the top part, to the extent you can with the data).
### Aquirvo já carregado anteriormente em outra atividade
#Criando variaveis
### Regressão 
regcap37 <- lm(lwage76 ~ ed76 + exp + exp2 + black +reg76r + smsa76r + smsa66r
          + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668
          + reg669,  data = nls1)
summary(regcap37)
##5.2.How should we interpret the estimate for education in row (2) (0.132)?

##6.Create a two-sage IV estimator using matrix algebra and using the bootstrap to create a measure of uncertainty around the estimates.  Compare the results the estimator used in (4) and (5).
###Usando matrizes....
y <- nls1$lwage76
X <- cbind(nls1$ed76, nls1$exp, nls1$exp2, nls1$black, nls1$reg76r,
           nls1$smsa76r, nls1$smsa66r, nls1$reg662, nls1$reg663,
           nls1$reg664, nls1$reg665, nls1$reg666, nls1$reg667,
           nls1$reg668, nls1$reg669)
nls1$age2 <- nls1$age76^2
Z1 <- cbind(nls1$nearc4, nls1$age76, nls1$age2, nls1$black,
            nls1$reg76r,nls1$smsa76r, nls1$smsa66r, nls1$reg662,
            nls1$reg663,nls1$reg664, nls1$reg665, nls1$reg666,
            nls1$reg667, nls1$reg668, nls1$reg669)

res <- lm_iv(y,X, Reps=1000)
res <- lm_iv(y,X,Z1, Reps=1000)

rownames(res) <- c("intercept","ed76","exp", "exp2",
                   "black", "reg76r","smsa76r", "smsa66r",
                   "reg662","reg663","reg664", "reg665",
                   "reg666","reg667", "reg668", "reg669")

###Comparando medias para domicilios proximos ou não da escola
tab_cols <- c("Near College", "Not Near College")
tab_rows <- c("ed76", "exp", "black", "south66", 
              "smsa66r", "reg76r", "smsa76r")
table_dist <- matrix(NA,7,2)


###Teste de identificacao
Z2 <- cbind(nls1$momdad14, nls1$age76, nls1$age2, nls1$black,
            nls1$reg76r, nls1$smsa76r, nls1$smsa66r, nls1$reg662,
            nls1$reg663, nls1$reg664, nls1$reg665, nls1$reg666,
            nls1$reg667, nls1$reg668, nls1$reg669)

###Bootstrap
set.seed(123)
bs_diff <- matrix(NA,1000,1)
N <- length(y)

for (i in 1:1000) {
  index_bs <- round(runif(N,min = 1,max = N))
  y_bs <- y[index_bs]
  X_bs <- X[index_bs,]
  Z1_bs <- Z1[index_bs,]
  Z2_bs <- Z2[index_bs,]
  bs_diff[i] <- (solve(t(Z1_bs)%*%X_bs)%*%t(Z1_bs)%*%y_bs)[2,1] - 
    (solve(t(Z2_bs)%*%X_bs)%*%t(Z2_bs)%*%y_bs)[2,1]
}

summary(bs_diff)
quantile(bs_diff,c(0.05, 0.95))
## Chapter 4
##1. Download the data for Tying Odysseus to the Mast (Google Sheets - dta file).  Think of the treatment assignment as an instrument.
####install.packages("readstata13")
library(readstata13)
database1 <- read.dta13("seedanalysis_011204_080404.dta")
##2.Download the data for Using Geographic Variation in College Proximity to Estimate Returns to Schooling by David Card (http://www.nber.org/papers/w4483).  The data is available here: Data (Google Sheets)
##2.1. Split the observations into two groups:
##2.1.1. Received 12 or few years of education prior to 1976
nls2 <- nls1 %>% 
  filter(nls1$ed76 <= 12)
##2.1.2. Received 13 or more years of education prior to 1976
nls3 <- nls1 %>% 
  filter(nls1$ed76 >= 13)
##2.2. For the two groups in (a) calculate the proportion in each group and average log wages in 1976.

regcap41 <- lm(lwage76 ~ ed76 + exp + exp2 + black +reg76r + smsa76r + smsa66r
               + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668
               + reg669,  data = nls2)
summary(regcap41)
regcap42 <- lm(lwage76 ~ ed76 + exp + exp2 + black +reg76r + smsa76r + smsa66r
               + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668
               + reg669,  data = nls3)
summary(regcap42)
### Proporcinalmente pessoas com menos educação tende a se beneficar mais de quantidades maiores de educação.
 # Atividade Final
## Experimento 1
## Obejtivo: Neste primeiro experimento o objetivo é linkar diferentes bancos de dados e realizar uma regressão que possa trraçar um correlação entre o Pib per capita e indicadores da saúde básica de Tocantins
# Realizando o Dowwloand dos dados
library(basedosdados)
#Setando o o billing do Google Clound
set_billing_id("teste-base-dos-dados-376717")
#Puxandos os dados referentes ao Estado do Tocantins das bases de dados de população e pib e juntando ambas, para os municípios do Estado do Tocantins
pib_per_cap <- basedosdados::read_sql(query = "SELECT pib.id_municipio,pop.ano,  pib.PIB / pop.populacao as pib_pc
FROM `basedosdados.br_ibge_pib.municipio` as pib 
INNER JOIN `basedosdados.br_ibge_populacao.municipio` as pop
ON pib.id_municipio = pop.id_municipio AND pib.ano = pop.ano
WHERE pib.id_municipio BETWEEN '1700000' AND '1800000' AND pib.ano BETWEEN 2010 AND 2020")
#Baíxando os dados referente a indicadores da atenção básica dos municípios do estado do Tocantins
atenca_basica <- basedosdados::read_sql(query = "SELECT * 
FROM `basedosdados.br_ms_atencao_basica.municipio` 
WHERE id_municipio BETWEEN '1700000' AND '1800000' AND ano BETWEEN 2010 AND 2020")
#Realizando a junção bancos de dados
final2 <- atenca_basica  %>% 
  left_join(pib_per_cap, by = c('id_municipio' = 'id_municipio', 'ano' = 'ano'))
#Tratando os dados 
final2$lpib <- log(final2$pib_pc)
final23 <- final2 %>% 
  mutate(quantidade_equipes_atencao_basica_equivalente = as.numeric(quantidade_equipes_atencao_basica_equivalente)) %>%
  mutate(quantidade_equipes_atencao_basica_parametrizada   = as.numeric(quantidade_equipes_atencao_basica_parametrizada  )) %>%
  mutate(quantidade_equipes_saude_familia = as.numeric(quantidade_equipes_saude_familia)) %>%
  mutate(quantidade_equipes_atencao_basica_total  = as.numeric(quantidade_equipes_atencao_basica_total )) %>%
  mutate(populacao_coberta_estrategia_saude_familia = as.numeric(populacao_coberta_estrategia_saude_familia)) %>%
  mutate(populacao_coberta_total_atencao_basica = as.numeric(populacao_coberta_total_atencao_basica))
# Modelo

#Realizando a regressão 
options(scipen = 9999)
rgm1 <- lm(lpib ~ carga_horaria_medica_atencao_basica_tradicional + carga_horaria_enfermagem_atencao_basica_tradicional +
             proporcao_cobertura_estrategia_saude_familia + populacao_coberta_total_atencao_basica+
             quantidade_equipes_atencao_basica_equivalente + quantidade_equipes_atencao_basica_parametrizada+
             quantidade_equipes_saude_familia + quantidade_equipes_atencao_basica_total +
             populacao_coberta_estrategia_saude_familia + populacao_coberta_total_atencao_basica, data = final23)
summary(rgm1)

# Conclusão Observa-se que que a Carga horária ambulatorial de enfermagem na atenção básica tradicional,Proporção da população coberta pela Estratégia Saúde da Família e Número de equipes de atenção básica tradicional equivalentes a equipes da Estratégia Saúde da Família foram as variáveis mais relevantes, observa-se tambem um R2 expremamente baíxo, logo as variáveis independentes explicam pouco da variavel dependente      
## Experimento 1
## Obejtivo: Neste experimento o objetivo é estimar as relação entre homicidio doloso no estado do Rio de Janeiro e outras variaveis presente no modelo
# Realizando o Dowwloand dos dados 
#Setando o o billing do Google Clound
set_billing_id("teste-base-dos-dados-376717")
#Realizando as Querys e baixando o banco de dados
query6 <-  bdplyr("br_isp_estatisticas_seguranca.evolucao_mensal_uf")
#puxando os dados especificos do ano de 2019 em todo estado do Rio de Janeiro x cidades
seguranca_dados_ano5 <- basedosdados::read_sql(query = "SELECT * FROM `basedosdados.br_isp_estatisticas_seguranca.evolucao_mensal_uf` WHERE ANO BETWEEN 2010 AND 2021")
#Tratando dados
seguranca_dados_ano55 <- seguranca_dados_ano5 %>% 
  mutate(hom_doloso = as.numeric(hom_doloso)) %>% 
  mutate(lesao_corp_morte = as.numeric(lesao_corp_morte)) %>% 
  mutate(lesao_corp_dolosa = as.numeric(lesao_corp_dolosa)) %>% 
  mutate(outros_roubos = as.numeric(outros_roubos)) %>% 
  mutate(total_roubos = as.numeric(total_roubos)) %>% 
  mutate(total_furtos = as.numeric(total_furtos)) %>% 
  mutate(posse_drogas = as.numeric(posse_drogas)) %>%
  mutate(sequestro = as.numeric(sequestro))
#Realizando a Regressaão 
attach(seguranca_dados_ano55)
regm <- lm(hom_doloso ~ lesao_corp_morte +
             lesao_corp_dolosa + outros_roubos + total_roubos 
           +total_furtos + sequestro + posse_drogas)
summary(regm)
#Conclusão : As variáveis Lesão corporal dolosa, roubos e posse de dogras possuem maior relevância para explicar a taxa de homicídios dolosos no estado  do Rio de Janeiro. Nesse experimento é possível observer um r2 ajustado maior do que no anterior, demosntrando uma melhor capacidade de explicação do modelo.