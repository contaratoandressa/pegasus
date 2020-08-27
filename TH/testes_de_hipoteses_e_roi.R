# Questão 1
# Os 36 indivíduos de uma turma são divididos ao acaso em dois
# grupos de 18 indivíduos. Para o primeiro grupo o ensino de
# português é feito usando elementos visuais. Enquanto isso, no
# segundo grupo o ensino é feito pelo método tradiiconal. No final
# do período é aplicado um teste, comum aos dois grupos, com os
# seguintes resultados:

# Grupo 1
g1 = c(7.3,8.2,6.0,7.7,8.0,6.1,5.6,5.3,5.9,5.8,5.8,7.1,5.1,8.0,7.6,8.3,4.9,6.5)
# Grupo 2
g2 = c(7.5,6.2,5.7,4.4,4.7,5.8,5.0,6.0,6.5,5.8,4.5,5.1,5.5,6.0,5.8,5.8,5.7,7.5)

mean(g1)
mean(g2)

median(g1)
median(g2)

mean(g1-g2)

# verificando a quantidade entre grupos
length(g1)==length(g2)

class = rep(c("G1","G2"),c(length(g1),length(g1)))
data = data.frame(grupos=c(g1,g2),classe=class)

# teste de hipóteses de diferenca de médias

# Hipóteses a serem testadas
# H0: mu1 = mu2
# H1: Mu1 != mu2

alpha = 0.05
t.test(grupos ~ classe, data, conf.level=0.95)

# Considerando o nivel de significancia de 5% > p-valor, pode-se afirmar que há diferença entre as médias das notas. 

# IC de diferenca de médias

d_mu - qnorm(1-alpha/2) * d_sd/sqrt(length(g1)); d_mu + qnorm(1-alpha/2) * d_sd/sqrt(length(g1))


# calculate mean of difference
d_mu <- mean(g1) - mean(g2)
# calculate SD of difference
d_sd <- sqrt(var(g1) + var(g2))

# a diferença entre as médias está entre [-2.04, 3.78]
# mas como a nota não pode ser abaixo de 0 

# Considerando os dois grupos como amostras aleatórias de duas
# populações independentes e Normalmente distribuídas,
# determine um intervalo de confiança de 95% para a verdadeira
# diferenças das médias populacionais dos dois grupos.

# Questão 2
# A tabela a seguir contém as notas na 1° prova (x) e na 2° prova
# (y) de uma amostra de alunos de uma determinada disciplina.
# Deseja-se construir um intervalo de confiança a 95% para as
# diferenças entre as médias populacionais das notas da 1° e da 2°
# prova. Os dados são os seguintes:
  
# Notas da 1ª e 2ª provas para um grupo de alunos
# 1° Prova X: 
x = c(6.3,1.5,5.9,6.4,5.5,5.4,5.4,8.0,5.9,8.0,6.5,2.0,3.6,6.0,9.8,6.8,5.3,
  8.7,6.5,6.4,7.7,8.5,5.3,6.9,8.0,8.2,7.1,8.4,6.0,5.5,7.2,6.4,5.5,6.4)

# 2° Prova Y: 
y = c(3.6,3.8,3.0,6.0,4.3,4.6,6.4,5.5,6.0,4.3,4.3,5.2,3.4,2.8,8.3,7.1,5.5,
  8.2,3.8,5.5,6.7,6.7,4.4,3.4,5.9,6.0,5.9,6.8,5.0,6.2,5.4,4.7,3.6,5.2)

# a) Que suposições deveriam ser feitas? O método estatística a ser usado neste caso 
# deve considerar amostras pareadas ou amostras independentes? Porque?

# Suposições: normalidade dos dados.
# O método deveria considerar amostras pareadas, por se tratar de um subconjunto de alunos que compõem o total de uma classe.

# b) Use o método que você considera mais adequado para obter o intervalo de confiança.

# Teste de normalidade
# considerando alpha = 1%

# Hipóteses a serem testadas:
# H0: os dados seguem uma distribuição normal
# H1: os dados não seguem uma distribuição normal

shapiro.test(x)
shapiro.test(y)

# Como o nível de significância é menor que o p-valor, não há evidências de se rejeitar
# a hipótese nula, consluimos que os dados são oriundos de uma distribuição normal.

# Hipóteses a serem testadas
# H0: mu1 = mu2
# H1: Mu1 != mu2

t.test(x, y, paired = TRUE, conf.level = 0.01)

# c) Pode-se concluir com base nessa análise que, em média, o
# desempenho dos alunos da 1° prova para a 2° prova?

# considerando alpha = 0.01 > p-valor, podemos concluir que as médias das notas são diferentes.

# Questão 4 (EXTRA)
# Uma empresa recebeu R$70.000,00 em vendas e que
# R$25.000,00 tenha vindo do e-commerce. Para manter o site
# funcionando é preciso gastar R$7.000,00 e na loja física precisa
# de R$17.500,00. Calculo o ROI para cada uma das formas de atuação e comente o resultado.

# ROI = (Ganho obtido – Investimento) / Investimento

ganho_obtido = 70000
gasto_ecommerce = 25000
gasto_site = 7000
gasto_loja_fisica = 17500

(ROI_ecommerce = (ganho_obtido - gasto_ecommerce)/gasto_ecommerce)
(ROI_site = (ganho_obtido - gasto_site)/gasto_site)
(ROI_loja_fisica = (ganho_obtido - gasto_loja_fisica)/gasto_loja_fisica)

# O ROI é um parâmetro que serve para analisar o retorno sobre qualquer tipo de 
# investimento – seja um projeto de pesquisa tecnológica, uma campanha de marketing, 
# a compra de uma nova máquina ou a aquisição de um novo título de renda fixa para sua 
# carteira de investimentos.

# Avaliar como iniciativas e investimentos diversos contribuem para a obtenção de resultados;
# Planejar objetivos e metas com base em resultados atingíveis;
# Identificar o prazo de retorno dos investimentos, bem como a curva de resposta específica de cada um;
# Viabilizar um processo mais objetivo de tomada de decisões, fundamentado em números;
# Proporcionar um potencial aumento nos retornos e nos lucros.

# ROI_ecommerce = 1.8

# Isso significa que o retorno sobre o investimento foi de 1.8 vezes o valor aplicado
# inicialmente. Como o ROI normalmente é expresso em forma de porcentagem, multiplica-se
# tal resultado por 100. Com isso, no exemplo acima, o ROI foi de 180%.

# ROI_site = 9

# Isso significa que o retorno sobre o investimento foi de 9 vezes o valor aplicado
# inicialmente. Como o ROI normalmente é expresso em forma de porcentagem, multiplica-se
# tal resultado por 100. Com isso, no exemplo acima, o ROI foi de 900%.


# ROI_loja_fisica = 3

# Isso significa que o retorno sobre o investimento foi de 3 vezes o valor aplicado
# inicialmente. Como o ROI normalmente é expresso em forma de porcentagem, multiplica-se
# tal resultado por 100. Com isso, no exemplo acima, o ROI foi de 300%.
