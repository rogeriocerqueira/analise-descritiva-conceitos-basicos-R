setwd("/home/rogerio/Documents/Universidade Estadual de Feira de Santana/Probabilidade e Estaística/Unidade I/listas/resolvidas/classroom/analise_descritiva/")

coeficiente_variacao <- function(vetor){
  x = mean(vetor)
  s =sd(vetor)
  result = round((s/x), 3) *100
  
  return(result)  
}
#Calculando o Desvio Padrão relativo, ou Coeficiente de Variação de 
#cada uma das variáveis quantitativas
idade = coeficiente_variacao(Banco_de_Dados_2$Idade)
altura = coeficiente_variacao(Banco_de_Dados_2$Altura)
salario = coeficiente_variacao(Banco_de_Dados_2$Salario)
numero_filhos = coeficiente_variacao(Banco_de_Dados_2$N_Filhos)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Calculando a variabilidade dos sexos em relação ao salários
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sexo = as.factor(Banco_de_Dados_2$Genero); sexo; # Transforme a variável qualitativa em um fator quantitativo
salario = Banco_de_Dados_2$Salario; salario; # Crie uma variável para os dados salario(opicional)
variabilidade_sexo_salario = round(tapply(salario, sexo, var), 3); sexo_salario # Use a função tapply para fazer a comparação

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Assimetria do grupo de dados, Gênero em ralação aos salários
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# # Primeiro defina a função, de assimetria, neste caso foi criada uma função
# As_excel<-function(x){
#   n<-length(x)        
#   As_excel<- sum(((x-mean(x))/sd(x))^3)*(n/((n-1)*(n-2)))
#   return(As_excel)
# }

#vamos usar as variáveis já instanciadas aqui na memória, verifique de instanciálas
#utilize a função tamppy
assimetria_sexo_salario = round(tapply(salario, sexo, As_excel), 3); assimetria_sexo_salario

# #Calculando o percentual de funcionários com grau de instrução de nível superior
# grau_instrucao = table(Banco_de_Dados_2$Grau_de_Instruçao) #Crie uma tabela com os dados alinhados em classes
# percentual_grau_instrucao = prop.table(grau_instrucao); percentual_grau_instrucao
# percentual_grau_instrucao= round(percentual_grau_instrucao, 3)*100

#Percentual de Mulheres com nível médio
genero_instrucao = table(Banco_de_Dados_2$Genero, Banco_de_Dados_2$Grau_de_Instruçao);genero_instrucao
percentual_genero_instrucao = prop.table(genero_instrucao)*100; percentual_genero_instrucao
percentual_genero_instrucao = round(percentual_genero_instrucao, 3); percentual_genero_instrucao

#Percentual de Funcionários que ganham de 1 a 4.3 salários mínimos
salarios_tabela = table(Banco_de_Dados_2$Salario);salarios_tabela
porcentage_salario_categoria = prop.table(salarios_tabela)*100; porcentage_salario_categoria

#Melhorae a analise descritiva

