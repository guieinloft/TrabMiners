library(rjson)
library(arules)

data = fromJSON(file="./padaria_trab.json")

# cria a lista header contendo os nomes de todos os produtos
header = unique(unlist(data))[-1]

# cria a lista header_reduced contendo os nomes de todos os tipos de produtos
header_reduced = unique(sapply(header, function(item){
  strsplit(item, " ")[[1]][1]
}))

# inicializa a matriz com os dados principais
mat = rep(0, times=length(data))
# inicializa a matriz com os dados reduzidos
mat_reduced = rep(0, times=length(data))

# preenche a matriz principal
for(i in 1:length(header)){
  vec = rep(0, times=length(data)) # cria um vetor com tamanho dos dados
  for(j in 1:length(data)){
    if(header[i] %in% unlist(data[j][[1]]["produtos"])) vec[j] = 1
    # se o item i do header esta na linha j, preencher o indice j do vetor com 1
  }
  mat = rbind(mat, vec) v
}

# preenche a matriz reduzida
for(i in 1:length(header_reduced)){
  vec = rep(0, times=length(data)) # cria um vetor com tamanho dos dados
  for(j in 1:length(data)){
    linha = sapply(unlist(data[j][[1]]["produtos"]), function(item2){
      strsplit(item2, " ")[[1]][1]
    }) # salva somente os tipos dos produtos da linha j
    if(header_reduced[i] %in% linha) vec[j] = 1
    # se o item i do header esta na linha j, preencher o indice j do vetor com 1
  }
  mat_reduced = rbind(mat_reduced, vec) # adiciona o vetor a matriz
}

df = t(mat[-1,])
colnames(df) = header
df_reduced = t(mat_reduced[-1,])
colnames(df_reduced) = header_reduced
# roda o algoritmo apriori nos dados principais
rules = apriori(df, parameter=list(supp=0.05, conf=0.4), appearance=list())
inspect(rules)
# roda o algoritmo apriori nos dados reduzidos
rules_reduced = apriori(df_reduced, parameter=list(supp=0.1, conf=0.58), appearance=list())
inspect(rules_reduced)

# cria a lista de doces
doces = unique(unlist(sapply(header, function(item){
  if(strsplit(item, " ")[[1]][1] == "Doce") item
})))

# cria a matriz de doces
mat_doces = rep(0, times=length(data))
mat_doces = rbind(mat_doces, rep(0, times=length(data)))
for(i in 1:length(header)){
  if(header[i] %in% doces){
    print(header[i])
    for(j in 1:length(data)){
      if(header[i] %in% unlist(data[j][[1]]["produtos"])) mat_doces[2,j] = 1
      # caso o item i seja um doce e esteja na linha j,
      # preencher a segunda linha da matriz com 1
    }
  }
  else{
    vec = rep(0, times=length(data)) # caso nao seja um doce, criar um vetor 
    for(j in 1:length(data)){
      if(header[i] %in% unlist(data[j][[1]]["produtos"])) vec[j] = 1
      # se o item i do header esta na linha j,
      # preencher o indice j do vetor com 1
    }
    mat_doces = rbind(mat_doces, vec)
  }
}

# cria a lista not_doces
not_doces = unique(unlist(sapply(header, function(item){
  if(strsplit(item, " ")[[1]][1] != "Doce") item
})))

df_doces = (t(mat_doces[-1,]))
colnames(df_doces) = append("Doce", not_doces)

inspect(subset(apriori(df_doces, parameter = list(supp = 0.05, conf = 0.4)),
               subset = (rhs %in% "Doce")))