library(rjson)
library(arules)

data = fromJSON(file="./padaria_trab.json")

# cria a lista header contendo os nomes de todos os produtos
header = unique(unlist(data))[-1]

# cria a lista header_reduced contendo os nomes de todos os tipos de produtos
header_reduced = unique(sapply(header, function(item){
  strsplit(item, " ")[[1]][1]
}))

# preenche a matriz principal
df = sapply(header, function(item){
  vec = rep(0, times=length(data)) # cria um vetor com tamanho dos dados
  for(j in 1:length(data)){
    if(item %in% unlist(data[j][[1]]["produtos"])) vec[j] = 1
    # se o item i do header esta na linha j, preencher o indice j do vetor com 1
  }
  vec
})

# preenche a matriz reduzida
df_reduced = sapply(header_reduced, function(item){
  vec = rep(0, times=length(data)) # cria um vetor com tamanho dos dados
  for(j in 1:length(data)){
    linha = sapply(unlist(data[j][[1]]["produtos"]), function(item2){
      strsplit(item2, " ")[[1]][1]
    }) # salva somente os tipos dos produtos da linha j
    if(item %in% linha) vec[j] = 1
    # se o item i do header esta na linha j, preencher o indice j do vetor com 1
  }
  vec
})

# cria a lista de doces
doces = unique(unlist(sapply(header, function(item){
  if(strsplit(item, " ")[[1]][1] == "Doce") item
})))

# cria a lista not_doces
not_doces = unique(unlist(sapply(header, function(item){
  if(strsplit(item, " ")[[1]][1] != "Doce") item
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

df_doces = sapply(not_doces, function(item){
  vec = rep(0, times=length(data)) # cria um vetor com tamanho dos dados
  for(j in 1:length(data)){
    if(item %in% unlist(data[j][[1]]["produtos"])) vec[j] = 1
    # se o item i do header esta na linha j, preencher o indice j do vetor com 1
  }
  vec
})

df_doces = cbind(df_doces, df_reduced[,"Doce"])
colnames(df_doces) = append(not_doces, "Doce")

# roda o algoritmo apriori nos dados principais
rules = apriori(df, parameter=list(supp=0.05, conf=0.4))

# roda o algoritmo apriori nos dados reduzidos
rules_reduced = apriori(df_reduced, parameter=list(supp=0.1, conf=0.58))


rules_doce = subset(apriori(df_doces,
                            parameter = list(supp = 0.05, conf = 0.4)),
                    subset = (rhs %in% "Doce"))

inspect(rules)
inspect(rules_reduced)
inspect(rules_doce)