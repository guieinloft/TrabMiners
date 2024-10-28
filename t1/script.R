library(rjson)
library(arules)

data = fromJSON(file="./padaria_trab.json")

header = unique(unlist(data))[-1]

header_reduced = unique(sapply(header, function(item){
  strsplit(item, " ")[[1]][1]
}))

mat = rep(0, times=length(data))

mat_reduced = rep(0, times=length(data))

for(i in 1:length(header)){
  vec = rep(0, times=length(data))
  for(j in 1:length(data)){
    if(header[i] %in% unlist(data[j][[1]]["produtos"])) vec[j] = 1
  }
  mat = rbind(mat, vec)
}

for(i in 1:length(header_reduced)){
  vec = rep(0, times=length(data))
  for(j in 1:length(data)){
    linha = sapply(unlist(data[j][[1]]["produtos"]), function(item2){
      strsplit(item2, " ")[[1]][1]
    })
    if(header_reduced[i] %in% linha) vec[j] = 1
  }
  mat_reduced = rbind(mat_reduced, vec)
}

df = t(mat[-1,])
colnames(df) = header
df_reduced = t(mat_reduced[-1,])
colnames(df_reduced) = header_reduced
typeof(df)

rules = apriori(df, parameter=list(supp=0.05, conf=0.4), appearance=list())
inspect(rules)

rules_reduced = apriori(df_reduced, parameter=list(supp=0.1, conf=0.58), appearance=list())
inspect(rules_reduced)

doces = unique(unlist(sapply(header, function(item){
  if(strsplit(item, " ")[[1]][1] == "Doce") item
})))
inspect(subset(apriori(df, parameter=list(supp=0.018, conf=0.22)), subset=(rhs %in% doces)))

        
