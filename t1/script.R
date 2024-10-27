library(rjson)
library(arules)

data = fromJSON(file="~/Downloads/padaria_trab.json")

header = unique(unlist(data))[-1]
header_reduced = unique(sapply(header, function(item){
  strsplit(item, " ")[[1]][1]
}))

mat_reduced = rep('n', times=length(data))

for(i in 1:length(header_reduced)){
  vec = rep('n', times=length(data))
  for(j in 1:length(data)){
    linha = sapply(data[i][[1]]["produtos"], function(item2){
      strsplit(item2, " ")[[1]][1]
    })
    if(header_reduced[i] %in% linha) vec[j] = 's'
  }
  mat_reduced = rbind(mat_reduced, vec)
}

df_reduced = as.data.frame(mat_reduced[-1,], row.names = header_reduced)

rules = apriori(df_reduced, parameter=list(supp=0.3, conf=0.6))