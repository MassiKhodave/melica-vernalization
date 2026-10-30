library("UpSetR")

res = read.table("data/N0.tsv", header=TRUE, sep="\t")

# removing 2nd and 3rd columns
res <- res[,-2:-3]

for (col in colnames(res)[2:8]){
  res[col][res[col]!=""] = 1
  res[col][res[col]==""] = 0 
  res[col] = as.numeric(res[col][,1])
}


dev.new()
upset(res, nsets=7, 
      nintersects=70,
      empty.intersections="on", 
      order.by="freq")
