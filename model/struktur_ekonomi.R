strukEko <- function(ValAdd, tax){
  jcol <- ncol(ValAdd)
  jrow <- nrow(ValAdd)
  ValAdd[,1]<-as.character(ValAdd[,1])
  dt <- rbind(ValAdd,tax)
  pdb <- c("Produk Domestik Bruto"," ",apply(dt[,3:jcol],2,sum))
  struktur <- dt[,1:2]
  for(i in 3:jcol){
    struktur <- cbind(struktur,dt[i]/as.numeric(pdb[i])*100)
  }
  ValAdd <- struktur[1:jrow,]
  Total_ValAdd <- c("Total Nilai Tambah"," ",apply(ValAdd[,3:jcol],2,sum))
  tax <- struktur[jrow+1,]
  pdb <- c("Produk Domestik Bruto"," ",apply(struktur[,3:jcol],2,sum))
  DF<-data.frame(rbind(ValAdd,Total_ValAdd,tax, pdb))
  return(DF)
  
}

graph_format_struktur<-function(ValAdd,tax,tAwal,tAkhir){
  jcol <- ncol(ValAdd)
  jrow <- nrow(ValAdd)
  dt <- rbind(ValAdd,tax)
  year <- rep(tAwal:tAkhir,each = jrow)
  print(year)
  print(length(year))
  sector <- rep(ValAdd[,1],times = jcol-2)
  sector <- as.character(sector)
  print(sector)
  print(length(sector))
  value <- c()
  for(i in 3:jcol) value<-c(value,ValAdd[,i])
  print(length(value))
  return(data.frame(sector,year,value))
}