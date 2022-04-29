rm(list = ls())

library(openxlsx)
library(zeallot)

### Select directory to save files ###
diretorio <- choose.dir(default = "", caption = "Select folder")
setwd(diretorio)

### Indicates city name that will calculate water consumption ###
name_city <- "sume"

### Select xlsx file where contains three sheets "stretchs_nodes",  "comsumption_by_area", "node_area" ###
file_name <- choose.files(default = "", caption = "Select files",
                          multi = FALSE, filters = Filters,
                          index = nrow(Filters)) 
data <- read.xlsx(file_name,sheet = "stretchs_nodes")
consumo <- read.xlsx(file_name, sheet = "comsumption_by_area")
tipo_no <- read.xlsx(file_name, sheet = "node_area")
data[,3]<- as.numeric(data[,3])

### Creates function that calculates the stretch length to each node ###
comp_no <- function(data) {
  ## Each node will receive half of a stretch between two nodes ##
  v <- vector()
  nos <- vector()
  t <- vector()
  trechos <- vector()
  for(i in 1:length(data[,1])){
    v <- as.vector(c(data[i,1], data[i,2]))
    nos <- as.vector(c(nos, v))
    t <- as.vector(rep((data[i,3]/2), 2))
    trechos <- as.vector(c(trechos, t))
  }
  print(nos)
  print(trechos)
  data_new <- as.data.frame(cbind(nos, trechos))
  
  id_nos <- unique(data_new[,1])
  data_new$trechos <- as.numeric(data_new$trechos)
  comp <- vector()
  comp_nos <- vector()
  for(x in 1:length(id_nos)){
    idx <- vector()
    idx <- which(data_new[,1]==id_nos[x])
    comp <-c (id_nos[x], sum(data_new[idx,2]))
    comp_nos<- as.data.frame(rbind(comp_nos, comp))
  }
  comp_nos$V2 <- as.numeric(comp_nos$V2)
  colnames(comp_nos) <- c("node", "stretch_lengths")
  return(comp_nos)
}
comp_nos <- comp_no(data)

### Sort table of stretch nodes length and  node area type to the nodes have the same order ###
comp_nos<- comp_nos[order(comp_nos[,1], decreasing = FALSE),]
tipo_nos <- tipo_no[order(tipo_no[,1], decreasing = FALSE),]


### Creates function that calculates the node consumption due to consumption by city area ###
## Input stretch length to each node, node area type and consumption by area ##
consumo_no <- function(comp_nos, tipo_no, consumo) {
  tipos <- unique(tipo_no[,2])
  a<-vector()
  coef_area <- vector()
  for(i in 1:length(tipos)){
    idx <- which(tipo_no[,2]==tipos[i])
    comp_rede <- sum(comp_nos[idx,2])
    a<- c(a, comp_rede)
    print(sum(a))
    cons <- consumo[which(consumo[,1]==tipos[i]),2]
    c <- cons/comp_rede
    comp_nos[idx,2] <- comp_nos[idx,2]*c
    coef_area <- c(coef_area, c)
    print(coef_area)  
  }
  cons_nos <- comp_nos
  table_coef <- as.data.frame(cbind(tipos, as.numeric(coef_area)))
  colnames(cons_nos) <- c("No", "Consumo (l/s)") 
  colnames(table_coef) <- c("Area", "Consumo (l/s/m)") 
  return(list(table_coef, cons_nos))
}

c(table_coef, cons_nos) %<-%  consumo_no(comp_nos, tipo_nos, consumo)

### Write xlsx file with three sheets node_consumption(water consumption by node), 
### stretch_length_node (stretch length to each node) and consumption_coefficient_area (stretch area water consumption by meter)

output <- paste0("node_consumption", name_city,".xlsx")

wb = createWorkbook()

sheet = addWorksheet(wb, "Consumo_no")

writeData(wb, cons_nos, sheet=sheet)

sheet = addWorksheet(wb, "stretch_lengths_nodes")

writeData(wb, comp_nos, sheet=sheet)

sheet = addWorksheet(wb, "consumption_coefficient_area")

writeData(wb, table_coef, sheet=sheet)

saveWorkbook(wb, output, overwrite = TRUE)

