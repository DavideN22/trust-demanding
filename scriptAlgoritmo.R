library(igraph)
g = read.graph(file="C:\\Users\\david\\Desktop\\graph1.txt", format="gml")
coords1 = layout.fruchterman.reingold(g)
plot(g, layout=coords1, vertex.size=10)
structure_paths = all_simple_paths(g, 1,33)
library("readxl")
tupla = read_excel("C:\\Users\\david\\Desktop\\tuplaGrafo.xlsx")
pathList <- list()
paths <- list()

for(index in 1:length(structure_paths)){
    percorsi = unlist(structure_paths[index]) 
    pathList <<- c(pathList, list(percorsi)) 
}

for(index in 1:length(pathList)){
  p = strsplit(as.character(unlist(pathList[index])), " ") 
  paths <<- c(paths, list(p))  
}

#paths <- list(unlist(structure_paths[1]), unlist(structure_paths[2]), unlist(structure_paths[3]))
#path1 = unlist(paths[1])
#path2 = unlist(paths[2])
#path3= unlist(paths[3])
#p1 = strsplit(as.character(path1), " ") 
#p2 = strsplit(as.character(path2), " ") 
#p3 = strsplit(as.character(path3), " ") 
#paths = list(p1,p2,p3)
NodeTD <- c()
ObjectTD <- c()

checkPreviousNode <- function(objects, x,Pool, indexPreviusNode, indexCurrentNode) {
bool <- TRUE
objects = as.list(strsplit(as.character(objects), ";")[[1]])
for(i in length(objects):1){
    actionObject = as.list(strsplit(as.character(objects[i]), " ")[[1]])  
      if(actionObject[2] == "R" && unlist(actionObject[1]) == unlist(x) && Pool == tupla[as.numeric(indexPreviusNode),2] ){
          bool <- FALSE
          break
      }  
      if(actionObject[2] == "U" && unlist(actionObject[1]) == unlist(x) && Pool == tupla[as.numeric(indexPreviusNode),2]){
          bool <- FALSE
          break
      }  
      if(actionObject[2] == "U" && unlist(actionObject[1]) == unlist(x) && Pool != tupla[as.numeric(indexPreviusNode),2]){
          toAddP <- paste("Nodo ",indexPreviusNode, "-", "[", indexPreviusNode,"] ", tupla[unlist(indexPreviusNode),3], sep="")
          toaddC <- paste("Nodo ",indexCurrentNode, "-", "[", indexCurrentNode,"] ", tupla[unlist(indexCurrentNode),3], sep="")
          NodeTD <<- c(NodeTD, toAddP)
          NodeTD <<- c(NodeTD, toaddC)
          ObjectTD <<- c(ObjectTD, x)
          bool <- FALSE
          break
      } 
}
return <-bool
}



readFrom <- function(path) {
    for(i in length(path):1) {
     objects = tupla[i,4]
     objects = as.list(strsplit(as.character(tupla[as.numeric(unlist(path[i])),4]), ";")[[1]])
     mylist <- c()
     for(j in length(objects):1){
      actionObject = as.list(strsplit(as.character(objects[j]), " ")[[1]])
      if(actionObject[2] == "R"){
          continue = TRUE
          iPN <- path[i-1]
          iCN <- path[i]
          c <- i
          while (continue) {
            continue = checkPreviousNode(tupla[as.numeric(iPN),4],actionObject[1],tupla[as.numeric(path[i]),2], iPN,iCN)
            c <- c - 1
            if(c < 0){
              continue <- FALSE
            }
            iPN <- path[c]
            } 
      }
     }
}
}

checkNextNode <- function(end, objects, x, index, indexTrustNode) {
bool <- TRUE
objects = as.list(strsplit(as.character(objects), ";")[[1]])
for(i in length(objects):1){
    actionObject = as.list(strsplit(as.character(objects[i]), " ")[[1]])
      if((actionObject[2] == "U" && unlist(actionObject[1]) == unlist(x)) || (actionObject[2] == "R" && unlist(actionObject[1]) == unlist(x))){
          bool <- FALSE
          break
      }   
}
if (bool == TRUE && end){
    NodeTD <<- c(NodeTD, indexTrustNode)
    ObjectTD <<- c(ObjectTD, x)
}
return <- bool
}

finalUpdate <- function(path) {
  end <- FALSE
    for(i in 1:length(path)) {
     objects = as.list(strsplit(as.character(tupla[as.numeric(unlist(path[i])),4]), ";")[[1]])
     mylist <- c()
     for(j in 1:length(objects)){
      actionObject = as.list(strsplit(as.character(objects[j]), " ")[[1]])
      if(actionObject[2] == "U"){
          continue <- TRUE
          iPN <- path[i+1]
          c <- i
          while (continue) {
            if(c == length(path)){
              end <- TRUE
            }
            continue = checkNextNode(end,tupla[as.numeric(iPN),4],actionObject[1], iPN, path[i])
            c <- c + 1
            if(c > length(path)-1){
              continue <- FALSE
            }
            iPN <- path[c]
            } 
      }
     }
}
}

for(path in paths){
    readFrom(path)
    finalUpdate(path)
}

NodeTD <- NodeTD[!duplicated(NodeTD)]
ObjectTD <- ObjectTD[!duplicated(ObjectTD)]

print(NodeTD)

print(ObjectTD)
  


