#ruta para guardar programa en el computador
setwd(path.expand("~/Drive/2017_EnglishClass/R-program-vocabulary"))
#directorio de trabajo
getwd()
#borrar todas las variables en memoria
rm(list = ls())
#librerias para extraer datos desde la web, para interactuar con la web
library(httr)
library(xml2)
library(rvest)

#dirección https de la cual deseamos inforamción:
pageV <- "https://toefl.magoosh.com/flashcards/vocabulary/decks"
#BROWSE(pageV)  #: solo abre la página

#tipo de palabras:
# 'Hard Words','Hard Words 2', 'Hard Words 3', 'Hard Words 4'
difficulty <- 'Hard Words 3' 
#información que se ve en la página
#en este caso deseamos extraer toda la lista de palabras
#con el título asignado a difficulty

#número de veces que se hace la petición a la página,
#para ir cambiando de palabra
num <- 100
#varable auxiliar del while
i <- 1

#crear la tabla donde se almacenan las palabras correspondientes a 
# la variable $difficulty
# nrow = num + as.integer(num/3), arbitrario
# simplemente se desea nrow > num
titles <- list(c(),c("#", "word", "definition", "example"))
filas <-  num + as.integer(num/3)
vocab <- matrix("", ncol=4, nrow = filas, byrow = TRUE) 
dimnames(vocab) <- titles

#se usa https://selectorgadget.com/ , 
#instalado en Chrome para obtener los css desde la página web

#inicio del while, 
while(i < filas){
  #palabra
  link_1 <- html_session(pageV) %>% follow_link(difficulty)
  temp1 <- session_history(link_1)$url 
  nodeID_1 <- html_nodes(read_html(temp1), '.flashcard-word')
  wordID <- html_text(nodeID_1, trim = T) #guardamos la palabra

  #definiciones
  link_2 <- html_session(temp1) %>% follow_link('Click to see definition and example')
  temp2 <- session_history(link_2)$url
  nodeID_2 <- html_nodes(read_html(temp2),'.flashcard-text p')
  defTexID <- html_text(nodeID_2, trim = T) #guardamos la definición

  #ejemplos
  nodeID_3 <- html_nodes(read_html(temp2),'.flashcard-example p')
  examTexID <- html_text(nodeID_3, trim = T) #guardamos el ejemplo
 
  #llenando la tabla
  data2 <- wordID[1]
  #caso de una sola definición
  if(length(defTexID) == 1){
    data3 <- defTexID[1] 
    data4 <- examTexID[1]
    vocab[i,] <- c(i,data2,data3,data4)
    i <- i + 1
  }
  #caso de varias definiciones
  if(length(defTexID) > 1){
    for(j in 1:length(defTexID)){
      data3 <- defTexID[j] 
      data4 <- examTexID[j]
      vocab[i,] <- c(i,data2,data3,data4)
      i <- i + 1
    }
  }
} 
#finalmente se extrae la tabla para ser manipulada en Excel.
#nombre de archivo = variable difficulty y extensión .csv
nomb_1<- c(difficulty,"csv")
nomb_2 <- paste(nomb_1,collapse = ".")
write.table(vocab, file= nomb_2, sep=",", row.names = FALSE)
#en el caso de Excel es importante que row.names = FALSE
#dado que el separador de columnas es "," 
#por defecto creará una columna con el nombre de la fila
