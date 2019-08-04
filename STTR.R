##########################################################################
##                                                                      ##
##    LOS TEXTOS POÉTICOS DE FERNANDO DE HERRERA.                       ##
##    APROXIMACIONES DESDE LA ESTILÍSTICA DE CORPUS Y LA ESTILOMETRÍA   ##
##    Tesis doctoral escrita por Laura Hernández Lorenzo                ##
##    Capítulo 6. Análisis con métodos de corpus y computacionales      ##
##    Apartado 6.2.1. Densidad léxica                                   ##
##    Script 1 de la Tesis                                              ##
##                                                                      ##
##########################################################################

# Creamos la ruta al directorio de trabajo donde está el corpus:
setwd("Dropbox/Tesis1/Corpus_Herrera/definitivo/Anexo_3")

# Introducimos en R el archivo del que queremos calcular la STTR:
text = scan("Pacheco_poemasmod2.txt", what="character", sep="\n")

# Convertimos a minúsculas:
Textminus <- tolower(text)

# Creamos una lista con las palabras del texto:
Textpalabras <- strsplit(Textminus, "\\W")

# Deshacemos la lista:
Textpalabrasdef <- unlist(Textpalabras)

# Eliminamos los huecos entre palabras:
no.huecos.v <- which(Textpalabrasdef!="")
Textpalabrasdef <- Textpalabrasdef[no.huecos.v]

Textpalabrasdef
totalwords= length(Textpalabrasdef)

# Establecemos la longitud que tendrán los segmentos a testar:
segmentLength = 2000

# Establecemos el número de segmentos en los que queda dividido el texto:
segmentNumber=as.integer(totalwords/segmentLength)

totalUnique= length(unique(Textpalabrasdef))


segments = matrix(list(), nrow=segmentNumber,ncol= segmentLength)
chunks=vector(mode="character",length=segmentNumber)

# Creamos el primer bucle:
for (i in 1:segmentNumber){
  j=1
  while(j<segmentLength+1){
   segments[[i,j]] = c(segments[[i,j]],Textpalabrasdef[(i-1)*segmentLength+j])
    j=j+1
  }
}

# Creamos una función para calcular la TTR:
TTR <- function(textArg) 100*length(unique(textArg))/length(textArg)

# Creamos el segundo bucle:
TTRs=vector(mode="numeric",length=segmentNumber)
for(i in 1:segmentNumber){
  TTRs[i]=TTR(unlist(segments[i,]))
  meanTTR=mean(TTRs)
}

# Obtenemos la medida de las TTR por fragmentos del texto, esto es, la STTR:
meanTTR
