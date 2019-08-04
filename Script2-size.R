##########################################################################
##                                                                      ##
##    LOS TEXTOS POÉTICOS DE FERNANDO DE HERRERA.                       ##
##    APROXIMACIONES DESDE LA ESTILÍSTICA DE CORPUS Y LA ESTILOMETRÍA   ##
##    Tesis doctoral escrita por Laura Hernández Lorenzo                ##
##    Capítulo 7. Análisis con métodos estilométricos                   ##
##    Apartado 7.1. Métodos supervisados de atribución de autoría:      ##
##    cross-validation atendiendo al tamaño de las muestras             ##
##    Script 2 de la Tesis                                              ##
##                                                                      ##
##########################################################################

# Una vez que tengo una tabla csv con todos los resultados ponderados de los diferentes autores,
# introduzco la tabla en R:

tabla = read.csv("Size/Resultados.csv", header = FALSE)
tabla

# Creo dos variables para guardar, por un lado, los resultados (accuracy),
# y, por otro, el nº de palabras (words):
accuracy = tabla[,3]
accuracy
words = tabla[,4]
words
barplot(accuracy, words, plot = TRUE)
length(words)

# Escribo un bucle para obtener las medias de accuracy por intervalos:
final = c()
intervals = c(0, 1500, 3000, 4500, 6000, 9999999)
for(j in 2:length(intervals)) {
  R<- 0
  for(i in 1:length(words)) {
    if (words[i]>intervals[j-1] & words[i] < intervals[j])
    { R <- c(R, accuracy[i])
    }
  }
  final <- c(final, mean(R))
}
# Aquí termina el bucle

# Genero la figura 29:
barplot(final,  xlab = "Texts size (words)", ylab= "Accuracy", col = c("red", "blue"), ylim = c(0,1.0), names.arg = c("0-1500", "1500-3000", "3000-4500", "4500-6000", "6000-or more"))

