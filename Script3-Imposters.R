##########################################################################
##                                                                      ##
##    LOS TEXTOS POÉTICOS DE FERNANDO DE HERRERA.                       ##
##    APROXIMACIONES DESDE LA ESTILÍSTICA DE CORPUS Y LA ESTILOMETRÍA   ##
##    Tesis doctoral escrita por Laura Hernández Lorenzo                ##
##    Capítulo 7. Análisis con métodos estilométricos                   ##
##    Apartado 7.7.Verificación de autoría: impostores                 ##
##    Script 3 de la Tesis                                              ##
##                                                                      ##
##########################################################################


library(stylo)   # version >= 0.6.7


## Para el corpus de sonetos de poesía áurea
# cargo los ficheros en R desde una carpeta
tokenized.texts = load.corpus.and.parse(files = "all", corpus.dir="Desktop/corpus")

# creo una lista de las palabras más frecuentes (limitada a 5000, por ejemplo, pero lo puedo cambiar según el análisis):
features = make.frequency.list(tokenized.texts, head = 5000)

# creo una tabla de frecuencias relativas:
data = make.table.of.frequencies(tokenized.texts, features, relative = TRUE)


# Aplico Impostores: ¿quién es AN? (usando Delta clásica como medida de distancia)
results = imposters(reference.set = data[-c(3),], test = data[3,])
write.csv(cbind(results, names(results)), file = "my_resultsANDeltadef3000.csv")



# Aplico Impostores: ¿quién es AN? (usando la Delta de Eder como medida de distancia)
results = imposters(reference.set = data[-c(3),], test = data[3,], distance = "eder")
write.csv(cbind(results, names(results)), file = "my_resultsANEder4000.csv")


# Aplico Impostores: ¿quién es AN? (usando Cosine Delta como medida de distancia)
results = imposters(reference.set = data[-c(3),], test = data[3,], distance = "wurzburg")
write.csv(cbind(results, names(results)), file = "my_resultsANWurzburgdef5000.csv")


# Para obtener los valores que marcan los límites de la zona gris
imposters.optimize(data)
