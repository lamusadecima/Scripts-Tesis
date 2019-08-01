setwd("Dropbox/Tesis1/Corpus_Herrera/definitivo/Anexo_3")

text = scan("Pacheco_poemasmod2.txt", what="character", sep="\n")

Textminus <- tolower(text)
Textpalabras <- strsplit(Textminus, "\\W")
Textpalabrasdef <- unlist(Textpalabras)
no.huecos.v <- which(Textpalabrasdef!="")
Textpalabrasdef <- Textpalabrasdef[no.huecos.v]

Textpalabrasdef
totalwords= length(Textpalabrasdef)

segmentLength = 2000
segmentNumber=as.integer(totalwords/segmentLength)

totalUnique= length(unique(Textpalabrasdef))


segments = matrix(list(), nrow=segmentNumber,ncol= segmentLength)
chunks=vector(mode="character",length=segmentNumber)
for (i in 1:segmentNumber){
  j=1
  while(j<segmentLength+1){
    #print(segments[i])
    segments[[i,j]] = c(segments[[i,j]],Textpalabrasdef[(i-1)*segmentLength+j])
    j=j+1
  }
}

TTR <- function(textArg) 100*length(unique(textArg))/length(textArg)

TTRs=vector(mode="numeric",length=segmentNumber)
for(i in 1:segmentNumber){
  TTRs[i]=TTR(unlist(segments[i,]))
  meanTTR=mean(TTRs)
}
meanTTR
