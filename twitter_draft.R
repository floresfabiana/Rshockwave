library(rjson)
library(BreakoutDetection)

maldonado.tweets.dir<-"/usr/local/maldonado/"
files.tweets<-dir(maldonado.tweets.dir)
cf<-files.tweets[2]
cf.path<-paste(maldonado.tweets.dir,cf,sep="")
tweets<-getTweets(cf.path,max.lines = 1000)
applier<-ApplyFileLineFromJSON.class$new()
tweets<-applyLinesFile(cf.path,max.lines=1000,applier)
applier$parsed
length(tweets)
names(tweets[[1]])
tweets[[1]]$possibly_sensitive

tweet<-tweets[[1]]

keywords.ca<-c("accidente","ahogado")
keywords.uc<-c("desparecido","desaparicion","gendarmeria")

ThematicUnitStrategyText.class$new("accidente")

#test strategy
strategy.text<-ThematicUnitStrategyText.class$new("maldonado")
tweets[[3]]$id_str
names(tweets[[3]])
strategy.text$isComplaint(tweets[[3]])

#test splitter
splitter<-ApplyFileLineSplitter.class$new()
splitter$init()
val<-tweets[[1]]$text
splitted<-splitter$apply(tweets[[1]]$text)
splitted

#test word counter
word.counter<-ApplyFileLineWordCounter.class$new()
word.counter$init()
word.counter$apply(splitted)
self<-word.counter
word.counter$ret()

#words frecuency
futile.logger::flog.appender(futile.logger::appender.tee("RPolyedra.log"), name = "Rexpansive")
flog.threshold(DEBUG, name="Rexpansive")

names(tweets[[2]])
tweets[[2]]$text
tweets[[2]]$user$screen_name

paste(lapply(tweets[[2]],FUN=function(x){print(length(grep("\\@",x)))
                                    if (length(grep("\\@",x))>0) x else ""}),collapse=",")

word.counter<-c(ApplyFileLineFromJSON.class$new(),
                ApplyFileLineFieldsExtractor.class$new(c("text","timestamp_ms","id_str","user$screen_name")),
                ApplyFileLineSplitter.class$new(),
                ApplyFileLineWordFrequency.class$new())
tweets<-applyLinesFile(cf.path,max.lines=1000,word.counter)
fromJSON(linn)$text


