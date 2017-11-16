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

#words counter
futile.logger::flog.appender(futile.logger::appender.tee("RPolyedra.log"), name = "Rexpansive")
flog.threshold(DEBUG, name="Rexpansive")

names(tweets[[2]])
tweets[[2]]$text
tweets[[2]]$user$screen_name

paste(lapply(tweets[[2]],FUN=function(x){print(length(grep("\\@",x)))
                                    if (length(grep("\\@",x))>0) x else ""}),collapse=",")

#show metadata from extracted tweets
word.counter<-c(ApplyFileLineFromJSON.class$new(),
                ApplyFileLineFieldsExtractor.class$new(c("text","timestamp_ms","id_str","user$screen_name"))
                )
tweets<-applyLinesFile(cf.path,max.lines=1000,word.counter)
fromJSON(linn)$text

word.counter<-list(ApplyFileLineFromJSON.class$new(),
                    ApplyFileLineFieldsExtractor.class$new("text"),
                    ApplyFileLineSplitter.class$new(),
                    ApplyFileLineWordCounter.class$new())
tweets<-applyLinesFile(cf.path,max.lines=10000,apply=word.counter)
word.counter[[3]]
word.counter[[4]]$words
words.count.df<-as.data.frame.dictionary(dictionary = word.counter[[4]]$words)
head(words.count.df,n=20)
words.count.df.final<-unique(words.count.df[,c("word.final","count.final")])
head(words.count.df.final,n=30)
nrow(words.count.df.final)
total.count<-sum(words.count.df.final$count.final)
words.count.df.final$freq<-words.count.df.final$count.final/total.count
nrow(words.count.df.final)
words.count.df.final<-words.count.df.final[words.count.df.final$freq>0.00001,]
nrow(words.count.df.final)
tail(words.count.df.final,n=50)

plot(log(words.count.df.final$count.final))



#scrape of all tweets and saves words dictionary
#
#
#

word.counter<-list(ApplyFileLineFromJSON.class$new(),
                   ApplyFileLineFieldsExtractor.class$new("text"),
                   ApplyFileLineSplitter.class$new(),
                   ApplyFileLineWordCounter.class$new())

files.tweets<-dir(maldonado.tweets.dir)
files.tweets<-files.tweets[grep("json",files.tweets)]
for (cf in files.tweets){
  #cf<-files.tweets[2]
  cf.path<-paste(maldonado.tweets.dir,cf,sep="")
  #debug
  print(paste("processing file ",cf))
#  tweets<-applyLinesFile(cf.path,max.lines=1500,apply=word.counter)
  tweets<-applyLinesFile(cf.path,max.lines=0,apply=word.counter)
}
words.count.df<-as.data.frame.dictionary(dictionary = word.counter[[4]]$words)
head(words.count.df,n=20)
words.count.df.final<-unique(words.count.df[,c("word.final","count.final")])
head(words.count.df.final,n=30)
nrow(words.count.df.final)
total.count<-sum(words.count.df.final$count.final)
words.count.df.final$freq<-words.count.df.final$count.final/total.count
nrow(words.count.df.final)
words.count.df.final<-words.count.df.final[words.count.df.final$freq>0.00001,]
nrow(words.count.df.final)
tail(words.count.df.final,n=50)
head(words.count.df.final,n=50)

