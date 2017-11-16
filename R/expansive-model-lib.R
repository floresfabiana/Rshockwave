library("R6")
library("futile.logger")

removeAccents<-function(txt){
  #TODO stuff '
  ret<-iconv(txt, to='ASCII//TRANSLIT')
  ret<-gsub("'|\\~","",ret)
  ret
}

# ' Normalize String for simplifying interpretation
normalizeString<-function(txt){
  txt<<-txt
  txt <-iconv(txt, "latin1", "ASCII", sub="")
  removeAccents(trimws(tolower(txt)))
}
normalizeStringsFromDataFrame<-function(df){
  if (!is.data.frame(df)){
    df<-normalizeString(df)
  }
  else{
    for (field in names(df)){
      val <- df[,field]
      if(is.character(val)){
        df[,field] <- normalizeString(val)
      }
    }
  }
  df
}


applyLinesFile<-function(filename,max.lines=0,apply){
  timestamp.begin<-Sys.time()
  meta.applier<-ApplyFileMetaApplier.class$new(apply)
  meta.applier$init()
  con <- file(filename,open="r")
  line <- readLines(con,n=max.lines)
  secs<-difftime(Sys.time(),timestamp.begin,"secs")[[1]]
  print(paste("counted lines in file in ",round(secs)," secs",sep=""))
  timestamp.begin<-Sys.time()
  long <- length(line)
  for (i in 1:long){
    linn<-readLines(con,1)
    futile.logger::flog.debug(paste("executing line",i))
    #debug
    if ((i/max.lines*100)%%1==0){
      print(paste("executing line ",i,"/",long,": ",round(i/long*100,2),"%",sep=""))
      secs<-difftime(Sys.time(),timestamp.begin,units = "secs")[[1]]
      total.estimated.time<-secs*long/(i)
      print(paste("elapsed ",round(secs),"secs. Estimated ",round(total.estimated.time),"secs",sep=""))
    }
    #debug
    linn<<-linn

    meta.applier$apply(linn)
  }
  close(con)
  meta.applier$ret()
}

#' Apply File Line. For scrapping and on the fly executing and not using memory
ApplyFileLine.class<-R6Class("ApplyFileLine",
  public = list(
    initialize = function(){
    },
    init=function(){
      stop("Abstract class")
    },
    apply=function(linn,val){
      stop("Abstract class")
    },
    ret=function(){
      stop("Abstract class")
    }))

ApplyFileLineFromJSON.class<-R6Class("ApplyFileLineFromJSON",
 inherit=ApplyFileLine.class,
 public = list(
   parsed=NA,
   initialize = function(){
   },
   init=function(){
     self$parsed<-list()
   },
   apply=function(val){
     i<-length(self$parsed)+1
     self$parsed[[i]]<-fromJSON(val)
     self$parsed[[i]]
   },
   ret=function(){
     self$parsed
   }))

ApplyFileLineFieldsExtractor.class<-R6Class("ApplyFileLineFieldsExtractor",
   inherit=ApplyFileLine.class,
   public = list(
     fields = NA,
     prototype = NA,
     initialize = function(fields){
       self$fields<-fields
       proto.list<-list()
       for (field in fields){
         proto.list[[field]]<-""
       }
       self$prototype<-as.data.frame(proto.list)
     },
     init=function(){
     },
     apply=function(val){
       ret<-self$prototype
       for (field in self$fields){
        members<-strsplit(field,split ="\\$")[[1]]
        if (length(members)==1){
          ret[,field]<- val[[field]]
        }
        else{
          for (member in members){
            val <- val[[member]]
          }
          ret[,field]<- val
        }
       }
       # #debug
       # print(ret)

       ret
     },
     ret=function(){
     }))

ApplyFileMetaApplier.class<-R6Class("ApplyFileMetaApplier",
    inherit=ApplyFileLine.class,
    public = list(
      appliers=NA,
      initialize = function(appliers){
        self$appliers<-appliers
      },
      applyAll=function(function2apply,initial.val=NULL){
        ret<-initial.val
        if (!is.list(self$appliers)){
          ret<-function2apply(self$appliers,ret)
        }
        else{
          for (apply in self$appliers){
            ret<-function2apply(apply,ret)
          }
        }
        ret
      },
      init=function(){
        self$applyAll(function(x,y){x$init()})
      },
      apply=function(val){
        self$applyAll(function(x,y){y<-x$apply(y)},val)
      },
      ret=function(){
        if (!is.list(self$appliers)){
          ret <- self$appliers$ret()
        }
        else{
          ret <- self$appliers[[length(self$appliers)]]$ret()
        }
        ret
      }))


ApplyFileLineSplitter.class<-R6Class("ApplyFileLineSplitter",
  inherit=ApplyFileLine.class,
  public = list(
    col = NA,
    initialize = function(col=1){
      self$col <- col
    },
    init=function(){
    },
    apply=function(val){
      val<-normalizeStringsFromDataFrame(val)
      strsplit(val[,self$col],split = " ")[[1]]
    },
    ret=function(){
      NULL
    }))

ApplyFileLineWordCounter.class<-R6Class("ApplyFileLineWordCounter",
 inherit=ApplyFileLine.class,
 public = list(
   words=NULL,
   initialize = function(){
   },
   init=function(){
     if (is.null(self$words))
        self$words<-list()
   },
   apply=function(val){
     for (word in val){
       if (nchar(word)>0){
         if (!word %in% names(self$words)){
           self$words[[word]]<-0
         }
         self$words[[word]]<-self$words[[word]]+1
       }
     }
     val
   },
   ret=function(){
     self$words
   }))



#' TematicUnit
ThematicUnit.class<-R6Class("ThematicUnit",
  public = list(
    strategy=NA,
    initialize = function(strategy){
      self$strategy<-strategy
    },
    isComplaint=function(tweet){
      self$strategy$isComplaint(tweet)
    }
    ))


ThematicUnitStrategy.class<-R6Class("ThematicUnitStrategy",
  public = list(
    initialize = function(){
    },
    isComplaint=function(tweet){
      stop("Abstract class")
    }
  ))

ThematicUnitStrategyText.class<-R6Class("ThematicUnitStrategyText",
  inherit=ThematicUnitStrategy.class,
  public = list(
    text = NA,
    initialize = function(text){
      self$text<-text
    },
    isComplaint=function(tweet){
       length(grep(self$text,tweet$text,ignore.case = TRUE))>0
    }
  ))


as.data.frame.dictionary<-function(dictionary){
  ret<-data.frame(word=names(dictionary),
                  count=0,freq=0,word.corrected="",correction="",word.final="",
                  stringsAsFactors = FALSE)
  for (word in names(dictionary)){
    row<-which(ret$word==word)
    ret[row,c("count")]<-dictionary[[word]]
  }
  ret$freq<-ret$count/sum(ret$count)
  #TODO trivial corrections of :
  regexp.final.punct<-"^([[:alnum:]]+)[[:punct:]]+$"
  rows.final.punct<-grep(regexp.final.punct,ret$word)
  ret[,"correction"]<-ret[,"word"]
  ret[rows.final.punct,"correction"]<-sub(regexp.final.punct,"\\1",ret[rows.final.punct,"word"])
  #TODO dictionary corrections
  head(ret[which(ret$word.final==""),])

  #TODO transitive closure
  ret[,"word.final"]<-ret[,"correction"]
  #COUNT TOTAL
  ret.agg<-aggregate(ret[,"count"],by=list(word.final=ret$word.final),FUN=sum)
  names(ret.agg)[2]<-"count.final"
  #ret.agg<-ret.agg[order(ret.agg$count,decreasing = TRUE),]
  ret<-merge(ret,ret.agg,by=c("word.final"))
  ret<-ret[order(-ret$count.final,-ret$count,ret$word),]
  ret
}
