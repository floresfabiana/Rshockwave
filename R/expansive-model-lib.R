library("R6")

applyLinesFile<-function(filename,max.lines=0,apply){
  meta.applier<-ApplyFileMetaApplier.class$new(apply)
  meta.applier$init()
  con <- file(filename,open="r")
  line <- readLines(con,n=max.lines)
  long <- length(line)
  for (i in 1:long){
    linn<-readLines(con,1)
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

ApplyFileLineFromJSON.class<-R6Class("ApplyFileLine",
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

ApplyFileMetaApplier.class<-R6Class("ApplyFileMetaApplier",
    inherit=ApplyFileLine.class,
    public = list(
      appliers=NA,
      initialize = function(appliers){
        self$appliers<-appliers
      },
      applyAll=function(function2apply){
        for (apply in self$appliers){
          ret<-function2apply(apply,ret)
        }
        ret
      },
      init=function(){
        self$applyAll(function(x,y){x$init()})
      },
      apply=function(val){
        y<-val
        self$applyAll(function(x,y){y<-x$apply(y)})
      },
      ret=function(){
        self$appliers[[length(self$appliers)]]$ret()
      }))


ApplyFileLineSplitter.class<-R6Class("ApplyFileLineSplitter",
  inherit=ApplyFileLine.class,
  public = list(
    initialize = function(){
    },
    init=function(){
    },
    apply=function(val){
      val<-tolower(val)
      strsplit(val,split = " ")[[1]]
    },
    ret=function(){
      NULL
    }))

ApplyFileLineWordCounter.class<-R6Class("ApplyFileLineWordCounter",
 inherit=ApplyFileLine.class,
 public = list(
   words=NA,
   initialize = function(){
   },
   init=function(){
     self$words<-list()
   },
   apply=function(val){
     for (word in val){
       if (!word %in% names(self$words)){
         self$words[[word]]<-0
       }
       self$words[[word]]<-self$words[[word]]+1
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

