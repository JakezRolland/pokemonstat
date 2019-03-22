
#' addShinyColumns
#'
#' @param data history of catches
#' @param pokedex pokedex
#'
#'
#' @export
#'
#'
addShinyColumns<-function(data,pokedex){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    data$shiny<-NULL
  shiny = numeric(dim(data)[1])
    for (row in 1:dim(data)[1]){
      pokemon = data[row,];
      nr = pokemon$Nr;
      pokemonpokedex = pokedex[match(nr,pokedex$No),]
      date = pokemon$scan.day
      if(as.character(pokemonpokedex$Start)!=""){
        isshiny = (as.Date(pokemonpokedex$Start)<=date)&pokemonpokedex$Shiny
      }
      else {
        isshiny = pokemonpokedex$Shiny
      }

      shiny[row]=as.numeric(isshiny)
    }
  data$shiny = shiny
  return(data)
  }, error = function(err) onError(err,functionName,step ))
}


#' addCatchDuringCDColumn
#'
#' @param data history of catches
#' @param pokedex pokedex
#'
#'
#' @export
#'
#'
addCatchDuringCDColumn<-function(data,pokedex){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    data$shiny<-NULL
    duringCd = numeric(dim(data)[1])
    for (row in 1:dim(data)[1]){
      pokemon = data[row,];
      nr = pokemon$Nr;
      pokemonpokedex = pokedex[match(nr,pokedex$No),]
      date = pokemon$scan.day
      if(as.character(pokemonpokedex$cd)!=""){
        duringcd = (as.Date(pokemonpokedex$cd)==date)
      }
      else {
        duringcd = FALSE
      }

      duringCd[row]=as.numeric(duringcd)
    }
    data$duringCd = duringCd
    return(data)
  }, error = function(err) onError(err,functionName,step ))
}
#' countShinyByDay
#'
#' @param data history of catch
#' @param day day to count shinys
#'
#'
#' @export
#'
#'
 countShinyByDay<-function(data,day){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  dataDay = data[data$scan.day==day,];
  sums = sum(dataDay$shiny,na.rm = TRUE);
  return(sums);
  }, error = function(err) onError(err,functionName,step ))
 }

 #' countFoundShinyByDay
 #'
 #' @param data history of catch
 #' @param day day to count shinys
 #'
 #'
 #' @export
 #'
 #'
 countFoundShinyByDay<-function(data,day){
   functionName<-match.call()[[1]]
   step<-"Start"
   tryCatch({
     dataDay = data[data$scan.day==day,];
     sums = sum(dataDay$isShiny,na.rm = TRUE);
     return(sums);
   }, error = function(err) onError(err,functionName,step ))
 }

#' countCapturedByDay
#'
#' @param data history of catch
#' @param day Day
#'
#'
#' @export
#'
#'
countCapturedByDay<-function(data,day){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  dataDay = data[data$scan.day==day,];
  sums = dim(dataDay)[1];
  return(sums);
  }, error = function(err) onError(err,functionName,step ))
}

#' countCapturedPokemonByDay
#'
#' @param data history of catch
#' @param day Day
#' @param pokemonNr Pokemon Nr
#'
#'
#' @export
#'
#'
countCapturedPokemonByDay<-function(data,day,pokemonNr){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  dataDay = data[data$scan.day==day,];
  dataDay = dataDay[dataDay$Nr==pokemonNr,];
  sums = dim(dataDay)[1];
  return(sums);
  }, error = function(err) onError(err,functionName,step ))
}


#' summaryByDay
#'
#' @param data data to summary by day
#'
#'
#' @export
#'
#'
summaryByDay<-function(data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  minDay = min(data$scan.day);
  maxDay = max(data$scan.day)
  days =  seq(as.Date(minDay), as.Date(maxDay), by="days")
  sumShiny = numeric(length(days));
  ratioShiny = numeric(length(days));
  shinyFound = numeric(length(days));
  nshinyOdd = numeric(length(days));
  captured = numeric(length(days));
  ratio = numeric(length(days));
  shinyodds = numeric(length(days));
  for (i in 1:length(days)){
    sumShiny[i] = countShinyByDay(data,days[i]);
    captured[i] = countCapturedByDay(data,days[i]);
    ratio[i] = round(sumShiny[i]*100/captured[i],2)
    shinyodds[i] = shinyOdd(sumShiny[i]);
    shinyFound[i] = countFoundShinyByDay(data,days[i]);
    nshinyOdd[i] = NshinyOdd(sumShiny[i],shinyFound[i])
  }

  return(data.frame(day=days,captured = captured,nShiny=sumShiny,ratioshiny = ratio,shinyOdd = shinyodds,shinyFound = shinyFound,nshinyOdd=nshinyOdd))
  }, error = function(err) onError(err,functionName,step ))
}

#' summaryPokemonByDay
#'
#' @param data history of catch
#' @param pokemonNr number of pokemon
#' @export
summaryPokemonByDay<-function(data,pokemonNr){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  minDay = min(data$scan.day);
  maxDay = max(data$scan.day)
  days =  seq(as.Date(minDay), as.Date(maxDay), by="days")
  sumShiny = numeric(length(days));
  ratioShiny = numeric(length(days));
  captured = numeric(length(days));
  ratio = numeric(length(days));
  for (i in 1:length(days)){
    captured[i] = countCapturedPokemonByDay(data,days[i],pokemonNr);
    totalCaptured = countCapturedByDay(data,days[i])
    ratio[i] = round(captured[i]*100/totalCaptured,2)

  }

  return(data.frame(day=days,captured = captured,ratio = ratio))
  }, error = function(err) onError(err,functionName,step ))
}


#' plotCapturePokemonByDay
#'
#' @param data data to plot from
#' @param pokemonNr nr of the pokemon to plot
#' @param pokedex pokedex
#'
#'
#' @export
#' @import ggplot2
#'
plotCapturePokemonByDay<-function(data,pokemonNr,pokedex){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  datas = summaryPokemonByDay(data,pokemonNr);
  day = datas$day;
  captured = datas$captured
  p<- ggplot(data=datas, aes(x=day, y=captured)) +
    geom_bar(stat="identity")+ggtitle(pokedex$Name[pokemonNr])
  return(p)
  }, error = function(err) onError(err,functionName,step ))
}

#' cleanPokemonHistory
#'
#' @param data Data to clean

#' @export

cleanPokemonHistory<-function(data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({

  pokedex = getPokedex();
  data$DPS<-NULL
  data=unique(data)
  data = data[order(data$Level),]


  data$isShinyCD<-rep(0,dim(data)[1])
  data$isShiny<-rep(0,dim(data)[1])

  pokemonlist=data[1,];
  # colnames(data)<-c("Ancestor","Scan.date","Nr","Name","Nickname","Gender" ,
  #                   "Level"      ,       "possibleLevels"  ,  "CP"            ,    "HP"    ,            "Dust.cost"     ,    "Overall.appraisal"
  #                   ,    "ATT.max."   ,       "DEF.max."     ,     "HP.max."     ,      "Stats.appraisal"  , "min.IV."       ,    "mean.IV.",
  #                   "max.IV."    ,       "mean.ATT.IV" ,  "mean.DEF.IV",   "mean.HP.IV" ,   "Unique."     ,      "Fast.move"
  #                   , "Special.move",          "Sword"   ,          "Shield"   ,         "Eye"    ,           "Star"
  #                   ,  "Custom1"       ,    "Custom2"      ,     "Saved"        ,     "Form"        ,      "Egg"        ,       "Lucky"
  # )
  data=addShinyColumns(data,pokedex)
  data=addCatchDuringCDColumn(data,pokedex)
  #transforme les dates en string
  data$Scan.date<-as.character(data$Scan.date)
 # data$mean.IV.<-as.numeric(as.character(data$mean.IV.))
  #data$mean.ATT.IV<-as.numeric(as.character(data$mean.ATT.IV))
  data$CP<-as.numeric(as.character(data$CP))
  #data$mean.DEF.IV<-as.numeric(as.character(data$mean.DEF.IV))

  #data$mean.HP.IV<-as.numeric(as.character(data$mean.HP.IV))
  data$Level<-as.numeric(as.character(data$Level))

  data$scan.day = sapply(data$Scan.date,function(x){
    return = strsplit(x,split=' ')[[1]][1]
    return = as.Date(return,"%m/%d/%y")

    return(return)}
    ,USE.NAMES = FALSE)

  data$scan.day = as.Date(data$scan.day,origin="1970-01-01")
  data$scan.hour = sapply(data$Scan.date,function(x){
    return(strsplit(x,split=' ')[[1]][2])}
    ,USE.NAMES = FALSE)


  return(data);
  }, error = function(err) onError(err,functionName,step ))
}


#' updateDatas
#' @export

updateDatas<-function(){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({

  if(file.exists("pokemonHistory.rds")){
    data = readRDS("pokemonHistory.rds")
  }
  else {
    data<-data.frame();
  }
  nameFilesLoaded = "filesLoaded.rds"
  nameFileHistory="pokemonHistory.rds"
  folder <- paste(getwd(),"/pokemonhistory",sep="");
  if(file.exists("filesLoaded.rds")){
    filesLoaded = readRDS("filesLoaded.rds")
  }
  else{
    filesLoaded=c();
  }

  filesList = dir(folder)
  newFiles = setdiff(filesList,filesLoaded);

  newdata=data.frame();
  for (file in newFiles){
    print(file)
    folder_file=file;
    if(folder!=""){
      folder_file <- paste(folder,"/",file,sep="")
    }
    file_info = file.info(folder_file);
    if(file_info$size==0){
      filesLoaded = c(filesLoaded,file);
      next;
    }
    filesLoaded = c(filesLoaded,file);
    data_file <- read.csv2(folder_file, header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE,encoding="UTF-8")
    print(dim(data_file))
    data_file$Special.move.2<-NULL
    data_file = cleanPokemonHistory(data_file)
    newdata<-rbind(newdata,data_file)

  }
  if(dim(newdata)[1]>0){
    data = rbind(data,newdata)
  }

  data=unique(data)
  saveRDS(data,file=nameFileHistory)
  saveRDS(filesLoaded,file=nameFilesLoaded)

  return(data)
  }, error = function(err) onError(err,functionName,step ))
}


#' getPokedex
#' @export
#' @import utils
getPokedex <- function(){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  #pokedex <- read.csv2("~/Documents/pokedex.csv", header = TRUE, sep = ";", quote = "\"",
  # dec = ".", fill = TRUE,encoding="UTF-8")
  pokedex <- read.csv2("pokedex.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE,encoding="UTF-8")

  return(pokedex);

  }, error = function(err) onError(err,functionName,step ))
}

#' getCDInfos
#' @export
#' @import utils
getCDInfos <- function(){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    cdinfo <- read.csv2("community_day_info.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE,encoding="UTF-8")

    return(cdinfo);

  }, error = function(err) onError(err,functionName,step ))
}






#' getCaptureByDay
#'
#' @param day Day 2019-12-31
#' @param data All history of pokemon
#' @export
getCaptureByDay<-function(day,data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
  data_day<-data[data$scan.day==day,]
  data_day_shiny = data_day[data_day$shiny==1,]
  captures = as.data.frame(table(data_day$Name))
  captures = captures[captures$Freq>0,];
  captures = captures[order(captures$Freq,decreasing = TRUE),]

  captureShiny = as.data.frame(table(data_day_shiny$Name))

  captureShiny = captureShiny[captureShiny$Freq>0,];
  captureShiny = captureShiny[order(captureShiny$Freq,decreasing = TRUE),]

  total = sum(captures$Freq);
  shiny = sum(captureShiny$Freq)
  colnames(captures)<-c("Pokemon","Catched")
  return(list(captures=captures,total=total,shiny=shiny,ratio=shiny/total))
  }, error = function(err) onError(err,functionName,step ))
}



#' shinyOdd
#'
#' @param nPotentialShinyCatched  number of shiny captured
#' @import stats
#' @export

shinyOdd<-function(nPotentialShinyCatched){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    if(is.nan(nPotentialShinyCatched) | is.na(nPotentialShinyCatched) | nPotentialShinyCatched == 0){
      print("0 shiny")
      return(0)
    }

  return(round(sum(dbinom(x=1:nPotentialShinyCatched,size=nPotentialShinyCatched,prob=1/450))*100,2))
  }, error = function(err) onError(err,functionName,step ))
}

#' NshinyOdd
#'
#' @param nshiny number of shiny catched
#' @param nPotentialShinyCatched  number of shiny captured
#'
#' @import stats
#' @export

NshinyOdd<-function(nPotentialShinyCatched,nshiny){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    if(is.nan(nPotentialShinyCatched) | is.na(nPotentialShinyCatched) | nPotentialShinyCatched == 0){
      print("0 shiny")
      return(0)
    }
    if(is.nan(nshiny) | is.na(nshiny) | nshiny == 0){
      print("0 shiny")
      return(0)
    }

    return(round(sum(dbinom(x=nshiny:nPotentialShinyCatched,size=nPotentialShinyCatched,prob=1/450))*100,2))
  }, error = function(err) onError(err,functionName,step ))
}

#' setIsShiny
#'
#' @param rowNumber row to change
#' @param data history of capture
#'
#' @param value value to put
#'
#' @export
#'
#'
setIsShiny<-function(rowNumber,data,value){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    data[rowNumber,'isShiny'] <- value
    saveHistory(data)
}, error = function(err) onError(err,functionName,step ))
}


#' setIsShinyCD
#'
#' @param rowNumber row to change
#' @param data history of capture
#'
#' @param value value to put
#'
#' @export
#'
#'
setIsShinyCD<-function(rowNumber,data,value){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    data[rowNumber,'isShinyCD'] <- value
    saveHistory(data)
  }, error = function(err) onError(err,functionName,step ))
}

#' saveHistory
#'
#' @param data history to save
#'
#' @export
#'
#'
saveHistory<-function(data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
   saveRDS(data);
  }, error = function(err) onError(err,functionName,step ))
}



#' getTotalShinyProbaOutCD
#'
#' @param data history of catch
#'
#'
#' @export
#'
#'
getTotalShinyProbaOutCD<-function(data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    notshinyPokemons = getPokemonCatchOutCD(data)
    nshiny = sum(notshinyPokemons$isShiny);
    nshinyPossible = sum(notshinyPokemons$shiny);
  return(nshinyPossible/nshiny)

  }, error = function(err) onError(err,functionName,step ))
}

#' getTotalShinyProbaDuringCD
#'
#' @param data history of catch
#'
#'
#' @export
#'
#'
getTotalShinyProbaDuringCD<-function(data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({
    notshinyPokemons = getPokemonCatchDuringCD(data)
    nshiny = sum(notshinyPokemons$isShiny);
    nshinyPossible = sum(notshinyPokemons$shiny);
    return(nshinyPossible/nshiny)

  }, error = function(err) onError(err,functionName,step ))
}

#' getPokemonCatchOutCD
#'
#' @param data of catch
#'
#'
#' @export
#'
#'
getPokemonCatchOutCD <- function(data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({

    return(data[data$duringCd==0,])

  }, error = function(err) onError(err,functionName,step ))
}


#' getPokemonCatchDuringCD
#'
#' @param data of catch
#'
#'
#' @export
#'
#'
getPokemonCatchDuringCD <- function(data){
  functionName<-match.call()[[1]]
  step<-"Start"
  tryCatch({

    return(data[data$duringCd==1,])

  }, error = function(err) onError(err,functionName,step ))
}
