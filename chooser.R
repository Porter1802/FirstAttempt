ready.for.match<-function(participant.file=Participant.file){
dfeveryone<-read_excel(participant.file)
df<-subset(dfeveryone,Participating=="Yes")
Seniors<-subset(df,Senior=="Yes")
Juniors<-subset(df,Senior=="No")

sen<-nrow(Seniors)
jun<-nrow(Juniors)
difference<-sen-jun
to.move<-difference%/%2
need.a.double<-difference%%2==1



if(to.move>0){
movers<-sample(1:sen,to.move,replace = FALSE)
movers.data<-Seniors[movers,]
Juniors<-rbind(Juniors,movers.data)
Seniors<-Seniors[-movers,]
if(need.a.double==TRUE){
  lucky<-sample(1:nrow(Juniors),1)
  luckydetails<-Juniors[lucky,]
  Juniors<-rbind(Juniors,luckydetails)
  }
}

if(to.move<0){
  movers<-sample(1:jun,abs(to.move),replace = FALSE)
  movers.data<-Juniors[movers,]
  Seniors<-rbind(Seniors,movers.data)
  Juniors<-Juniors[-movers,]
  if(need.a.double==TRUE){
    lucky<-sample(1:nrow(Seniors),1)
    luckydetails<-Seniors[lucky,]
    Seniors<-rbind(Seniors,luckydetails)
  }
}
involved<-list(Seniors,Juniors)
return(involved)
}

make.matches.noprob<-function(Participants = involved){
Seniors<-Participants[[1]]
Juniors<-Participants[[2]]


matches<-sample(Seniors$Name,replace = FALSE)
matchee<-Juniors$Name
combined<-data.frame(matches,matchee)
reversed<-combined[,c(2,1)]
colnames(reversed)<-c("matches","matchee")
fullmatches<-rbind(combined,reversed)

colnames(fullmatches)<-c("Name","match")
fullmatches <- data.frame(lapply(fullmatches, as.character), stringsAsFactors=FALSE)
return(fullmatches)
}

initialise<-function(participant.file){
  library(readxl)
  library(dplyr)
  library(matchingR)
  dfeveryone<-read_excel(participant.file)
  df<-subset(dfeveryone,Participating=="Yes")


  parameters<-list(df,dfeveryone)
  return(parameters)
}

update.past <- function(Participants.data = dfeveryone, matches = fullmatches, Past.Matches = past){
week.no<-length(Past.Matches)+1
past[[week.no]]<-matches
return(past)
}

create.history <- function(Past.matches = past){
  Participants.data<-Past.matches[[length(Past.matches)]]
  History<-matrix(0L,nrow = nrow(Participants.data),ncol = nrow(Participants.data),dimnames = list(Participants.data$Name,Participants.data$Name))
  for(n in 1:length(past)){
  matches.for.period<-past[[n]]
  
      for (k in 1:nrow(matches.for.period)) {
      History[matches.for.period[[k,"Name"]],matches.for.period[[k,"match"]]]<-History[matches.for.period[[k,"Name"]],matches.for.period[[k,"match"]]]+1
      }
  }
return(History)  
}

make.matches.with.prob<-function(Participants = involved,Pref.Matrix = pref.matrix){
  Seniors<-Participants[[1]]
  Juniors<-Participants[[2]]

  Juniors.pref<-Pref.Matrix[Juniors$Name,Seniors$Name]
  Juniors.pref<-Juniors.pref[as.vector(randomise.orders(nrow(Juniors.pref))),]
  Seniors.pref<-Pref.Matrix[Seniors$Name,Juniors$Name]
  Seniors.pref<-Seniors.pref[as.vector(randomise.orders(nrow(Seniors.pref))),]
  matching <- galeShapley.marriageMarket(proposerUtils =  Juniors.pref, reviewerUtils =  Seniors.pref)
  juniors.index<-as.vector(matching$proposals)
  seniors.index<-as.vector(matching$engagements)
  
  Juniors.matches<-Seniors[juniors.index,1]
  Seniors.matches<-Juniors[seniors.index,1]
  Seniors<-bind_cols(Seniors,Seniors.matches)
  Juniors<-bind_cols(Juniors,Juniors.matches)
  
  Juniors.matched<-Juniors[,c(1,5)]
  Seniors.matched<-Seniors[,c(1,5)]
  
  colnames(Juniors.matched)<-c("Name","match")
  colnames(Seniors.matched)<-c("Name","match")
  
  fullmatches<-bind_rows(Juniors.matched,Seniors.matched)

  return(fullmatches)
}

make.pref.matrix<-function(history=History){
  prefs<-t(apply(history,1,function(x) replace(x,x==max(x),1000)))
  pref.matrix<-(prefs+1)^(-1)
  
  return(pref.matrix)
}

save.past<-function(data.Past=past){
    saveRDS(data.Past,file = "Past.RDS")
  }

load.past<-function(filename.Past = "Past.RDS"){
  past<-list()
  if(sum(grepl("Past.RDS",list.files()))==1){
    past<-readRDS(file = filename.Past)
  }
  return(past)
}

find.new<-function(history=History,listofparticipants=dfeveryone$Name){
  namesofparticipants<-listofparticipants
  previousnames<-rownames(history)
  new.participants<-namesofparticipants[which(!namesofparticipants %in% previousnames)]
  return(new.participants)
}

create.new<-function(new.parts=new.participants,Pref.Matrix=pref.matrix){
  no.new<-length(new.parts)
  if(no.new==0){
    return(Pref.Matrix)
  }else{
    
    current.rows<-nrow(Pref.Matrix)
    current.cols<-ncol(Pref.Matrix)
    new.rows<-matrix(1,nrow = no.new,ncol = current.cols,dimnames = list(new.parts,colnames(Pref.Matrix)))
    
    added.rows<-rbind(Pref.Matrix,new.rows)
    new.cols<-matrix(1,nrow = current.rows+no.new,ncol = no.new,dimnames = list(row.names(added.rows),new.parts))
    added.cols<-cbind(added.rows,new.cols)
    return(added.cols)
  }
}

randomise.orders<-function(size){
  new.order<-sample(1:size,size,replace = FALSE)
  return(new.order)
}

add.random.to.pref<-function(Pref.Matrix=pref.matrix.with.new){
  area<-runif(50,0.75,1)
  prefs<-t(apply(Pref.Matrix,1,function(x) replace(x,x==1,sample(area,1,replace = TRUE))))
  return(prefs)
}
update.past.excel<-function(This.Week.Matches=fullmatches,filename="History.xlsx",Past=past){
  library(xlsx)
  week.num<-length(Past)
  sheetname.for.history.file<-paste("Week",week.num,sep = "-")
  write.xlsx(This.Week.Matches,filename,sheetName = sheetname.for.history.file,append = TRUE)
  
}
teamlist<-function(dfeveryone=dfeveryone){
  team.list<-dfeveryone[,c("Name","Team")]
  return(team.list)
}
modify.pref.for.team<-function(teams=team.list,Prefs=pref.matrix.with.new){
  teampref<-matrix(nrow = nrow(Prefs),ncol = ncol(Prefs),dimnames = dimnames(Prefs))
  lookupname<-colnames(Prefs)
  row.names(teams)<-teams$Name
  
  for (n in 1:nrow(Prefs)) {
  for (m in 1:ncol(Prefs)) {
   lookupname1<-lookupname[n]
   lookupname2<-lookupname[m]
   
   teampref[n,m]<-teams[lookupname1,"Team"]== teams[lookupname2,"Team"]
  }  
  }
  
  teampref<-(-1*teampref+1.01)
  Prefs<-Prefs*teampref
  return(Prefs)
}

load.past.excel<-function(filename="History.xlsx"){
  library(readxl)
  weeks<-excel_sheets(filename)
  k<-length(weeks)
  past.excel<-list()
  for (p in 1:k) {
    
    frames<-read.xlsx(filename,sheetName = weeks[p])
    frames<-frames[,-1]
    past.excel[[p]]<-frames
  }
  return(past.excel)
}
load.current.history<-function(file="currennt history.xlsx"){
  library(readxl)
  past<-list()
currennt_history <- read_excel(file,
sheet = "Sheet1")
helper<-as.data.frame(currennt_history[,c(1,2)])
past[[1]]<-helper
helper<-as.data.frame(currennt_history[,c(1,3)])
past[[2]]<-helper
helper<-as.data.frame(currennt_history[,c(1,4)])
past[[3]]<-helper
helper<-as.data.frame(currennt_history[,c(1,5)])
past[[4]]<-helper
helper<-as.data.frame(currennt_history[,c(1,6)])
past[[5]]<-helper
return(past)
}