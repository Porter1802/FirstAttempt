
matcher<-function(Participant.file="real.xlsx",filename.Past = "Past.RDS",history = "Historyex.xlsx"){
  #55555
source('~/R Projects/Coffee chooser/chooser.R')
library(readxl)
library(dplyr)
library(matchingR)
print("Packages Loaded")

Parameters<-initialise(participant.file = Participant.file)
print("Participant list loaded")
dfeveryone<-Parameters[[2]]
involved<-ready.for.match(participant.file = Participant.file)
team.list<-teamlist(dfeveryone)
print("Participants sorted for matching")
past<-load.past(filename.Past)
print("History loaded")
View(past)

if(length(past)==0){
fullmatches<-make.matches.noprob()
print("No History provided, matches made without history")
}else{
print("History provided")

History<-create.history(Past.matches = past)
pref.matrix<-make.pref.matrix(history = History)
new.participants<-find.new()
print("New Participants located")
pref.matrix.with.new<-create.new()
pref.matrix.with.new<-add.random.to.pref(Pref.Matrix = pref.matrix.with.new)
pref.matrix.with.new<-modify.pref.for.team(teams = team.list,Prefs = pref.matrix.with.new)
fullmatches<-make.matches.with.prob(Participants = involved, Pref.Matrix = pref.matrix.with.new)
print("Matches made using history provided")
}

past<-update.past(Participants.data = dfeveryone,matches = fullmatches,Past.Matches = past)
update.past.excel(This.Week.Matches = fullmatches,filename = "History.xlsx",Past = past)
save.past(data.Past = past)
print("Updated history file to reflect new matching")
return(fullmatches)
}

