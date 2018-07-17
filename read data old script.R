
#  This script loads 2014 data files from VIAPPL porject

#  LOAD SOME LIBRARIES-----------

# install.packages("pacman")  RUN THIS IF YOU DON"T have it
library(pacman)
p_load(ggplot2, descr, vcd, tidyr, dplyr, magrittr,
        xtable,simsalapar, readxl,tools, lubridate, stringr, moments,
       nlme, lme4, effects, car)


#  DATA PROCESSING 2014 -------------

# This script reads all Excel files it finds in a named folder 
# according to specifications for reading VIAPPL 2014 output format,
# that is, reading specific sheets, and extracting data there.

# User chooses folder, and we catch file extensions to check
# for non-Excel files


vplfolder <- choose.dir(getwd(),"Choose a folder with VIAPPL Excel files")
file_list <-  list.files(vplfolder,full.names=TRUE)   
exts      <- file_ext(file_list)

for (i in 1:length(exts)){
  if(exts[i]!="xls" & exts[i]!="xlsx"){
    stop("There are files in that folder that do not
         seem to be Excel files")
  }    
  }

#   Set datafile name for later saving with today's date
x <- now()
datafile1 <- str_c(getwd(),"//", "vpldata2014wider-", year(x),month(x),day(x),".Rdata")
datafile2 <- str_c(getwd(),"//", "vpldata2014longer-", year(x),month(x),day(x),".Rdata")

#   This block of code creates an R datafile from VIAPPL 2014 Excel sheets
#   We run a loop to read in both kinds of sheet we want, from
#   all the files in the folder, and
#   do some manipulations and merging

msg <- c("Processing file: ")
vpldata2014 <- data.frame(NULL)  # initialise data frame for storing final data


for (file in file_list){
  
  print(paste(msg,file))
  
  # rounddetail <- read_excel(file, sheet = "RoundDetail",
  #                           col_names = TRUE, col_types = NULL, 
  #                           na = "") %>%
  #   filter(trialNo!=1) %>%
  #   select(tokensToSelf,fromParticipantId,roundNo)
  # 
  # 
  vpldata2014cum <- read_excel(file, sheet="ParticipantRounds",
                            col_names = TRUE, col_types = NULL, 
                            na = "") %>% 
    filter(trialNo!=1)  %>%
    select(accessCode, participantId, roundNo, groupNo, tokensToSelf, 
           tokensToInGroup, tokensToOutGroup)%>%  
    rename(game=accessCode,id=participantId, groupno=groupNo) %>%
    # Watch out!  the next line corrects mistakes in game naming
    mutate(game = toupper(game)) %>% 
    mutate(game = ifelse(str_sub(game,2,2)=="2",paste(str_sub(game,1,1), 
                        str_sub(game,3,9), sep=""),game)) %>% 
    mutate(groupness = ifelse(str_sub(game, 2, 2)=="G", "group", 
                              "individual")) %>% 
    mutate(status = ifelse(str_sub (game, 1, 1)=="E", "equality",
                           "inequality")) %>%
    unite(equalitycondition,status,groupno,remove=TRUE) %>% 
    mutate(equalitycondition=ifelse(equalitycondition=="inequality_1",
                                    "lowstatus",
                                      ifelse(equalitycondition=="inequality_2",
                                             "highstatus","equalstatus")))
    vpldata2014 <- rbind(vpldata2014, vpldata2014cum)
}

#   NOTE that in the import above from Excel, participants in the original games
#   appear to have been able to award more than one token per round.  We are 
#   not sure of
#   the provenance of this, but that is how it is!

#   write all variable names to lowercase, consistent with style convention
colnames(vpldata2014) %<>% tolower

#   re-order data so that design variables are contiguous in the dataframe
vpldata2014 <- vpldata2014[c("game", "id", "roundno",  
                             "groupness","equalitycondition", "tokenstoingroup",
                             "tokenstooutgroup","tokenstoself")]


# Validate your data - you need to check
# to see that you have the right N,
# the right number of cells in your design 
# for each factor

table(vpldata2014$groupness)
table(vpldata2014$id)
table(vpldata2014$equalitycondition)
table(vpldata2014$game)

#  Create factor variables (convert data from character strings
#  to R type categorical variables 
#  and reduce roundno to 5 rounds of 8

vpldata2014 %<>%
  mutate(roundcat=cut(roundno, breaks=5, labels=1:5), id = factor(id), 
         roundno=factor(roundno), equalitycondition=factor(equalitycondition)) 
#tokentype=factor(tokentype, labels=c("ingroup","outgroup","self")))
str(vpldata2014, width=60)


#   Create cumulative sums over the 8 rounds per 5 waves 
#   and retain simplified data

vpldata2014wider <- vpldata2014 %>% 
  group_by(roundcat, id) %>% 
  mutate(proptokenstoingroup = cumsum(tokenstoingroup),
         proptokenstooutgroup = cumsum(tokenstooutgroup),
         proptokenstoself = cumsum(tokenstoself),
         ingroupfav = proptokenstoingroup/(proptokenstoingroup + proptokenstooutgroup)) %>% 
  mutate(ingroupfav = ifelse((proptokenstoingroup + proptokenstooutgroup) == 0,
                             0, ingroupfav)) %>%
  mutate(selfgiving = proptokenstoself/8) %>% 
  select(-c(proptokenstoingroup,proptokenstooutgroup,proptokenstoself)) %>% 
  filter(as.numeric(roundno)%%8 == 0)


#   tidy data up, a la Wickham 
#   recodes the data from wide format to long format
vpldata2014longer <- vpldata2014 %>% 
  gather(tokentype, tokengiven, matches("token")) %>% 
  mutate(tokentype=factor(tokentype, labels=c("ingroup","outgroup","self")))

vpldata2014longer <- vpldata2014longer %>% 
  group_by(roundcat, id, tokentype) %>% 
  mutate(tokensum = cumsum(tokengiven)) %>% 
  filter(as.numeric(roundno)%%8 == 0)

#  write data to both R and csv format in folder of origin
save(vpldata2014wider, file=paste(datafile1))
save(vpldata2014longer, file=paste(datafile2))

write.csv(vpldata2014wider,file=paste(datafile1,".csv"))
write.csv(vpldata2014longer,file=paste(datafile2,".csv"))

# clear workspace
remove("datafile1",  "datafile2","exts","vpldata2014cum",
       "file", "file_list", "i", "msg", "vplfolder","x")







