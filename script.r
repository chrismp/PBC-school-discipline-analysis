# install.packages("dplyr")

require(dplyr)


## FUNCTIONS
func.cors <- function(dat){
  dat.cor <- cor(
    x = dat[, sapply(dat,is.numeric)],
    method = "pearson"
  )
  plot(
    x = dat$PercentOfWhiteStudentsPunished,
    y = dat$PercentOfBlackStudentsPunished
  )
  plot(
    x = dat$PercentOfNonBlackStudentsPunished,
    y = dat$PercentOfBlackStudentsPunished
  )
  plot(
    x = dat$PercentBlack,
    y = dat$PercentOfBlackStudentsPunished
  )
  plot(
    x = dat$PercentWhiteTeachers,
    y = dat$PercentOfBlackStudentsPunished
  )
  plot(
    x = dat$PercentBlack,
    y = dat$PercentOfNonBlackStudentsPunished
  )
  return(dat.cor)
}

func.makePunishmentRateData <- function(dat){
  dat.output <- data.frame(
    "Group" = c("Total","Black","Hispanic","White"),
    "Enrollment" = c(  sum(dat$TotalEnrollment), sum(dat$BlackEnrollment), sum(dat$HispanicEnrollment), sum(dat$WhiteEnrollment) ),
    "Punished" = c( sum(dat$TotalPunished), sum(dat$BlackStudentsPunished), sum(dat$HispanicStudentsPunished), sum(dat$WhiteStudentsPunished) )
  )
  dat.output$PunishmentRate <- dat.output$Punished / dat.output$Enrollment
  return(dat.output)
}


## ENROLLMENT
  dat.enrollment <- read.csv(
    file = "2016-17 enrollment by race raw.csv",
    header = T,
    na.strings = c("**")
  )
  
  dat.enrollment.groupByDistrictSchool <- group_by(
    .data = dat.enrollment,
    District..,
    District,
    School..,
    School
  )
  
  dat.enrollment.summarized <- summarize(
    .data = dat.enrollment.groupByDistrictSchool,
    BlackEnrollment = sum(Black.or.African.American, na.rm = T),
    WhiteEnrollment = sum(White, na.rm = T),
    HispanicEnrollment = sum(Hispanic.Latino, na.rm = T),
    TotalEnrollment = sum(TOTAL, na.rm = T)
  )
  
  dat.enrollment.summarized.WithBlack <- filter(
    .data = dat.enrollment.summarized,
    is.na(BlackEnrollment)==F
  )
  dat.enrollment.summarized.WithBlack$PercentBlack <- dat.enrollment.summarized.WithBlack$BlackEnrollment / dat.enrollment.summarized.WithBlack$TotalEnrollment
  dat.enrollment.summarized.WithBlack$PercentWhite <- dat.enrollment.summarized.WithBlack$WhiteEnrollment / dat.enrollment.summarized.WithBlack$TotalEnrollment
  
  
## PUNISHMENT
  dat.punishment <- read.csv(
    file = "2016-17 punishment raw.csv",
    header = T,
    na.strings = c('*')
  )
  
  dat.punishment.groupBySchoolDistrict <- group_by(
    .data = dat.punishment,
    District,
    District.Name,
    Discipline.School,
    Discipline.School.Name
  )
  
  dat.punishment.summarized <- summarize(
    .data = dat.punishment.groupBySchoolDistrict,
    BlackStudentsPunished = sum(Black.or.African.American, na.rm = T),
    WhiteStudentsPunished = sum(White, na.rm = T),
    HispanicStudentsPunished = sum(Hispanic.Latino, na.rm = T),
    TotalPunished = sum(Total, na.rm = T)
  )
  dat.punishment.summarized$BlackStudentsPercentOfPunished <- dat.punishment.summarized$BlackStudentsPunished / dat.punishment.summarized$TotalPunished
  
  
## TEACHERS
  dat.teachers <- read.csv(
    file = "2016-17 teachers by race raw.csv",
    header = T
  )
  dat.teachers$TotalTeachers <- dat.teachers$White + dat.teachers$Black.or.African.American + dat.teachers$Hispanic.Latino + dat.teachers$Asian + dat.teachers$American.Indian.or.Alaska.Native + dat.teachers$Native.Hawaiian.or.Other.Pacific.Islander + dat.teachers$Two.or.More.Races
  dat.teachers$PercentWhiteTeachers <- dat.teachers$White / dat.teachers$TotalTeachers

  
# MERGE DATASETS
  dat.merge.state <- merge(
    x = dat.enrollment.summarized.WithBlack,
    y = dat.punishment.summarized,
    by.x = c("District..","School.."),
    by.y = c("District","Discipline.School")
  )
  dat.merge.state <- merge(
    x = dat.merge.state,
    y = dat.teachers,
    by.x = c("District..","School.."),
    by.y = c("Dist..","Schl..")
  )
  dat.merge.state <- filter(
    .data = dat.merge.state,
    dat.merge.state$PercentBlack < 1,
    dat.merge.state$PercentBlack > 0,
    dat.merge.state$WhiteEnrollment > 0,
    dat.merge.state$TotalPunished < dat.merge.state$TotalEnrollment
  )
  dat.merge.state$PercentOfBlackStudentsPunished <- dat.merge.state$BlackStudentsPunished / dat.merge.state$BlackEnrollment
  dat.merge.state$PercentOfWhiteStudentsPunished <- dat.merge.state$WhiteStudentsPunished / dat.merge.state$WhiteEnrollment
  dat.merge.state$PercentOfNonBlackStudentsPunished <- dat.merge.state$TotalPunished / dat.merge.state$TotalEnrollment
  dat.merge.state$PunishmentDisparityBlackVsWhite <- dat.merge.state$PercentOfBlackStudentsPunished - dat.merge.state$PercentOfWhiteStudentsPunished
  dat.merge.state$PunishmentDisparityBlackVsNonBlack <- dat.merge.state$PercentOfBlackStudentsPunished - dat.merge.state$PercentOfNonBlackStudentsPunished
  dat.merge.state$PunishmentDisproportion <- dat.merge.state$PercentBlack - dat.merge.state$BlackStudentsPercentOfPunished
  dat.merge.state$TeacherStudentRatio <- dat.merge.state$TotalTeachers / dat.merge.state$TotalEnrollment
  dat.cor.state <- func.cors(dat.merge.state)


# PALM BEACH COUNTY
  dat.merge.pbc <- filter(
    .data = dat.merge.state,
    District.x == "PALM BEACH"
  )
  dat.cor.pbc <- func.cors(dat.merge.pbc)

  
# MAKE DATAFRAMES 
  dat.output.punishmentRates.state <- func.makePunishmentRateData(dat.merge.state)
  dat.output.punishmentRates.pbc <- func.makePunishmentRateData(dat.merge.pbc)


# WRITE TO CSVs
  outputDir <- "output/"
  write.csv(
    x = dat.output.punishmentRates.state,
    file = paste0(outputDir,"Punishment and enrollment by race - state.csv"),
    row.names = F
  )
  write.csv(
    x = dat.output.punishmentRates.pbc,
    file = paste0(outputDir,"Punishment and enrollment by race - PBC.csv"),
    row.names = F
  )
  
  
  
  