# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages('ggthemes', dependencies = TRUE)
# install.packages("showtext", dependencies = T)

require(dplyr)
require(ggplot2)
require(ggthemes)
require(showtext)


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

func.percentFormatX <- function(x) c( paste0(x[1]*100,'%'), x[-1]*100 )
func.percentFormatY <- function(y) c( y[1]*100, paste0(y[-1]*100,'%') )


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


# MAKE CHARTS
  # Pre-styling: Common styles for charts
  showtext_auto()
  hedFont <- "News Cycle" # Or 'Pragati Narrow'
  font_add_google(
    name = hedFont,
    family = hedFont,
    regular.wt = 400,
    bold.wt = 700
  )
  
  chartStyle.backgroundColor <- "#eeeeee"
  chartStyle.lineColor <- "#cccccc"
  chartStyle.trendLineThickness <- 0.5
  chartStyle.scatterplot.dotSize <- 3
  chartStyle.scatterplot.alpha <- 0.75
  
  chartStyle.theme <- theme(
    plot.title = element_text(
      size = 18,
      family = hedFont,
      face = "bold"#,
      # hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 14,
      # hjust = -1.19,
      margin = margin(
        b = unit(20, "pt")
      )
    ),
    plot.caption = element_text(
      size = 10
    ),
    axis.title = element_text(
      face = "bold"
    ),
    plot.background = element_rect(
      fill = chartStyle.backgroundColor
    ),
    axis.ticks = element_line(
      color = chartStyle.lineColor
    ),
    panel.background = element_rect(
      fill = chartStyle.backgroundColor
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 10
      )
    ),
    axis.title.y = element_text(
      margin = margin(
        r = 10
      )
    ),
    axis.text = element_text(
      size = 12
    ),
    panel.grid.major = element_line(
      color = chartStyle.lineColor,
      size = 0.25
    ),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(
      color = "#000000"
    )
  )
  
  # Scatterplot: % white students punished vs % black students punished, by school in PBC
  ggplot(
    data = dat.merge.pbc,
    aes(
      x = PercentOfWhiteStudentsPunished,
      y = PercentOfBlackStudentsPunished
    )
  ) + 
    geom_point(
      size = chartStyle.scatterplot.dotSize,
      alpha = chartStyle.scatterplot.alpha,
      color = "#ff8a59"
    ) +
    geom_smooth(
      method = "lm",
      se = F,
      size = chartStyle.trendLineThickness,
      color = "black"
    ) + 
    scale_x_continuous(
      name = "Percent of white students punished",
      labels = func.percentFormatX,
      breaks = seq(
        from = min(dat.merge.pbc$PercentOfWhiteStudentsPunished), 
        to = max(dat.merge.pbc$PercentOfWhiteStudentsPunished),
        by = 0.05
      )
    ) +
    scale_y_continuous(
      name = "Percent of black students punished",
      labels = func.percentFormatY,
      expand = c(0,0),
      breaks = seq(
        from = min(dat.merge.pbc$PercentOfBlackStudentsPunished), 
        to = max(dat.merge.pbc$PercentOfBlackStudentsPunished),
        by = 0.1
      )
    ) +
    labs(
      title = "Punishment by race in Palm Beach County, Fla. schools",
      subtitle = "Punishment rates for black students vs. white students, 2016-17 school year",
      caption = "Source: Florida Dept. of Education"
    ) + 
    coord_cartesian(clip = "off") +
    chartStyle.theme
  
  
  
# # WRITE TO CSVs
#   outputDir <- "output/"
#   write.csv(
#     x = dat.output.punishmentRates.state,
#     file = paste0(outputDir,"Punishment and enrollment by race - state.csv"),
#     row.names = F
#   )
#   write.csv(
#     x = dat.output.punishmentRates.pbc,
#     file = paste0(outputDir,"Punishment and enrollment by race - PBC.csv"),
#     row.names = F
#   )
  
  
  
  