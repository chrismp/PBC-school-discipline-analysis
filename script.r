# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("showtext", dependencies = T)
# install.packages("lettercase")

require(dplyr)
require(ggplot2)
require(showtext)
require(lettercase)
require(gridExtra)


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

func.makePunishmentRateData <- function(dat,geography){
  dat.output <- data.frame(
    "Group" = c("Total","Black","Hispanic","White"),
    "Enrollment" = c(  sum(dat$TotalEnrollment), sum(dat$BlackEnrollment), sum(dat$HispanicEnrollment), sum(dat$WhiteEnrollment) ),
    "Punished" = c( sum(dat$TotalPunished), sum(dat$BlackStudentsPunished), sum(dat$HispanicStudentsPunished), sum(dat$WhiteStudentsPunished) )
  )
  dat.output$PunishmentRate <- dat.output$Punished / dat.output$Enrollment
  dat.output$Geography <- geography
  return(dat.output)
}

func.percentFormatX <- function(x) c( paste0(x[1]*100,'%'), x[-1]*100 )
func.percentFormatY <- function(y) c(y[1:length(y)-1]*100 , paste0(rev(y)[1]*100,'%'))

func.simpleCap <- function(theString) {
  x <- tolower(theString) # School names are all caps, so lowercase them here
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
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
  # dat.merge.pbc$SchoolCapitalized <- func.simpleCap(dat.merge.pbc$School)

  
# MAKE DATAFRAMES 
  dat.output.punishmentRates.state <- func.makePunishmentRateData(dat = dat.merge.state, geography = "Florida")
  dat.output.punishmentRates.pbc <- func.makePunishmentRateData(dat = dat.merge.pbc, geography = "Palm Beach County")
  dat.output.punishmentRates.combined <- rbind(dat.output.punishmentRates.state, dat.output.punishmentRates.pbc)


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
    chartStyle.scatterplot.dotLabelSize <- chartStyle.scatterplot.dotSize * 1.125
    chartStyle.scatterplot.dotStroke <- 0.5
    chartStyle.bar.order <- c("Total","White","Hispanic","Black")
    chartStyle.caption <- "Chris Persaud / Datavizz.com\nSource: Florida Dept. of Education"
    
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
        size = 10,
        face = "italic",
        color = "#333333"
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
    
  # Bar chart: Punishment rates by race, PBC vs Florida
    ggplot(
      data = dat.output.punishmentRates.combined,
      aes(
        fill = Geography,
        x = reorder(
          x = Group,
          -PunishmentRate
        ),
        y = PunishmentRate
      )
    ) +
      geom_bar(
        position = position_dodge2(
          width = 400,
          padding = 0
        ),
        stat = "identity",
        width = 0.5
      ) +
      scale_fill_manual(
        values = c("#1f78b4","#b2df8a")
      ) +
      scale_x_discrete(
        limits = chartStyle.bar.order
      ) +
      scale_y_continuous(
        labels = func.percentFormatX,
        expand = c(0,0),
        breaks = seq(
          from = 0, 
          to = max(dat.output.punishmentRates.combined$PunishmentRate),
          by = 0.05
        )
      ) +
      geom_text(
        aes(
          label = Geography,
          y = 0
        ),
        hjust = 0,
        position = position_dodge(1)
      ) +
      coord_flip() +
      chartStyle.theme +
      theme(
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank()
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
        fill = "#ff8a59",
        shape = 21,
        stroke= ifelse(
          test = dat.merge.pbc$PercentOfBlackStudentsPunished > 0.6,
          yes = chartStyle.scatterplot.dotStroke,
          no = ifelse(
            test = dat.merge.pbc$PercentOfWhiteStudentsPunished > 0.3,
            yes = chartStyle.scatterplot.dotStroke,
            no = 0
          )
        )
      ) +
      geom_smooth(
        method = "lm",
        se = F,
        size = chartStyle.trendLineThickness,
        color = "black"
      ) + 
      geom_text( # Highlight school with highest rate of punishing black students
        aes(
          label = ifelse(
            test = PercentOfBlackStudentsPunished > 0.6,
            yes = as.character(
              x = str_title_case(
                x = tolower(
                  x = School
                )
              )
            ),
            no = ''
          )
        ),
        hjust = 1.06,
        vjust = 1,
        size = chartStyle.scatterplot.dotLabelSize
      ) +
      geom_text( # Highlight school with highest rate of punishing whites relative to blacks
        aes(
          label = ifelse(
            test = PercentOfWhiteStudentsPunished > 0.3,
            yes = as.character(
              x = str_title_case(
                x = tolower(
                  x = School
                )
              )
            ),
            no = ''
          )
        ),
        hjust = 1.04,
        vjust = 1.5,
        size = chartStyle.scatterplot.dotLabelSize
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
        caption = chartStyle.caption
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
  
  
  
  