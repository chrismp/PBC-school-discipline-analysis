# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lettercase")
# install.packages("extrafont")

require(dplyr)
require(ggplot2)
require(showtext)
require(lettercase)
require(extrafont)


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
  plotOutputDir <- "output/plots/"
  
  # Pre-styling: Common styles for charts
    # These fonts only work if your computer has them installed. Get 'em from Google Fonts.
    # This bash script installs every Google Font on Debian-based OS's: https://gist.github.com/keeferrourke/d29bf364bd292c78cf774a5c37a791db
    # hedFont <- "News Cycle"
    hedFont <- "Pragati Narrow" # alternative choice for font

    # Style snippets for all chartd
      chartStyle.backgroundColor <- "#eeeeee"
      chartStyle.lineColor <- "#cccccc"
      chartStyle.trendLineThickness <- 0.5
      chartStyle.caption <- "Chris Persaud / Datavizz.com\nSource: Florida Dept. of Education"
      
    # Style snippets for scatterplots  
      chartStyle.scatterplot.dotSize <- 3
      chartStyle.scatterplot.alpha <- 0.75
      chartStyle.scatterplot.dotLabelSize <- chartStyle.scatterplot.dotSize * 1.125
      chartStyle.scatterplot.dotStroke <- 0.5
    
    # ggplot theme() functions for all charts
      chartStyle.theme <- theme(
        plot.title = element_text(
          size = 20,
          family = hedFont,
          face = "bold"
        ),
        plot.subtitle = element_text(
          size = 13,
          # family = hedFont,
          # face = "bold",
          margin = margin(
            b = unit(20, "pt")
          )
        ),
        plot.caption = element_text(
          size = 9,
          face = "italic",
          color = "#333333"
        ),
        axis.line.x = element_line(
          color = "#000000"
        ),
        axis.title = element_text(
          face = "bold"
        ),
        axis.ticks = element_line(
          color = chartStyle.lineColor
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
        plot.background = element_rect(
          fill = chartStyle.backgroundColor
        ),
        panel.background = element_rect(
          fill = chartStyle.backgroundColor
        ),
        panel.grid.major = element_line(
          color = chartStyle.lineColor,
          size = 0.25
        ),
        panel.grid.minor = element_blank(),
        plot.margin = margin(
          t = 5,
          r = 5,
          b = 5,
          l = 5
        )
      )
      
      chartStyle.theme.bar <- theme(
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none"
      )
    
  # Bar chart: Punishment rates by race, PBC vs Florida
    chartStyle.PBCvFL.titleLeftMargin <- -0.025
    chartStyle.PBCvFL.titleTopMargin <- 3.25
    chartStyle.PBCvFL.subtitleTopMargin <- chartStyle.PBCvFL.titleTopMargin - 0.25
    chartStyle.PBCvFL.barWidth <- 0.75
    dat.output.punishmentRates.combined$Group <- factor(
      x = dat.output.punishmentRates.combined$Group,
      levels = c("Total","Black","Hispanic","White") # Rearrange the order in which bars will appear
    )
    chart.PBCvFL <- ggplot(
      data = dat.output.punishmentRates.combined,
      aes(
        fill = Group,
        x = Geography,
        y = PunishmentRate
      )
    ) +
      geom_bar(
        position = position_dodge(),
        stat = "identity",
        width = chartStyle.PBCvFL.barWidth
      ) +
      scale_fill_manual(
        values = c("#66c2a5","#fc8d62","#8da0cb","#e78ac3")
      ) +
      scale_x_discrete(
        limits = c("Florida","Palm Beach County"),
        labels= c("Florida","Palm\nBeach\nCounty"),
        expand = c(0,chartStyle.PBCvFL.barWidth)
      ) +
      scale_y_continuous(
        name = "Percent of students punished",
        labels = func.percentFormatX,
        expand = c(0,0),
        breaks = seq(
          from = 0, 
          to = max(dat.output.punishmentRates.combined$PunishmentRate),
          by = 0.05
        )
      ) +
      geom_text(
        data = subset(dat.output.punishmentRates.combined, Geography == "Palm Beach County"),
        aes(
          label = Group,
          y = 0.005
        ),
        position = position_dodge(chartStyle.PBCvFL.barWidth),
        hjust = 0,
        color = "#ffffff",
        fontface = "bold"
      ) +
      labs(caption = chartStyle.caption) +
      geom_text(
        x = chartStyle.PBCvFL.titleTopMargin,
        y = chartStyle.PBCvFL.titleLeftMargin,
        inherit.aes = F,
        label = "Florida schools punish black students more then their peers",
        check_overlap = T,
        hjust = 0,
        size = 8,
        family = hedFont,
        fontface = "bold"
      ) +
      geom_text(
        x = chartStyle.PBCvFL.subtitleTopMargin,
        y = chartStyle.PBCvFL.titleLeftMargin,
        inherit.aes = F,
        label = "In Palm Beach County, black students were punished three times more often than\nwhite pupils in the 2016-17 school year",
        check_overlap = T,
        hjust = 0,
        size = 5,
        lineheight = 1
      ) +
      coord_flip(clip = "off") +
      chartStyle.theme +
      chartStyle.theme.bar + 
      theme(
        plot.margin = unit(
          x = c(5,1,1,1),
          units = "line"
        ),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(
          hjust = 0
        )
      )
    chart.PBCvFL
    ggsave(
      filename = "PBCvFL.png",
      plot = chart.PBCvFL,
      device = "png",
      path = plotOutputDir,
      width = 200,
      height = 150,
      units = "mm",
      dpi = 144
    )
  
  # Scatterplot: % white students punished vs % black students punished, by school in PBC
    chart.WhiteBlackPunishmentScatter <- ggplot(
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
        title = "Punishment rates for black students vs white in Palm Beach County",
        subtitle = "Schools that punished white kids at high rates usually disciplined black pupils more often in the 2016-17 school year",
        caption = chartStyle.caption
      ) + 
      coord_cartesian(clip = "off") + 
      chartStyle.theme +
      theme(
        axis.text.y = element_text(
          hjust = 0
        ),
        plot.title = element_text(
          margin = margin(
            b = 5,
            l = -10
          )
          # hjust = 1.5
        ),
        plot.subtitle = element_text(
          # hjust = -0.5
        )
      )
    
    chart.WhiteBlackPunishmentScatter
    ggsave(
      filename = "PBC-black-white-punishment-scatter.png",
      plot = chart.WhiteBlackPunishmentScatter,
      device = "png",
      path = plotOutputDir,
      width = 200,
      height = 150,
      units = "mm",
      dpi = 144
    )
    
  # Scatterplot: Black punishment vs nonblack punishment in PBC
    chart.NonblackBlackPunishmentScatter <- ggplot(
      data = dat.merge.pbc,
      aes(
        x = PercentOfNonBlackStudentsPunished,
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
          no = 0
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
      scale_x_continuous(
        name = "Percent of nonblack students punished",
        labels = func.percentFormatX,
        breaks = seq(
          from = min(dat.merge.pbc$PercentOfNonBlackStudentsPunished), 
          to = max(dat.merge.pbc$PercentOfNonBlackStudentsPunished),
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
        title = "Punishment rates for black students vs non-black in Palm Beach County",
        subtitle = "Schools that punished non-black students usually punished black pupils at higher rates in 2016-17",
        caption = chartStyle.caption
      ) + 
      coord_cartesian(clip = "off") + 
      chartStyle.theme +
      theme(
        axis.text.y = element_text(
          hjust = 0
        ),
        plot.title = element_text(
          margin = margin(
            b = 5,
            l = -10
          )
          # hjust = 1.5
        ),
        plot.subtitle = element_text(
          # hjust = -0.5
        )
      )
    
    chart.NonblackBlackPunishmentScatter
    ggsave(
      filename = "PBC-black-nonblack-punishment-scatter.png",
      plot = chart.NonblackBlackPunishmentScatter,
      device = "png",
      path = plotOutputDir,
      width = 200,
      height = 150,
      units = "mm",
      dpi = 144
    )
    
  # Diverging chart: Black-white punishment disparity by school
  # More info: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Diverging%20Bars
  dat.merge.pbc$BlackWhitePunishmentDisparityCategory <- ifelse(
    test = dat.merge.pbc$PunishmentDisparityBlackVsWhite > 0,
    yes = "Black",
    no= "White"
  )
  chart.WhiteBlackDisparityBySchool <- ggplot(
    data = dat.merge.pbc,
    aes(
      x = reorder(
        as.character(
          x = str_title_case(
            x = tolower(
              x = School
            )
          )
        ),
        PunishmentDisparityBlackVsWhite
      ),
      y = PunishmentDisparityBlackVsWhite,
      label = PunishmentDisparityBlackVsWhite
    )
  ) +
    geom_bar(
      stat = "identity",
      aes(
        fill = BlackWhitePunishmentDisparityCategory
      ),
      width = 0.5
    ) +
    scale_fill_manual(
      # name = "Black-white punishment disparity",
      values = c("#f1a340","#998ec3")
    ) +
    scale_y_continuous(
      breaks = seq(
        from = -0.1,
        to = max(dat.merge.pbc$PunishmentDisparityBlackVsWhite),
        by = 0.1
      ),
      name = element_blank(),
      position = "right", # Because of coord_flip(), this will put scale up top
      labels = func.percentFormatX # Use the x-axis function since this chart's axes will be flipped
    ) +
    labs(
      title = "Most Palm Beach County schools punish black students more often than white students",
      subtitle = "Percent of white students punished minus percent of black students punished in the 2016-17 school year",
      caption = chartStyle.caption
    ) +
    coord_flip() +
    chartStyle.theme +
    chartStyle.theme.bar +
    theme(
      plot.subtitle = element_text(
        margin = margin(
          b = 0
        )
      ),
      axis.text.y = element_text(
        size = 10
      )
    )
  
  chart.WhiteBlackDisparityBySchool
  ggsave(
    filename = "PBC-black-white-punishment-disparity-by-school-diverging-bar.png",
    plot = chart.WhiteBlackDisparityBySchool,
    device = "png",
    path = plotOutputDir,
    width = 200,
    height = 800,
    units = "mm",
    dpi = 144
  )
  
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
  
  
  
  