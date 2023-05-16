library(ggplot)
library(tidyverse)
library(ggpubr)


this_year <- as.numeric(format(Sys.Date(), "%Y"))
textdir <- 0.06
arrowdir <- 0.04
since <- 2017

edu <- data.frame(
    event = c("Hello world", "School", "High School", "Bachelor Degree", "Master's Degree")
    , start = c(1999, 2003, 2014, 2017, 2022)
    , end = c(this_year, 2014, 2017, 2021, 2023)
    , desc = c("Revived", "Maadi Narmer School", "Manor House British School", "Arab Academy For Science, Technology & Maritime","Universidad Carlos III de Madrid")
    , loc = c("Cairo - EG", "Cairo - EG", "Cairo - EG", "Cairo - EG", "Madrid - ES")
    , Time_line = 1
    , type="education"
)

exp <- data.frame(
    event = c("KnowDental", "Vodafone", "noon"),
    start = c(2017, 2019, 2021),
    end = c(2018, 2019, this_year), 
    desc = c("Volunteer", "Intern", "Business Intelligence Analyst"),
    loc = c("Cairo - EG", "Cairo - EG", "Cairo - EG"),
    Time_line = 1,
    type="experiance"

)



pivot_df <- function(df) {
    df %>% pivot_longer(
    cols = c('start','end'),
    names_to = "year",
    values_to = "year2"
    )%>%group_by(event)%>%
    mutate(min_year = min(year2),Time_line = 1)%>%
    ungroup()%>%
    filter(event!="Hello world")%>%mutate(text = paste0(event,"\n",desc,"\n",loc))
}

df_group <- function (df){
    df%>%
    group_by(event,desc,text,Time_line)%>%
    summarise(avg = mean(year2))%>%
    filter(event!="Hello world")%>%
    arrange(avg)%>%
    mutate(ydesc = textdir,yarrow = arrowdir)%>%
    group_by(ydesc)%>%
    # mutate(y=row_number())
    mutate(ydesc = (ydesc*row_number())/1.5)%>%
    ungroup()%>%
    mutate(yarrow = ydesc-0.002)
}

plot_timeline <- function(pivot_all,pivot_all_group){

ggplot(pivot_all,aes(x=year2,y=y))+
# ploting the year on the timeline and connecting them
geom_segment(data=data.frame(start=min(pivot_all$year2),end=max(pivot_all$year2),y=max(pivot_all$y)), mapping=aes(x=start, y=y, xend=end+0.5, yend=y),arrow = arrow(length = unit(0.5, "cm")))+
#connecting the duration
geom_line(data= pivot_all,mapping= aes(x=year2,y=Time_line,color=event))+
#duration point
geom_point(data= pivot_all,mapping= aes(x=year2,y=Time_line,color=event),size=6,fill="white")+

#closing the duration to the years
geom_segment(data=pivot_all, mapping=aes(x=duration, y=Time_line, xend=duration, yend=y,color=event))+
# adding desc to the duration
annotate("text", x=pivot_all_group$avg_dur, y=pivot_all_group$Time_line, hjust=0.5, label=pivot_all_group$text, face = "bold")+

# year points 
geom_point(fill = "white",size=12,colour = "black", pch=21)+
# timeline line 
annotate("text", x=pivot_all$year2, y=pivot_all$y, hjust=0.5, label=pivot_all$year2,size=3, face = "bold")+

theme_void()+
theme(plot.title = element_text(hjust = 0.5,size = 40, face = "bold"),legend.position="none",aspect.ratio=6/10)+
#adjusting x scale
xlim(min(pivot_all$year2),(this_year+0.5))
# ylim(min(pivot_all$Time_line,na.rm = TRUE)-0.01,max(pivot_all$Time_line,na.rm = TRUE)+0.01)
}




p_edu <- pivot_df(edu)%>%mutate(type="education")
edu_group <- df_group(p_edu)%>%filter(avg>since)
p_edu <- p_edu%>%filter(min_year>=since)

p_exp <- pivot_df(exp)%>%mutate(type="experiance")
exp_group <- df_group(p_exp)





p_all <- data.frame(year2 =min(c(p_edu$year2,p_exp$year2)):this_year)%>%
left_join(rbind(p_edu,p_exp)%>%mutate(duration = year2))%>%
mutate(y=1,Time_line = ifelse(type=="education",Time_line-0.05,Time_line+0.05))




p_all_group <- p_all%>%
group_by(text,desc,Time_line,event)%>%
summarise(avg_dur = mean(duration))%>%
arrange(text)%>%
drop_na()%>%
mutate(Time_line = ifelse(Time_line>1,Time_line+0.01,Time_line-0.01))




final_plot <- plot_timeline(p_all,p_all_group)

ggsave("timeline.png",plot = final_plot, dpi=400, dev='png')#, height=4.5, width=6.5, units="in")
