library(cowplot)
library("viridis") 
library(ggrepel)
library(colorspace)
library(plotly)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("...")

dt <- read.csv("....csv", header = T, skipNul = T,fileEncoding="UCS-2LE", sep = ",")
#dt <- dt %>% separate(Id.Date.Category.Amount.Currency.Note.Wallet, into = c("Id","Date","Category","Amount","Currency","Note","Wallet"), sep = ",")
is.na(dt$Date)
dt$Amount <- abs(as.numeric(dt$Amount))
unique(dt$Category)

#Categorize
fixed <- c("Bills & Utilities", "Grocery", "Health & Fitness",
           "Transportation", "Rentals", "Laundry", "Pharmacy", "Phone Bill", "Insurances")

future <- c("Education", "Spaceship", "Investment", "UniSuper")

fun <- c("Shopping", "Personal Care", "Dinner",
         "Gifts & Donations", "Lunch", "Café", "Entertainment", "Travel", 
         "Breakfast", "Food & Beverage", "Clothing", "Home Improvement", "Furniture")

#check which spending not been categorized
dt$Category[which(!(dt$Category %in% c(fixed, future, fun)))]

dt
dt$BiggerCategory <- ifelse(dt$Category %in% fixed, "fixed",
                            ifelse(dt$Category %in% future, "future",
                                   ifelse(dt$Category %in% fun, "fun", NA)))

#change date
#additional functions
right = function (string, char) {
  substr(string,nchar(string)-(char-1),nchar(string))
}
left = function (string,char) {
  substr(string,1,char)
}
dt$Date <- (as.Date(dt$Date, format = "%d/%m/%Y"))
dt$MY <- format(dt$Date,"%Y-%m")
dt$Y <- format(dt$Date,"%Y")
#dt$MY <- right(dt$Date, 7) 
#dt$Y <- right(dt$Date, 4) 

dt %>% group_by(Y, MY, BiggerCategory, Category) %>%
  summarise(sumY = sum(Amount, na.rm = T))

eachmonth <- dt %>% 
  group_by(MY, BiggerCategory, add=TRUE) %>%
  summarize(sumY = sum(Amount, na.rm = T)) %>%
  mutate(sumM = sum(sumY),
         perc = round(sumY*100/sum(sumY), 2))

#add ideal
income_after_tax <- 5200
i_p <- c(50, 30, 20)
p_fix_i <- i_p[1]/100*income_after_tax
p_fun_i <- i_p[2]/100*.3*income_after_tax
p_fut_i <- i_p[3]/100*.2*income_after_tax
BiggerCategory <- c("fixed", "fun", "future")
sumY <- c(p_fix_i, p_fun_i, p_fut_i)
perc <- i_p
ideal_dt <- cbind.data.frame(MY="2021-09", BiggerCategory, sumY, sumM = income_after_tax, perc)
toplot_eachmonth <- rbind.data.frame(ideal_dt, eachmonth)

#add up and down

toplot_eachmonth$YMD <- paste0(toplot_eachmonth$MY, "-01")
toplot_eachmonth$YMD <- as.Date(toplot_eachmonth$YMD, format = "%Y-%m-%d")
toplot_eachmonth <- toplot_eachmonth %>% mutate(YearMonth = format(YMD,"%Y-%m"))

toplot_eachmonth <- toplot_eachmonth %>% group_by(BiggerCategory) %>%
  mutate(updown = perc - lag(perc)) %>%
  mutate(arrow = ifelse(updown>0, "↑", ifelse(updown<0, "↓", NA))) %>% 
  mutate(updowncolor = ifelse(updown <0, "blue", 
                              ifelse(updown >0, "red", 
                                     "black"))) %>%
  mutate(updowncolor = ifelse(updown < 0 & BiggerCategory == "future", "red",
         ifelse(updown > 0 & BiggerCategory == "future", "blue", updowncolor))) %>%
  mutate(sumMsign = ifelse(sumM-lag(sumM) > 0, "↑", "↓")) %>%
  group_by(MY) %>%
  mutate(MYcolor = ifelse(sumM>income_after_tax, "red", "black")) #each month spend
  

toplot_eachmonth[is.na(toplot_eachmonth$updowncolor), ]$updowncolor <- "black"
toplot_eachmonth[is.na(toplot_eachmonth$arrow),]$arrow <- ""
toplot_eachmonth[is.na(toplot_eachmonth$sumMsign),]$sumMsign <- ""
toplot_eachmonth[is.na(toplot_eachmonth$updown),]$updown <- 0


colorset <- c("#FF9A00", "#1FB57B", "#F3558E")


#pie chart
p1 <- ggplot(data=toplot_eachmonth, aes(x=" ", y=perc, group=MY, fill=BiggerCategory)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ MY + paste0("$",sumM,sumMsign)) + theme_void() +
  geom_text(aes(label = paste0("$", sumY, " (", perc, "%", arrow,")")),
            position = position_stack(vjust = 0.75),
            color = toplot_eachmonth$updowncolor, size=5)+
  theme(text=element_text(size=16,  family="Times New Roman"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=colorset)  + ggtitle("Overall spending")

#line graph
p2 <- ggplot(data=toplot_eachmonth, aes(x=MY, y=sumY, color=BiggerCategory, group = BiggerCategory)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values=colorset) +
  theme_bw() +
  geom_text(aes(label=paste0(round(updown, 2), "%"), vjust=2)) +
  geom_text(data = toplot_eachmonth[toplot_eachmonth$MY == max(toplot_eachmonth$MY),], 
            aes(label=paste0("$",sumY, " ", BiggerCategory), hjust=-0.2)) +
  xlab("Year-month") + ylab("Amount ($)") +
  scale_x_discrete(labels=unique(paste0(toplot_eachmonth$MY, "\n", "$",toplot_eachmonth$sumM,toplot_eachmonth$sumMsign))) +
  theme(legend.position = "none")

plot_grid(p1, p2, labels = "AUTO", ncol = 1)
ggsave(paste0("overall_pie_line",".png"),
       scale = 1,
       width = 30,
       height = 20,
       units = c("cm"),
       dpi = 300)
#need to find what category was outstanding

#fixed
      
      fdt <- dt[dt$BiggerCategory == "fixed",]
      fdt
      colorset2 <- viridis(length(unique(fdt$Category)))
      eachcat <- fdt %>% 
        group_by(MY, Category, add=TRUE) %>%
        summarize(sumY = sum(Amount, na.rm = T)) %>%
        mutate(sumM = sum(sumY),
               perc = round(sumY*100/sum(sumY), 2)) %>%
        mutate(
        cs = rev(cumsum(rev(perc))), 
        pos = cumsum(perc) - perc/2,#perc/2 + lead(cs, 1),
        pos = if_else(is.na(pos), perc/2, pos))
      
      toploteachcat <- eachcat %>% group_by(Category) %>%
        mutate(updown = perc - lag(perc)) %>%
        mutate(arrow = ifelse(updown>0, "↑", ifelse(updown<0, "↓", NA))) %>% 
        mutate(updowncolor = ifelse(updown <0, "blue", 
                                    ifelse(updown >0, "red", 
                                           "black"))) %>%
        mutate(sumMsign = ifelse(sumM-lag(sumM) > 0, "↑", "↓")) %>%
        group_by(MY) %>%
        mutate(MYcolor = ifelse(sumM>income_after_tax, "red", "black"))#each month spend
      
      
      toploteachcat[is.na(toploteachcat$updowncolor),]$updowncolor <- "black"
      toploteachcat[is.na(toploteachcat$sumMsign), ]$sumMsign <- ""
      toploteachcat[is.na(toploteachcat$updown), ]$updown <- 0
      toploteachcat[is.na(toploteachcat$arrow), ]$arrow <- ""
      
      
          #pie chart
          p3 <- ggplot(data=toploteachcat, aes(x=" ", y=perc, group=MY, fill=Category)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) + 
            facet_grid(.~ MY+paste0("$", sumM)) + theme_void() +
            theme(text=element_text(size=16,  family="Times New Roman"), plot.title = element_text(hjust = 0.5)) +
            scale_fill_manual(values=colorset2) +
            geom_label_repel(data = toploteachcat, aes(y = pos, 
                                                       label = paste0("$", sumY, " (", perc, "%", ")", arrow)), 
                             size=4, show.legend = F, nudge_x = 1) + ggtitle("Fixed")
          
          #line graph
          p4 <- ggplot(data=toploteachcat, aes(x=MY, y=sumY, color=Category, group = Category)) +
            geom_line(size = 1) +
            geom_point() +
            scale_color_manual(values=colorset2) +
            theme_bw() +
            geom_text(aes(label=paste0(round(updown, 2), "%"), vjust=2)) +
            geom_text_repel(data = toploteachcat[toploteachcat$MY == unique(toploteachcat$MY)[order(unique(toploteachcat$MY), decreasing = T)][2],], 
                            aes(label=paste0("$",sumY)), vjust=2) +
            geom_text_repel(data = toploteachcat[toploteachcat$MY == max(toploteachcat$MY),], 
                      aes(label=paste0("$",sumY, " ", Category, arrow), hjust=-0.2)) +
            
            xlab("Year-month") + ylab("Amount ($)") +
            scale_x_discrete(labels=unique(paste0(toploteachcat$MY, "\n", "$",toploteachcat$sumM,toploteachcat$sumMsign)))
          
          plot_grid(p3, p4, labels = "AUTO", ncol = 1)
          ggsave(paste0("fixed_pie_line",".png"),
                 scale = 1,
                 width = 30,
                 height = 20,
                 units = c("cm"),
                 dpi = 300) 
          
          
#fun
          
          fdt <- dt[dt$BiggerCategory == "fun",]
          fdt
          colorset3 <- viridis(length(unique(fdt$Category)))
          eachcat <- fdt %>% 
            group_by(MY, Category, add=TRUE) %>%
            summarize(sumY = sum(Amount, na.rm = T)) %>%
            mutate(sumM = sum(sumY),
                   perc = round(sumY*100/sum(sumY), 2)) %>%
            mutate(
              cs = rev(cumsum(rev(perc))), 
              pos = cumsum(perc) - perc/2,#perc/2 + lead(cs, 1),
              pos = if_else(is.na(pos), perc/2, pos))
          
          toploteachcat <- eachcat %>% group_by(Category) %>%
            mutate(updown = perc - lag(perc)) %>%
            mutate(arrow = ifelse(updown>0, "↑", ifelse(updown<0, "↓", NA))) %>% 
            mutate(updowncolor = ifelse(updown <0, "blue", 
                                        ifelse(updown >0, "red", 
                                               "black"))) %>%
            mutate(sumMsign = ifelse(sumM-lag(sumM) > 0, "↑", "↓")) %>%
            group_by(MY) %>%
            mutate(MYcolor = ifelse(sumM>income_after_tax, "red", "black"))#each month spend
          
          
          toploteachcat[is.na(toploteachcat$updowncolor),]$updowncolor <- "black"
          toploteachcat[is.na(toploteachcat$sumMsign), ]$sumMsign <- ""
          toploteachcat[is.na(toploteachcat$updown), ]$updown <- 0
          toploteachcat[is.na(toploteachcat$arrow), ]$arrow <- ""
          
          
          #pie chart
          p5 <- ggplot(data=toploteachcat, aes(x=" ", y=perc, group=MY, fill=Category)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) + 
            facet_grid(.~ MY+paste0("$", sumM)) + theme_void() +
            theme(text=element_text(size=16,  family="Times New Roman"), plot.title = element_text(hjust = 0.5)) +
            scale_fill_manual(values=colorset3) +
            geom_label_repel(data = toploteachcat, aes(y = pos, 
                                                       label = paste0("$", sumY, " (", perc, "%", ")", arrow)), 
                             size=4, show.legend = F, nudge_x = 1) + ggtitle("Fun")
            
          
          #line graph
          p6 <- ggplot(data=toploteachcat, aes(x=MY, y=sumY, color=Category, group = Category)) +
            geom_line(size = 1) +
            geom_point() +
            scale_color_manual(values=colorset3) +
            theme_bw() +
            geom_text(aes(label=paste0(round(updown, 2), "%"), vjust=2)) +
            geom_text_repel(data = toploteachcat[toploteachcat$MY == unique(toploteachcat$MY)[order(unique(toploteachcat$MY), decreasing = T)][2],], 
                            aes(label=paste0("$",sumY)), vjust=2) +
            geom_text_repel(data = toploteachcat[toploteachcat$MY == max(toploteachcat$MY),], 
                            aes(label=paste0("$",sumY, " ", Category, arrow), hjust=-0.2)) +
            
            xlab("Year-month") + ylab("Amount ($)") +
            scale_x_discrete(labels=unique(paste0(toploteachcat$MY, "\n", "$",toploteachcat$sumM,toploteachcat$sumMsign)))
            
          
          plot_grid(p5, p6, labels = "AUTO", ncol = 1)
          ggsave(paste0("fun_pie_line",".png"),
                 scale = 1,
                 width = 30,
                 height = 20,
                 units = c("cm"),
                 dpi = 300)  
          
          
#future
          
          fdt <- dt[dt$BiggerCategory == "future",]
          fdt
          colorset4 <- viridis(length(unique(fdt$Category)))
          eachcat <- fdt %>% 
            group_by(MY, Category, add=TRUE) %>%
            summarize(sumY = sum(Amount, na.rm = T)) %>%
            mutate(sumM = sum(sumY),
                   perc = round(sumY*100/sum(sumY), 2)) %>%
            mutate(
              cs = rev(cumsum(rev(perc))), 
              pos = cumsum(perc) - perc/2,#perc/2 + lead(cs, 1),
              pos = if_else(is.na(pos), perc/2, pos))
          
          toploteachcat <- eachcat %>% group_by(Category) %>%
            mutate(updown = perc - lag(perc)) %>%
            mutate(arrow = ifelse(updown>0, "↑", ifelse(updown<0, "↓", NA))) %>% 
            mutate(updowncolor = ifelse(updown <0, "blue", 
                                        ifelse(updown >0, "red", 
                                               "black"))) %>%
            mutate(sumMsign = ifelse(sumM-lag(sumM) > 0, "↑", "↓")) %>%
            group_by(MY) %>%
            mutate(MYcolor = ifelse(sumM>income_after_tax, "red", "black"))#each month spend
          
          
          toploteachcat[is.na(toploteachcat$updowncolor),]$updowncolor <- "black"
          toploteachcat[is.na(toploteachcat$sumMsign), ]$sumMsign <- ""
          toploteachcat[is.na(toploteachcat$updown), ]$updown <- 0
          toploteachcat[is.na(toploteachcat$arrow), ]$arrow <- ""
          
          
          #pie chart
          p7 <- ggplot(data=toploteachcat, aes(x=" ", y=perc, group=MY, fill=Category)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) + 
            facet_grid(.~ MY+paste0("$", sumM)) + theme_void() +
            theme(text=element_text(size=16,  family="Times New Roman"), plot.title = element_text(hjust = 0.5)) +
            scale_fill_manual(values=colorset4) +
            geom_label_repel(data = toploteachcat, aes(y = pos, 
                                                       label = paste0("$", sumY, " (", perc, "%", ")", arrow)), 
                             size=4, show.legend = F, nudge_x = 1) + ggtitle("Future")
          
          #line graph
          p8 <- ggplot(data=toploteachcat, aes(x=MY, y=sumY, color=Category, group = Category)) +
            geom_line(size = 1) +
            geom_point() +
            scale_color_manual(values=colorset4) +
            theme_bw() +
            geom_text(aes(label=paste0(round(updown, 2), "%"), vjust=2)) +
            geom_text_repel(data = toploteachcat[toploteachcat$MY == unique(toploteachcat$MY)[order(unique(toploteachcat$MY), decreasing = T)][2],], 
                            aes(label=paste0("$",sumY)), vjust=2) +
            geom_text_repel(data = toploteachcat[toploteachcat$MY == max(toploteachcat$MY),], 
                            aes(label=paste0("$",sumY, " ", Category, arrow), hjust=-0.2)) +
            
            xlab("Year-month") + ylab("Amount ($)") +
            scale_x_discrete(labels=unique(paste0(toploteachcat$MY, "\n", "$",toploteachcat$sumM,toploteachcat$sumMsign)))
          
          
          plot_grid(p7, p8, labels = "AUTO", ncol = 1)
          ggsave(paste0("future_pie_line",".png"),
                 scale = 1,
                 width = 30,
                 height = 20,
                 units = c("cm"),
                 dpi = 300)   

#https://plotly.com/r/pie-charts/
#https://stackoverflow.com/questions/59672562/arrange-4-plotly-pie-graphs-in-r
          
          
#projection after buying a house
fixnorent <- dt[!(dt$Category %in% c("Rentals", "Health & Fitness")) & dt$BiggerCategory == "fixed",] %>% group_by(MY) %>% summarise(xbar = sum(Amount))
mean(fixnorent$xbar)
