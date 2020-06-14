#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyapp)
install.packages("shinyapp")
world.vtb<-readRDS("world.vtb.rds")
# Define UI for application that draws a histogram
ui<-fluidPage(
    
    # Application title
    titlePanel("COVID-19 Case Report Number Corrector"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(position = "left",
    sidebarPanel(
        selectInput("act_country", "Country", world.vtb$iso_code, "USA")
        ),
    
    #Show a plot of the generated distribution
    mainPanel(
        fluidRow(column(8,
                    plotOutput("actplot")), 
                 
        fluidRow(column(8,
                 
                 plotOutput("cumplot"))
           
    )
    
    ))))


server<-function(input, output) {
    
    potential.case<-function(active_country, data=world.vtb){
        world.vtb$active_country<-active_country
        bench<-world.vtb%>%filter(iso_code=="KOR")
        testttt<-world.vtb%>%dplyr::group_by(iso_code)%>%
            dplyr:: mutate(scaling=death.rate/(vtb* sum(bench$death.rate) / length(bench$death.rate)),
                           potential_cases=ifelse(is.na(confirmed.d*scaling), 0,confirmed.d*scaling))%>%
            dplyr:: mutate(cumulative_potentialcase=cumsum(potential_cases))%>%ungroup()%>%
            dplyr::filter(iso_code==active_country)%>%
            reshape2::melt(id="date", measure.vars = c("active", "confirmed", "deceased", "recovered", "confirmed.d",
                                                       "recovered.d", "deceased.d", "death.rate", "cumulative_potentialcase"))%>%
            dplyr::filter(variable%in%c("confirmed", "cumulative_potentialcase"))
        
        g<-ggplot(data=testttt)+
            geom_line(aes(x=date, y=value/100000, color=variable))+xlab("Time")+ylab("Cases, per hundred thousand")+
            scale_color_manual(labels = c("Confirmed cases", "Potential cases"), values = c("blue", "red"))+
            ggtitle(paste0('Potential cases of ', active_country))+theme_bw()+theme(aspect.ratio=3/4)
        print(g)
        
    }
    
    output$actplot <- renderPlot({potential.case(input$act_country, data=world.vtb) })
    
    
    cumulative.case<-function(active_country, data=world.vtb){
        world.vtb$active_country<-active_country
        testttt<-world.vtb%>%dplyr::group_by(iso_code)%>%
            dplyr:: mutate(scaling=death.rate/(vtb* sum(bench$death.rate) / length(bench$death.rate)),
                           potential_cases=ifelse(is.na(confirmed.d*scaling), 0,confirmed.d*scaling))%>%
            dplyr:: mutate(cumulative_potentialcase=cumsum(potential_cases))%>%ungroup()%>%dplyr::filter(iso_code==active_country)%>%
            reshape2::melt(id="date", measure.vars = c("active", "confirmed", "deceased", "recovered", "confirmed.d",
                                                       "recovered.d", "deceased.d", "death.rate", "cumulative_potentialcase"))%>%
            dplyr::filter(variable%in%c("confirmed", "recovered", "deceased" ))
        
        g1<-ggplot(data=testttt)+
            geom_line(aes(x=date, y=value/100000, color=variable))+xlab("Time")+ylab("Cases, per hundred thousand")+
            scale_color_manual(labels = c("Confirmed cases",  "Death","Recovered"), values = viridis(3))+
            ggtitle(paste0('COVID-19 epidemic in ', active_country))+theme_bw()+theme(aspect.ratio=3/4)
        print(g1)
        
    }
    
    output$cumplot <- renderPlot({cumulative.case(input$act_country, data=world.vtb) })
   
    
    
}
shinyApp(ui, server)

