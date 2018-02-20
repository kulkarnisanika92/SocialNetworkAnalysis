# Installing libraries
#install.packages("shiny")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("igraph")
#install.packages("networkD3")
#install.packages("sqldf")
#install.packages("DT")
#install.packages("shinythemes")

# Import libraries
library(shiny)
library(plyr)
library(dplyr)
library(igraph)
library(networkD3)
library(sqldf)
library(DT)
library(shinythemes)


ui <- fluidPage(
  
  titlePanel(
    "SanikaSatishKulkarni_Social_Network_Analysis"
  ),
  theme = shinythemes::shinytheme("cyborg"),
  sidebarLayout(
    sidebarPanel(
      p('For email connection data:'),
      fileInput(inputId = "file1",label = "Choose a file to upload",
                multiple = FALSE,
                accept = c("text/csv","text/comma-seperated-values,
                           text/plain",".csv")),
      p('For department label data:'),
      fileInput(inputId = "file2",label = "Choose a file to upload",
                multiple = FALSE,
                accept = c("text/csv","text/comma-seperated-values,
                           text/plain",".csv")),
      
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Space=' '), selected = " "
      ),
      numericInput("NoOfConnections","Enter the number of connections to be displayed",
                   value="10")
      
                ),
    mainPanel(
      tabsetPanel(
        tabPanel("Connections", simpleNetworkOutput("plot1")),
        tabPanel("Email Summary",
                 tabsetPanel(
                   tabPanel("Total Emails Sent per person",  shiny :: dataTableOutput("table2")),
                   tabPanel("Total Emails Received per person", shiny :: dataTableOutput("table3"))
                 )
        ),
        
        tabPanel("2-hop neighbors", 
                 tabsetPanel(
                   tabPanel("For top 10 by Email Sent",simpleNetworkOutput("plot2")),
                   tabPanel("For top 10 by Email Received", simpleNetworkOutput("plot3"))
                 )),
        tabPanel("Visualization",
                 tabsetPanel(
                   tabPanel("Degree Centrality", forceNetworkOutput("plot4")),
                   tabPanel("Betweeness Centrality", forceNetworkOutput("plot5")),
                   tabPanel("Top 10 indegree Centrality", forceNetworkOutput("plot6"))
                 )),
        tabPanel("Department", 
                 tabsetPanel(
                   tabPanel("Table", shiny :: dataTableOutput("table4")),
                   tabPanel("Visualization", forceNetworkOutput("plot7"))
                 )
        )
      )
      
    )
      )
)


server <- function(input, output) {
  
  # Stored Excel sheet data(Email data) in reactive variable for multiple use   
  excelsheet1 <- reactive({
    req(input$file1)
    my_data1 <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, col.names = c("EmailSent","EmailReceived"))
    return(data.frame(my_data1))
  })
  
  # Stored Excel sheet data(Department data) in reactive variable for multiple use
  excelsheet2 <- reactive({
    req(input$file2)
    my_data2 <- read.csv(input$file2$datapath, sep = input$sep , header = input$header, col.names = c("Individual","Department"))
    return(data.frame(my_data2))
  })
  
  
  
  
  # Display n connections  n = user input
  
  output$plot1 <- renderSimpleNetwork({
    my_data <- as.data.frame(excelsheet1())
    network <- head(my_data, n=input$NoOfConnections)
    simpleNetwork(network,fontSize = 14, zoom = FALSE, opacity = 0.85,
                  height = 500, width = 1000)
  })
  
  # computed total number of emails sent by a person
  output$table2 <- shiny :: renderDataTable({
    
    my_data <- as.data.frame(excelsheet1())
    sentMail <- plyr:: count(my_data, "EmailSent")
    names(sentMail) <- c("Person","Email Count")
    return(sentMail)
  }, options = list(pageLength = 9))
  
  
  # computed total numberof emails receied by a person
  output$table3 <- shiny :: renderDataTable({
    my_data <- as.data.frame(excelsheet1())
    receivedmail <- plyr:: count(my_data, "EmailReceived")
    names(receivedmail) <- c("Person", "Email Count")
    return(receivedmail)
  }, options = list(pageLength = 9))
  
  
  # Function to compute 2 hop neighbors:input dataframe and top 10 vectors
  # output: upto 2 hop neighbors for vector nodes
  # currently calculating only 3 neighbors for each node
  neighbornodes <- function(my_data,v){
    d1 <- subset(my_data,EmailSent %in% v)
    subset1 <- d1 %>% group_by(EmailSent) %>% top_n(n = 3)
    v2 <- unlist(subset1[,2])
    d2 <- subset(my_data,EmailSent %in% v2)
    subset2 <- d2 %>% group_by(EmailSent) %>% top_n(n = 3)
    neighborData <- merge(subset1,subset2, all = T)
    return(data.frame(neighborData))
  }
  
  # Visualized 2 hop neighbors for top 10 email sent
  output$plot2 <- renderSimpleNetwork({
    my_data <- as.data.frame(excelsheet1())
    sentMail <- plyr::count(my_data, "EmailSent")
    names(sentMail) <- c("Person","Frequency")
    
    # calculate rank based on frequency
    sentMail$Rank <- rank(-sentMail$Frequency, ties.method = "min")
    RankSentMail <- sentMail[order(sentMail$Rank, decreasing = F),]
    
    # Selected top 10 nodes based on frequency of email sent
    topSent <- head(RankSentMail[,1],10)
    v <- unlist(topSent[])
    
    # called 2 hop neighbor function for finding neighbors
    hopneighbors <- as.data.frame(neighbornodes(my_data = my_data,v))
    simpleNetwork(hopneighbors, fontSize = 14, opacity = 0.85)
  })
  
  # Visualized 2 hop neighbors of top 10 email received
  output$plot3 <- renderSimpleNetwork({
    my_data <- as.data.frame(excelsheet1())
    receivedmail <- plyr:: count(my_data, "EmailReceived")
    names(receivedmail) <- c("Person","Frequency")
    receivedmail$Rank <- rank(-receivedmail$Frequency, ties.method = "min")
    receivedmail <- receivedmail[order(receivedmail$Rank, decreasing = F),]
    
    # Selected top 10 nodes based on frequency of email sent
    topReceived <- head(receivedmail[,1],10)
    v <- unlist(topReceived[])
    
    # Called neighbors function to find upto 2 hop neighbors for top 10
    hopneighbors <- as.data.frame(neighbornodes(my_data = my_data,v))
    simpleNetwork(hopneighbors, fontSize = 14, opacity = 0.85)
  })
  
  # Visualized 2 hop neighbors for top 10 degree centrality
  output$plot4 <- renderForceNetwork({
    my_data <- as.data.frame(excelsheet1())
    dept_data <- as.data.frame(excelsheet2())
    g <- graph.data.frame(my_data, directed = FALSE)
    
    # calculated degree for each node
    deg <- degree(g)
    sortedDegree <- sort(degree(g), decreasing = T)[1:10]
    top <- data.frame("Name"=V(g)$name,"Degree"=deg)
    
    # Selected top 10 nodes based on degree centrality
    top <- subset(top,top$Degree %in% sortedDegree)
    v <- unlist(top[,1])
    hopneighbors <- as.data.frame(neighbornodes(my_data = my_data,v))
    
    simpleNetwork(hopneighbors, Source = "EmailSent", Target = "EmailReceived")
    sources <- hopneighbors$EmailSent
    targets <- hopneighbors$EmailReceived
    group_names <- dept_data$Department
    node_names <- factor(sort(unique(c(as.character(sources), as.character(targets)))))
    group_data <- factor(sort(unique(c(as.character(node_names)))))
    nodes <- data.frame(name = node_names, group = group_data)
    links <- data.frame(source = match(sources, node_names) -1,
                        target = match(targets, node_names) -1,
                        value = 1
    )
    
    forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                 NodeID = "name", Group = "group", Value = "value", opacity = 1, fontSize = 16,
                 height = 500, width = 1000, zoom = TRUE, opacityNoHover = 0.1
    )
    
  })
  
  # Visualized 2 hop neighbors for top 10 betweeness centrality
  output$plot5 <- renderForceNetwork({
    my_data <- as.data.frame(excelsheet1())
    dept_data <- as.data.frame(excelsheet2())
    g <- graph.data.frame(my_data, directed = FALSE)
    
    # Calculated betweeness centrality for each node
    betDegree <- betweenness(g)
    sortedDegree <- sort(betDegree, decreasing = T)[1:10]
    top <- data.frame("Name"=V(g)$name,"Degree"=betDegree)
    
    # Selected top 10 nodes based on betweeness centrality
    top <- subset(top, top$Degree %in% sortedDegree)
    v <- unlist(top[,1])
    hopneighbors <- as.data.frame(neighbornodes(my_data = my_data,v))
    
    simpleNetwork(hopneighbors, Source = "EmailSent", Target = "EmailReceived")
    sources <- hopneighbors$EmailSent
    targets <- hopneighbors$EmailReceived
    group_names <- dept_data$Department
    node_names <- factor(sort(unique(c(as.character(sources), as.character(targets)))))
    group_data <- factor(sort(unique(c(as.character(node_names)))))
    nodes <- data.frame(name = node_names, group = group_data)
    links <- data.frame(source = match(sources, node_names) -1,
                        target = match(targets, node_names) -1,
                        value = 1
    )
    forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                 NodeID = "name", Group = "group", Value = "value", opacity = 1, fontSize = 16,
                 height = 500, width = 1000, zoom = TRUE, opacityNoHover = 0.1)
    
  })
  
  # Visualized 2 hop neighbors for top 10 indegree centrality
  output$plot6 <- renderForceNetwork({
    my_data <- as.data.frame(excelsheet1())
    dept_data <- as.data.frame(excelsheet2())
    g <- graph.data.frame(my_data, directed = TRUE)
    
    # Calculated indegree centrality for each mode
    deg <- degree(g, mode = "in")
    sortedDegree <- sort(deg, decreasing = T)[1:10]
    top <- data.frame("Name"=V(g)$name,"Degree"=deg)
    
    # Selected top 10 nodes based on indegree centrality
    top <- subset(top, top$Degree %in% sortedDegree)
    v <- unlist(top[,1])
    
    # Called neighbornodes function for finding neighbor nodes
    hopneighbors <- as.data.frame(neighbornodes(my_data = my_data,v))
    
    simpleNetwork(hopneighbors, Source = "EmailSent", Target = "EmailReceived")
    sources <- hopneighbors$EmailSent
    targets <- hopneighbors$EmailReceived
    group_names <- dept_data$Department
    node_names <- factor(sort(unique(c(as.character(sources), as.character(targets)))))
    group_data <- factor(sort(unique(c(as.character(node_names)))))
    nodes <- data.frame(name = node_names, group = group_data)
    links <- data.frame(source = match(sources, node_names) -1,
                        target = match(targets, node_names) -1,
                        value = 1
    )
    forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                 NodeID = "name", Group = "group", Value = "value", opacity = 1, fontSize = 16,
                 height = 500, width = 1000, zoom = TRUE, opacityNoHover = 0.1)
    
  })
  
  # Function to compute total number of emails sent and received at department level
  TotalEmail <- function(){
    emailsent <- inner_join(excelsheet1(),excelsheet2(),by = c("EmailSent" = "Individual"))
    emailAll <- inner_join(emailsent,excelsheet2(), by = c("EmailReceived" = "Individual"))
    names(emailAll) <- c("EmailSent","EmailReceived","EmailFromDept","EmailToDept")
    dept_data <- data.frame(sqldf("select EmailFromDept,EmailToDept, count(*) as TotalEmailSent from emailAll group by EmailFromDept, EmailToDept"))
    return(dept_data)
  }
  
  # Computed total number of email sent and received at Department level
  output$table4 <- shiny :: renderDataTable({
    return(TotalEmail())
  }, options = list(pageLength = 9))
  
  # Visualized email sent and received at Department level
  output$plot7 <- renderForceNetwork({
    my_data <- as.data.frame(excelsheet1())[1:100,]
    dept_data <- as.data.frame(excelsheet2())
    simpleNetwork(my_data, Source = "EmailSent", Target = "EmailReceived")
    sources <- my_data$EmailSent
    targets <- my_data$EmailReceived
    group_names <- dept_data$Department
    node_names <- factor(sort(unique(c(as.character(sources), as.character(targets)))))
    group_data <- factor(sort(unique(c(as.character(node_names)))))
    nodes <- data.frame(name = node_names, group = group_data)
    links <- data.frame(source = match(sources, node_names) -1,
                        target = match(targets, node_names) -1,
                        value = 1
                        
                        
    )
    forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                 NodeID = "name", Group = "group", Value = "value", opacity = 1, fontSize = 16,
                 height = 500, width = 1000, zoom = TRUE, opacityNoHover = 0.1,
                 clickAction = TRUE, arrows = TRUE)
    
  })
  
}
shinyApp(ui = ui, server = server)
