#REQUIRED LIBRARIES
library(datasets);library(plotly);library(plotrix);library(NLP);library(RColorBrewer);library(tm);library(dplyr);library(wordcloud);library(wordcloud2)
library(ggplot2);library(syuzhet);library(quanteda);library("SnowballC");library(shiny);library(shinydashboard);library(DT);library(shinyjs);library(stringr);library(scatterplot3d);library(shinyalert);library(shinyBS);
library(shinyAce);library(mailR);library(devtools);library(sendmailR);library(sqldf);library(RSQLite)

#Shiny server functionality starts here
shinyServer(function(input, output,session) {
  
  db <- dbConnect(SQLite(), dbname="AppAdministrators.db")
  
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$login, {
    if(input$username!="" && input$password!=""){
      c<-sqldf("SELECT * FROM DataManagers ", dbname = "AppAdministrators.db")  
      nrow(c)
      start1<- 0;
      
      for (row in 1:nrow(c)) {
        username <- c[row, "Username"]
        passwordd  <- c[row, "Password"]
        t<- grepl(input$username, username)
        f<- grepl(input$password, passwordd)
        if(t == TRUE & f == TRUE) {
          start1<- start1+1
        } 
      }
      if(start1>0){
        USER$Logged <- TRUE 
      }
      else{
        output$message = renderText("Invalid user name or password,please try again!")
        show("message")
        delay(10000, hide("message", anim = TRUE, animType = "fade")) 
      }
    }
    else{
      output$message = renderText("Enter user name or password,please!!!")
      
      show("message")
      
    }
  })
  observeEvent(input$logout,{
    USER$Logged<-FALSE
  })
  
  output$ourAPP <- renderUI(
    if (!isTRUE(USER$Logged)) {
      
      fluidRow(column(width=9, offset = 4,
                      wellPanel
                      ( tags$div(tags$h3("APP MANAGER LOGIN "),align="center",style="background:limegreen;"),id = 'panel_login',
                                textInput('username', 'USER NAME:'),
                                passwordInput('password', 'PASSWORD:'),
                                div(actionButton('login', 'LOGIN'),actionButton('exit', 'EXIT'), style='text-align: center;')
                      ),
                      textOutput("message")
      ))
    } else {
                   fluidPage(theme="css.css", fileInput("file","Upload file")    
                                 
                               )
                                fluidRow(
                                useShinyjs(),
                                actionButton("logout","LOGOUT"),  
                                tabsetPanel(id="navbar1",
                                                           tabPanel("HOME",uiOutput("home"),
                                                           textOutput("check"),textOutput("check1"),align='center',style="font-size:50px;margin-top:85px;font-family:Times New Roman;text-shadow:5px 5px 10px #00FF00;"),
                                                           tabPanel("UPLOAD DATASET",uiOutput("put_file")),
                                                           tabPanel("VIEW DATA",tableOutput("uploaded_data"),style="margin-left:2.5%"),
                                                           tabPanel("CHAT VISUALS",uiOutput("statistics")),
                                                           tabPanel("SENTIMENT ANALYSIS",uiOutput("simple")),
                                                           tabPanel("DEPARTMENT ANALYSIS",uiOutput("department")),
                                                           tabPanel("DOWNLOAD",uiOutput("downloads")),
                                                           tabPanel("HELP",uiOutput("help"))
                                               
                                   )
                                 )
                                 
          
    } )
  
    output$home<-renderUI(
                          wellPanel(paste("WELCOME",input$username,"TO CAA"))
                            )
    
    
  #------uploading the dataset--------  
  output$put_file<-renderUI({
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("file","Upload file"),
      
        tags$br(),tags$br(), tags$br()  
      ),
      mainPanel()
    )
  }) 
  #---uploading dataset ends here-----
    #===SAVING DOWNLOADS================
  output$downloads<-renderUI({
    sidebarLayout(
      sidebarPanel(width = 3,
                   useShinyalert(),
                   fluidRow(
                     selectInput("nameit","Download Model",choices=c("select download","PIE CHART","BAR GRAPH","WORDCLOUD","CHAT","TIME","ANALYSIS OF EMOTIONS","COUNTRY","SCATTER PLOT")),
                     actionButton("down","Download"),style="margin-left:10%;margin-top:10.5%"
                   )
          
                   
      ),
      mainPanel()
    )
  }) 
    
  #Rendering data in the table    
  output$uploaded_data<-renderTable({
    req(data()) },height = 400,width = 1000)
  data <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return()
    }
    info<-read.csv(file=file1$datapath)
    info[is.na(info)]<-0
    return(info)
  })
  
  output$sum <- renderTable({
    if (is.null(data())) {
      return()
    }
    summary(data())[,1:1]
  }) 
  
  #plotting Bar graph using this funtion
  
  output$bargraph<-renderPlot({
    mydata<- req(data())
    operatives<-c('SimonPeter','atuhaire','PaulOchen','Joseph','Stellamaris')
    dv<-c('Simon Peter  Engoru','atuhaire elizabeth','Paul Ochen','joseph banyu','stellamaris Nabisere')
    rt<- 0;jo<- 0;st<- 0;si<- 0;at<- 0; pa<- 0;
    
    for(i in 1:length(dv)){
      for(row in 1:nrow(mydata)) { 
        price <- mydata[row, "Chat.content"]
        t<- grepl(dv[i], price)
        if(t == TRUE ) { rt<- rt+1 } }
      
      if(dv[i]=='joseph banyu'){ jo<-rt  }else if(dv[i]=='stellamaris Nabisere'){
        st<-(rt-jo)  
      } else if(dv[i]=='Simon Peter  Engoru'){ si<-(rt-jo-st)
      } else if(dv[i]=='atuhaire elizabeth'){ 
      } else if(dv[i]=='Paul Ochen'){
        pa<-(rt-jo-st-si-at) }   
    } 
    print(jo) ; print(st); print(si);print(at); print(pa); x = c(jo,st,si,at,pa) 
    real<-data.frame(operatives,x)
    real
    barplot(real$x, las = 1,
            main ="BAR GRAPH SHOWING NUMBER OF RESPONSES,EMPLOYEES CONDUCTED WITH IN CHAT ",ylab = "Number of Responses",
            legend =operatives, 
            col = c("linen","grey","red","orange","powderblue")) 
    
    
  })
  
  
  #plotting pie chart using this funtion
  output$piechart<-renderPlot({
    mydata<-req(data())
    x<-"ID"
    y<-"Department"
    piePlotData = aggregate(formula(paste0(".~",y)), mydata, sum)
    percent<-(piePlotData[[x]]/sum(piePlotData[[x]]))*100
    percent<-round(percent,2)
    labels<-piePlotData[[y]]
    pie3D(piePlotData[[x]], main="Department Analysis",labels=paste(labels,percent,"%"),theta=pi/6,mar=c(0,0,0,0)     
          )

  })
  
  #Filtering and cleaning the data goes here
  
  output$wordcloud<-renderPlot({
    mydata<-req(data())
    docs <- Corpus(VectorSource(mydata$Chat.content))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    docs <- tm_map(docs, content_transformer(tolower))
    
    docs <- tm_map(docs, removeNumbers)
    
    docs <- tm_map(docs, removeWords, stopwords("english"))
    wordz<-c("chat","can","get","newvision","www","uganda","system","https"
             ,"the","you","vision","group","engoru","stellamaris","eatuhaire"
             ,"assistant","joseph","simon","peter","vizuri.visiongroup.co.ug",
             "www.visiongroup.co.ug","banyu","newvision.co.ug","closed","accepted",
             "live","elizabeth","atuhaire","support","http","paul","josephbanyu","jbanyu",
             "ochen","nabisere","peter","office","article","gmailcom","inquiries","bukedde",
             "click","directlysengoru","number","june","letter","daniel","stellalule",
             "vpgvisiongroupcoug","samuel","denis","newspaper","anjana","also","phone","advertising",
             "library","flippaper","sengoru","website","anonymous","head","advertisement","coug",
             "pochen","respective","advertisements","papers","able","mobile","online","visitor",
             "gmailcom","time","link","via","contact","julius","stella","adverts","wasswa","company",
             "desktop","app","one","coug","epaper","various","companies","going","francis",
             "close","list","date","muhindo","look","sign","paper","links","specific","news",
             "account","new","using","category","visiongroup","may","money","com","gmail","websites",
             "co","ug","made","much","vpg","well"
             
    )
    docs <- tm_map(docs, removeWords,wordz) 
    
    docs <- tm_map(docs, removePunctuation)
    
    docs <- tm_map(docs, stripWhitespace)
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq =20 , max.words=200, 
              random.order=FALSE, rot.per=0.2,colors=brewer.pal(8, "Dark2"))
    
    
    
    
  })
  output$simple<-renderUI({
    
    sidebarLayout(
      sidebarPanel(id="sidebar",
                   selectInput("chat", "SENTIMENT ANALYSIS",
                               c("select analysis tool","WORDCLOUD","CHAT","ANALYSIS OF EMOTIONS","COUNTRY","TIME","SCATTER PLOT"), "Graphics")            
                   
      ),
      
      mainPanel(id="mypanel",
            
                plotOutput("kk")
      )
    ) 
    
    
  })
  
  output$kk <- renderPlot({
    
    if (input$chat == "WORDCLOUD") { 
      mydata<-req(data())
      docs <- Corpus(VectorSource(mydata$Chat.content))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      
      docs <- tm_map(docs, content_transformer(tolower))
      
      docs <- tm_map(docs, removeNumbers)
      
      docs <- tm_map(docs, removeWords, stopwords("english"))
      wordz<-c("chat","can","get","newvision","www","uganda","system","https" ,"the","you","vision","group","engoru","stellamaris","eatuhaire","assistant","joseph","simon","peter","vizuri.visiongroup.co.ug",
               "www.visiongroup.co.ug","banyu","newvision.co.ug","closed","accepted",
               "live","elizabeth","atuhaire","support","http","paul","josephbanyu","jbanyu",
               "ochen","nabisere","peter","office","article","gmailcom","inquiries","bukedde",
               "click","directlysengoru","number","june","letter","daniel","stellalule",
               "vpgvisiongroupcoug","samuel","denis","newspaper","anjana","also","phone","advertising",
               "library","flippaper","sengoru","website","anonymous","head","advertisement","coug",
               "pochen","respective","advertisements","papers","able","mobile","online","visitor",
               "gmailcom","time","link","via","contact","julius","stella","adverts","wasswa","company",
               "desktop","app","one","coug","epaper","various","companies","going","francis",
               "close","list","date","muhindo","look","sign","paper","links","specific","news",
               "account","new","using","category","visiongroup","may","money","com","gmail","websites",
               "co","ug","made","much","vpg","well"
               
               )
      docs <- tm_map(docs, removeWords,wordz) 
      
      docs <- tm_map(docs, removePunctuation)
      
      docs <- tm_map(docs, stripWhitespace)
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      
     set.seed(1234)
     wordcloud(words = d$word, freq = d$freq, min.freq =20 , max.words=200, 
               random.order=FALSE, rot.per=0.2,colors=brewer.pal(8, "Dark2"))
     
       
    } 
    else if(input$chat=="ANALYSIS OF EMOTIONS"){
      mydata<- req(data())
      comments<-iconv(mydata$Chat.content)
      s<-get_nrc_sentiment(comments)
      
      barplot(colSums(s), las=2, col=rainbow(10) ,ylab = 'Emotional level' ,
              main = 'BARGRAPH SHOWING EMOTIONAL REACTIONS OF CUSTOMERS',
                    legend = c("Anger", "Anticipation","Disgust","Fear","Joy","Sadness","Suprise",
                                "Trust","Negative","Positive"), 
                    fill = c("red","orange","yellow","lightgreen","green","blue","blue","blue","purple","pink"),lwd=1)
      
    }
    else if(input$chat=="OTHER ANALYSIS"){
      sidebarLayout(
        sidebarPanel(
          
          
          selectInput("date", "select any date to find out number of chats:",
                      c("2017-07-07" = "2017-07-07","2017-07-06" = "2017-07-06",  "2017-07-05" = "2017-07-05","2017-07-04" = "2017-07-04","2017-07-03" = "2017-07-03", "2017-07-02" = "2017-07-02",
                        "2017-07-01" = "2017-07-01",
                        "2017-06-30" = "2017-06-30",
                        "2017-06-29" = "2017-06-29",
                        "2017-06-28" = "2017-06-28",
                        "2017-06-27" = "2017-06-27",
                        "2017-06-26" = "2017-06-26",
                        "2017-06-25" = "2017-06-25",
                        "2017-06-24" = "2017-06-24",
                        "2017-06-23" = "2017-06-23",
                        "2017-06-22" = "2017-06-22",
                        "2017-06-21" = "2017-06-21",
                        "2017-06-20" = "2017-06-20",
                        "2017-06-19" = "2017-06-19",
                        "2017-06-18" = "2017-06-18",
                        "2017-06-17" = "2017-06-17",
                        "2017-06-16" = "2017-06-16",
                        "2017-06-15" = "2017-06-15",
                        "2017-06-14" = "2017-06-14",
                        "2017-06-13" = "2017-06-13",
                        "2017-06-12" = "2017-06-12",
                        "2017-06-11" = "2017-06-11",
                        "2017-06-10" = "2017-06-10",
                        "2017-06-09" = "2017-06-09",
                        "2017-06-08" = "2017-06-08",
                        "2017-06-07" = "2017-06-07",
                        "2017-06-06" = "2017-06-06",
                        "2017-06-05" = "2017-06-05",
                        "2017-06-04" = "2017-06-04",
                        "2017-06-03" = "2017-06-03",
                        "2017-06-02" = "2017-06-02",
                        "2017-06-01" = "2017-06-01"
                      )
          )
          
          
        ),
        
        mainPanel(id="mypanel",
          
          
          uiOutput("result"),
          uiOutput("datedisplay")
        )
      ) 
      
      
    }
    else if(input$chat=="CHAT"){
      mydata<-req(data())
      docs <- Corpus(VectorSource(mydata$Chat.content))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      
      docs <- tm_map(docs, content_transformer(tolower))
      
      docs <- tm_map(docs, removeNumbers)
      
      docs <- tm_map(docs, removeWords, stopwords("english"))
      wordz<-c("chat","can","get","newvision","www","uganda","system","https"
               ,"the","you","vision","group","engoru","stellamaris","eatuhaire"
               ,"assistant","joseph","simon","peter","vizuri.visiongroup.co.ug",
               "www.visiongroup.co.ug","banyu","newvision.co.ug","closed","accepted",
               "live","elizabeth","atuhaire","support","http","paul","josephbanyu","jbanyu",
               "ochen","nabisere","peter","office","article","gmailcom","inquiries","bukedde",
               "click","directlysengoru","number","june","letter","daniel","stellalule",
               "vpgvisiongroupcoug","samuel","denis","newspaper","anjana","also","phone","advertising",
               "library","flippaper","sengoru","website","anonymous","head","advertisement","coug",
               "pochen","respective","advertisements","papers","able","mobile","online","visitor",
               "gmailcom","time","link","via","contact","julius","stella","adverts","wasswa","company",
               "desktop","app","one","coug","epaper","various","companies","going","francis",
               "close","list","date","muhindo","look","sign","paper","links","specific","news",
               "account","new","using","category","visiongroup","may","money","com","gmail","websites",
               "co","ug","made","much","vpg","well"
               
      )
      docs <- tm_map(docs, removeWords,wordz) 
      
      docs <- tm_map(docs, removePunctuation)
      
      docs <- tm_map(docs, stripWhitespace)
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
  
      barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,col ="blue", main ="BAR GRAPH SHOWING 20 MOST USED WORDS IN THE CHAT CONTENT",ylab = "Word frequencies")
      
      
      
    }
    else if(input$chat=="COUNTRY"){
      mydata <- req(data())
      barplot(table(mydata$Country),
              main="Number of participants from different Countries",
              xlab="Country",
              ylab="Number of participates",
              border="red",
              las = 2,
              col="blue",
              density=50
      )
      
    }
    else if(input$chat=="SCATTER PLOT"){
      mydata<-req(data())
      mydata$Minutes<- as.POSIXct(mydata$Minutes, format="%H:%M")
      plot(mydata$Minutes, mydata$Wait.time/1000, xaxt='s', pch=10, las=1, xlab='Chat Starting Time(in minutes)', ylab='Waiting Time/1000')
      axis.POSIXct(1, mydata$Minutes, format="%H:%M")
    }
    else if(input$chat=="TIME"){
      mydata <- req(data())
      sum0<-0;sum1<-0;sum2<-0;sum3<-0;sum5<-0;sum6<-0;sum7<-0;sum8<-0; sum9<-0; sum10<-0;sum11<-0;sum12<-0; sum13<-0;sum14<-0;sum15<-0;sum16<-0;sum17<-0;sum18<-0; sum19<-0;sum20<-0; sum21<-0;sum22<-0; sum23<-0
      for (row in 1:nrow(mydata)) {
        m<- mydata[row, "Minutes"]
        if(str_extract(m,'^.{2}')=="00"){sum0<-sum0+1
        }
        else if(str_extract(m,'^.{2}')=="01"){sum1<-sum1+1
        }
        else if(str_extract(m,'^.{2}')=="02"){ sum2<-sum2+1
        }
        else if(str_extract(m,'^.{2}')=="03"){ sum3<-sum3+1
        }
        else if(str_extract(m,'^.{2}')=="04"){ sum4<-sum4+1
        }
        else if(str_extract(m,'^.{2}')=="05"){sum5<-sum5+1
        }
        else if(str_extract(m,'^.{2}')=="06"){ sum6<-sum6+1
        }
        else if(str_extract(m,'^.{2}')=="07"){sum7<-sum7+1
        }
        else if(str_extract(m,'^.{2}')=="08"){ sum8<-sum8+1
        }
        else if(str_extract(m,'^.{2}')=="09"){ sum9<-sum9+1
        }
        else if(str_extract(m,'^.{2}')=="10"){sum10<-sum10+1
        } 
        else if(str_extract(m,'^.{2}')=="11"){sum11<-sum11+1
        }
        else if(str_extract(m,'^.{2}')=="12"){ sum12<-sum12+1
        }
        else if(str_extract(m,'^.{2}')=="13"){ sum13<-sum13+1
        }
        else if(str_extract(m,'^.{2}')=="14"){sum14<-sum14+1
        }
        else if(str_extract(m,'^.{2}')=="15"){ sum15<-sum15+1
        }
        else if(str_extract(m,'^.{2}')=="16"){ sum16<-sum16+1
        }
        else if(str_extract(m,'^.{2}')=="17"){ sum17<-sum17+1
        }
        else if(str_extract(m,'^.{2}')=="18"){ sum18<-sum18+1
        }
        else if(str_extract(m,'^.{2}')=="19"){ sum19<-sum19+1
        }
        else if(str_extract(m,'^.{2}')=="20"){  sum20<-sum20+1
        }
        else if(str_extract(m,'^.{2}')=="21"){ sum21<-sum21+1
        }
        else if(str_extract(m,'^.{2}')=="22"){ sum22<-sum22+1
        }
        else if(str_extract(m,'^.{2}')=="23"){ sum23<-sum23+1
        }
        
      }
      
      pe<-c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
      xx<-c(sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17,sum18,sum19,sum20,sum21,sum22,sum23)
      rea<-data.frame(pe,xx)
      rea
      barplot(rea$xx, las = 1, names.arg = rea$pe,col ="powderblue", main ="Number of Chats Started between 00:00 to 23:60",ylab = "Number of Chats",xlab = "Hours in 24 hour clock") 
      x<-sum(sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17,sum18,sum19,sum20,sum21,sum22,sum23)
   
    }
  
  }) 
  
  output$department<-renderUI({
    sidebarLayout(
      sidebarPanel(
        selectInput("name", "Choose Department:", c("Hot News" = "Hot News", "customer support" = "customer support", "Advertising" = "Advertising", "Editorial" = "Editorial"))),
         mainPanel( plotOutput("department_analysis") ) ) })   
  
 
  observeEvent(input$send, {
    sender <- input$from
    recipient <- input$to
    subject = input$subject
    body = input$message_body
    
  })
  
 
  observeEvent(input$logout,{
    USER$Logged <- FALSE

  })
  observeEvent(input$exit, {
    stopApp(returnValue = invisible())
  })
  
  
  observe({
    if(is.null(input$send) || input$send==0) return(NULL)
    from <- isolate(input$from)
    to <- isolate(input$to)
    subject <- isolate(input$subject)
    msg <- isolate(input$message_body)
    sendmail(from, to, subject, msg)
  })
  
  output$result <- renderText({
    mydata<- req(data())
    rt<- 0;
    
    for (row in 1:nrow(mydata)) {
      price <- mydata[row, "Chat.content"]
      
      t<- grepl(input$assistant, price)
      if(t == TRUE) {
        
        rt<- rt+1
        
        
      }
      
      
    }
    
    paste(input$assistant," was involved in ",rt," chats")
    
    
  })
  
  
  output$datedisplay <- renderText({
    mydata<- req(data())
    r<- 0;
    
    for (row in 1:nrow(mydata)) {
      da <- mydata[row, "Chat.content"]
      
      h<- grepl(input$date, da)
      if(h == TRUE) {
        
        r<- r+1
        
      }
      
    }
    
    paste(input$date," had ",r," chats")
  })
  
  output$statistics<-renderUI({
    
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput("select_model", "STATISTICS",
                    c("select analysis tool","DATA SUMMARY","PIE CHART","BAR GRAPH"), "stat")
        
        
        
      ),
      
      mainPanel(id="xxxx",
         uiOutput("pp")      
      )
    )
   
    
  })
  
  
  output$pp<-renderUI({
    
    if(input$select_model=="PIE CHART"){
      plotOutput("piechart")
    }
    else if(input$select_model=="BAR GRAPH"){
      plotOutput("bargraph")
    }
   
    else if(input$select_model=="DATA SUMMARY"){
      tableOutput("summary")
    }
    
    
    
  })
  output$summary<-renderTable({
  
    summary(data())
    
  })
  
  
  
  
  
  output$other<-renderUI({
    
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput("assistant", "Choose System assistant:",
                    c("System assistant:","joseph banyu" = "joseph banyu",
                      "stellamaris Nabisere" = "stellamaris Nabisere",
                      "Simon Peter  Engoru" = "Simon Peter  Engoru",
                      "atuhaire elizabeth" = "atuhaire elizabeth",
                      "Paul Ochen" = "Paul Ochen"
                    )
        ),
        
        selectInput("date", "Choose Date:",
                    c("Date","2017-07-07" = "2017-07-07",
                      "2017-07-06" = "2017-07-06",
                      "2017-07-05" = "2017-07-05",
                      "2017-07-04" = "2017-07-04",
                      "2017-07-03" = "2017-07-03",
                      "2017-07-02" = "2017-07-02",
                      "2017-07-01" = "2017-07-01",
                      "2017-06-30" = "2017-06-30",
                      "2017-06-29" = "2017-06-29",
                      "2017-06-28" = "2017-06-28",
                      "2017-06-27" = "2017-06-27",
                      "2017-06-26" = "2017-06-26",
                      "2017-06-25" = "2017-06-25",
                      "2017-06-24" = "2017-06-24",
                      "2017-06-23" = "2017-06-23",
                      "2017-06-22" = "2017-06-22",
                      "2017-06-21" = "2017-06-21",
                      "2017-06-20" = "2017-06-20",
                      "2017-06-19" = "2017-06-19",
                      "2017-06-18" = "2017-06-18",
                      "2017-06-17" = "2017-06-17",
                      "2017-06-16" = "2017-06-16",
                      "2017-06-15" = "2017-06-15",
                      "2017-06-14" = "2017-06-14",
                      "2017-06-13" = "2017-06-13",
                      "2017-06-12" = "2017-06-12",
                      "2017-06-11" = "2017-06-11",
                      "2017-06-10" = "2017-06-10",
                      "2017-06-09" = "2017-06-09",
                      "2017-06-08" = "2017-06-08",
                      "2017-06-07" = "2017-06-07",
                      "2017-06-06" = "2017-06-06",
                      "2017-06-05" = "2017-06-05",
                      "2017-06-04" = "2017-06-04",
                      "2017-06-03" = "2017-06-03",
                      "2017-06-02" = "2017-06-02",
                      "2017-06-01" = "2017-06-01"
                    )
        ),
        paste("Guideline: Choose any operator of the CO & see how many
              number of chats he or she has been involved in total and on a specific date as well.
              
              
              ")
        
        
        ),
      
      mainPanel(id="xxxx",
                tags$br(),
                box(title="Total Number",uiOutput("result"),collapsible = TRUE,status = "success",height =180,solidHeader = TRUE ),
                box(title="On a certain Date",uiOutput("datedisplay"),collapsible = TRUE,status = "success",height =180,solidHeader = TRUE)
      )
      ) 
    
  })
  
  output$sms<-renderMenu({
    
    received_sms<-apply(read.csv("messages.csv"),1,function(row){
      messageItem(from=row[["from"]],message=row[["message"]])  
      
    })
    dropdownMenu(type="messages",.list=received_sms)
  })   
  
  output$department_analysis<- renderPlot({
    
    mydata<- req(data())
    sumu<-0
    sumt<-0
    sumuk<-0
    sumj<-0
    sumh<-0
    sums<-0
    sumus<-0
    sumua<-0
    sumc<-0
    suma<-0
    sumaz<-0
    sume<-0
    sumom<-0
    sumco<-0
    sumM<-0
    sumq<-0
    sumd<-0
    sumI<-0
    sumN<-0
    sumg<-0
    sumge<-0
    summa<-0
    sumch<-0
    sumrw<-0
    sumro<-0
    sumet<-0
    sumIn<-0
    sumza<-0
    sumje<-0
    summal<-0
    sumss<-0
    sumke<-0
    sumno<-0
    sumba<-0
    sumcam<-0
    for (row in 1:nrow(mydata)) {
      country <- mydata[row, "Country"]
      dept <- mydata[row, "Department"]
      
      if(dept ==input$name) {
        if(country=="Uganda"){
          
          sumu<-sumu+1
        }
        else if(country=="Tanzania"){
          
          sumt<-sumt+1
        }
        else if(country=="United Kingdom"){
          
          sumuk<-sumuk+1
        }
        else if(country=="Japan"){
          
          sumj<-sumj+1
        }
        else if(country=="Hong Kong"){
          
          sumh<-sumh+1
        }
        else if(country=="South Africa"){
          
          sums<-sums+1
        }
        else if(country=="United States"){
          
          sumus<-sumus+1
        }
        else  if(country=="United Arab Emirates"){
          
          sumua<-sumua+1
        }
        else if(country=="Canada"){
          
          sumc<-sumc+1
        }
        else if(country=="Australia"){
          
          suma<-suma+1
        }
        else if(country=="Azerbaijan"){
          
          sumaz<-sumaz+1
        }
        else if(country=="Egypt"){
          
          sume<-sume+1
        }
        else if(country=="Oman"){
          
          sumom<-sumom+1
        }
        else if(country=="Congo"){
          
          sumco<-sumco+1
        }
        else if(country=="Morocco"){
          
          sumM<-sumM+1
        }
        else if(country=="Qatar"){
          
          sumq<-sumq+1
        }
        else if(country=="Denmark"){
          
          sumd<-sumd+1
        }
        else if(country=="Iraq"){
          
          sumI<-sumI+1
        }
        else if(country=="Netherlands"){
          
          sumN<-sumN+1
        }
        else if(country=="Ghana"){
          
          sumg<-sumg+1
        }
        else if(country=="Germany"){
          
          sumge<-sumge+1
        }
        else if(country=="Mauritius"){
          
          summa<-summa+1
        }
        else if(country=="China"){
          
          sumch<-sumch+1
        }
        else if(country=="Rwanda"){
          
          sumrw<-sumrw+1
        }
        else if(country=="Republic of Moldova"){
          
          sumro<-sumro+1
        }
        else if(country=="Ethiopia"){
          
          sumet<-sumet+1
        }
        else if(country=="India"){
          
          sumIn<-sumIn+1
        } 
        else if(country=="Zambia"){
          
          sumza<-sumza+1
        }
        else if(country=="Jersey"){
          
          sumje<-sumje+1
        }
        else if(country=="Malta"){
          
          summal<-summal+1
        }
        else if(country=="South Sudan"){
          
          sumss<-sumss+1
        }
        else if(country=="Kenya"){
          
          sumke<-sumke+1
        }
        else if(country=="Norway"){
          
          sumno<-sumno+1
        }
        else if(country=="Bangladesh"){
          
          sumba<-sumba+1
        }
        else if(country=="Cameroon"){
          
          sumcam<-sumcam+1
        }
      }
      
    }
    #print(sumu)
    
    allsum<-sum(sumu,sumt,sumuk,sumj,sumh,sums,sumus,sumua,sumc,suma,sumaz,sume,sumom,sumco,sumM,sumq,sumd,sumI,sumN,sumg,sumge,summa,sumch,sumrw,sumro,sumet,sumIn,sumza,sumje,summal,sumss,sumke,sumno,sumba,sumcam)
    divide<-c(sumu/allsum,sumt/allsum,sumuk/allsum,sumj/allsum,sumh/allsum,sums/allsum,sumus/allsum,sumua/allsum,sumc/allsum,suma/allsum,sumaz/allsum,sume/allsum,sumom/allsum,sumco/allsum,sumM/allsum,sumq/allsum,sumd/allsum,sumI/allsum,sumN/allsum,sumg/allsum,sumge/allsum,summa/allsum,sumch/allsum,sumrw/allsum,sumro/allsum,sumet/allsum,sumIn/allsum,sumza/allsum,sumje/allsum,summal/allsum,sumss/allsum,sumke/allsum,sumno/allsum,sumba/allsum,sumcam/allsum)
    
    plotdept<-c(sumu,sumt,sumuk,sumj,sumh,sums,sumus,sumua,sumc,suma,sumaz,sume,sumom,sumco,sumM,sumq,sumd,sumI,sumN,sumg,sumge,summa,sumch,sumrw,sumro,sumet,sumIn,sumza,sumje,summal,sumss,sumke,sumno,sumba,sumcam)
    labb<-c('Ug','Tz','Uk','Jp','Hk','Sa','Us','UAE','CA','Aus','Aze','Egy','Oman','Congo','Moro','Qatar','Den','Irq','Neth','Gha','Ger','Mau','Chi','Rwa','ROM','Ethi','Ind','Zam','Jer','Mal','SS','Ken','Nor','Bang','Came')
    
    deptt<-data.frame(plotdept,labb)
    
    deptpro<-data.frame(divide,labb)
    
    barplot(deptt$plotdept, las = 2, names.arg = deptt$labb,col = brewer.pal(9, "Blues"), main ="BAR GRAPH SHOWING EVALUATION OF DEPARTMENT'S PERFORMANCE ON EACH COUNTRY",ylab = "Number of times Selected",xlab = "Country")
    
    # barplot(deptpro$divide, las = 2, names.arg = deptt$labb,col = brewer.pal(9, "Blues"), main ="Department selected by each country between 2017-06-01 and 2017-07-07",ylab = "Number of times Selected",xlab = "Country")
    
  })
  
  #download visualization models
  observeEvent(input$down, {
  
    mydata<-req(data())
    
    if(input$nameit=='BAR GRAPH'){
      pdf("Downloads/bargraph.pdf",width=8,height = 6)
      mydata<- req(data())
      operatives<-c('joseph','stellamaris','SimonPeter','atuhaire','PaulOchen')
      dv<-c('joseph banyu','stellamaris Nabisere','Simon Peter  Engoru','atuhaire elizabeth','Paul Ochen')
      rt<- 0;
      jo<- 0;
      st<- 0;
      si<- 0;
      at<- 0;
      pa<- 0;
      
      for(i in 1:length(dv)){
        for(row in 1:nrow(mydata)) {
          
          
          
          price <- mydata[row, "Chat.content"]
          t<- grepl(dv[i], price)
          if(t == TRUE ) {
            
            rt<- rt+1
          }
          
        }
        
        if(dv[i]=='joseph banyu'){
          jo<-rt
          
        }
        else if(dv[i]=='stellamaris Nabisere'){
          st<-(rt-jo)
          
        }
        
        else if(dv[i]=='Simon Peter  Engoru'){
          si<-(rt-jo-st)
        }
        else if(dv[i]=='atuhaire elizabeth'){
          at<-(rt-jo-st-si)
          
        }
        
        else if(dv[i]=='Paul Ochen'){
          pa<-(rt-jo-st-si-at)
          
        }
        
        
        
        
      }
      
      print(jo) 
      print(st)
      print(si)
      print(at)
      print(pa)
      
      x = c(jo,st,si,at,pa)   
      real<-data.frame(operatives,x)
      real
      barplot(real$x, las = 1,
              main ="BAR GRAPH SHOWING NUMBER OF CHATS OPERATIVES CONDUCTED WITH IN CHAT CONTENT",ylab = "Number of Chats",
              legend =operatives, 
              col = c("green","yellow","red","purple","blue")) 
      
      shinyalert("Yes","Download complete!",type="success")
      dev.off()

    }
    else if(input$nameit=='PIE CHART'){
      pdf("Downloads/piechart.pdf",width=8,height = 6)
      mydata<-req(data())
      piePlotData = aggregate(formula(paste0(".~",input$y)), mydata, sum)
      
      percent<-(piePlotData[[input$x]]/sum(piePlotData[[input$x]]))*100
      percent<-round(percent,2)
      
      labels<-piePlotData[[input$y]]
      
      pie3D(piePlotData[[input$x]], main="Department Evaluation",labels=paste(labels,percent,"%"),explode=0.1,theta=pi/6,mar=c(0,0,0,0)
            
            
      )
      shinyalert("Yes","Download complete!",type="success")
      dev.off()
      
    }
    else if(input$nameit=='ANALYSIS OF EMOTIONS'){
      pdf("Downloads/EmotionalAnalysis.pdf",width=8,height = 6)
      mydata<- req(data())
      comments<-iconv(mydata$Chat.content)
      s<-get_nrc_sentiment(comments)
      
      barplot(colSums(s), las=2, col=rainbow(10) ,ylab = 'Emotional level' ,
              main = 'BARGRAPH SHOWING EMOTIONAL REACTIONS OF CUSTOMERS',
              legend = c("Anger", "Anticipation","Disgust","Fear","Joy","Sadness","Suprise",
                         "Trust","Negative","Positive"), 
              fill = c("red","orange","yellow","lightgreen","green","blue","blue","blue","purple","pink"),lwd=1)
      
      shinyalert("Yes","Download complete!",type="success")
      dev.off()
      
    }
    else if(input$nameit=='WORDCLOUD'){
      pdf("Downloads/wordcloud.pdf",width=8,height=6)
      mydata<-req(data())
      docs <- Corpus(VectorSource(mydata$Chat.content))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      
      docs <- tm_map(docs, content_transformer(tolower))
      
      docs <- tm_map(docs, removeNumbers)
      
      docs <- tm_map(docs, removeWords, stopwords("english"))
      wordz<-c("chat","can","get","newvision","www","uganda","system","https"
               ,"the","you","vision","group","engoru","stellamaris","eatuhaire"
               ,"assistant","joseph","simon","peter","vizuri.visiongroup.co.ug",
               "www.visiongroup.co.ug","banyu","newvision.co.ug","closed","accepted",
               "live","elizabeth","atuhaire","support","http","paul","josephbanyu","jbanyu",
               "ochen","nabisere","peter","office","article","gmailcom","inquiries","bukedde",
               "click","directlysengoru","number","june","letter","daniel","stellalule",
               "vpgvisiongroupcoug","samuel","denis","newspaper","anjana","also","phone","advertising",
               "library","flippaper","sengoru","website","anonymous","head","advertisement","coug",
               "pochen","respective","advertisements","papers","able","mobile","online","visitor",
               "gmailcom","time","link","via","contact","julius","stella","adverts","wasswa","company",
               "desktop","app","one","coug","epaper","various","companies","going","francis",
               "close","list","date","muhindo","look","sign","paper","links","specific","news",
               "account","new","using","category","visiongroup","may","money","com","gmail","websites",
               "co","ug","made","much","vpg","well"
               
      )
      docs <- tm_map(docs, removeWords,wordz) 
      
      docs <- tm_map(docs, removePunctuation)
      
      docs <- tm_map(docs, stripWhitespace)
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      
      set.seed(1234)
      wordcloud(words = d$word, freq = d$freq, min.freq =20 , max.words=200, 
                random.order=FALSE, rot.per=0.2,colors=brewer.pal(8, "Dark2"))
      shinyalert("Yes","Download complete!",type="success")
      dev.off()
   
    }
    else if(input$nameit=='CHAT'){
      pdf("Downloads/CHAT.pdf",width=8,height = 6)
      mydata<-req(data())
      docs <- Corpus(VectorSource(mydata$Chat.content))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      
      docs <- tm_map(docs, content_transformer(tolower))
      
      docs <- tm_map(docs, removeNumbers)
      
      docs <- tm_map(docs, removeWords, stopwords("english"))
      wordz<-c("chat","can","get","newvision","www","uganda","system","https"
               ,"the","you","vision","group","engoru","stellamaris","eatuhaire"
               ,"assistant","joseph","simon","peter","vizuri.visiongroup.co.ug",
               "www.visiongroup.co.ug","banyu","newvision.co.ug","closed","accepted",
               "live","elizabeth","atuhaire","support","http","paul","josephbanyu","jbanyu",
               "ochen","nabisere","peter","office","article","gmailcom","inquiries","bukedde",
               "click","directlysengoru","number","june","letter","daniel","stellalule",
               "vpgvisiongroupcoug","samuel","denis","newspaper","anjana","also","phone","advertising",
               "library","flippaper","sengoru","website","anonymous","head","advertisement","coug",
               "pochen","respective","advertisements","papers","able","mobile","online","visitor",
               "gmailcom","time","link","via","contact","julius","stella","adverts","wasswa","company",
               "desktop","app","one","coug","epaper","various","companies","going","francis",
               "close","list","date","muhindo","look","sign","paper","links","specific","news",
               "account","new","using","category","visiongroup","may","money","com","gmail","websites",
               "co","ug","made","much","vpg","well"
               
      )
      docs <- tm_map(docs, removeWords,wordz) 
      
      docs <- tm_map(docs, removePunctuation)
      
      docs <- tm_map(docs, stripWhitespace)
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      
      barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,col ="blue", main ="BAR GRAPH SHOWING 20 MOST USED WORDS IN THE CHAT CONTENT",ylab = "Word frequencies")
      
      shinyalert("Yes","Download complete!",type="success")
      dev.off()
      
    }
    else if(input$nameit=='COUNTRY'){
      pdf("Downloads/country.pdf",width=8,height = 6)
      mydata <- req(data())
      barplot(table(mydata$Country),
              main="Number of participants from different Countries",
              xlab="Country",
              ylab="Number of participates",
              border="red",
              las = 2,
              col="blue",
              density=50
      )
      shinyalert("Yes","Download complete!",type="success")
      dev.off()
     
    }
   
    else if(input$nameit=='SCATTER PLOT'){
      pdf("Downloads/scatter.pdf",width=8,height = 6)
      mydata<-req(data())
      mydata$Minutes<- as.POSIXct(mydata$Minutes, format="%H:%M")
      plot(mydata$Minutes, mydata$Wait.time/1000, xaxt='s', pch=10, las=1, xlab='Chat Starting Time(in minutes)', ylab='Waiting Time/1000')
      axis.POSIXct(1, mydata$Minutes, format="%H:%M")
      shinyalert("Yes","Download complete!",type="success")
      dev.off()
      
    }
    else if(input$nameit=='TIME'){
      pdf("Downloads/TIME.pdf",width=8,height = 6)
      mydata<-req(data())
      sum0<-0
      sum1<-0
      sum2<-0
      sum3<-0
      sum4<-0
      sum5<-0
      sum6<-0
      sum7<-0
      sum8<-0
      sum9<-0
      sum10<-0
      sum11<-0
      sum12<-0
      sum13<-0
      sum14<-0
      sum15<-0
      sum16<-0
      sum17<-0
      sum18<-0
      sum19<-0
      sum20<-0
      sum21<-0
      sum22<-0
      sum23<-0
      for (row in 1:nrow(mydata)) {
        m<- mydata[row, "Minutes"]
        if(str_extract(m,'^.{2}')=="00"){
          sum0<-sum0+1
        }
        else if(str_extract(m,'^.{2}')=="01"){
          sum1<-sum1+1
        }
        
        else if(str_extract(m,'^.{2}')=="02"){
          sum2<-sum2+1
        }
        else if(str_extract(m,'^.{2}')=="03"){
          sum3<-sum3+1
        }
        else if(str_extract(m,'^.{2}')=="04"){
          sum4<-sum4+1
        }
        else if(str_extract(m,'^.{2}')=="05"){
          sum5<-sum5+1
        }
        else if(str_extract(m,'^.{2}')=="06"){
          sum6<-sum6+1
        }
        else if(str_extract(m,'^.{2}')=="07"){
          sum7<-sum7+1
        }
        else if(str_extract(m,'^.{2}')=="08"){
          sum8<-sum8+1
        }
        else if(str_extract(m,'^.{2}')=="09"){
          sum9<-sum9+1
        }
        else if(str_extract(m,'^.{2}')=="10"){
          sum10<-sum10+1
        } 
        else if(str_extract(m,'^.{2}')=="11"){
          sum11<-sum11+1
        }
        else if(str_extract(m,'^.{2}')=="12"){
          sum12<-sum12+1
        }
        else if(str_extract(m,'^.{2}')=="13"){
          sum13<-sum13+1
        }
        else if(str_extract(m,'^.{2}')=="14"){
          sum14<-sum14+1
        }
        else if(str_extract(m,'^.{2}')=="15"){
          sum15<-sum15+1
        }
        
        else if(str_extract(m,'^.{2}')=="16"){
          sum16<-sum16+1
        }
        
        else if(str_extract(m,'^.{2}')=="17"){
          
          sum17<-sum17+1
        }
        
        else if(str_extract(m,'^.{2}')=="18"){
          
          sum18<-sum18+1
        }
        else if(str_extract(m,'^.{2}')=="19"){
          
          sum19<-sum19+1
        }
        else if(str_extract(m,'^.{2}')=="20"){
          
          sum20<-sum20+1
        }
        else if(str_extract(m,'^.{2}')=="21"){
          
          sum21<-sum21+1
        }
        else if(str_extract(m,'^.{2}')=="22"){
          
          sum22<-sum22+1
        }
        else if(str_extract(m,'^.{2}')=="23"){
          
          sum23<-sum23+1
        }
        
      }
      
      pe<-c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
      xx<-c(sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17,sum18,sum19,sum20,sum21,sum22,sum23)
      rea<-data.frame(pe,xx)
      rea
      barplot(rea$xx, las = 1, names.arg = rea$pe,col ="lightblue", main ="Number of Chats Started between 00:00 to 23:60",ylab = "Number of Chats",xlab = "hours in 24 hour clock") 
      x<-sum(sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10,sum11,sum12,sum13,sum14,sum15,sum16,sum17,sum18,sum19,sum20,sum21,sum22,sum23)
      shinyalert("Yes","Download complete!",type="success")
      dev.off()
      
    }
    
  })

  
   
})
