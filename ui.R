#rsconnect::setAccountInfo(name='zilfimian',token='53632CD295162F53982EF0C376F6F945',secret='XPzJZJeXMVhOreJ5tDzywuhoGEduBDR7YcpYN1SL')
#install.packages("shinydashboard")
#install.packages("shiny")
#install.packages("graphics")
library(shiny)
library(graphics)
library(shinydashboard)
library(readr)
library(plotly)
library(here)
setwd(here())

#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("ropensci/plotly")
#options(shiny.sanitize.errors = TRUE)


options(encoding = "UTF-8") 
# Define UI 
shinyUI(
  dashboardPage( title =  "Lusine Zilfimian 2018 Thesis", #skin="green", 
    
                 
    dashboardHeader(title = " Պրեզենտացիա", 
            dropdownMenu(type = "task", taskItem(value =30,color="orange","Description"),taskItem(value =63,color="red","Armenia"),taskItem(value =100,color="green","VAR")),
            dropdownMenu(type = "message",messageItem(from = "I HAVE DONE IT", message = "Yuhu", icon=icon("handshake-o"), time="15-05-18"))#DDmenu
             ),
    
    dashboardSidebar( #width = 200, 
      
       sidebarMenu(
       textOutput("currentTime"),
         sidebarSearchForm("searchText", "buttomSearch","Search"),
      menuItem("Description",tabName = "description", icon = icon("tasks")),
      menuItem("Armenian Data analysis",tabName = "ADA",icon=icon("bar-chart-o")),
      menuItem("Detailed Analysis",icon=icon("industry"), tabName = "third_part", badgeLabel = "VEC", badgeColor = "green"),
               menuSubItem("Suggestions",icon=icon("list-alt"), tabName = "sug" )
      
      #_#  ,
      #_# textInput("project", "enter your project code"),
      #_# textOutput("project_")
                )#SIDEBAR# to return from menues
        ), #DashboardSidebar
    
    
    dashboardBody(
     tabItems(
      
        tabItem(tabName = "description",
#_________________________________________________________________________________________1_________________________________________________________________________
                #_____1.1.1_____________
                 fluidRow(
                   tabBox(
                   tabPanel( title= "*", 
                          h4("ԵՐԵՎԱՆԻ ՊԵՏԱԿԱՆ ՀԱՄԱԼՍԱՐԱՆ",br(),br(), "ՏՆՏԵՍԱԳԻՏՈՒԹՅԱՆ ԵՎ ԿԱՌԱՎԱՐՄԱՆ ՖԱԿՈՒԼՏԵՏ",br(),br(), "Տնտեսագիտության մեջ մաթեմատիկական մոդելավորման ամբիոն",br(),br(),align="center"),
                          h3("ՄԱԳԻՍՏՐՈՍԱԿԱՆ ԹԵԶ",br(),br(),br(),align="center"),
                          em(h1("ԴՐԱՄԱԿԱՆ ՓՈԽԱՆՑՈՒՄՆԵՐԻ ԱԶԴԵՑՈՒԹՅՈՒՆԸ ՀՀ ՏՆՏԵՍՈՒԹՅԱՆ ՎՐԱ",br(),br(),br(),br(),align="center")),
                          h3("Ուսանողուհի`Լ. Զիլֆիմյան       Ղեկավար՝ Ն. Ջրբաշյան",br(),br(),align="center",br())
                         ),
                   tabPanel( title= " Վճարային հաշվեկշիռ"
                             , imageOutput("BOP")
                           
                             
                   ), tabPanel(title="**")),
                    
                #_____1.1.2_____________   
                   tabBox(tabPanel( title= "Նպատակ/խնդիր" ,
                                    tags$ul(
                                      tags$li(em(h2(" Օբյեկտ՝"), style="color:#A81459"), h4("Դրամական փոխանցումները ՀՀ-ում")), 
                                      tags$li(em(h2("Առարկա՝"), style="color:#A81459"),h4("ՀՀ-ում դրամական փոխանցումների ազդեցության գնահատումը")), 
                                      tags$li(em(h2("Նպատակ՝"), style="color:#A81459"),h4("Ուսումնասիրել դրամական փոխանցումների բնույթն ու ազդեցությունը զարգացող երկներում, գնահատել, մոդելավորել ՀՀ դրամական փոխանցումների ազդեցությունը ՀՀ տնտեսական ցուցանիշների վրա")),
                                      tags$li(em(h2("Խնդիրներ՝"), style="color:#A81459"), br(),
                                              tags$ol( tags$li(h4(" Դրամական փոխանցումների հաշվարկման մեթոդաբանության ուսումնասիրություն, այլ ֆինանսական հոսքերի հետ համեմատություն, ազդեցության խմբավորում միկրո-մակրո մակարդակներում
")),tags$li(h4("ՀՀ-ում միգրացիայի  և  դրամական փոխանցումների բնույթի, պատճառների և հետևանքների  ուսումնասիրություն")),tags$li(h4("ՀՀ մի քանի մակրոտնտեսական ցուցանիշների և դրամական փոխանցումների կապի բացահայտում։ ՀՀ ՀՆԱ-ի, սպառման և դրամական փոխանցումների միջև երկարաժամկետ դինամիկ հավասարակշռության մոդելավորում
"))))
                                    )
                                   
                                    ),
                          tabPanel( title= " Մի քանի նշումներ",
                                    
                                    tags$ul( 
                                      tags$li(em(h2(" Փոխանակում vs տրանսֆերտ"), style="color:#162994")), 
                                      br(),
                                      tags$li(em(h2("Ռեզիդենտայնություն"), style="color:#162994")), 
                                      br(),
                                      tags$li(em(h2("Հիմնական բաղկացուցիչներ"), style="color:#162994"),
                                              tags$ol( tags$li(h4("Աշխատավարձ")),tags$li(h4(" Անձնական տրանսֆերտներ")))),
                                      tags$li(em(h2("Բնույթ"), style="color:#162994"),
                                              tags$ol( tags$li(h4("Մասնավոր")),tags$li(h4("Պակաս տատանողական"))))
                                    )#Ul
                                    ), tabPanel(title="**") ),
                          
                #_____1.2______________
                fluidRow(
                  column (width=12,
                  valueBox(value="243 միլիոն մարդ", subtitle = "2015 թ․ միջազգային միգրանտների թիվ՝ ամբողջի 3․3%", icon = icon("warning"), color = "yellow"),
                  valueBox(value="537 միլիարդ դոլար", subtitle = "2016 թ․ դրամական փոխանցումներ", icon = icon("bar-chart-o"), color = "red"),
                  valueBox(value="75%", subtitle = "2016 թ․ զարգացող երկրներ", icon = icon("warning"), color = "blue")
                  )),
                
                #valubox~infobox  
          #______1.3_____________   
                          fluidRow(#FR1
                            
                 box(title = "Ֆինանսական հոսքեր", status ="primary", solidHeader =  T,  #background = "aqua",
                     selectizeInput("name", label="Ընտրի՛ր խումբը", choices= c("World", "Developed", "Developing"), multiple=F, select="Developing"),width =7,
                     plotlyOutput("plot1")
                     
                     ),
                 
                 box(title = "Բաշխում", status = "warning", solidHeader = T, 
                     selectizeInput("piename", label="Ընտրի՛ր տարին", choices= c("X2012","X2013","X2014","X2015","X2016"), multiple=F, select="X2016"), width = 5,plotlyOutput("plot2")
                    # ,submitButton("Update!")
                     )
                  ),#FR1
                fluidRow(#FR2
                  
                  box(title = "Թոփ 10 ըստ դրամական փոխանցումների ծավալի", status ="info", solidHeader =  T,  #background = "aqua",
                     plotlyOutput("plot3")
                      
                  ),
                  
                  box(title = "Թոփ 10 ըստ դրամական փոխանցումներ/ՀՆԱ հարաբերակցության", status = "info", solidHeader = T, 
                       plotlyOutput("plot4")
                      # ,submitButton("Update!")
                  )
                )#FR2
                
                
                 )# fluid row verevi eji##fluid # title, status, solid dizayn e
               
               ), #imagenary row for graps and commanrds
#____________________________________________________________________________________________________2_____________________________________________________________        
        
        
       ##__________2.1________________
        tabItem (tabName = "ADA",
                 fluidRow(
                   valueBox(value="15%", subtitle = "Բնակչության թվաքանակը 1990 − 2017թթ. նվազել է", icon = icon("warning"), color = "yellow"),
                   valueBox(value= "7-8 միլիոն մարդ", subtitle = "«Հին» սփյուռք + «նոր» սփյուռք", icon = icon("globe"), color ="aqua"),
                   valueBox(value= "1 055 200 մարդ", subtitle = "Միգրացիայի բացասական մնացորդ (1990-2017 թթ․)", icon = icon("warning"), color = "red")
                   
                 ),
        ##_________2.2________________Migr         
                 fluidRow(
                   tabBox( 
                     tabPanel( 
                   title = "Միգրացիոն վիճակագրություն ԱՎԾ", status = "warning", solidHeader = T, plotlyOutput("plot5")   ),
                   width = 8,
                   tabPanel( title = "Ուղևորափոխադրումներ", status = "warning", solidHeader = T,plotlyOutput("plot6") ),
                   tabPanel(title = "Միգրացիայի ուղղություն", status = "warning", solidHeader = T, numericInput("number1", "Ընտրի՛ր երկրների քանակը", 5, min=1, max=29),
                    plotlyOutput("plot7"))
                   ),
                   
                   fluidRow( 
                          valueBox(value="24 000 մարդ", subtitle = "2014թ․ 3 ամիս և ավելի տևողությամբ մեկնած և 2016թ.-ի դրությամբ դեռևս չվերադարձած միգրանտների միջին տարեկան գնահատական", icon = icon("warning"), color = "yellow"),
                       
                          valueBox(value= "", subtitle = " Աշխատանքային բնույթ  " ,  color = "green", width = 4) ,  
                          valueBox(value= "20.51%", subtitle = "Ունեն կապ միգրացիայի հետ", icon = icon("warning"), color = "aqua", width = 4) ,
                          valueBox(value= "63%", subtitle = "Զբաղվել են շինարարությամբ", icon = icon("warning"), color = "aqua", width = 4) ,  
                          
                          width = 3 ),
        #_________________2.2_____________Remit           
        fluidRow(
          valueBox(value="9-րդ տեղը ՄՑԵ-ում", subtitle = "Ըստ ԴՓ/ՀՆԱ 2016 թթ․, ՄՑԵ խմնի միջինը՝ 7%", icon = icon("globe"), color = "yellow"),
          valueBox(value= "4-րդ տեղը ԱՊՀ-ում", subtitle = "Ըստ ԴՓ/ՀՆԱ 2016 թթ․, ԱՊՀ միջինը՝ 9,7%", icon = icon("warning"), color ="aqua"),
          valueBox(value= "5-րդ տեղը ԿԱԵ-ում", subtitle = "Ըստ ԴՓ/ՀՆԱ 2016 թթ․, ԿԱԵ միջինը՝ 4%", icon = icon("globe"), color = "red")
          
        ),
         fluidRow(
          tabBox( 
            tabPanel( 
              title = "ՀՀ դիրքը", status = "warning", solidHeader = T, 
            box(selectizeInput("name2", label="Ընտրի՛ր երկրի խումբը", choices= c("Low and middle income", "CIS", "Europe and Central Asia"), multiple=F, select="CIS"),
           width = 5,status = "info", solidHeader = T ), 
           box(radioButtons("button1", "Ընտրի՛ր տարին  (միայն Միջին և ցածր եկամտային խմբի համար)", choices = c("2015", "2016"), "2016"),
            width = 5,status = "info", solidHeader = T), 
           numericInput("number2", "Երկների քանակ (բացառությամբ ԱՊՀ)", 9, min=1, max=19),
               
          plotlyOutput("plot8")
            ),
            width = 8,
            tabPanel( title = "ՀՀ ԴՓ դինամիկան", status = "warning", solidHeader = T,plotlyOutput("plot9") ),
            tabPanel(title = "Տատանողականությունը", status = "warning", solidHeader = T, 
                     plotlyOutput("plot10")),
          
          tabPanel(title = "Բաշխումը ըստ երկրներ", status = "warning", solidHeader = T,
                   plotlyOutput("plot13"),
                   plotlyOutput("plot12")
                              ),
          
            tabPanel(title = "Կապ ՀՆԱ-ների հետ", status = "warning", solidHeader = T,
                     plotlyOutput("plot11"))
            
          ),
          
          fluidRow( 
            valueBox(value= "19,7%", subtitle = " 2013-ին ԴՓ/ՀՆԱ՝ ամենաբարձր  " ,  color = "green", icon = icon("bar-chart-o"), width = 4 ) ,  
            valueBox(value="16.9%", subtitle = "Վերջին 10 տարում ԴՓ/ՀՆԱ", icon = icon("bar-chart-o"), color = "yellow", width = 3),
             
            valueBox(value= "13,1%", subtitle = " 2016-ին ԴՓ/ՀՆԱ՝ ամենացածր  ", icon = icon("bar-chart-o"), color = "aqua", width = 3) ,
            valueBox(value= tags$ul(tags$li(h4(em("Տատանողականություն"))), tags$li(h4(em("Կենտրոնացվածություն"))),tags$li(h4(em("Համացիկլայնություն")))), subtitle = " ", icon = icon("tasks"), color ="green", width = 4),
            width = 3 )
                  
         )),
        #_____________2.3_____________IMPACT       
         fluidRow(h1("Դրամական փոխանցումների ազդեցությունը", align = "center"),
                         #_____2.3.1__
                          
                  
                  valueBox(value= tags$ul(tags$li(h5(em("Եկամտի աճ, մարդկային կապիտալի ձևավորում"))),tags$li(h5(em("Աղքատության կրճատում"))),tags$li(h5(em("Սոցիալական ապահովագրություն"))), tags$li(h5(em("Բիզնեսի խթանում"))),  tags$li(h5(em("Ոչ նյութական հետևաքներ"))), tags$li(h5(em("Ֆինանսական համակարգի զարգացում"))), tags$li(h5(em("Միձազգային վարկունակության աճ և տատանողականության նվազմ")))), 
                           subtitle = " ", icon = icon("tasks"),color= "green", width =6),
                  
                  #_____2.3.2__
                  
                  valueBox(value= 
                             tags$ul(tags$li(h5(em("Կախվածության աճ"))),
                                     tags$li(h5(em("Աշխատելու ցանկության կորուստ")))   , tags$li(h5(em("Մրցունակության կորուստ, Հոլանդական հիվանդություն")) )
                                     , tags$li(h5(em ("Երկարաժամկետում ՀՆԱ-ի կրճատում")))), 
                           subtitle = " ", icon = icon("tasks") ,
                           color= "red", width =6),
                  infoBox(title="cor.={0.63, 0.88, -058}", value = "Ուսանողների; 14-35 տարեկանների մեջ վճարովի համակարգում սովորողներ; վերջինիս թվաքանակ", subtitle = "Նշանակալիության 1% մակարդակի վրա",
                          icon = icon("book"), color = "green", width = 6),
                  br( infoBox(title="{cor.=0.503, sig.t=0.04;cor.=-0.35, sig.t=0.04}", value = "Գործազրկության մակարդակ, տնտեսապես ոչ ակտիվ բնակչություն", subtitle = "Զբաղվածության հետ չկա",
                          icon = icon("address-card"), color = "red", width = 6)
                  ),
                  infoBox(title="cor.={0.53, 0.51, 0.65}", value = "ՀՆԱ, ներդրում, սպառում", subtitle = "Նշանակալիության 5%-ի մակարդակի վրա",
                          icon = icon("bar-chart-o"), color = "green", width = 4),
                  infoBox(title="{cor.=0.5,sig.t=0.03}", value = "Մանրածախ առևտրի  ապրանքաշրջանառություն", subtitle = "Ավանդ/վարկերի հետ գծային կապ չկա",
                          icon = icon("cart-plus"), color = "red", width = 4),
                  infoBox(title="cor.=0.89, sig.t=0.00", value = "Փողի առաջարկ", subtitle = "Գնաճի հետ կապը նշանակիալի չէ",
                          icon = icon("bar-chart-o"), color = "red", width = 4)
                 
                  
                  
                )        ),#

        
#________________________________________________________________________________________________3_____________________________________________________

       ##___________3.1_____________________________
        tabItem (tabName = "third_part",
                 
                 
                
                

                
                
  ##ARDL1___________________________________________________________ARDL2 tabBox2-um
                 fluidRow(
                   tabBox(
                   
                     
                     tabPanel(
                       title= "Ներկրում", 
                       h2("∆ln(IMt)=0.8∆ln(IMt-1)+0.43∆ln(Remt)  + 0.36∆ln(Remt−1)-0.009", align="center") 
                       ,
                       h3(" ∂∆ln(IMt)/∂∆ ln(Remt)  = 0.43   ",align="center"),
                       h3(" ∂∆ln(IMt)/∂∆ ln(Remt-1)  = 0.36   ",align="center"),
                       h3(" ∂∆ln(IMt+2)/∂∆ ln(Remt)  = 0.8*∂∆ln(IMt+1)/∂∆ ln(Remt)=0.8*(0.8*0.43)+0.36=0.56  ",align="center"),
                       box( numericInput("ar1", "Ընտրի՛ր p լագի քանակը", 1, min=1, max=5) ), 
                      box( numericInput("dl1", "Ընտրի՛ q լագի քանակը", 1, min=0, max=5) ),
                       verbatimTextOutput("summ1"),
                       plotOutput("plot14")
                     ),
                     tabPanel( 
                       title = "Մնացորդների ավտոկորելյացիա", status = "warning", solidHeader = T, numericInput("lag1", "Ընտրի՛ր  լագերի քանակը", 5, min=1, max=36),tableOutput("table1")  ,  width = 8)
                     
                   ),#tabBox
                   
                   tabBox(
                     
                     tabPanel(
                       title= "Արտաքին պետական պարտք", 
                       br(), h2("∆ln(Govt)=0.19∆ln(Govt-1)+0.12∆ln(Remt)  - 0.31∆ln(Remt-1)+0.003∆ln(excht)+0.03",align="center"),
                       br(),br(),     
                       h3(" ∂∆ln(Gov_t)/∂∆ln(Remt-1)=-0.31",align="center"),
                       br(),br(),
                       box( numericInput("ar2", "Ընտրի՛ր p լագի քանակը", 1, min=1, max=5) ),
                       box( numericInput("dl2", "Ընտրի՛ q լագի քանակը", 1, min=0, max=5) ), 
                       br(),br(),
                       verbatimTextOutput("summ2"),
                       br(),br(),
                       plotOutput("plot15")
                     ),
                     tabPanel( 
                       title = "Մնացորդների ավտոկորելյացիա", status = "warning", solidHeader = T, numericInput("lag2", "Ընտրի՛ր  լագերի քանակը", 5, min=1, max=36),tableOutput("table2")  ,  width = 8)
                     
                     
                   ),#TabBox
                   
                   
                   
                   valueBox(value= tags$ul(
                     tags$li(h4(em("Կարճաժամկետում ԴՓ-ի 10 տոկոս աճը բերում է սպառման 1․6 տոկոս աճ"))),
                     tags$li(h4(em("Երկարաժամկետում ԴՓ-ի  10 տոկոս աճը բերում է ՀՆԱ-իճ 8․2 տոկոս աճի "))), 
                     tags$li(h4(em(" Կարճաժամկետում 2․8 տոկոս")))
                     ), 
                     
                     subtitle = " ", icon = icon("tasks"),color= "aqua",width =6),
  
  valueBox(value= tags$ul(
    tags$li(h5(em("ՀՆԱ-ն հավասարակշռային արժեքին վերադառնալու արագությունը բարձր է "))), 
    
   
    tags$li(h5(em("Կա երկարաժամկետ հավասարակշռություն ՀՆԱ-ի և դրամական փոխանցումների միջև")))), 
    
    subtitle = " ", icon = icon("tasks"),color= "yellow",width =6),

                   
            #_____________________________________VECM       
                   tabBox( 
                     tabPanel("",
                           h3("∆lna(GDP_t) =  - 0.52***( lna(GDP_(t-1)) - 0.82lna(Rem_(t-1))-4.26 ) + 0.7***( lna(C_(t-1))  + 0.82lna(Rem_(t-1))- 3.98 )  - 0.27**∆lna(GDP_(t-1))  - 0.14∆lna(C_(t-1)) - 0.28***∆lna(Rem_(t-1)) + 0.11***dummy2009 +0.002dummy20014",align="center" ),
                           h3("∆lna(C_t) = 0.11( lna(GDP_(t-1)) - 0.82lna(Rem_(t-1)) -4.26 ) - 0.28( lna(C_(t-1))  - 0.82lna(Rem_(t-1))-3.98 )  + 0.05∆lna(GDP_(t-1))  - 0.18∆lna(C_(t-1)) + 0.16**∆lna(Rem_(t-1)) - 0.03dummy2009 - 0.003dummy20014",align="center" ),
                           h3("∆lna(Rem_t) =  - 0.52( lna(GDP_(t-1)) - 0.82lna(Rem_(t-1))-4.26 ) + 1.1( lna(C_(t-1))  - 0.82lna(Rem_(t-1))-3.98 )  + 0.33∆lna(GDP_(t-1))  - 0.84∆lna(C_(t-1)) - 0.19∆lna(Rem_(t-1)) + 0.14dummy2009 +0.12dummy20014",align="center" )
                     ),
                     tabPanel( "", 
                          tags$b(h2("ECT_1=lna(GDP_(t-1)) - 0.82lna(Rem_(t-1)) -4.26 (1)"), style="color:#A81459",align="center" ),
                           tags$b(h2("ECT_2=lna(C_(t-1))  - 0.82lna(Rem_(t-1))-3.98 (2)"),style="color:#A81459",align="center" ),
                           h3("∆lna(GDP_t) =  - 0.52ECT_1 + 0.7ECT_2  - 0.27∆lna(GDP_(t-1))  - 0.14∆lna(C_(t-1)) + 0.28∆lna(Rem_(t-1)) + 0.11dummy2009 +0.002dummy20014 (3)",align="center" ),
                           h3("∆lna(C_t) = 0.11ECT_1  - 0.28ECT_2   + 0.05∆lna(GDP_(t-1))  - 0.18∆lna(C_(t-1)) + 0.16∆lna(Rem_(t-1)) - 0.03dummy2009 - 0.003dummy20014 (4)",align="center" )
                     ),
                          width =12
                          
                         
                          ),#tabBox
                  
                     
                     
                     box(width =6,
                       title= "acf/pacf +" , 
                      plotOutput("plot16"),
                      imageOutput("ARIMA"), status = "primary", solidHeader = T
                     ),
                     box(width =6,
                       title="ARIMA",
                       box( numericInput("arr", "Ընտրի՛ր ar լագի քանակը", 4, min=0, max=5) ), 
                       box( numericInput("ma", "Ընտրի՛ր ma լագի քանակը", 3, min=0, max=5) ), 
                       box( numericInput("sar", "Ընտրի՛ր sar լագի քանակը", 0, min=0, max=5) ), 
                       box( numericInput("sma", "Ընտրի՛ sma լագի քանակը",4, min=0, max=5) ) ,
                       verbatimTextOutput("summ3"),
                       plotOutput("plot17"),
                       plotOutput("plot18"), status = "info", solidHeader = T
                       
                     )
                   #)#tabbox

                  )#fluidROW
  
  
    ),#chgitem bayc chjnjel
  
tabItem (tabName = "sug", 
       tabBox(
          tabPanel( title="Առաջարկություններ",
                    
                    tags$ul(
                      tags$li(em(h3(" ՀՀ-ից ՌԴ և ԱՄՆ մեկնողների վերադարձի ծրագրի մշակում, անկանոն արտագաղթի տեմպի կասեցում 
"))),
                      tags$li(em(h3(" Բանկերի կողմից գործիկքների ներդրում, որոնք թույլ կտան բանկային համակարգով կատարվող դրամական փոխանցումների ներգրավում ֆինանսական ոլորտ 
"))),
                      tags$li(em(h3(" Նվազեցնեն փոխանցումների հետ կապված ծախսերը, որը թույլ կտա նվազեցնել  ոչ ֆորմալ ճանապարհով հոսող դրամական փոխանցումների քանակը՝ միաժամական բարելավելով տվյալների վիճակ
"))),
                      tags$li(em(h3(" Աշխատատեղերի ստեղծմանը ուղղված պետական քաղաքականության իրականացում հարկային արտոնույթունների միջոցով
"))),
                      tags$li(em(h3(" Բանկային արտոնյալ վարկերի ստացման պայմանների թեթևացում  տեղական գործատուների և ձեռներեցների համար
"))),
                      
                      tags$li(em(h3(" Կանխարգելել երիտասարդ « ուղեղների արտահոսքը », մասնավորապես՝ բարձր վարձատրվող բարձրորակ երիտասարդ մասնագետների համար
"))))),
                     
        tabPanel(title="Շնորհակալություն", tags$b(h1("ՇՆՈՐՀԱԿԱԼՈՒԹՅՈՒՆ ՀԱՄԲԵՐՈՒԹՅԱՆ ՀԱՄԱՐ") ,style="color:#A81459"), 
                     align="center")
       ,width=12)
         
         )
         
         
 
                       )#tableItemS
       
     
    )#dashboardBody()
    
  )#dashboardPage()
)#shinyUi
