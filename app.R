library(shiny)
library(ggplot2)
library(plotly)


ct<-list("中國大陸","加拿大","南韓","巴林","新加坡","日本","汶萊","阿聯","香港")
idr<-list("產量(公噸)","出口值(千美元)","進口值(千美元)","市場規模(千美元)","貿易專業化指數(TSI)",
         "進口市場成長趨勢","目標市場佔全球貿易市場佔有率","平均每人GDP(美元)","人口數(人)",
         "市場距離(公里)","市場成長相對強度","進口市場的表現","市場開放程度","市場相對優惠關稅")
yr<-list(2015,2016)
fr<-list("鳳梨")
col<-c("country","year","production","price","export_quantity","export_value","import_quantity",
       "import_value","global_import_value","global_import_quantity","global_export_value","GDP",
       "population","Taiwan_export_value","Taiwan_tariff","country1_tariff","country2_tariff",
       "country3_tariff","country4_tariff","country5_tariff","Taiwan_market_export_value","Taiwan_matket_distance")
path<-"~/"

ui <- fluidPage(
  
  # App title ----
  titlePanel("市場吸引力"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      #select item
      radioButtons("radio", label = h3("水果"),
                   choices = fr,select="鳳梨"
                  ),
      #select box
      selectInput("select", label = h3("年度"), 
                  choices =yr,selected=2016),
      #Checkbox
      checkboxGroupInput("checkGroup",label = h3("國家"),
                         choices = ct
                         ,selected=ct),
      #checkboxGroupInput("checkGroup2",label = h3("指標"),
       #                  choices = idr
        #                 ,selected=idr),
      sliderInput("slider1",label=idr[1],min=0,max=1,value=1,step=0.2),
      sliderInput("slider2",label=idr[2],min=0,max=1,value=1,step=0.2),
      sliderInput("slider3",label=idr[3],min=0,max=1,value=1,step=0.2),
      sliderInput("slider4",label=idr[4],min=0,max=1,value=1,step=0.2),
      sliderInput("slider5",label=idr[5],min=0,max=1,value=1,step=0.2),
      sliderInput("slider6",label=idr[6],min=0,max=1,value=1,step=0.2),
      sliderInput("slider7",label=idr[7],min=0,max=1,value=1,step=0.2),
      sliderInput("slider8",label=idr[8],min=0,max=1,value=1,step=0.2),
      sliderInput("slider9",label=idr[9],min=0,max=1,value=1,step=0.2),
      sliderInput("slider10",label=idr[10],min=0,max=1,value=1,step=0.2),
      sliderInput("slider11",label=idr[11],min=0,max=1,value=1,step=0.2),
      sliderInput("slider12",label=idr[12],min=0,max=1,value=1,step=0.2),
      sliderInput("slider13",label=idr[13],min=0,max=1,value=1,step=0.2),
      sliderInput("slider14",label=idr[14],min=0,max=1,value=1,step=0.2)
      
      #fluidRow(column(12), verbatimTextOutput("value"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
       # Output: Table
      tableOutput("view"),
      downloadButton("downloadData", "Download Table"),
      h3(),
      # Output: Histogram ----
      plotlyOutput(outputId = "distPlot")
      #downloadButton("downloadPlot", "Download Plot")
      )
      
    )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  item<-reactive({input$radio})
  select_year<-reactive({input$select})
  s_country<-reactive({input$checkGroup})
  s_index<-reactive({input$checkGroup2})
  file<-reactive({list.files(path=path,pattern=paste0(item(),"市場吸引力"))})
  combine1<-reactive({
    combine<-c()
    country2<-c()
    for(i in 1:length(file())){
      data <- read.csv(paste0(path,list.files(path=path,pattern=paste0(item(),"市場吸引力"))[i]),fileEncoding="Big-5",stringsAsFactors = FALSE,as.is=T)
      subdata<-data
      country1<-substr(paste0(file()[i]),1,regexpr(paste0(item()),paste0(file()[i]))[1]-1)
      subdata2<-cbind(country1,subdata)
      
      colnames(subdata2)<-col
      
      combine<-rbind(combine,subdata2)
      country2<-c(country2,country1)
    }
    refine<-combine

    
    for(i in 15:20){
      refine[,i]<-suppressWarnings(as.numeric(sub("%","",refine[,i]))/100)
    }
    refine[,3:ncol(refine)]<-suppressWarnings(sapply(refine[,3:ncol(refine)], as.numeric))
    refine[which(refine[,1]=="巴林" | refine[,1]=="加拿大"|refine[,1]=="阿聯"|refine[,1]=="香港"|refine[,1]=="新加坡"),3]<-0
    for(i in 1:length(levels(refine[,1]))){
      sub_refine<-refine[which(refine[,1]==levels(refine[,1])[i]),]
      distance<-max(sub_refine[which(!is.na(sub_refine[,22])),22],na.rm=T)
      refine[which(refine[,1]==levels(refine[,1])[i]),22]<-distance}
    
    
    
    market_size<-refine[,3]*refine[,4]/1000+refine[,8]-refine[,6]
    TSI<-(refine[,6]-refine[,8])/(refine[,6]+refine[,8])
    trend<-(refine[,8]-c(NA,refine[1:(nrow(refine)-1),8]))/c(NA,refine[1:(nrow(refine)-1),8])
    market_share<-(refine[,8]/refine[,9])
    
    relative_strength<-c()
    for(i in 1:length(levels(refine[,1]))){
      
      sub_refine<-refine[which(refine[,1]==levels(refine[,1])[i]),]
      
      if(as.character(sub_refine[1,1])!="阿聯"){
        imp11<-sub_refine[which(sub_refine[,2]==2011),8]
        glo11<-sub_refine[which(sub_refine[,2]==2011),9]
        strength<-(sub_refine[,8]-imp11)/imp11-(sub_refine[,9]-glo11)/glo11
        m_strength<-matrix(strength,ncol=1)
        relative_strength<-rbind(relative_strength,m_strength)
      }
      else{
        imp12<-sub_refine[which(sub_refine[,2]==2012),8]
        glo12<-sub_refine[which(sub_refine[,2]==2012),9]
        strength<-(sub_refine[,8]-imp12)/imp12-(sub_refine[,9]-glo12)/glo12
        m_strength<-matrix(strength,ncol=1)
        relative_strength<-rbind(relative_strength,m_strength)
        
      }
    }
    
    
    
    
    relative_strength[which(refine[,2]<2011),1]<-NA
    import_market<-refine[,8]/refine[,9]-refine[,21]/refine[,14]
    open_level<-refine[,15]
    
    
    relative_tariff<-rowMeans(refine[,16:20],na.rm=T)-refine[,15]
    relative_tariff[which(relative_tariff %in% "NaN")]<-NA
    
    
    country<-as.character(refine[,1])
    year<-refine[,2]
    production<-refine[,3]
    export_value<-refine[,6]
    import_value<-refine[,8]
    GDP<-refine[,12]
    population<-refine[,13]
    Taiwan_market_distance<-refine[,22]
    
    
    
    
    combine_refine<-data.frame(country,year,production,export_value,import_value,market_size,TSI,trend,
                               market_share,GDP,population,Taiwan_market_distance,relative_strength,
                               import_market,open_level,relative_tariff)
    
    sub_combine_refine<-combine_refine[which(combine_refine[,2]==select_year()),]
    sub_combine_refine<-sub_combine_refine[which(sub_combine_refine[,1] %in% s_country()),]
    
    #####
    #calculate values
    production_v<-c()
    if(length(which(is.na(sub_combine_refine[,"production"])))>0){
      production_v<-0} else{
        production_v<-suppressWarnings((5-4*(sub_combine_refine[,"production"]-min(sub_combine_refine[,"production"]))/(max(sub_combine_refine[,"production"])-min(sub_combine_refine[,"production"])))*input$slider1)
      }
    
    export_value_v<-c()
    if(length(which(is.na(sub_combine_refine[,"export_value"])))>0){
      export_value_v<-0} else{
        export_value_v<-suppressWarnings((5-4*(sub_combine_refine[,"export_value"]-min(sub_combine_refine[,"export_value"]))/(max(sub_combine_refine[,"export_value"])-min(sub_combine_refine[,"export_value"])))*input$slider2)
      }
    
    import_value_v<-c()
    if(length(which(is.na(sub_combine_refine[,"import_value"])))>0){
      import_value_v<-0} else{
        import_value_v<-suppressWarnings((1+4*(sub_combine_refine[,"import_value"]-min(sub_combine_refine[,"import_value"]))/(max(sub_combine_refine[,"import_value"])-min(sub_combine_refine[,"import_value"])))*input$slider3)
      }
    
    market_size_v<-c()
    if(length(which(is.na(sub_combine_refine[,"market_size"])))>0){
      market_size_v<-0} else{
        market_size_v<-suppressWarnings((1+4*(sub_combine_refine[,"market_size"]-min(sub_combine_refine[,"market_size"]))/(max(sub_combine_refine[,"market_size"])-min(sub_combine_refine[,"market_size"])))*input$slider4)
      }
    
    
    TSI_v<-c()
    if(length(which(is.na(sub_combine_refine[,"TSI"])))>0){
      TSI_v<-0} else{
        TSI_v<-suppressWarnings((5-4*(sub_combine_refine[,"TSI"]-(-1))/(1-(-1)))*input$slider5)
      }
    
    
    trend_v<-c()
    if(length(which(is.na(sub_combine_refine[,"trend"])))>0){
      trend_v<-0} else{
        trend_v<-suppressWarnings((1+4*(sub_combine_refine[,"trend"]-min(sub_combine_refine[,"trend"]))/(max(sub_combine_refine[,"trend"])-min(sub_combine_refine[,"trend"])))*input$slider6)
      }
    
    
    market_share_v<-c()
    if(length(which(is.na(sub_combine_refine[,"market_share"])))>0){
      market_share_v<-0} else{
        market_share_v<-suppressWarnings((1+4*(sub_combine_refine[,"market_share"]-min(sub_combine_refine[,"market_share"]))/(max(sub_combine_refine[,"market_share"])-min(sub_combine_refine[,"market_share"])))*input$slider7)
      }
    
    
    GDP_v<-c()
    if(length(which(is.na(sub_combine_refine[,"GDP"])))>0){
      GDP_v<-0} else{
        GDP_v<-suppressWarnings((1+4*(sub_combine_refine[,"GDP"]-min(sub_combine_refine[,"GDP"]))/(max(sub_combine_refine[,"GDP"])-min(sub_combine_refine[,"GDP"])))*input$slider8)
      }
    
    
    population_v<-c()
    if(length(which(is.na(sub_combine_refine[,"population"])))>0){
      population_v<-0} else{
        population_v<-suppressWarnings((1+4*(sub_combine_refine[,"population"]-min(sub_combine_refine[,"population"]))/(max(sub_combine_refine[,"population"])-min(sub_combine_refine[,"population"])))*input$slider9)
      }
    
    
    Taiwan_market_distance_v<-c()
    if(length(which(is.na(sub_combine_refine[,"Taiwan_market_distance"])))>0){
      Taiwan_market_distance_v<-0} else{
        Taiwan_market_distance_v<-suppressWarnings((5-4*(sub_combine_refine[,"Taiwan_market_distance"]-min(sub_combine_refine[,"Taiwan_market_distance"]))/(max(sub_combine_refine[,"Taiwan_market_distance"])-min(sub_combine_refine[,"Taiwan_market_distance"])))*input$slider10)
      }
    
    
    relative_strength_v<-rep(0,length(sub_combine_refine[,"relative_strength"]))
    if(length(which(is.na(sub_combine_refine[,"relative_strength"])))>0){
      relative_strength_v<-0} else{
        relative_strength_v[which(sub_combine_refine[,"relative_strength"]>=0.15)]<-suppressWarnings((5)*input$slider11)
        relative_strength_v[which(sub_combine_refine[,"relative_strength"]<=(-0.15))]<-suppressWarnings((1)*input$slider11)
        relative_strength_v[which(sub_combine_refine[,"relative_strength"]>(-0.15)&sub_combine_refine[,"relative_strength"]<0.15)]<-suppressWarnings((1+4*(sub_combine_refine[which(sub_combine_refine[,"relative_strength"]>(-0.15)&sub_combine_refine[,"relative_strength"]<0.15),"relative_strength"]-(-0.15))/(0.15-(-0.15)))*input$slider11)
        
      }
    
    
    
    import_market_v<-rep(0,length(sub_combine_refine[,"import_market"]))
    
    if(length(which(is.na(sub_combine_refine[,"import_market"])))>0){
      import_market_v<-0} else{
        import_market_v[which(sub_combine_refine[,"import_market"]>=0.05)]<-suppressWarnings((5)*input$slider12)
        import_market_v[which(sub_combine_refine[,"import_market"]<=0)]<-suppressWarnings((1)*input$slider12)
        import_market_v[which(sub_combine_refine[,"import_market"]>0&sub_combine_refine[,"import_market"]<0.05)]<-suppressWarnings((1+4*(sub_combine_refine[which(sub_combine_refine[,"import_market"]>0&sub_combine_refine[,"import_market"]<0.05),"import_market"]-0)/(0.05-0))*input$slider12)
        
      }
    
    
    
    open_level_v<-rep(0,length(sub_combine_refine[,"open_level"]))
    
    if(length(which(is.na(sub_combine_refine[,"open_level"])))>0){
      open_level_v<-0} else{
        open_level_v[which(sub_combine_refine[,"open_level"]<=0.05)]<-suppressWarnings((5)*input$slider13)
        open_level_v[which(sub_combine_refine[,"open_level"]>=0.3)]<-suppressWarnings((1)*input$slider13)
        open_level_v[which(sub_combine_refine[,"open_level"]>0.05&sub_combine_refine[,"open_level"]<0.3)]<-suppressWarnings((1+4*(sub_combine_refine[which(sub_combine_refine[,"open_level"]>0.05&sub_combine_refine[,"open_level"]<0.3),"open_level"]-0.3)/(0.05-0.3))*input$slider13)
        
      }
    
    relative_tariff_v<-rep(0,length(sub_combine_refine[,"relative_tariff"]))
    
    if(length(which(is.na(sub_combine_refine[,"relative_tariff"])))>0){
      relative_tariff_v<-0} else{
        relative_tariff_v[which(sub_combine_refine[,"relative_tariff"]>=0.1)]<-suppressWarnings((5)*input$slider14)
        relative_tariff_v[which(sub_combine_refine[,"relative_tariff"]<=(-0.1))]<-suppressWarnings((1)*input$slider14)
        relative_tariff_v[which(sub_combine_refine[,"relative_tariff"]>(-0.1)&sub_combine_refine[,"relative_tariff"]<0.1)]<-suppressWarnings((1+4*(sub_combine_refine[which(sub_combine_refine[,"relative_tariff"]>(-0.1)&sub_combine_refine[,"relative_tariff"]<0.1),"relative_tariff"]-(-0.1))/(0.1-(-0.1)))*input$slider14)
        
      }
    
    #complete_table<-data.frame(sub_combine_refine[,1:2],sub_combine_refine[,3],production_v,sub_combine_refine[,4],export_value_v,sub_combine_refine[,5],import_value_v,sub_combine_refine[,6],market_size_v,
     #                          sub_combine_refine[,7],TSI_v,sub_combine_refine[,8],trend_v,sub_combine_refine[,9],market_share_v,sub_combine_refine[,10],GDP_v,sub_combine_refine[,11],
      #                         population_v,sub_combine_refine[,12],Taiwan_market_distance_v,sub_combine_refine[,13],relative_strength_v,sub_combine_refine[,14],import_market_v,
       #                        sub_combine_refine[,15],open_level_v,sub_combine_refine[,16],relative_tariff_v)
    complete_table<-suppressWarnings(data.frame(sub_combine_refine[,1:2],sub_combine_refine[,3],production_v,sub_combine_refine[,4],export_value_v,sub_combine_refine[,5],import_value_v,sub_combine_refine[,6],market_size_v,
                                                     sub_combine_refine[,7],TSI_v,sub_combine_refine[,8],trend_v,sub_combine_refine[,9],market_share_v,sub_combine_refine[,10],GDP_v,sub_combine_refine[,11],
                                                  population_v,sub_combine_refine[,12],Taiwan_market_distance_v,sub_combine_refine[,13],relative_strength_v,sub_combine_refine[,14],import_market_v,
                                    sub_combine_refine[,15],open_level_v,sub_combine_refine[,16],relative_tariff_v
                               ))
    
    ##according select country and index to regenerate table
    #select_country<-complete_table[which(complete_table[,"country"] %in% s_country() ),"country"]

    #aa<-which(select_index[,"select2"]==1)
    
   # aa<-which(idr %in% s_index())
    #select_2<-c()
    #select_name<-c()
    #for(i in 1:length(aa)){
      #name<-as.character(select_index[aa[i],"indicator"])
     # select_name<-c(paste0(select_name),paste0(s_index()[i]),paste0(s_index()[i],"_分數"))
      #temp<-c((2+2*aa[i]-1),2+2*aa[i])
      
      #select_2<-c(select_2,temp)}
    select_name<-c()
    score<-paste0(idr,"_分數")
    for( i in 1:length(idr)){
      select_name<-c(select_name,idr[[i]][1],score[i])
    } 
  
    #select_table<-data.frame(country2[country2 %in% s_country()],select_year(),complete_table[which(country2 %in% s_country()),])
     select_table<-data.frame(complete_table)
     ind<-seq(1,length(idr))*2+2
     if(length(ind)==1){
       total<-select_table[,ind]
       #total<-complete_table[,ind]
     } else {
    #   #total<-rowSums (complete_table[,ind], na.rm = T, dims = 1)
       total<-rowSums (select_table[,ind], na.rm = T, dims = 1)
     }
     
     
    
     select_rank<-as.integer(rank(1/total))
    final_table<-cbind(select_table[,1:2],total,select_rank,select_table[,3:ncol(select_table)])
    # #final_table<-cbind(complete_table,total,select_rank)
     final_table1<-final_table
     colnames(final_table1)<-c("國家","年度","總分","排序",select_name)
    final_table1
    #sub_combine_refine
    #complete_table
    #data.frame(sub_combine_refine[,1:2],open_level_v,sub_combine_refine[,16],relative_tariff_v)
  })
  
  
  output$view<-renderTable({
  combine1()})
  
  
  output$distPlot <- renderPlotly({
    reshape<-data.frame()
    for(i in 1:nrow(combine1())){
      #Country<-as.character(rep(combine1()[i,"國家"],length(s_index())))
      Country<-as.character(rep(combine1()[i,"國家"],length(idr)))
      #Variable<-as.character(colnames(combine1()[,seq(1,length(which(idr %in% s_index())))*2+2]))
      Variable<-as.character(colnames(combine1()[,seq(1,length(idr))*2+4]))
      #Value<-as.numeric(combine1()[i,seq(1,length(which(idr %in% s_index())))*2+2])
      Value<-as.numeric(combine1()[i,seq(1,length(idr))*2+4])
      ddd<-data.frame(Country,Variable,Value)
      reshape<-rbind(reshape,ddd)
    }
    country2<-c()
    for(i in 1:length(file())){
      
      country1<-substr(paste0(file()[i]),1,regexpr(paste0(item()),paste0(file()[i]))[1]-1)
      
      country2<-c(country2,country1)
    }
    
    # Histogram on a Categorical variable
    g <- ggplot(reshape, aes(x=Country,y=Value,fill=Variable))
    p<-g + geom_bar(stat="identity")+geom_text(aes(country2[country2 %in% s_country()],combine1()[,"總分"]+3,fill=NULL,label=combine1()[,"排序"]),data=combine1(),vjust =-1)+
      ggtitle(paste0(input$radio,input$select,"市場吸引力"))+theme(plot.title = element_text(size = 24, face = "bold"))
    ggplotly(p,height = 600, width=1000)
  })
  #output$value <- renderPrint({str(combine1())})
  
  #download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$radio,"_",input$select,"_","市場吸引力", ".csv")
    },
    content = function(file) {
      write.csv(combine1(), file, row.names = FALSE,fileEncoding="Big-5")
    }
  )
  #download plot
  #output$downloadPlot <- downloadHandler(
   # filename = function() {
    #  paste0(input$radio,"_",input$select,"_","市場吸引力", ".png")
    #},
    #content = function(file) {
      #ggsave(file, device = "png", width=10, height=10)
    #}
  #)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)