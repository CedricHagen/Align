library(shiny)
library(dplyr)
library(ggplot2)
library(ggtext)
library(viridis)
source("~/Desktop/Align/dtw_r.R")
setwd("~/Desktop/Align/")

ui <- fluidPage(
  navbarPage("Align",
             tabPanel('Generate New Alignments',
                      sidebarPanel(
                        h3(strong("Upload new data")),
                        tags$hr(),
                        fileInput("targetFile", "Target dataset"),
                        helpText("Must be a .csv. Default max. file size is 5 Mb."),
                        helpText("Format: column 1 = d13C, column 2 = height"),
                        textInput("target_name", label = h5("Name of your target section")),
                        tags$hr(),
                        fileInput("candidateFile", "Candidate dataset(s) "),
                        helpText("Must be a .csv. Default max. file size is 5 Mb."),
                        helpText("Format: column 1 = d13C, column 2 = height"),
                        textInput("candidate_num", label = h5("How many candidate datasets are you warping?")),
                        helpText("Input an integer between 1 (minimum) and 3 (maximum)"),
                        textInput("candidate_names", label = h5("Name of your candidate section(s)")),
                        helpText("Separate multiple candidate names with commas"),
                        tags$hr(),
                        actionButton('run','Run DTW algorithm', 
                                     style="color: #fff; background-color: #7ab733; border-color: #45681d")),
                      mainPanel(
                        h3(strong("Review your uploaded data")),
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"), plotOutput("targetplot"), plotOutput("candidateplot"))
                        ),
                        uiOutput("candyplot"),
                        actionButton('plotnow','Plot', 
                                     style="color: #fff; background-color: #7ab733; border-color: #45681d")
                      )),
             tabPanel('Alignment library viewer',
              sidebarPanel(
               uiOutput("alignmentSelection"),
               hr(),
               sliderInput("xc_cut", label = h5("Choose your xc cutoff:"), min = 0, max = 1, value = 0.80, step = 0.05),
               sliderInput("ov_cut", label = h5("Choose your overlap cuttoff (%):"), min = 0, max = 100, value = 10, step = 5),
               textInput("critnick", label = h5("Criteria nickname"),value="June30_1pm"),
               actionButton('alignme','Narrow alignment library', 
                            style="color: #fff; background-color: #7ab733; border-color: #45681d"),
               hr(),
               uiOutput("culledalignmentSelection"),
               hr(),
               actionButton('narrowplot','Plot alignment', 
                            style="color: #fff; background-color: #7ab733; border-color: #45681d")),
              mainPanel("",conditionalPanel(condition = "input.agemodel == 'linear' || 'expdecay' || 'bchron'", h3(strong("DTW alignment")),mainPanel(plotOutput("alignmentplot")))))
             ))

server <- function(input, output, session) {

  observeEvent( input$plotnow,{
  inFile1 <- input$targetFile
  if (is.null(inFile1))
    return(NULL)
  targetvalues <- read.csv(inFile1$datapath)
  targetx<-as.numeric(targetvalues[,1])
  targety<-as.numeric(targetvalues[,2])
  targetdf<-data.frame(targetx,targety)
  names(targetdf)<-NULL
  write.csv(targetdf,'~/Desktop/Align/Cache/target.csv', row.names=FALSE)
  })
  
  observeEvent( input$plotnow,{
  inFile2 <- input$candidateFile
  if (is.null(inFile2))
    return(NULL)
  
  cnum=as.numeric(input$candidate_num[1])
  candidatevalues <- read.csv(inFile2$datapath)
  
  if (cnum>0) {
    candidate1x<-as.numeric(candidatevalues[,1])
    candidate1y<-as.numeric(candidatevalues[,2])
    candidate1df<-data.frame(candidate1x,candidate1y)
    names(candidate1df)<-NULL
    candidate1df <- na.omit(candidate1df)
    write.csv(candidate1df,'~/Desktop/Align/Cache/candidate1.csv', row.names=FALSE)
  }
  if (cnum>1) {
    candidate2x<-as.numeric(candidatevalues[,4])
    candidate2y<-as.numeric(candidatevalues[,5])
    candidate2df<-data.frame(candidate2x,candidate2y)
    names(candidate2df)<-NULL
    candidate2df <- na.omit(candidate2df)
    write.csv(candidate2df,'~/Desktop/Align/Cache/candidate2.csv', row.names=FALSE)
  }
  if (cnum>2) {
    candidate3x<-as.numeric(candidatevalues[,7])
    candidate3y<-as.numeric(candidatevalues[,8])
    candidate3df<-data.frame(candidate3x,candidate3y)
    names(candidate3df)<-NULL
    candidate3df <- na.omit(candidate3df)
    write.csv(candidate3df,'~/Desktop/Align/Cache/candidate3.csv', row.names=FALSE)
  }
  })
  
  observeEvent( input$plotnow,{
  targetdata <- read.csv('~/Desktop/Align/Cache/target.csv')
  output$targetplot<-renderPlot({
    plot(targetdata[,1],targetdata[,2],xlim=c(min(targetdata[,1]),max(targetdata[,1])),ylim=c(min(targetdata[,2]),max(targetdata[,2])),xlab="",ylab="",col='black',pch=16,main="Target")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
})
  })


output$candyplot <- renderUI({
  selectInput("whichcandy", "Candidate record:", choices=unlist(strsplit(input$candidate_names,"[,]")))
})

observeEvent( input$plotnow,{
output$candidateplot <- renderPlot({
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==1){
    candidatexy <- read.csv('Cache/candidate1.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==2){
    candidatexy <- read.csv('Cache/candidate2.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==3){
    candidatexy <- read.csv('Cache/candidate3.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==4){
    candidatexy <- read.csv('Cache/candidate4.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==5){
    candidatexy <- read.csv('Cache/candidate5.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==6){
    candidatexy <- read.csv('Cache/candidate6.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==7){
    candidatexy <- read.csv('Cache/candidate7.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==8){
    candidatexy <- read.csv('Cache/candidate8.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichcandy)==9){
    candidatexy <- read.csv('Cache/candidate9.csv')
    plot(candidatexy[,1],candidatexy[,2],xlim=c(min(candidatexy[,1]),max(candidatexy[,1])),ylim=c(min(candidatexy[,2]),max(candidatexy[,2])),xlab="",ylab="",col='black',pch=16,main="Candidate")
    mtext("Height", side=2, line=2, font=2)
    mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  }
})
})

observeEvent(input$run, {
  
  inFile1 <- input$targetFile
  if (is.null(inFile1))
    return(NULL)

  inFile2 <- input$candidateFile
  if (is.null(inFile2))
    return(NULL)
  
 candynames<-unlist(strsplit(input$candidate_names,"[,]"))
 targyname=as.character(input$target_name)
 
  
  if (is.null(inFile1) || is.null(inFile2)) {
    showNotification("Please upload a target and candidate dataset",duration = 10, closeButton = TRUE, type = "error")
  } else {
    
    cnum=as.numeric(input$candidate_num[1])
    
    g=c(0.98,0.99,1,1.01)
    edge=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.11,0.12,0.13,0.14,0.15)
    
    if (cnum>0) {
      showNotification("Running DTW alignment 1...",duration = 20, closeButton = TRUE, type = "warning")
      targetdata <- read.csv('Cache/target.csv')
      candidatexy <- read.csv('Cache/candidate1.csv')
      
      dir.create(paste(candynames[1],'-',targyname,sep=''))
      dir.create(paste(candynames[1],'-',targyname,'/Output_Data',sep=''))
      dir.create(paste(candynames[1],'-',targyname,'/Output_Images',sep=''))
      
      png(file = paste(candynames[1],'-',targyname,'/Output_Images/',candynames[1],targyname,'_original.png',sep=''),width=1600,height=1200,res=200)
      orig=ggplot() +
        geom_line(aes(y=targetdata[,1], x=targetdata[,2]), color="black")+
        geom_line(aes(y=candidatexy[,1], x=candidatexy[,2]), color="red")+
        theme_classic()+
        ylab((expression(paste(delta^{13}, "C (\u2030)")))) + 
        xlab("Height (m)")
      print(orig)
      dev.off()
      
      
      xcvals1=vector()
      for (i in 1:length(g)){
        for (j in 1:length(edge)){
          results=dtw_r(targetdata,candidatexy,g[i],edge[j])
          
          ri=unlist(results[1])
          t_out=unlist(results[2])
          xc=unlist(results[3])
          xcvals1=c(xcvals1,xc)
          
          xcdf<-data.frame(xcvals1)
          write.csv(xcdf,paste(candynames[1],'-',targyname,'/Output_Data/xcvals.csv',sep=''))

          png(file = paste(candynames[1],'-',targyname,'/Output_Images/',candynames[1],targyname,'_g',toString(g[i]),'_edge',toString(edge[j]),'.png',sep=''),width=800,height=1200,res=200)
          alignment = ggplot() +
            geom_point(aes(x=targetdata[,1],y=targetdata[,2]),colour="black",size=1) +
            geom_point(aes(x=ri, y=t_out),colour="blue",size=1) +
            theme_classic() +
            ggtitle(paste('r=',toString(round(xc,4)),' g=',toString(g[i]),' edge=',toString(edge[j]),sep='')) +
            xlab((expression(paste(delta^{13}, "C (\u2030)")))) +
            ylab("Height (m)")
          print(alignment)
          dev.off()

          df=data.frame(d13c=ri,m=t_out)
          write.csv(df,paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',toString(g[i]),'_edge',toString(edge[j]),'.csv',sep=''), row.names = FALSE)
        }
      }
      
      gvals=c(matrix(1,1,15)*0.98,matrix(1,1,15)*0.99,matrix(1,1,15)*1.00,matrix(1,1,15)*1.01)
      edgevals=c(seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01))
      df=data.frame(gvals,edgevals,xcvals1)
      png(file = paste(candynames[1],'-',targyname,'/Output_Images/',candynames[1],targyname,'_corr.png',sep=''),width=1600,height=1200,res=200)
      corr=ggplot(df, aes(x=edgevals, y=gvals, fill=xcvals1))+
        geom_tile()+
        theme_classic()+
        #scale_fill_distiller(palette = "viridis")
        scale_fill_viridis(name="",limits=c(0,1))+
        xlab('Edge value') +
        ylab('G value')
      print(corr)
      dev.off()
      
      showNotification("DTW alignment 1 complete!",duration = 20, closeButton = TRUE, type = "message")
    }
    
    if (cnum>1) {
      showNotification("Running DTW alignment 2...",duration = 20, closeButton = TRUE, type = "warning")
      
      targetdata <- read.csv('Cache/target.csv')
      candidatexy <- read.csv('Cache/candidate2.csv')
      
      dir.create(paste(candynames[2],'-',targyname,sep=''))
      dir.create(paste(candynames[2],'-',targyname,'/Output_Data',sep=''))
      dir.create(paste(candynames[2],'-',targyname,'/Output_Images',sep=''))
      
      png(file = paste(candynames[2],'-',targyname,'/Output_Images/',candynames[2],targyname,'_original.png',sep=''),width=1600,height=1200,res=200)
      orig=ggplot() +
        geom_line(aes(y=targetdata[,1], x=targetdata[,2]), color="black")+
        geom_line(aes(y=candidatexy[,1], x=candidatexy[,2]), color="red")+
        theme_classic()+
        ylab((expression(paste(delta^{13}, "C (\u2030)")))) + 
        xlab("Height (m)")
      print(orig)
      dev.off()
      
      
      xcvals2=vector()
      for (i in 1:length(g)){
        for (j in 1:length(edge)){
          results=dtw_r(targetdata,candidatexy,g[i],edge[j])
          
          ri=unlist(results[1])
          t_out=unlist(results[2])
          xc=unlist(results[3])
          xcvals2=c(xcvals2,xc)
          
          xcdf<-data.frame(xcvals2)
          write.csv(xcdf,paste(candynames[2],'-',targyname,'/Output_Data/xcvals.csv',sep=''))
          
          
          png(file = paste(candynames[2],'-',targyname,'/Output_Images/',candynames[2],targyname,'_g',toString(g[i]),'_edge',toString(edge[j]),'.png',sep=''),width=800,height=1200,res=200)
          alignment = ggplot() +
            geom_point(aes(x=targetdata[,1],y=targetdata[,2]),colour="black",size=1) +
            geom_point(aes(x=ri, y=t_out),colour="blue",size=1) +
            theme_classic() +
            ggtitle(paste('r=',toString(round(xc,4)),' g=',toString(g[i]),' edge=',toString(edge[j]),sep='')) +
            xlab((expression(paste(delta^{13}, "C (\u2030)")))) +
            ylab("Height (m)")
          print(alignment)
          dev.off()
          
          df=data.frame(d13c=ri,m=t_out)
          write.csv(df,paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',toString(g[i]),'_edge',toString(edge[j]),'.csv',sep=''), row.names = FALSE)
        }
      }
      
      gvals=c(matrix(1,1,15)*0.98,matrix(1,1,15)*0.99,matrix(1,1,15)*1.00,matrix(1,1,15)*1.01)
      edgevals=c(seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01))
      df=data.frame(gvals,edgevals,xcvals2)
      png(file = paste(candynames[2],'-',targyname,'/Output_Images/',candynames[2],targyname,'_corr.png',sep=''),width=1600,height=1200,res=200)
      corr=ggplot(df, aes(x=edgevals, y=gvals, fill=xcvals2))+
        geom_tile()+
        theme_classic()+
        #scale_fill_distiller(palette = "viridis")
        scale_fill_viridis(name="",limits=c(0,1))+
        xlab('Edge value') +
        ylab('G value')
      print(corr)
      dev.off()
      
      showNotification("DTW alignment 2 complete!",duration = 20, closeButton = TRUE, type = "message")
    }
    
    if (cnum>2) {
      showNotification("Running DTW alignment 3...",duration = 20, closeButton = TRUE, type = "warning")
      targetdata <- read.csv('Cache/target.csv')
      candidatexy <- read.csv('Cache/candidate3.csv')
      
      dir.create(paste(candynames[3],'-',targyname,sep=''))
      dir.create(paste(candynames[3],'-',targyname,'/Output_Data',sep=''))
      dir.create(paste(candynames[3],'-',targyname,'/Output_Images',sep=''))
      
      png(file = paste(candynames[3],'-',targyname,'/Output_Images/',candynames[3],targyname,'_original.png',sep=''),width=1600,height=1200,res=200)
      orig=ggplot() +
        geom_line(aes(y=targetdata[,1], x=targetdata[,2]), color="black")+
        geom_line(aes(y=candidatexy[,1], x=candidatexy[,2]), color="red")+
        theme_classic()+
        ylab((expression(paste(delta^{13}, "C (\u2030)")))) + 
        xlab("Height (m)")
      print(orig)
      dev.off()
      
      
      xcvals3=vector()
      for (i in 1:length(g)){
        for (j in 1:length(edge)){
          results=dtw_r(targetdata,candidatexy,g[i],edge[j])
          
          ri=unlist(results[1])
          t_out=unlist(results[2])
          xc=unlist(results[3])
          xcvals3=c(xcvals3,xc)
          
          xcdf<-data.frame(xcvals3)
          write.csv(xcdf,paste(candynames[3],'-',targyname,'/Output_Data/xcvals.csv',sep=''))
          
          
          png(file = paste(candynames[3],'-',targyname,'/Output_Images/',candynames[3],targyname,'_g',toString(g[i]),'_edge',toString(edge[j]),'.png',sep=''),width=800,height=1200,res=200)
          alignment = ggplot() +
            geom_point(aes(x=targetdata[,1],y=targetdata[,2]),colour="black",size=1) +
            geom_point(aes(x=ri, y=t_out),colour="blue",size=1) +
            theme_classic() +
            ggtitle(paste('r=',toString(round(xc,4)),' g=',toString(g[i]),' edge=',toString(edge[j]),sep='')) +
            xlab((expression(paste(delta^{13}, "C (\u2030)")))) +
            ylab("Height (m)")
          print(alignment)
          dev.off()
          
          df=data.frame(d13c=ri,m=t_out)
          write.csv(df,paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',toString(g[i]),'_edge',toString(edge[j]),'.csv',sep=''), row.names = FALSE)
        }
      }
      
      gvals=c(matrix(1,1,15)*0.98,matrix(1,1,15)*0.99,matrix(1,1,15)*1.00,matrix(1,1,15)*1.01)
      edgevals=c(seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01),seq(0.01,0.15,by=0.01))
      df=data.frame(gvals,edgevals,xcvals3)
      png(file = paste(candynames[3],'-',targyname,'/Output_Images/',candynames[3],targyname,'_corr.png',sep=''),width=1600,height=1200,res=200)
      corr=ggplot(df, aes(x=edgevals, y=gvals, fill=xcvals3))+
        geom_tile()+
        theme_classic()+
        #scale_fill_distiller(palette = "viridis")
        scale_fill_viridis(name="",limits=c(0,1))+
        xlab('Edge value') +
        ylab('G value')
      print(corr)
      dev.off()
      
      showNotification("DTW alignment 3 complete!",duration = 20, closeButton = TRUE, type = "message")
    }
    
  }
})

output$secondSelection <- renderUI({
  selectInput("whichcandidate", "Candidate record:", choices=unlist(strsplit(input$candidate_names,"[,]")))
})




output$alignmentSelection <- renderUI({
  selectInput("whichalignment", "Candidate record:", choices=unlist(strsplit(input$candidate_names,"[,]")))
})

queryList<-reactiveValues()
queryList$values<-c()

observeEvent(input$alignme, {
  inFile <- input$candidateFile
  if (is.null(inFile))
    return(NULL)
  candidatevalues <- read.csv(inFile$datapath)
  cnum=as.numeric(input$candidate_num[1])
  
  inFile <- input$targetFile
  targetvalues <- read.csv(inFile$datapath)
  targetx<-as.numeric(targetvalues[,1])
  targety<-as.numeric(targetvalues[,2])
  candynames<-unlist(strsplit(input$candidate_names,"[,]"))
  targyname=as.character(input$target_name)
  
  gname<-as.character(input$gval)
  edgename<-as.character(input$edgeval)

  gvalues=c(0.98,0.99,1,1.01)
  edgevalues=c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15)

  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==1){

  overlap<-as.numeric()
  for (n in gvalues){
    for (m in edgevalues){
      gv=as.character(n)
      ev=as.character(m)
      alignvals <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',gv,'_edge',ev,'.csv',sep=""),header=TRUE)
      alignx<-as.numeric(alignvals[,1])
      aligny<-as.numeric(alignvals[,2])
      ind1<-which(targety==min(aligny))
      if (length(ind1)==0){ind1<-which(targety==min(targety))}
      if (length(ind1)>1){ind1<-ind1[1]}
      ind2<-which(targety==max(aligny)) #xtoy
      if (length(ind2)==0){ind2<-which(targety==max(targety))}
      if (length(ind2)>1){ind2<-ind2[1]}
      ov=ind2-ind1
      pov=(ov/length(alignx))*100
      overlap<-c(overlap,pov)
    }
  }
  
    xcvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/xcvals.csv',sep=''),header=TRUE)
  
  xcvals_cull<-unique(xcvals1$xcvals1)
  xcvals_cull<-xcvals_cull[which(xcvals_cull>input$xc_cut)]
  overlap_cull<-overlap[which(overlap>input$ov_cut)]
  #add sim_cut code here to further refine libraries
  
  xcvals_ind<-as.numeric()
  for (n in xcvals_cull){
    ind=which(xcvals1$xcvals1==n)
    if (length(ind)>1){ind<-ind[1]}
    xcvals_ind<-c(xcvals_ind,ind)
  }
  
  overlap_ind<-as.numeric()
  for (n in overlap_cull){
    ind=which(overlap==n)
    if (length(ind)>1){ind<-ind[1]}
    overlap_ind<-c(overlap_ind,ind)
  }
  
  alignment_inds=intersect(xcvals_ind,overlap_ind)
  newgvalues<-as.numeric()
  newedgevalues<-as.numeric()
  for (n in alignment_inds){
    if (n<16){
      newgvalues<-c(newgvalues,0.98)
      newedgevalues<-c(newedgevalues,n/100)
      }
    if (n>15&n<31){
      newgvalues<-c(newgvalues,0.99)
      newedgevalues<-c(newedgevalues,(n-15)/100)
      }
    if (n>30&n<46){
      newgvalues<-c(newgvalues,1)
      newedgevalues<-c(newedgevalues,(n-30)/100)
      }
    if (n>45){
      newgvalues<-c(newgvalues,1.01)
      newedgevalues<-c(newedgevalues,(n-45)/100)
      }
  }
  
  culleddf<-data.frame(newgvalues,newedgevalues)
  names(culleddf)<-NULL
  culleddf <- na.omit(culleddf)
  write.csv(culleddf, paste(candynames[1],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""), row.names=FALSE)
  queryList$values<-alignment_inds
  
  if (length(newgvalues)<1){
    showNotification('ERROR: No alignments meet that criteria!',duration=8,type='error')
    plotflag<-99
  } else {
    plotflag<-1
  }
  pfdf<-data.frame(plotflag)
  write.csv(pfdf,'Cache/plotflag.csv', row.names=FALSE)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==2){

    overlap<-as.numeric()
    for (n in gvalues){
      for (m in edgevalues){
        gv=as.character(n)
        ev=as.character(m)
        alignvals <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',gv,'_edge',ev,'.csv',sep=""),header=TRUE)
        alignx<-as.numeric(alignvals[,1])
        aligny<-as.numeric(alignvals[,2])
        ind1<-which(targety==min(alignx))
        if (length(ind1)==0){ind1<-which(targety==min(targety))}
        if (length(ind1)>1){ind1<-ind1[1]}
        ind2<-which(targety==max(alignx))
        if (length(ind2)==0){ind2<-which(targety==max(targety))}
        if (length(ind2)>1){ind2<-ind2[1]}
        ov=ind2-ind1
        pov=(ov/length(alignx))*100
        overlap<-c(overlap,pov)
      }
    }
    
    xcvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
    xcvals_cull<-unique(xcvals2$xcvals2)
    xcvals_cull<-xcvals_cull[which(xcvals_cull>input$xc_cut)]
    overlap_cull<-overlap[which(overlap>input$ov_cut)]
    #add sim_cut code here to further refine libraries
    
    xcvals_ind<-as.numeric()
    for (n in xcvals_cull){
      ind=which(xcvals2$xcvals2==n)
      if (length(ind)>1){ind<-ind[1]}
      xcvals_ind<-c(xcvals_ind,ind)
    }
    
    overlap_ind<-as.numeric()
    for (n in overlap_cull){
      ind=which(overlap==n)
      if (length(ind)>1){ind<-ind[1]}
      overlap_ind<-c(overlap_ind,ind)
    }
    
    alignment_inds=intersect(xcvals_ind,overlap_ind)
    newgvalues<-as.numeric()
    newedgevalues<-as.numeric()
    for (n in alignment_inds){
      if (n<16){
        newgvalues<-c(newgvalues,0.98)
        newedgevalues<-c(newedgevalues,n/100)
      }
      if (n>15&n<31){
        newgvalues<-c(newgvalues,0.99)
        newedgevalues<-c(newedgevalues,(n-15)/100)
      }
      if (n>30&n<46){
        newgvalues<-c(newgvalues,1)
        newedgevalues<-c(newedgevalues,(n-30)/100)
      }
      if (n>45){
        newgvalues<-c(newgvalues,1.01)
        newedgevalues<-c(newedgevalues,(n-45)/100)
      }
    }
    
    culleddf<-data.frame(newgvalues,newedgevalues)
    names(culleddf)<-NULL
    culleddf <- na.omit(culleddf)
    write.csv(culleddf, paste(candynames[2],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""), row.names=FALSE)
    queryList$values<-alignment_inds
    
    if (length(newgvalues)<1){
      showNotification('ERROR: No alignments meet that criteria!',duration=8,type='error')
      plotflag<-99
    } else {
      plotflag<-1
    }
    pfdf<-data.frame(plotflag)
    write.csv(pfdf,'Cache/plotflag.csv', row.names=FALSE)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==3){
    
    overlap<-as.numeric()
    for (n in gvalues){
      for (m in edgevalues){
        gv=as.character(n)
        ev=as.character(m)
        alignvals <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',gv,'_edge',ev,'.csv',sep=""),header=TRUE)
        alignx<-as.numeric(alignvals[,1])
        aligny<-as.numeric(alignvals[,2])
        ind1<-which(targety==min(alignx))
        if (length(ind1)==0){ind1<-which(targety==min(targety))}
        if (length(ind1)>1){ind1<-ind1[1]}
        ind2<-which(targety==max(alignx))
        if (length(ind2)==0){ind2<-which(targety==max(targety))}
        if (length(ind2)>1){ind2<-ind2[1]}
        ov=ind2-ind1
        pov=(ov/length(alignx))*100
        overlap<-c(overlap,pov)
      }
    }
    
    xcvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
    xcvals_cull<-unique(xcvals3$xcvals3)
    xcvals_cull<-xcvals_cull[which(xcvals_cull>input$xc_cut)]
    overlap_cull<-overlap[which(overlap>input$ov_cut)]
    #add sim_cut code here to further refine libraries
    
    xcvals_ind<-as.numeric()
    for (n in xcvals_cull){
      ind=which(xcvals3$xcvals3==n)
      if (length(ind)>1){ind<-ind[1]}
      xcvals_ind<-c(xcvals_ind,ind)
    }
    
    overlap_ind<-as.numeric()
    for (n in overlap_cull){
      ind=which(overlap==n)
      if (length(ind)>1){ind<-ind[1]}
      overlap_ind<-c(overlap_ind,ind)
    }
    
    alignment_inds=intersect(xcvals_ind,overlap_ind)
    newgvalues<-as.numeric()
    newedgevalues<-as.numeric()
    for (n in alignment_inds){
      if (n<16){
        newgvalues<-c(newgvalues,0.98)
        newedgevalues<-c(newedgevalues,n/100)
      }
      if (n>15&n<31){
        newgvalues<-c(newgvalues,0.99)
        newedgevalues<-c(newedgevalues,(n-15)/100)
      }
      if (n>30&n<46){
        newgvalues<-c(newgvalues,1)
        newedgevalues<-c(newedgevalues,(n-30)/100)
      }
      if (n>45){
        newgvalues<-c(newgvalues,1.01)
        newedgevalues<-c(newedgevalues,(n-45)/100)
      }
    }
    
    culleddf<-data.frame(newgvalues,newedgevalues)
    names(culleddf)<-NULL
    culleddf <- na.omit(culleddf)
    write.csv(culleddf, paste(candynames[3],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""), row.names=FALSE)
    queryList$values<-alignment_inds
    
    if (length(newgvalues)<1){
      showNotification('ERROR: No alignments meet that criteria!',duration=8,type='error')
      plotflag<-99
    } else {
      plotflag<-1
    }
    pfdf<-data.frame(plotflag)
    write.csv(pfdf,'Cache/plotflag.csv', row.names=FALSE)
  }
})

output$culledalignmentSelection <- renderUI({
  selectInput("whichculled", "Candidate record:", choices=paste('Alignment ',as.character(1:length(queryList$values)),sep=""),"[,]")
})
  observeEvent(input$narrowplot, {

  inFile <- input$targetFile
  targetvalues <- read.csv(inFile$datapath)
  targetx<-as.numeric(targetvalues[,1])
  targety<-as.numeric(targetvalues[,2])
  candynames<-unlist(strsplit(input$candidate_names,"[,]"))
  targyname=as.character(input$target_name)
  pf <- read.csv('Cache/plotflag.csv',header=TRUE)
  plotflag<-as.numeric(pf[,1])
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==1 && plotflag==1){
  
  dir.create(paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick, sep=""))
      
  cullval<-read.csv(paste(candynames[1],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""),header=FALSE)
  newgvalues<-as.numeric(cullval[,1])
  newedgevalues<-as.numeric(cullval[,2])
    
  cullname<-input$whichculled
  cullname<-unlist(strsplit(cullname," "))
  cull_ind<-as.numeric(cullname[2])
  
  cull_g<-newgvalues[cull_ind]
  cull_edge<-newedgevalues[cull_ind]
  
  cullalign<-read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',cull_g,'_edge',cull_edge,'.csv',sep=""),header=TRUE)
  cullalignx<-as.numeric(cullalign[,1])
  cullaligny<-as.numeric(cullalign[,2])
  
  corrvals <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
  
  if (cull_g==0.98){
    cull_xc<-corrvals$xcvals1[cull_edge*100]
  }
  if (cull_g==0.99){
    cull_xc<-corrvals$xcvals1[15+cull_edge*100]
  }
  if (cull_g==1){
    cull_xc<-corrvals$xcvals1[30+cull_edge*100]
  }
  if (cull_g==1.01){
    cull_xc<-corrvals$xcvals1[45+cull_edge*100]
  }
  
  aligneddf=data.frame(cullalignx,cullaligny)
  alignedxc=data.frame(cull_xc,cull_g,cull_edge)
  write.csv(aligneddf, '~/Desktop/Align/Cache/culled_data_plotting.csv', row.names=FALSE)
  write.csv(alignedxc, '~/Desktop/Align/Cache/culled_vals_plotting.csv', row.names=FALSE)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==2 && plotflag==1){
    
    dir.create(paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick, sep=""))
    
    cullval<-read.csv(paste(candynames[2],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""),header=FALSE)
    newgvalues<-as.numeric(cullval[,1])
    newedgevalues<-as.numeric(cullval[,2])
    
    cullname<-input$whichculled
    cullname<-unlist(strsplit(cullname," "))
    cull_ind<-as.numeric(cullname[2])
    
    cull_g<-newgvalues[cull_ind]
    cull_edge<-newedgevalues[cull_ind]
    
    cullalign<-read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',cull_g,'_edge',cull_edge,'.csv',sep=""),header=TRUE)
    cullalignx<-as.numeric(cullalign[,1])
    cullaligny<-as.numeric(cullalign[,2])
    
    corrvals <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
    
    if (cull_g==0.98){
      cull_xc<-corrvals$xcvals2[cull_edge*100]
    }
    if (cull_g==0.99){
      cull_xc<-corrvals$xcvals2[15+cull_edge*100]
    }
    if (cull_g==1){
      cull_xc<-corrvals$xcvals2[30+cull_edge*100]
    }
    if (cull_g==1.01){
      cull_xc<-corrvals$xcvals2[45+cull_edge*100]
    }
    
    aligneddf=data.frame(cullalignx,cullaligny)
    alignedxc=data.frame(cull_xc,cull_g,cull_edge)
    write.csv(aligneddf, '~/Desktop/Align/Cache/culled_data_plotting.csv', row.names=FALSE)
    write.csv(alignedxc, '~/Desktop/Align/Cache/culled_vals_plotting.csv', row.names=FALSE)
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==3 && plotflag==1){
    
    dir.create(paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick, sep=""))
               
    cullval<-read.csv(paste(candynames[3],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""),header=FALSE)
    newgvalues<-as.numeric(cullval[,1])
    newedgevalues<-as.numeric(cullval[,2])
    
    cullname<-input$whichculled
    cullname<-unlist(strsplit(cullname," "))
    cull_ind<-as.numeric(cullname[2])
    
    cull_g<-newgvalues[cull_ind]
    cull_edge<-newedgevalues[cull_ind]
    
    cullalign<-read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',cull_g,'_edge',cull_edge,'.csv',sep=""),header=TRUE)
    cullalignx<-as.numeric(cullalign[,1])
    cullaligny<-as.numeric(cullalign[,2])
    
    corrvals <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
    
    if (cull_g==0.98){
      cull_xc<-corrvals$xcvals3[cull_edge*100]
    }
    if (cull_g==0.99){
      cull_xc<-corrvals$xcvals3[15+cull_edge*100]
    }
    if (cull_g==1){
      cull_xc<-corrvals$xcvals3[30+cull_edge*100]
    }
    if (cull_g==1.01){
      cull_xc<-corrvals$xcvals3[45+cull_edge*100]
    }
    
    aligneddf=data.frame(cullalignx,cullaligny)
    alignedxc=data.frame(cull_xc,cull_g,cull_edge)
    write.csv(aligneddf, '~/Desktop/Align/Cache/culled_data_plotting.csv', row.names=FALSE)
    write.csv(alignedxc, '~/Desktop/Align/Cache/culled_vals_plotting.csv', row.names=FALSE)
  }
})
  
  
  observeEvent(input$narrowplot,{
  
  inFile <- input$candidateFile
  if (is.null(inFile))
    return(NULL)
  candidatevalues <- read.csv(inFile$datapath)
  cnum=as.numeric(input$candidate_num[1])
    
  inFile <- input$targetFile
  targetvalues <- read.csv(inFile$datapath)
  targetx<-as.numeric(targetvalues[,1])
  targety<-as.numeric(targetvalues[,2])
  candynames<-unlist(strsplit(input$candidate_names,"[,]"))
  targyname=as.character(input$target_name)
    
  cullalign <- read.csv('Cache/culled_data_plotting.csv',header=TRUE)
  cullalignx<-as.numeric(cullalign[,1])
  cullaligny<-as.numeric(cullalign[,2])
  cullalign_vals <- read.csv('Cache/culled_vals_plotting.csv',header=TRUE)
  cull_xc<-as.numeric(cullalign_vals[,1])
  cull_g<-as.numeric(cullalign_vals[,2])
  cull_edge<-as.numeric(cullalign_vals[,3])
  pf <- read.csv('Cache/plotflag.csv',header=TRUE)
  plotflag<-as.numeric(pf[,1])
  
  cull_xc<-round(cull_xc,digits=4)
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==1 && plotflag==1){
    
  output$alignmentplot <- renderPlot({
  plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullalignx),max(targetx,cullalignx)),ylim=c(min(targety,cullaligny),max(targety,cullaligny)),xlab="",ylab="",col='grey',pch=16)
  mtext("Height", side=2, line=2, font=2)
  mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  points(cullalignx,cullaligny,xlab="",ylab="",col='blue',pch=16)
  mtext(paste("g = ",as.character(cull_g),", edge = ",as.character(cull_edge)," , xc = ",as.character(cull_xc),sep=""))
  })
  
  tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/',as.character(input$whichculled),'_g',cull_g,'_edge',cull_edge,'.tiff',sep=""),units="in",width=6,height=8,res=300)
  plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullalignx),max(targetx,cullalignx)),ylim=c(min(targety,cullaligny),max(targety,cullaligny)),xlab="",ylab="",col='grey',pch=16)
  mtext("Height", side=2, line=2, font=2)
  mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  points(cullalignx,cullaligny,xlab="",ylab="",col='blue',pch=16)
  mtext(paste("g = ",as.character(cull_g),", edge = ",as.character(cull_edge)," , xc = ",as.character(cull_xc),sep=""))
  dev.off()
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==2 && plotflag==1){
  
  output$alignmentplot <- renderPlot({
  plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullalignx),max(targetx,cullalignx)),ylim=c(min(targety,cullaligny),max(targety,cullaligny)),xlab="",ylab="",col='grey',pch=16)
  mtext("Height", side=2, line=2, font=2)
  mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  points(cullalignx,cullaligny,xlab="",ylab="",col='blue',pch=16)
  mtext(paste("g = ",as.character(cull_g),", edge = ",as.character(cull_edge)," , xc = ",as.character(cull_xc),sep=""))
  })
    
  tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/',as.character(input$whichculled),'_g',cull_g,'_edge',cull_edge,'.tiff',sep=""),units="in",width=6,height=8,res=300)
  plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullalignx),max(targetx,cullalignx)),ylim=c(min(targety,cullaligny),max(targety,cullaligny)),xlab="",ylab="",col='grey',pch=16)
  mtext("Height", side=2, line=2, font=2)
  mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  points(cullalignx,cullaligny,xlab="",ylab="",col='blue',pch=16)
  mtext(paste("g = ",as.character(cull_g),", edge = ",as.character(cull_edge)," , xc = ",as.character(cull_xc),sep=""))
  dev.off()
  }
  
  if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==3 && plotflag==1){
    
  output$alignmentplot <- renderPlot({
  plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullalignx),max(targetx,cullalignx)),ylim=c(min(targety,cullaligny),max(targety,cullaligny)),xlab="",ylab="",col='grey',pch=16)
  mtext("Height", side=2, line=2, font=2)
  mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  points(cullalignx,cullaligny,xlab="",ylab="",col='blue',pch=16)
  mtext(paste("g = ",as.character(cull_g),", edge = ",as.character(cull_edge)," , xc = ",as.character(cull_xc),sep=""))
  })
    
  tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/',as.character(input$whichculled),'_g',cull_g,'_edge',cull_edge,'.tiff',sep=""),units="in",width=6,height=8,res=300)
  plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullalignx),max(targetx,cullalignx)),ylim=c(min(targety,cullaligny),max(targety,cullaligny)),xlab="",ylab="",col='grey',pch=16)
  mtext("Height", side=2, line=2, font=2)
  mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=2,font=2)
  points(cullalignx,cullaligny,xlab="",ylab="",col='blue',pch=16)
  mtext(paste("g = ",as.character(cull_g),", edge = ",as.character(cull_edge)," , xc = ",as.character(cull_xc),sep=""))
  dev.off()
  }
  
  })
  
  observeEvent(input$narrowplot,{
    
    candynames<-unlist(strsplit(input$candidate_names,"[,]"))
    targyname=as.character(input$target_name)
    pf <- read.csv('Cache/plotflag.csv',header=TRUE)
    plotflag<-as.numeric(pf[,1])
    
    if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==1 && plotflag==1){
      
    cullalign <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""),header=FALSE)
    cullxc <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
    all_xc <- as.numeric()
    all_g <- as.numeric()
    all_edge <- as.numeric()
    
    for (n in 1:length(cullalign[,1])){
      cullg<-cullalign[n,1]
      culledge<-cullalign[n,2]
      all_g<-c(all_g,cullg)
      all_edge<-c(all_edge,culledge)
      if (cullg==1.01){
        #cg<-4
        ci<-45
      }
      if (cullg==1.00){
        #cg<-3
        ci<-30
      }
      if (cullg==0.99){
        #cg<-2
        ci<-15
      }
      if (cullg==0.98){
        #cg<-1
        ci<-0
      }
      ce<-culledge*100
      indval<-ce+ci
      xcval<-cullxc[indval,2]
      all_xc<-c(all_xc,xcval)
    }
    
    sortthem<-sort(all_xc,decreasing=TRUE,index.return=TRUE)
    all_xc<-all_xc[sortthem$ix]
    all_g<-all_g[sortthem$ix]
    all_edge<-all_edge[sortthem$ix]
    
    if (length(all_xc)==1){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      dev.off()
      
    }
    
    if (length(all_xc)==2){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      par(mfrow=c(1,2))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      dev.off()
      
    }
    
    if (length(all_xc)==3){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      cullvals3 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
      cullx3<-as.numeric(cullvals3[,1])
      cully3<-as.numeric(cullvals3[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      par(mfrow=c(1,3))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
      dev.off()
    }
    
    if (length(all_xc)==4){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      cullvals3 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
      cullx3<-as.numeric(cullvals3[,1])
      cully3<-as.numeric(cullvals3[,2])
      
      cullvals4 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
      cullx4<-as.numeric(cullvals4[,1])
      cully4<-as.numeric(cullvals4[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      par(mfrow=c(2,3))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
      dev.off()
    }
    
    if (length(all_xc)==5){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      cullvals3 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
      cullx3<-as.numeric(cullvals3[,1])
      cully3<-as.numeric(cullvals3[,2])
      
      cullvals4 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
      cullx4<-as.numeric(cullvals4[,1])
      cully4<-as.numeric(cullvals4[,2])
      
      cullvals5 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
      cullx5<-as.numeric(cullvals5[,1])
      cully5<-as.numeric(cullvals5[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      par(mfrow=c(2,3))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
      dev.off()
    }
    
    if (length(all_xc)==6){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      cullvals3 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
      cullx3<-as.numeric(cullvals3[,1])
      cully3<-as.numeric(cullvals3[,2])
      
      cullvals4 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
      cullx4<-as.numeric(cullvals4[,1])
      cully4<-as.numeric(cullvals4[,2])
      
      cullvals5 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
      cullx5<-as.numeric(cullvals5[,1])
      cully5<-as.numeric(cullvals5[,2])
      
      cullvals6 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
      cullx6<-as.numeric(cullvals6[,1])
      cully6<-as.numeric(cullvals6[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      par(mfrow=c(2,3))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
      dev.off()
    }
    
    if (length(all_xc)==7){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      cullvals3 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
      cullx3<-as.numeric(cullvals3[,1])
      cully3<-as.numeric(cullvals3[,2])
      
      cullvals4 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
      cullx4<-as.numeric(cullvals4[,1])
      cully4<-as.numeric(cullvals4[,2])
      
      cullvals5 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
      cullx5<-as.numeric(cullvals5[,1])
      cully5<-as.numeric(cullvals5[,2])
      
      cullvals6 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
      cullx6<-as.numeric(cullvals6[,1])
      cully6<-as.numeric(cullvals6[,2])
      
      cullvals7 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
      cullx7<-as.numeric(cullvals7[,1])
      cully7<-as.numeric(cullvals7[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=8.5,height=11,res=300)
      par(mfrow=c(3,3))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
      dev.off()
    }
    
    if (length(all_xc)==8){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      cullvals3 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
      cullx3<-as.numeric(cullvals3[,1])
      cully3<-as.numeric(cullvals3[,2])
      
      cullvals4 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
      cullx4<-as.numeric(cullvals4[,1])
      cully4<-as.numeric(cullvals4[,2])
      
      cullvals5 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
      cullx5<-as.numeric(cullvals5[,1])
      cully5<-as.numeric(cullvals5[,2])
      
      cullvals6 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
      cullx6<-as.numeric(cullvals6[,1])
      cully6<-as.numeric(cullvals6[,2])
      
      cullvals7 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
      cullx7<-as.numeric(cullvals7[,1])
      cully7<-as.numeric(cullvals7[,2])
      
      cullvals8 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[8]),'_edge',as.character(all_edge[8]),'.csv',sep=""),header=TRUE)
      cullx8<-as.numeric(cullvals8[,1])
      cully8<-as.numeric(cullvals8[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      par(mfrow=c(3,3))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx8),max(targetx,cullx8)),ylim=c(min(targety,cully8),max(targety,cully8)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx8,cully8,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[8]),", e: ",as.character(all_edge[8]),", r: ",as.character(all_xc[8]),sep=""),side=3,line=1.5,font=2)
      dev.off()
    }
    
    if (length(all_xc)>8){
      
      inFile <- input$targetFile
      targetvalues <- read.csv(inFile$datapath)
      targetx<-as.numeric(targetvalues[,1])
      targety<-as.numeric(targetvalues[,2])
      
      cullvals1 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
      cullx1<-as.numeric(cullvals1[,1])
      cully1<-as.numeric(cullvals1[,2])
      
      cullvals2 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
      cullx2<-as.numeric(cullvals2[,1])
      cully2<-as.numeric(cullvals2[,2])
      
      cullvals3 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
      cullx3<-as.numeric(cullvals3[,1])
      cully3<-as.numeric(cullvals3[,2])
      
      cullvals4 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
      cullx4<-as.numeric(cullvals4[,1])
      cully4<-as.numeric(cullvals4[,2])
      
      cullvals5 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
      cullx5<-as.numeric(cullvals5[,1])
      cully5<-as.numeric(cullvals5[,2])
      
      cullvals6 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
      cullx6<-as.numeric(cullvals6[,1])
      cully6<-as.numeric(cullvals6[,2])
      
      cullvals7 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
      cullx7<-as.numeric(cullvals7[,1])
      cully7<-as.numeric(cullvals7[,2])
      
      cullvals8 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[8]),'_edge',as.character(all_edge[8]),'.csv',sep=""),header=TRUE)
      cullx8<-as.numeric(cullvals8[,1])
      cully8<-as.numeric(cullvals8[,2])
      
      cullvals9 <- read.csv(paste(candynames[1],'-',targyname,'/Output_Data/',candynames[1],targyname,'_g',as.character(all_g[9]),'_edge',as.character(all_edge[9]),'.csv',sep=""),header=TRUE)
      cullx9<-as.numeric(cullvals9[,1])
      cully9<-as.numeric(cullvals9[,2])
      
      all_xc<-round(all_xc,digits=2)
      
      tiff(file=paste(candynames[1],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
      par(mfrow=c(3,3))
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx8),max(targetx,cullx8)),ylim=c(min(targety,cully8),max(targety,cully8)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx8,cully8,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[8]),", e: ",as.character(all_edge[8]),", r: ",as.character(all_xc[8]),sep=""),side=3,line=1.5,font=2)
      
      plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx9),max(targetx,cullx9)),ylim=c(min(targety,cully9),max(targety,cully9)),xlab="",ylab="",col='grey',pch=16)
      mtext("Height", side=2, line=2, font=2)
      mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
      points(cullx9,cully9,xlab="",ylab="",col='blue',pch=16)
      mtext(paste("g: ",as.character(all_g[9]),", e: ",as.character(all_edge[9]),", r: ",as.character(all_xc[9]),sep=""),side=3,line=1.5,font=2)
      dev.off()
    }
    }
    
    if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==2 && plotflag==1){
      
      cullalign <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""),header=FALSE)
      cullxc <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
      all_xc <- as.numeric()
      all_g <- as.numeric()
      all_edge <- as.numeric()
      
      for (n in 1:length(cullalign[,1])){
        cullg<-cullalign[n,1]
        culledge<-cullalign[n,2]
        all_g<-c(all_g,cullg)
        all_edge<-c(all_edge,culledge)
        if (cullg==1.01){
          cg<-4
        }
        if (cullg==1.00){
          cg<-3
        }
        if (cullg==0.99){
          cg<-2
        }
        if (cullg==0.98){
          cg<-1
        }
        ce<-culledge*100
        xcval<-cullxc[cg,ce]
        all_xc<-c(all_xc,xcval)
      }
      
      sortthem<-sort(all_xc,decreasing=TRUE,index.return=TRUE)
      all_xc<-all_xc[sortthem$ix]
      all_g<-all_g[sortthem$ix]
      all_edge<-all_edge[sortthem$ix]
      
      if (length(all_xc)==1){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        dev.off()
        
      }
      
      if (length(all_xc)==2){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(1,2))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        dev.off()
        
      }
      
      if (length(all_xc)==3){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(1,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==4){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(2,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==5){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(2,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==6){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(2,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==7){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        cullvals7 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
        cullx7<-as.numeric(cullvals7[,1])
        cully7<-as.numeric(cullvals7[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=8.5,height=11,res=300)
        par(mfrow=c(3,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==8){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        cullvals7 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
        cullx7<-as.numeric(cullvals7[,1])
        cully7<-as.numeric(cullvals7[,2])
        
        cullvals8 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[8]),'_edge',as.character(all_edge[8]),'.csv',sep=""),header=TRUE)
        cullx8<-as.numeric(cullvals8[,1])
        cully8<-as.numeric(cullvals8[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(3,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx8),max(targetx,cullx8)),ylim=c(min(targety,cully8),max(targety,cully8)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx8,cully8,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[8]),", e: ",as.character(all_edge[8]),", r: ",as.character(all_xc[8]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)>8){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        cullvals7 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
        cullx7<-as.numeric(cullvals7[,1])
        cully7<-as.numeric(cullvals7[,2])
        
        cullvals8 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[8]),'_edge',as.character(all_edge[8]),'.csv',sep=""),header=TRUE)
        cullx8<-as.numeric(cullvals8[,1])
        cully8<-as.numeric(cullvals8[,2])
        
        cullvals9 <- read.csv(paste(candynames[2],'-',targyname,'/Output_Data/',candynames[2],targyname,'_g',as.character(all_g[9]),'_edge',as.character(all_edge[9]),'.csv',sep=""),header=TRUE)
        cullx9<-as.numeric(cullvals9[,1])
        cully9<-as.numeric(cullvals9[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[2],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(3,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx8),max(targetx,cullx8)),ylim=c(min(targety,cully8),max(targety,cully8)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx8,cully8,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[8]),", e: ",as.character(all_edge[8]),", r: ",as.character(all_xc[8]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx9),max(targetx,cullx9)),ylim=c(min(targety,cully9),max(targety,cully9)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx9,cully9,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[9]),", e: ",as.character(all_edge[9]),", r: ",as.character(all_xc[9]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
    }
    
    if (which(unlist(strsplit(input$candidate_names,"[,]"))==input$whichalignment)==3 && plotflag==1){
      
      cullalign <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/culled_alignments.csv',sep=""),header=FALSE)
      cullxc <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/xcvals.csv',sep=""),header=TRUE)
      all_xc <- as.numeric()
      all_g <- as.numeric()
      all_edge <- as.numeric()
      
      for (n in 1:length(cullalign[,1])){
        cullg<-cullalign[n,1]
        culledge<-cullalign[n,2]
        all_g<-c(all_g,cullg)
        all_edge<-c(all_edge,culledge)
        if (cullg==1.01){
          cg<-4
        }
        if (cullg==1.00){
          cg<-3
        }
        if (cullg==0.99){
          cg<-2
        }
        if (cullg==0.98){
          cg<-1
        }
        ce<-culledge*100
        xcval<-cullxc[cg,ce]
        all_xc<-c(all_xc,xcval)
      }
      
      sortthem<-sort(all_xc,decreasing=TRUE,index.return=TRUE)
      all_xc<-all_xc[sortthem$ix]
      all_g<-all_g[sortthem$ix]
      all_edge<-all_edge[sortthem$ix]
      
      if (length(all_xc)==1){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        dev.off()
        
      }
      
      if (length(all_xc)==2){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(1,2))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        dev.off()
        
      }
      
      if (length(all_xc)==3){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(1,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==4){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(2,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==5){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(2,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==6){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(2,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==7){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        cullvals7 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
        cullx7<-as.numeric(cullvals7[,1])
        cully7<-as.numeric(cullvals7[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=8.5,height=11,res=300)
        par(mfrow=c(3,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)==8){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        cullvals7 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
        cullx7<-as.numeric(cullvals7[,1])
        cully7<-as.numeric(cullvals7[,2])
        
        cullvals8 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[8]),'_edge',as.character(all_edge[8]),'.csv',sep=""),header=TRUE)
        cullx8<-as.numeric(cullvals8[,1])
        cully8<-as.numeric(cullvals8[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(3,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx8),max(targetx,cullx8)),ylim=c(min(targety,cully8),max(targety,cully8)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx8,cully8,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[8]),", e: ",as.character(all_edge[8]),", r: ",as.character(all_xc[8]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
      
      if (length(all_xc)>8){
        
        inFile <- input$targetFile
        targetvalues <- read.csv(inFile$datapath)
        targetx<-as.numeric(targetvalues[,1])
        targety<-as.numeric(targetvalues[,2])
        
        cullvals1 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[1]),'_edge',as.character(all_edge[1]),'.csv',sep=""),header=TRUE)
        cullx1<-as.numeric(cullvals1[,1])
        cully1<-as.numeric(cullvals1[,2])
        
        cullvals2 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[2]),'_edge',as.character(all_edge[2]),'.csv',sep=""),header=TRUE)
        cullx2<-as.numeric(cullvals2[,1])
        cully2<-as.numeric(cullvals2[,2])
        
        cullvals3 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[3]),'_edge',as.character(all_edge[3]),'.csv',sep=""),header=TRUE)
        cullx3<-as.numeric(cullvals3[,1])
        cully3<-as.numeric(cullvals3[,2])
        
        cullvals4 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[4]),'_edge',as.character(all_edge[4]),'.csv',sep=""),header=TRUE)
        cullx4<-as.numeric(cullvals4[,1])
        cully4<-as.numeric(cullvals4[,2])
        
        cullvals5 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[5]),'_edge',as.character(all_edge[5]),'.csv',sep=""),header=TRUE)
        cullx5<-as.numeric(cullvals5[,1])
        cully5<-as.numeric(cullvals5[,2])
        
        cullvals6 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[6]),'_edge',as.character(all_edge[6]),'.csv',sep=""),header=TRUE)
        cullx6<-as.numeric(cullvals6[,1])
        cully6<-as.numeric(cullvals6[,2])
        
        cullvals7 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[7]),'_edge',as.character(all_edge[7]),'.csv',sep=""),header=TRUE)
        cullx7<-as.numeric(cullvals7[,1])
        cully7<-as.numeric(cullvals7[,2])
        
        cullvals8 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[8]),'_edge',as.character(all_edge[8]),'.csv',sep=""),header=TRUE)
        cullx8<-as.numeric(cullvals8[,1])
        cully8<-as.numeric(cullvals8[,2])
        
        cullvals9 <- read.csv(paste(candynames[3],'-',targyname,'/Output_Data/',candynames[3],targyname,'_g',as.character(all_g[9]),'_edge',as.character(all_edge[9]),'.csv',sep=""),header=TRUE)
        cullx9<-as.numeric(cullvals9[,1])
        cully9<-as.numeric(cullvals9[,2])
        
        all_xc<-round(all_xc,digits=2)
        
        tiff(file=paste(candynames[3],'-',targyname,'/Output_Images/',input$critnick,'/TopNarrowed.tiff',sep=""),units="in",width=6,height=8,res=300)
        par(mfrow=c(3,3))
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx1),max(targetx,cullx1)),ylim=c(min(targety,cully1),max(targety,cully1)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx1,cully1,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[1]),", e: ",as.character(all_edge[1]),", r: ",as.character(all_xc[1]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx2),max(targetx,cullx2)),ylim=c(min(targety,cully2),max(targety,cully2)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx2,cully2,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[2]),", e: ",as.character(all_edge[2]),", r: ",as.character(all_xc[2]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx3),max(targetx,cullx3)),ylim=c(min(targety,cully3),max(targety,cully3)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx3,cully3,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[3]),", e: ",as.character(all_edge[3]),", r: ",as.character(all_xc[3]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx4),max(targetx,cullx4)),ylim=c(min(targety,cully4),max(targety,cully4)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx4,cully4,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[4]),", e: ",as.character(all_edge[4]),", r: ",as.character(all_xc[4]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx5),max(targetx,cullx5)),ylim=c(min(targety,cully5),max(targety,cully5)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx5,cully5,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[5]),", e: ",as.character(all_edge[5]),", r: ",as.character(all_xc[5]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx6),max(targetx,cullx6)),ylim=c(min(targety,cully6),max(targety,cully6)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx6,cully6,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[6]),", e: ",as.character(all_edge[6]),", r: ",as.character(all_xc[6]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx7),max(targetx,cullx7)),ylim=c(min(targety,cully7),max(targety,cully7)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx7,cully7,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[7]),", e: ",as.character(all_edge[7]),", r: ",as.character(all_xc[7]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx8),max(targetx,cullx8)),ylim=c(min(targety,cully8),max(targety,cully8)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx8,cully8,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[8]),", e: ",as.character(all_edge[8]),", r: ",as.character(all_xc[8]),sep=""),side=3,line=1.5,font=2)
        
        plot(targetx,targety,type="p",frame=FALSE,xlim=c(min(targetx,cullx9),max(targetx,cullx9)),ylim=c(min(targety,cully9),max(targety,cully9)),xlab="",ylab="",col='grey',pch=16)
        mtext("Height", side=2, line=2, font=2)
        mtext(expression(paste(delta^13, "C (\u2030)",sep="")),side=1,line=3,font=2)
        points(cullx9,cully9,xlab="",ylab="",col='blue',pch=16)
        mtext(paste("g: ",as.character(all_g[9]),", e: ",as.character(all_edge[9]),", r: ",as.character(all_xc[9]),sep=""),side=3,line=1.5,font=2)
        dev.off()
      }
    }
    })
}
shinyApp(ui = ui, server = server)
