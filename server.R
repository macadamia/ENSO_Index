
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ncdf4)
library(rdrop2)

shinyServer(function(input, output) {

  token <- readRDS('droptoken.rds')
  drop_acc(dtoken=token)

  csvThere <- drop_get('ENSO/ensoclasses1961_1990.csv',overwrite=T,local_file = 'ensoclasses1961_1990.csv')
  if(csvThere){
    cat('found enso file\n')
    ensoHistory <- read.csv('ensoclasses1961_1990.csv',header=T,stringsAsFactors = F)
  } else {
    cat('Did not find enso file')
  }

  b1 <- 1961 # base period start
  b2 <- 1990 # end

  soi.ln <- 5.5
  soi.en <- -5.5
  soi.ave.period <- 3 #months for running mean
  sst.ln <- -0.5
  sst.en <- 0.5
  sst.ave.period <- 5

  output$downloadENSO <- downloadHandler(
    filename = function() {
      'ensoclasses1961_1990.csv'
    },
    content = function(file) {
      write.csv(ensoHistory, file)
    }
  )

  #Potgieter_et_al_ENSOFootprints2005.pdf
  output$downloadPDF <- downloadHandler(
    filename = function() {
      'Potgieter_et_al_ENSOFootprints2005.pdf'
    },
    content = function(file) {
      file.copy('Potgieter_et_al_ENSOFootprints2005.pdf', file)
    }
  )

  RunningAve<-function(x,n){
    #Moving Ave
    #x the data
    # n number of datpoints to average
    lx<-length(x)
    ma<-vector()
    for (y in n:lx){
      st<-y-(n-1)
      ma<-c(ma,mean(x[st:y],na.rm=T))
    }
    return(ma)
  }

  createtab<-function(yrs,mths,index){
    indexave<-data.frame(yrs,mths,index)
    colnames(indexave)<-c("year","month","index")

    start.pad<-mths[1]-1
    end.pad<-12-mths[length(mths)]
    tab<-matrix(c(rep(NA,start.pad),indexave$index,rep(NA,end.pad)),byrow=T,ncol=12)
    rownames(tab)<-unique(yrs)
    colnames(tab)<-month.abb
    return(tab)
  }

  zfill <-  function(x, n) {
    # fill with leading zeros to get an n-digit character string
    nc <- nchar(x)
    zeros <- paste(rep(0, n), collapse = "")
    paste(substring(zeros, nchar(x) + 1, n), substring(x, 1, nchar(x)), sep = "")
  }



  output$soiPlot <- renderPlot({

    thisYear <- as.numeric(format(Sys.Date(),'%Y'))

    theurl<-'https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/MonthlySOIPhase1887-1989Base.txt'
    download.file(theurl,'soi.txt',method='curl')
    conn<-file('soi.txt','r')
    soi<-read.table(conn,header=F,skip=1,col.names=c("year","month","index","phase"))


    #SOI
    index<-RunningAve(soi$index,soi.ave.period)
    yrs<-soi$year[soi.ave.period:length(soi$year)]
    soi.yrs<-unique(yrs)
    mths<-soi$month[soi.ave.period:length(soi$year)]
    maxyearssoi<-max(yrs)
    tab<-createtab(yrs,mths,index)

    soi.tab<-data.frame(yrs,mths,index)
    colnames(soi.tab)<-c('year','month','index')

    soi.class<-rep(' ',length(unique(yrs)))

    #la Nina
    count<-matrix(0,nrow=dim(tab)[1],ncol=9)
    count[tab[,4:12]>= soi.ln]<-1
    soi.ln.count<-count
    row.sums<-apply(count,1,sum)
    soi.class[row.sums>=6]<-'Ln'

    #el Nino
    count<-matrix(0,nrow=dim(tab)[1],ncol=9)
    count[tab[,4:12]<= soi.en]<-1
    soi.en.count<-count
    row.sums<-apply(count,1,sum)
    soi.class[row.sums>=6]<-'En'

    soi.classes<-data.frame(unique(yrs),soi.class)
    colnames(soi.classes)<-c('year','soiclass')

    r<-soi.tab$year == thisYear
    anom<-soi.tab$index[r]
    ylim1<-min(-30,min(anom))
    ylim2<-max(30,max(anom))
    month<- soi.tab$month[r]
    ptype<-rep(20,length(month))
    ptype[month>=4]<-19

    colrs<-rep('black',length(month))
    colrs[anom >= soi.ln]<-'blue'
    colrs[anom <= soi.en]<-'red'

    plot(month,anom,xlab='Month',ylab=paste('SOI anom ',soi.ave.period,' mth ave',sep=''),main=as.character(thisYear),ylim=c(ylim1,ylim2),axes=F,pch=ptype,col=colrs)
    axis(1,month,month.abb[month],tcl=-0.7,cex.axis=0.7)
    axis(2,las=1)
    box()
    lines(rep(0,length(month)))
    y<-rep(soi.ln,length(4:max(month)))
    x<-seq(4,max(month))
    lines(x,y,col='blue')
    y<-rep(soi.en,length(4:max(month)))
    lines(x,y,col='red')

  })

  output$sstPlot <- renderPlot({

    mthnorms <- readRDS('ERSSTV3b/SSTBase.rds')
    thisYear <- as.numeric(format(Sys.Date(),'%Y'))

    yearSeq <- c(seq(b1,b2),thisYear)
    thisMth <- as.numeric(format(Sys.Date(),'%m')) - 1
    n34 <- rep(NA,12)
    for (mth in seq(1,thisMth)){
      ersstName <- paste('ersst.',thisYear,zfill(mth,2),'.nc',sep='')
      nfile <- paste('ERSSTV3b/',ersstName,sep='')
      if(!file.exists(nfile)){
        print('try and download the file')
        theurl <- paste('https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v3b/netcdf',ersstName,sep='/')
        download.file(theurl,nfile,method='curl')
        x<-file(nfile,'rb')
        if(mth == thisMth){ # might not be available yet
          if(length(grep('DOCTYPE',readLines(file(nfile,'r'),n=1)) > 0 )){
            file.remove(nfile)
            break
          }
        }
      }
      d <- nc_open(nfile)
      #get the SST

      sst <- ncvar_get(d,'sst') # ncdf is cleverer and autmatically does the scale factor
      longs <- ncvar_get(d,'lon')
      lats <- ncvar_get(d,'lat')

      lngE <- c(seq(0,180,2),seq(-178,-2,2))

      colnames(sst) <- lats
      rownames(sst) <- lngE
      n34lats <- which(lats>=-6 & lats <=6)
      n34longs <- which(longs>=190 & longs <=240)
      nino34X <- sst[n34longs,n34lats]
      #interpolate for between 4 and 6 degrees to get the 5 degree value
      nino34 <-nino34X
      ninoKeep<-nino34
      nino34[,1]<-rowMeans(nino34X[,1:2])
      nino34[,7]<-rowMeans(nino34X[,6:7])
      n34[mth] <- mean(nino34)
      nc_close(d)
    } #mth

    anom34 <- rep(NA,12)
    #calculate anomalies
    anom34 <- n34 - mthnorms
    month <- 1:thisMth
    ylim1<-min(-2,min(anom34,na.rm=T))
    ylim2<-max(2,max(anom34,na.rm=T))
    ptype<-rep(20,length(month))
    ptype[month>=4]<-19
    colrs<-rep('black',length(month))
    colrs[anom34<= sst.ln]<-'blue'
    colrs[anom34>= sst.en]<-'red'
    plot(month,anom34[month],xlab='Month',ylab=paste('SST anom ',sst.ave.period,' mth ave',sep=''),ylim=c(ylim1,ylim2),axes=F,pch=ptype,col=colrs)
    axis(1,month,month.abb[month],tcl=-0.7,cex.axis=0.7)
    axis(2,las=1)
    box()
    lines(rep(0,length(month)))
    y<-rep(sst.ln,length(4:max(month)))
    x<-seq(4,max(month))
    lines(x,y,col='blue')
    y<-rep(sst.en,length(4:max(month)))
    lines(x,y,col='red')

  })

})
