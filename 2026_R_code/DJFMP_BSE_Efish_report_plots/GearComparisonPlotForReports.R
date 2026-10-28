#-------------------------------------------------------------------------------
# Author: Bryan Matthias
# Email: bryan_matthias@fws.gov 
# Date: 10/23/2026
# Version: 1
# Purpose: This R script is designed to plot a simple comparison of the paired
# Beach Seine Efficiency (BSE) and Electrofishing (Efishing) catches for 
# semi-annual and annual reports. This code currently pulls unpublished data
# that is queried from the BSE and Efishing databases. All of these data
# are in the folder that houses this R code.
#
# Electrofishing data are available through EDI data posting
# Beach Seine Efficiency data are available upon request, we are working on 
# getting these data published to EDI. 
#
# The site-match file was manually created in Excel using the BSE and Efish 
# datasets. This needs to be completed for the code to work. 
#-----------------------------------------------------------------------------


rm(list=ls(all=T))

list.files()  # list of files in folder

# grab data from each survey and a site matching file
bse_dat<-read.csv("BSE_Crossover_all_data_to2026.csv",header=TRUE)
efish_dat<-read.csv("Efish_Crossover_all data_to2026.csv",header=TRUE)
site_match<-read.csv("Site_matches.csv",header=TRUE)
fish_tax<-read.csv("DJFMP_Fish_Taxonomy.csv",header=TRUE)

efish_dat$OrganismCode[which(efish_dat$OrganismCode=='LAMAM')]='LAM'

# take subsets of data files
take<-c('StationID','StationCode','Date','SampleNum','OrganismCode','Forklength','Count')
bdat<-bse_dat[,match(take,names(bse_dat))]
names(bdat)<-c('StationID','StationCode','Date','SampleNum','OrganismCode','ForkLength','Count')

take<-c('StationNumber','SampleDate','SegmentNumber','OrganismCode','ForkLength','Count')
edat<-efish_dat[,match(take,names(efish_dat))]

# add efishing station ID to BSE data
bdat$EfishStation<-NA
for(i in 1:dim(site_match)[1]){
  take<-which(bdat$StationCode==site_match$StationCode[i]&bdat$StationID==site_match$StationID[i])
  bdat$EfishStation[take]<-site_match$EfishStation[i]
}

# list of species caught in the surveys
tab<-table(c(bdat$OrganismCode,edat$OrganismCode))
tab

# compile data for summing catch across surveys
site_match$Date<-as.Date(site_match$Date,format='%m/%d/%Y')

# select all dates from sample year 2026
take_dates<-which(site_match$Date>=as.Date('2025/08/01')&
    site_match$Date<as.Date('2026/08/01'))

samps<-sort(site_match$EfishStation)
n_samp<-length(samps)


#-----------------------------------------------------------------------------
# 
# plot only species with at least 20 fish total caught - USED IN TECH MEMO
#
#-----------------------------------------------------------------------------
rmv<-c(which(names(tab)=='NOFISH'),which(tab<20))
spp<-sort(names(tab[-rmv]),decreasing=TRUE)
n_spp<-length(spp)
n_gear<-2
c_dat<-array(NA,c(n_gear,n_samp,n_spp))

for(i in 1:n_samp){
  for(j in 1:n_spp){
    # start with BSE, grab catch from a given site and species
    take<-which(bdat$EfishStation==samps[i]&bdat$OrganismCode==spp[j])
    c_dat[1,i,j]<-sum(bdat$Count[take])

    # Efish, grab catch from a given site and species
    take<-which(edat$StationNumber==samps[i]&edat$OrganismCode==spp[j])
    c_dat[2,i,j]<-sum(edat$Count[take])
  }
}


# create plot 
windows(width=7,height=10)

# 2 panels, main plot and legend on bottom
layout(matrix(1:2,nrow=2),height=c(1,0.2))

# Generate list of common names for the plot
common_names<-fish_tax$CommonName[match(spp,fish_tax$OrganismCode)]

n_plot<-length(take_dates)

# adjust margins and set up plotting area
par(mar=c(4,12,0.25,0.25))
ys<-seq(1,n_spp,by=1)
plot(NA,NA,ylim=range(ys),xlim=c(0.6,n_plot+0.4),las=1,cex.axis=1.5,cex.lab=1.75,
  ylab='',xlab='Paired Sample',yaxt='n')
axis(2,at=ys,labels=common_names,las=1)
mtext('Common Name of Catch',2,cex=1.75,padj=-9.5)
# add points representing catch or no catch
for(i in 1:n_plot){
  for(j in 1:n_spp){
    # plot BSE
    if(c_dat[1,i,j]==0){
      points(i-0.1,j,pch=3,cex=0.5)   # no catch
    }else{
      points(i-0.1,j,pch=21,cex=log(c_dat[1,i,j])+1,
        bg=adjustcolor("steelblue", alpha.f = 0.4))
    }
    # plot Efish
    if(c_dat[2,i,j]==0){
      points(i+0.1,j,pch=3,cex=0.5)   # no catch
    }else{
      points(i+0.1,j,pch=21,cex=log(c_dat[2,i,j])+1,
        bg=adjustcolor("goldenrod", alpha.f = 0.4))
    }
    
  }
}
# add legend
par(mar=rep(0,4))
plot.new()
legend('right',c('    0 Fish Caught    ','    1 Fish Caught','100 Fish Caught',
  '500 Fish Caught  ','','Beach Seine Efficiency','Electrofishing'),bty='n',
  cex=1.25,x.intersp=2,y.intersp=c(1,1,1.5,2.2),ncol=2,pch=c(3,21,21,21,NA,21,21),
  pt.cex=c(0.5,1,log(100)+1,log(500)+1,NA,log(50),log(50)),
  pt.bg=c(NA,rep('grey',3),NA,adjustcolor("steelblue",alpha.f=0.4),
    adjustcolor("goldenrod",alpha.f=0.4)))








#-----------------------------------------------------------------------------
# 
# plot low catch, those with less than 20 fish caught over study - not in memo
#
#-----------------------------------------------------------------------------

rmv<-c(which(names(tab)=='NOFISH'),which(tab>=20))
spp<-sort(names(tab[-rmv]),decreasing=TRUE)
n_spp<-length(spp)
n_gear<-2
c_dat<-array(NA,c(n_gear,n_samp,n_spp))

for(i in 1:n_samp){
  for(j in 1:n_spp){
    # start with BSE, grab catch from a given site and species
    take<-which(bdat$EfishStation==samps[i]&bdat$OrganismCode==spp[j])
    c_dat[1,i,j]<-sum(bdat$Count[take])
    
    # Efish, grab catch from a given site and species
    take<-which(edat$StationNumber==samps[i]&edat$OrganismCode==spp[j])
    c_dat[2,i,j]<-sum(edat$Count[take])
  }
}


# create plot 
windows(width=7,height=10)

# 2 panels, main plot and legend on bottom
layout(matrix(1:2,nrow=2),height=c(1,0.2))

# Generate list of common names for the plot
common_names<-fish_tax$CommonName[match(spp,fish_tax$OrganismCode)]

n_plot<-length(take_dates)

# adjust margins and set up plotting area
par(mar=c(4,12,0.25,0.25))
ys<-seq(1,n_spp,by=1)
plot(NA,NA,ylim=range(ys),xlim=c(0.6,n_plot+0.4),las=1,cex.axis=1.5,cex.lab=1.75,
  ylab='',xlab='Paired Sample',yaxt='n')
axis(2,at=ys,labels=common_names,las=1)
mtext('Common Name of Catch',2,cex=1.75,padj=-9.5)
# add points representing catch or no catch
for(i in 1:n_plot){
  for(j in 1:n_spp){
    # plot BSE
    if(c_dat[1,i,j]==0){
      points(i-0.1,j,pch=3,cex=0.5)   # no catch
    }else{
      points(i-0.1,j,pch=21,cex=log(c_dat[1,i,j])+1,
        bg=adjustcolor("steelblue", alpha.f = 0.4))
    }
    # plot Efish
    if(c_dat[2,i,j]==0){
      points(i+0.1,j,pch=3,cex=0.5)   # no catch
    }else{
      points(i+0.1,j,pch=21,cex=log(c_dat[2,i,j])+1,
        bg=adjustcolor("goldenrod", alpha.f = 0.4))
    }
    
  }
}
# add legend
par(mar=rep(0,4))
plot.new()
legend('right',c('    0 Fish Caught    ','    1 Fish Caught','100 Fish Caught',
  '500 Fish Caught  ','','Beach Seine Efficiency','Electrofishing'),bty='n',
  cex=1.25,x.intersp=2,y.intersp=c(1,1,1.5,2.2),ncol=2,pch=c(3,21,21,21,NA,21,21),
  pt.cex=c(0.5,1,log(100)+1,log(500)+1,NA,log(50),log(50)),
  pt.bg=c(NA,rep('grey',3),NA,adjustcolor("steelblue",alpha.f=0.4),
    adjustcolor("goldenrod",alpha.f=0.4)))










#-----------------------------------------------------------------------------
# 
# plot all catch - not in tech memo
# common names on vertical axis too small
#
#-----------------------------------------------------------------------------

rmv<-c(which(names(tab)=='NOFISH'))
spp<-sort(names(tab[-rmv]),decreasing=TRUE)
n_spp<-length(spp)
n_gear<-2
c_dat<-array(NA,c(n_gear,n_samp,n_spp))

for(i in 1:n_samp){
  for(j in 1:n_spp){
    # start with BSE, grab catch from a given site and species
    take<-which(bdat$EfishStation==samps[i]&bdat$OrganismCode==spp[j])
    c_dat[1,i,j]<-sum(bdat$Count[take])
    
    # Efish, grab catch from a given site and species
    take<-which(edat$StationNumber==samps[i]&edat$OrganismCode==spp[j])
    c_dat[2,i,j]<-sum(edat$Count[take])
  }
}


# create plot 
windows(width=7,height=10)

# 2 panels, main plot and legend on bottom
layout(matrix(1:2,nrow=2),height=c(1,0.2))

# Generate list of common names for the plot
common_names<-fish_tax$CommonName[match(spp,fish_tax$OrganismCode)]

n_plot<-length(take_dates)

# adjust margins and set up plotting area
par(mar=c(4,12,0.25,0.25))
ys<-seq(1,n_spp,by=1)
plot(NA,NA,ylim=range(ys),xlim=c(0.6,n_plot+0.4),las=1,cex.axis=1.5,cex.lab=1.75,
  ylab='',xlab='Paired Sample',yaxt='n')
axis(2,at=ys,labels=common_names,las=1,cex.axis=0.75)
mtext('Common Name of Catch',2,cex=1.75,padj=-9.5)
# add points representing catch or no catch
for(i in 1:n_plot){
  for(j in 1:n_spp){
    # plot BSE
    if(c_dat[1,i,j]==0){
      points(i-0.1,j,pch=3,cex=0.5)   # no catch
    }else{
      points(i-0.1,j,pch=21,cex=log(c_dat[1,i,j])+1,
        bg=adjustcolor("steelblue", alpha.f = 0.4))
    }
    # plot Efish
    if(c_dat[2,i,j]==0){
      points(i+0.1,j,pch=3,cex=0.5)   # no catch
    }else{
      points(i+0.1,j,pch=21,cex=log(c_dat[2,i,j])+1,
        bg=adjustcolor("goldenrod", alpha.f = 0.4))
    }
    
  }
}
# add legend
par(mar=rep(0,4))
plot.new()
legend('right',c('    0 Fish Caught    ','    1 Fish Caught','100 Fish Caught',
  '500 Fish Caught  ','','Beach Seine Efficiency','Electrofishing'),bty='n',
  cex=1.25,x.intersp=2,y.intersp=c(1,1,1.5,2.2),ncol=2,pch=c(3,21,21,21,NA,21,21),
  pt.cex=c(0.5,1,log(100)+1,log(500)+1,NA,log(50),log(50)),
  pt.bg=c(NA,rep('grey',3),NA,adjustcolor("steelblue",alpha.f=0.4),
    adjustcolor("goldenrod",alpha.f=0.4)))

