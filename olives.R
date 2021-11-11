setwd("~github/oil")

#####VARIABLES#####
year <- 2021
dates <- c("10-12","10-26","11-06")
date <- as.Date(dates,format="%m-%d") #data in formato mese-giorno
olives_sp <- c(100,NA,NA) #olive di San Placido (kg)
olives_f <- c(70,115,50) #olive di Foddiri
production <- c(27.8,19.2,NA) #olio in kg
volume <- round(1/.916,digit=3) #volume olio in base al peso specifico (lt per kg)
cost <- .21 #Euro cent per chilo di olive
iva <- .04 #"cost" and "iva" do not vary over date, hence I can digit them once and R will fill all pertinent rows

#####DATASET#####
oil <- data.frame(year,date,olives_sp,olives_f,production,volume,cost,iva)
oil <- transform(oil,
                 oil_return=round(production/rowSums(oil[,3:4],na.rm=TRUE),digit=3),
                 prod_litre=round(production*volume,digit=1),
                 tot_cost=ifelse(is.na(production)==FALSE,round(rowSums(oil[,3:4],na.rm=TRUE)*cost*(1+iva)),NA)) #add oil in litres and total cost per date (only if production is not NA)

#####TABLE#####
oil_table <- data.frame(format(as.Date(oil$date,format="%y-%m-%d"),"%d/%m/%Y"),
                        paste(rowSums(oil[,3:4],na.rm=TRUE)," Kg"),
                        ifelse(is.na(oil$production)==TRUE,"",paste(oil$production," Kg")),
                        ifelse(is.na(oil$oil_return)==TRUE,"",paste(oil$oil_return*100," %")),
                        ifelse(is.na(oil$prod_litre)==TRUE,"",paste(oil$prod_litre," Lt")),
                        ifelse(is.na(oil$tot_cost)==TRUE,"",paste("EUR ",oil$tot_cost)),
                        ifelse(is.na(oil$tot_cost)==TRUE,"",paste("EUR ",round(oil$tot_cost/oil$prod_litre,digit=2))))
colnames(oil_table) <- c("Data","Raccolto","Olio prodotto (Kg)","Resa",
                         "Olio prodotto (Lt)","Costo","Costo per litro")
knitr::kable(oil_table,caption="Le olive raccolte il 6 novembre non sono state portate al frantoio, ma consegnate a Mimmo. Le ha portate lui, per la spremitura.")


#####BARPLOTS#####
par(mfrow=c(1,3),mai=c(1,.4,1,.4))

olives <- cbind(oil$olives_sp,oil$olives_f)
olives[is.na(olives)==TRUE] <- 0

production <- oil$production
production[is.na(production)==TRUE] <- 0

###1###
oil_plot <- barplot(olives~oil$date,
                    beside=TRUE,ylim=c(0,200),col=c("#00A600","#EAB64E"),
                    xaxt="n",xlab="",ylab="Kg",
                    main="Olive raccolte (Kg)")
axis(1,at=c(2,5,8),labels=oil$date)
text(x=t(oil_plot),y=olives,
     labels=cbind(paste(olives," Kg")),
     pos=3,cex=.8) #note that I transpose the plot coordinates (t(oil_plot)), for the bar labels to be correct (they would be mixed up, otherwise)
legend(1,180,c("San Placido","Foddiri"),fill=c("#00A600","#EAB64E"),cex=.85)

###2###
oil_plot2 <- barplot(c(colSums(olives),sum(colSums(olives))),
                     beside=TRUE,ylim=c(0,360),col=c("#80C904","#EAB64E","#378805"),
                     xaxt="n",xlab="",main="Olive raccolte per campagna (Kg)")
axis(1,at=oil_plot2,labels=c("San Placido","Foddiri","Totale"))
text(x=oil_plot2,y=c(colSums(olives),sum(colSums(olives))),
     labels=paste(c(colSums(olives),sum(colSums(olives)))," Kg"),pos=3,cex=.9)
#legend(.4,185,c("San Placido","Foddiri","Totale"),fill=c("#80C904","#EAB64E","#378805"),cex=.85)

###3###
product_matrix <- cbind(rowSums(olives),production) #I put olives harvested by date and oil produced (in chilos) in a matrix
product_plot <- barplot(product_matrix~oil$date,
                        beside=TRUE,ylim=c(0,200),col=c("#ABC32F","#207567"),
                        xaxt="n",xlab="",
                        main="Olio prodotto per raccolto (Kg)") #I plot that matrix on date
axis(1,at=c(2,5,8),labels=oil$date)
text(x=product_plot,y=t(product_matrix),
     labels=paste(t(product_matrix)," Kg"),pos=3)
legend(x=3.5,y=190,c("Olive","Olio"),fill=c("#ABC32F","#207567"),cex=.85)

