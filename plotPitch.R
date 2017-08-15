# Huh project - plot pitch
# First version of script by FT
# Additions, changes, and comments by MD
# --------------------------------------

# On FT's system
indir = '~/Dropbox/Huh project/R/in'

# On MD's system
setwd(paste(indir,"/pitch_data", sep=""))

fileList <- list.files(pattern=".pitch")
numberOfFiles <- length(fileList)

ch = c()
sp = c()

quartz(w=8,h=4)
par(mfrow=c(1,2))
plot(0,0, ylim=c(-10,10), xlim=c(-0.1,1.1), col='white', xlab="Norm. Time", ylab="Centered pitch (semitones)", main="Spanish") 
for(file in fileList){
	d = read.delim(file)
	if(length(grep("Sp",file))>0){
		d$norm_st = d$st - mean(d$st)
		exc = d$norm_st[length(d$norm_st)] - d$norm_st[1]
		sp = c(sp, exc)
		lines(d$time, d$norm_st, lwd=1.1)
		}
	}

plot(0,0, ylim=c(-10,10), xlim=c(-0.1,1.1), col='white', xlab="Norm. Time", ylab="", main="Cha'palaa") 
for(file in fileList){
	d = read.delim(file)
	if(length(grep("Ch",file))>0){
		d$norm_st = d$st - mean(d$st)
		exc = d$norm_st[length(d$norm_st)] - d$norm_st[1]
		ch = c(ch, exc)
		lines(d$time, d$norm_st, lwd=1.1)
		}
	}

l = c(rep('sp',12), rep('ch',12))
d2 = data.frame(cbind(c(sp,ch),l))
d2$V1 = as.numeric(as.character(d2$V1))

# -----------------------------------------
# MD: Let's get all of the data into R.
# Easier to manipulate.

# Get all of the files into a list d
files <- list.files(pattern=".pitch")
d <- lapply(files, read.delim, header = TRUE, comment = "A")
names(d) <- gsub(".pitch","",files)

# Melt to make manageable
d.m <- melt(d, id=c('time','st'))
d.m$L1 <- as.factor(d.m$L1)

# Add language column
d.m$language <- NA
d.m[grep("Sp",d.m$L1),]$language <- "Spanish"
d.m[grep("Ch",d.m$L1),]$language <- "Chapalaa"
d.m$language <- as.factor(d.m$language)

# Compute means and normalised semitones
d.m <- ddply(d.m, "L1", transform, mean = mean(st))
d.m$norm_st <- d.m$st - d.m$mean

# ggplot, faceted by language
p <- ggplot(d.m, aes(time,norm_st,fill=L1)) + 
  geom_line() + 
  facet_grid(. ~ language)
p

