# Huh project analysis and plots
# Mark Dingemanse 2012-13
# ------------------------------

setwd(basedir)
d = read.csv('data.txt', sep=" ")

setwd(outdir)

# Select languages for which we have =<10 data points
numcases <- count(d,'language')
plus10 <- as.vector(numcases[which(numcases$freq > 9),]$language)
plus15 <- as.vector(numcases[which(numcases$freq > 14),]$language)
d.s <- drop.levels(d[d$language %in% plus10,])
cases <- count(d.s,'language')

# Compute standard deviation of cases/language
sd(cases[,2])

# Intonation
# ----------

# Create table of intonation by language
i = d.s[ , c("name", "language", "int")]

# order data by relative frequency of a level of a factor
i.o <- transform(i, language = ordered(language, levels = names( sort(prop.table(table(i$int, i$language),2)[1,]))))

# draw the points with jitter
ggplot(i, aes(language, int)) +
  geom_jitter() +
  ylab("Intonation") +
  xlab("Language")

# do a fluctuation plot
# http://socialdatablog.com/heatmap-tables-with-ggplot2-sort-of/

i.freq <- with(i, table(int,language)) # create a frequency table
i.freq.prop <- prop.table(i.freq,2)

cases <- sum(unlist(table(d.s$int)))
ggfluctuation(i.freq.prop, type = "size", floor = 0, ceiling = 2) +
  ylab("Intonation") +
  xlab("") +
  scale_y_discrete(breaks=seq(-3,3,1), labels=c("Falling","","","Level","","","Rising")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Figure_5_Intonation_n",cases,".png", sep=""), width=9,height=6)


# Nasality [nasal]
# ------------

nasal = d.s[ , c("name","language","nasal")]

nasal.f <- with(nasal, table(nasal, language))
nasal.fp <- prop.table(nasal.f,2)


cases <- sum(unlist(table(d.s$nasal)))
ggfluctuation(nasal.fp, type = "size", floor= 0, ceiling = 2) +
  ylab("Nasalisation") +
  xlab("") +
  scale_y_discrete(breaks=seq(1,3,1), labels=c("Oral","","Nasal")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Figure_S1_Nasality_n",cases,".png", sep=""), width=9,height=6)

# Closure [mouth]
# ------------

mouth = d.s[ , c("name", "language", "mouth")]


mouth.f <- with(mouth, table(mouth,language)) # create a frequency table
mouth.fp <- prop.table(mouth.f,2)

cases <- sum(unlist(table(d.s$mouth)))
ggfluctuation(mouth.fp, type = "size", floor = 0, ceiling = 2) +
  ylab("Mouth closure") +
  xlab("") +
  scale_y_discrete(breaks=seq(-4,2), labels=c("Closed","","","Intermediate", "","", "Open")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Figure_S2_Closure_n",cases,".png", sep=""), width=9,height=6)

# Onset combined [onset]
# --------------


of <- with(d.s, table(onset,language)) # create a frequency table
ofp <- prop.table(of,2)

cases <- sum(unlist(table(d.s$onset)))
ggfluctuation(ofp, type = "size", floor = 0, ceiling = 2) +
  ylab("Onset") +
  xlab("") +
  scale_y_discrete(breaks=seq(-3,3,1), labels=c("Glottal stop","","","Ø","","","Aspiration")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Figure_7_Onset_n",cases,".png", sep=""), width=9,height=6)



# Onset aspiration [asp]
# ----------------
# N.B. zero in this plot means coded as zero, not coded as "g" (which is also "not asp").

asp = d.s[ , c("name", "language", "asp")]


asp.f <- with(asp, table(asp,language)) # create a frequency table
asp.fp <- prop.table(asp.f,2)

cases <- sum(unlist(table(d.s$asp)))
ggfluctuation(asp.fp, type = "size", floor = 0, ceiling = 2) +
  ylab("Aspiration at onset") +
  xlab("") +
  scale_y_discrete(breaks=c("0", "1", "2", "3"), labels=c("Ø", "","", "h")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Onset aspiration - n",cases,".png", sep=""), width=9,height=6)

bp <- ggfluctuation(asp.fp, type = "size", floor = 0, ceiling = 2) +
  ylab("Aspiration at onset") +
  xlab("") +
  scale_y_discrete(breaks=c("0", "1", "2", "3"), labels=c("Ø", "","", "h")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# Glottis action [glot]
# --------------
# N.B. zero in this plot means coded as zero, not coded as "h" (which is also "not glot").

glot = d.s[ , c("name", "language", "glot")]


cases <- sum(unlist(table(d.s$glot)))
glot.f <- with(glot, table(glot,language)) # create a frequency table
glot.fp <- prop.table(glot.f,2)
glot.fp.df <- as.data.frame(glot.fp)

ggfluctuation(glot.fp, type = "size", floor = 0, ceiling = 3) +
  ylab("Glottal stop onset") +
  xlab("") +
  theme(axis.text.y = element_text(family="Times New Roman")) +
  scale_y_discrete(breaks=c("0", "1", "2", "3"), labels=c("Ø", "","", "ʔ")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Onset glottal - n",cases,".png", sep=""), width=9,height=6)


# Vowel quality
# -------------

# Create table of vowels by language
v = d.s[ , c("language", "name", "height", "front")]
# Get rid of entries with NA values
v <- v[!is.na(v$height), ]


# draw the front/height plots with some jitter and alpha
cases <- sum(unlist(table(v$name)))
ggplot(v, aes(height, front)) +
  geom_point(alpha=5/10,size=5,position = position_jitter(width = .2, height = .2)) +
  ylab("Height") +
  xlab("Backness") +
  coord_cartesian(ylim=c(-.5,3.5),xlim=c(-.5,3.5)) +
  scale_y_discrete(breaks=seq(0,3,1), labels=c("Low","","","Mid")) +
  scale_x_reverse(breaks=seq(0,3,1), labels=c("Central","","","Front")) +
  theme(panel.grid.minor=element_blank(),legend.position = "none") +
  facet_wrap(~language)
ggsave(file=paste("Figure_3_Vowel_quality_n",cases,".png", sep=""), width=9,height=6)


# draw every language at its mean front/height
v.langs <- ddply(v,.(language),summarise,height=mean(height),front=mean(front))
v.langs$height = 3 - v.langs$height # invert value so that axis doesn't need to be reversed

languages <- levels(v.langs$language)
# abbreviate the languages to their first letters
languages <- substr(levels(v.langs$language),1,3)

p <- ggplot(v.langs, aes(height, front, label=languages)) +
  geom_point(size=4,alpha=0) +
  ylab("Height") +
  xlab("Backness") +
  coord_cartesian(ylim=c(-.5,5.75),xlim=c(-.5,6)) +
  scale_y_continuous(breaks=seq(0,6,1.75), labels=c("Low","Low-Mid","High-Mid","High")) +
  scale_x_continuous(breaks=seq(0,5.5,2.75), labels=c("Front","Central","Back")) +
  scale_colour_manual(values=brewer.pal(10,"Paired"))
p + geom_dl(aes(label=languages),list("smart.grid",fontface="bold",cex=1))

# Break height into Low/Mid/Heigh to match individual Lg plots

p <- ggplot(v.langs, aes(height, front, label=languages)) +
  geom_point(size=4,alpha=0) +
  ylab("Height") +
  xlab("Backness") +
  coord_cartesian(ylim=c(-.5,6.5),xlim=c(-.5,6.5)) +
  scale_y_continuous(breaks=seq(0,6,3), labels=c("Low","Mid","High")) +
  scale_x_continuous(breaks=seq(0,6,3), labels=c("Front","Central","Back")) +
  scale_colour_manual(values=brewer.pal(10,"Paired"))
p + geom_dl(aes(label=languages),list("smart.grid",fontface="bold",cex=1))

ggsave(file="Fig_2_Languages_in_vowel_space.png", width=6.5,height=6)



# Let's make size dependent on frequency
# first add a frequency column -- with frequency by language
v.s$afreq = 0
v.s$rfreq = 0
# paste the vowel coordinates into a combined field for easy counting
v.s$combined <- paste(v.s$height, v.s$front, sep=",")

# get the frequencies of the 'combined' values
freqs <- tapply(v.s$combined, v.s$language, count)

talen <- as.vector(levels(v.s$language))
for (taal in talen) {
  # for every language,
  # make a vector that matches the 'combined' values with their frequency
  # freqs is a pretty crazy list structure: double [[ to drill down to language subframe, ,1 to select the right column
  m <- match(v.s[v.s$language==taal,'combined'],freqs[[taal]][,1])
  # make a vector of the token frequencies
  afreq <- freqs[[taal]][m,2]
  rfreq <- prop.table(freqs[[taal]][m,2])
  
  # for all tokens, add the matching frequency to the $freq column in v.s
  for (i in 1:nrow(v.s[v.s$language == taal,])) {
    v.s[v.s$language == taal ,]$afreq[i] = afreq[i] # afreq for absolute freq
    v.s[v.s$language == taal ,]$rfreq[i] = rfreq[i] # rfreq for relative freq
  }
  print(taal)
}

# draw the front/height plots with points sized by frequency
cases <- sum(unlist(table(v.s$name)))
ggplot(v.s, aes(height, front, size=rfreq)) +
  geom_point() +
  scale_size(range = c(2,16)) +
  ylab("Height") +
  xlab("Backness") +
  coord_cartesian(ylim=c(-.5,3.5),xlim=c(-.5,3.5)) +
  scale_y_discrete(breaks=seq(0,3,1), labels=c("Low","","","Mid")) +
  scale_x_reverse(breaks=seq(0,3,1), labels=c("Central","","","Front")) +
  theme(panel.grid.minor=element_blank(),legend.position = "none") +
  facet_wrap(~language)
ggsave(file=paste("Vowel quality relative - n",cases,".png", sep=""), width=9,height=6)


# draw the front/height plots with points colored by language
cases <- sum(unlist(table(v.s$name)))
ggplot(v.s, aes(height, front, size=rfreq, colour=language)) +
  geom_point(alpha = 10/10, position = position_jitter(width = .3, height = .3)) +
  scale_size(range = c(6,12)) +
  ylab("Height") +
  xlab("Backness") +
  coord_cartesian(ylim=c(-.5,3.5),xlim=c(-.5,3.5)) +
  scale_y_discrete(breaks=seq(0,3,1), labels=c("Low","","","Mid")) +
  scale_x_reverse(breaks=seq(0,3,1), labels=c("Central","","","Front")) +
  scale_colour_brewer(palette="Paired") +
  theme(panel.grid.minor=element_blank())
ggsave(file=paste("Vowel quality with languages coloured - n",cases,".png", sep=""), width=9,height=6)



# draw the front/height plots as a heatmap
ggplot(v.s, aes(height, front, fill=rfreq)) +
  geom_tile() +
  ylab("Low <> Mid") +
  xlab("Front <> Central") +
  coord_cartesian(ylim=c(-.5,3.5),xlim=c(-.5,3.5)) +
  scale_x_reverse() +
  scale_fill_gradient(low = "white", high = "black") +
  theme(panel.background=element_rect(fill = "white")) +
  theme(axis.text.y=element_blank(),axis.text.x=element_blank()) +
  facet_wrap(~language)
ggsave(file="Vowel quality heatmap 4x4.png", width=9,height=6)


# fluctuation plot
# ggfluctionation can't deal (yet) with matrices of >2 dimensions.
# so fluctuation plots can only be created per language

languages <- as.vector(levels(d.s$language))
for (language in languages) {
  setwd(outdir)
  print(language)
  vl <- v[which(v$language == language),]
  vlf <- with(vl, table(height,front))
  vlfp <- prop.table(vlf)
  
  vlfpd <- as.data.frame(vlfp)
  vlfpd$height <- as.numeric(vlfpd$height)
  vlfpd$front <- as.numeric(vlfpd$front)
  outer_row <- 
    
    ggfluctuation(vlfp, type = "size") +
    labs(title = language) +
    ylab("Height") +
    xlab("Front") +
    coord_cartesian(ylim=c(0,9),xlim=c(0,9)) +
    #    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  filename <- paste("vowelspace1-", language, ".png", sep="")
  #  ggsave(file=filename, width=9, height=6)
  
  ggplot(vlfpd, aes(height, front, fill=Freq)) + 
    geom_raster() +
    labs(title = language) +
    ylab("Height") +
    xlab("Front") +
    coord_cartesian(ylim=c(0,9),xlim=c(0,9)) +
    scale_y_continuous(breaks=seq(0,9,1)) +
    scale_x_continuous(breaks=seq(0,9,1)) +
    scale_fill_gradient(low = "white", high = "grey") +
    theme(axis.text = element_blank()) +
    #theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    theme(panel.background=element_rect(fill = "white"))
  filename <- paste("vowelspace2-", language, ".png", sep="")
  ggsave(file=filename, width=9, height=8)
}