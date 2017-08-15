# Huh project analysis and plots
# Mark Dingemanse 2012-13
# ------------------------------

# PLoS ONE theme
theme_md_pone <- function (ticks=TRUE, base_family="GillSans", base_size=18) {
  ret <- theme_grey(base_family=base_family, base_size=base_size) +
    theme(
      axis.title.x = element_text(vjust=0),
      axis.title.y = element_text(vjust=0.2),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      legend.title = element_text(colour="grey", face = "plain")
    )
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}
theme_set(theme_md_pone(base_size=18))

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





# Vowel quality
# -------------

# Create table of vowels by language
v = d.s[ , c("language", "name", "height", "front")]
# Get rid of entries with NA values
v <- v[!is.na(v$height), ]

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
  coord_cartesian(ylim=c(-.5,6.5),xlim=c(-.5,6.5)) +
  scale_y_continuous(breaks=seq(0,6,3), labels=c("Low","Mid","High")) +
  scale_x_continuous(breaks=seq(0,6,3), labels=c("Front","Central","Back")) +
  scale_colour_manual(values=brewer.pal(10,"Paired"))
p + geom_dl(aes(label=languages),list("smart.grid",fontface="bold",cex=1))
ggsave(file="Figure_2_Languages_in_vowel_space.png", width=6.5,height=6)


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



# Format data for Spanish and Cha'palaa
setwd(basedir)
d = read.delim('formantData.txt')


d$lg <- str_sub(d$language, 1,1)

p <- ggplot(d, aes(f2, f1)) +
  geom_point(size=4,alpha=0) +
  coord_cartesian(xlim=c(2500,900),ylim=c(1000,250)) +
  scale_x_reverse() +
  scale_y_reverse() +
  ylab("F1 (Hz)") +
  xlab("F2 (Hz)")
p + geom_dl(aes(label=d$lg),list(fontface="bold",cex=1))

setwd(outdir)
ggsave(file="Figure_4_Formants.png", width=6.5,height=6)


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
i.freq <- with(i, table(int,language)) # create a frequency table
i.freq.prop <- prop.table(i.freq,2)

cases <- sum(unlist(table(d.s$int)))
ggfluctuation(i.freq.prop, type = "size", floor = 0, ceiling = 2) +
  ylab("Intonation") +
  xlab("") +
  scale_y_discrete(breaks=seq(-3,3,1), labels=c("Falling","","","Level","","","Rising")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Figure_5_Intonation_n",cases,".png", sep=""), width=9,height=6)


# Intonation: Pitch tracks for Sp and Ch
# --------------------------------------

setwd(paste(indir,"/pitch_data", sep=""))

# Get all of the files into a list d
files <- list.files(pattern=".pitch")
d <- lapply(files, read.delim, header = TRUE, comment = "A")
names(d) <- gsub(".pitch","",files)

setwd(outdir)

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

# Plot
ggplot(d.m, aes(time,norm_st,fill=L1)) + 
 geom_line(size = 0.6) + 
  xlab("Normalised time") +
  ylab("Centered pitch (semitones)") +
  coord_cartesian(ylim=c(-10,10)) +
  scale_x_continuous(breaks=c(0,.2,.4,.6,.8,1.0),labels = comma) +
  facet_grid(. ~ language)
ggsave(file="Figure_6_Pitch_tracks.png", width=6,height=3.5)
  

# Onset combined [onset]
# ----------------------


of <- with(d.s, table(onset,language)) # create a frequency table
ofp <- prop.table(of,2)

cases <- sum(unlist(table(d.s$onset)))
ggfluctuation(ofp, type = "size", floor = 0, ceiling = 2) +
  ylab("Onset") +
  xlab("") +
  scale_y_discrete(breaks=seq(-3,3,1), labels=c("Glottal stop","","","Ø","","","Aspiration")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Figure_7_Onset_n",cases,".png", sep=""), width=9,height=6)


# Nasality [nasal]
# ----------------

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

# Aperture [mouth]
# ------------

mouth = d.s[ , c("name", "language", "mouth")]


mouth.f <- with(mouth, table(mouth,language)) # create a frequency table
mouth.fp <- prop.table(mouth.f,2)

cases <- sum(unlist(table(d.s$mouth)))
ggfluctuation(mouth.fp, type = "size", floor = 0, ceiling = 2) +
  ylab("Mouth aperture") +
  xlab("") +
  scale_y_discrete(breaks=seq(-4,2), labels=c("Closed","","","Intermediate", "","", "Open")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(file=paste("Figure_S2_Aperture_n",cases,".png", sep=""), width=9,height=6)



# Levenshtein distances
# ---------------------


install.packages('MiscPsycho')
library(MiscPsycho)

stringMatch('wat','them', normalize = "yes")
stringMatch('ha','he', normalize = "yes")

