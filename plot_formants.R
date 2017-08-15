# Huh project - plot formants
# First version of script by FT
# Additions by MD
# --------------------------------------

# On FT's system:
d = read.delim('/Users/frator/Documents/Repairs/formantData.txt')

# On MD's system
setwd(basedir)
d = read.delim('formantData.txt')


quartz(w=5.5,h=5.5)

plot(d$f2, d$f1, xlab="F2 (Hz)", ylab="F1 (Hz)", xlim=c(2400,700), ylim=c(1000,200), pch=as.character(d$language))

# -----------------------------

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
        
