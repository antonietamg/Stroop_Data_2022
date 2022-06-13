###---------------- Men Plots --------------------

library(ggplot2)
library(dplyr)
library(dbplyr)
library(Rttf2pt1)
library(extrafont)
library(extrafontdb)

load("Men_Data/Men_Plots.RData")

#------------------- ENTER THIS DATA ------------------------------------------

x <- Men_neutral  # Men or Men_Neutral
nam <- "homs_neut.png"  # plot file name

# -----------------------------------------------------------------------------

mtodasplot <- ggplot(x, aes(x=Reaction.Time, colour=Tempo)) + #crea la gráfica: ggplot(datos adjuntos, aes(x=datos apara eje x, color= nombre del grupo o columna))
  stat_ecdf() + 
  scale_x_continuous( 
    breaks = c(0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4))+ 
  coord_cartesian(xlim=c(0.8,2.4))

data100 = x %>% filter(Tempo == "100 bpm")
data140 = x %>% filter(Tempo == "140 bpm")
data180 = x %>% filter(Tempo == "180 bpm")

data100 = data100$Reaction.Time
data140 = data140$Reaction.Time
data180 = data180$Reaction.Time


summary(data100)
summary(data140)
summary(data180)

m1 <- format(round(median(data100),2))
m14 <- format(round(median (data140),2))
m18 <- format(round(median (data180),2))
mn1 <- format(round(min (data100),2))
mn14 <- format(round(min (data140), 2))
mn18 <- format(round(min (data180),2))
mx1 <- format(round(max (data100), 2))
mx14 <- format(round(max (data140),2))
mx18 <- format(round(max (data180),2))

mtodasplotmedian <- mtodasplot + 
  annotate ("text", x = 1.8, y = 0.25, label = sprintf ("100 bpm %s [%s, %s]", m1, mn1, mx1), size = 9, colour = "brown1", hjust = 0, family="Times New Roman") +
  annotate ("text", x = 1.8, y = 0.18, label = sprintf ("140 bpm %s [%s, %s]", m14, mn14, mx14), size = 9, colour = "green4", hjust = 0, family="Times New Roman") +
  annotate ("text", x = 1.8, y = 0.11, label = sprintf ("180 bpm %s [%s, %s]", m18, mn18, mx18), size = 9, colour = "steelblue2", hjust = 0, family="Times New Roman")

wt1 <- wilcox.test(data100,data140)
wt2 <- wilcox.test(data100,data180)
wt3 <- wilcox.test(data140,data180)

wt1 <- paste0("100 vs 140" ," = ", as.character(round(wt1$p.value, 3)))
wt2 <- paste0("100 vs 140" ," = ", as.character(round(wt2$p.value, 3)))
wt3 <- paste0("100 vs 140" ," = ", as.character(round(wt3$p.value, 3)))

mtodasplotPV <- mtodasplotmedian +
  annotate ("text", x = 0.8, y = 0.96, label = "p-value ", size = 12, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.87, label = wt1, size = 12, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.80, label = wt2, size = 12, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.73, label = wt3, size = 12, hjust = 0, family="Times New Roman") +
  theme(legend.position="none")+
  theme(text=element_text(size=16,  family="Times New Roman")) 

ggsave(nam, width = 15, height = 6, units = "in", dpi = 300)

mtodasplotPV

# -------------------- NEUTRAL PLOT ----------------------------------------

rm(list = ls())

load("Men_Data/Men_Plots.RData")

Group <- c(rep("Incongruent", nrow(Men)), rep("Neutral", nrow(Men_neutral)))
x <- data.frame(rbind(Men,Men_neutral))
x <- cbind(x, Group)


mtodasplot <- ggplot(x, aes(x=Reaction.Time, colour=Tempo, linetype=Group)) + #crea la gráfica: ggplot(datos adjuntos, aes(x=datos apara eje x, color= nombre del grupo o columna))
  stat_ecdf() + 
  scale_x_continuous( 
    breaks = c(0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4))+ 
  coord_cartesian(xlim=c(0.8,2.4))

data100 = x %>% filter(Tempo == "100 bpm" & Group == "Incongruent")
data140 = x %>% filter(Tempo == "140 bpm" & Group == "Incongruent")
data180 = x %>% filter(Tempo == "180 bpm" & Group == "Incongruent")

data100n = x %>% filter(Tempo == "100 bpm" & Group == "Neutral")
data140n = x %>% filter(Tempo == "140 bpm" & Group == "Neutral")
data180n = x %>% filter(Tempo == "180 bpm" & Group == "Neutral")

data100 = data100$Reaction.Time; data100n = data100n$Reaction.Time
data140 = data140$Reaction.Time; data140n = data140n$Reaction.Time
data180 = data180$Reaction.Time; data180n = data180n$Reaction.Time

s1 <- summary(data100); s1n <- summary(data100n)
s2 <- summary(data140); s2n <- summary(data140n)
s3 <- summary(data180); s3n <- summary(data180n)

mtodasplotmedian <- mtodasplot + 
  annotate ("text", x = 1.6, y = 0.25, label = sprintf ("100 bpm neutral %s [%s, %s]", round(s1n[3],3), round(s1n[1],3), round(s1n[6],3)), size = 9, colour = "brown1", hjust = 0, family="Times New Roman") +
  annotate ("text", x = 1.6, y = 0.18, label = sprintf ("140 bpm neutral %s [%s, %s]", round(s2n[3],3), round(s2n[1],3), round(s2n[6],3)), size = 9, colour = "green4", hjust = 0, family="Times New Roman") +
  annotate ("text", x = 1.6, y = 0.11, label = sprintf ("180 bpm neutral %s [%s, %s]", round(s3n[3],3), round(s3n[1],3), round(s3n[6],3)), size = 9, colour = "steelblue2", hjust = 0, family="Times New Roman")

# incongruent vs neutral

in1 <- wilcox.test(data100,data100n)
in2 <- wilcox.test(data140,data140n)
in3 <- wilcox.test(data180,data180n)

in1 <- ifelse(in1$p.value < 0.0006, 
              paste0("100 vs 100" ," = ", "< 6e-04"),
              paste0("100 vs 100" ," = ", as.character(round(in1$p.value, 3))))

in2 <- ifelse(in2$p.value < 0.006, 
              paste0("140 vs 140" ," = ", "< 6e-04"),
              paste0("140 vs 140" ," = ", as.character(round(in2$p.value, 3))))

in3 <- ifelse(in3$p.value < 0.006, 
              paste0("180 vs 180" ," = ", "< 6e-04"),
              paste0("180 vs 180" ," = ", as.character(round(in3$p.value, 3))))



# neutral vs neutral 

wt1n <- wilcox.test(data100n,data140n)
wt2n <- wilcox.test(data100n,data180n)
wt3n <- wilcox.test(data140n,data180n)

wt1n <- paste0("100 vs 140" ," = ", as.character(round(wt1n$p.value, 3)))
wt2n <- paste0("100 vs 180" ," = ", as.character(round(wt2n$p.value, 3)))
wt3n <- paste0("140 vs 180" ," = ", as.character(round(wt3n$p.value, 3)))


mtodasplotPV <- mtodasplotmedian +
  annotate ("text", x = 0.8, y = 0.95, label = "p-value stroop vs neutral", size = 7, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.90, label = in1, size = 7, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.85, label = in2 , size = 7, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.80, label = in3 , size = 7, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.70, label = "p-value neutral vs neutral ", size = 7, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.65, label = wt1n, size = 7, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.60, label = wt2n, size = 7, hjust = 0, family="Times New Roman") +
  annotate ("text", x = 0.8, y = 0.55, label = wt3n, size = 7, hjust = 0, family="Times New Roman") +
  theme(legend.position="none")+
  theme(text=element_text(size=22,  family="Times New Roman", face="bold"),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

ggsave("Men_INCvsNEUT.png", width = 13, height = 6, units = "in", dpi = 300)
