#organizando lo datos
datos<-read.csv("Hoja de Colegios - cop_simpl.csv",dec = ",")
#transformar tipo y valores religiosos a factor
#esto facilita algunos analisis
datos$Tipo<-as.factor(datos$Tipo)
datos$Rel<-as.factor(datos$Rel)
datos <- datos[order(datos$PUNT,decreasing = TRUE),]#de mayor a menor nota
datos

library(ggplot2)
#ordenados por nota
ggplot(data = datos, mapping = aes(x = reorder(Centro, PUNT), PUNT),fill=factor(Tipo)) + 
  geom_bar(stat = "identity",col = "black", size = 0.25, width=0.5) + coord_flip()+theme_bw()+
    ylab("puntuación") +xlab("")

table(datos$Tipo,datos$NOTA)

table(datos$Rel,datos$NOTA)
#solo los laicos llegan a tener una nota alta

#suma de valores
valores = colSums(datos[c(4:12)])
names<-c("Fem","Antibul","LGTB","Ecol","Exam","Coop","Dep","Cri","Pluril")
tabla.valores<-data.frame(names,valores)
ggplot(data = tabla.valores, mapping = aes(x = reorder(names,valores ), valores)) + 
  geom_bar(stat = "identity",col = "black", size = 0.5, width=0.5) + coord_flip()+theme_bw()+
    ylab("número de centros") + 
    xlab("valores") 

# overlay histogram and normal density
a<-ggplot(datos, aes(PUNT)) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(datos$PUNT), sd = sd(datos$PUNT)), 
    lwd = 1, 
    col = 'black'
  )+theme_bw()+
    ylab("densidad") + 
    xlab("puntuación") + 
    ggtitle("a) Densidad")
b<-ggplot(datos, aes(PUNT)) +
  geom_histogram()+theme_bw()+
    ylab("frecuencia") + 
    xlab("puntuación") + 
    ggtitle("b) Histograma")
library(gridExtra)
grid.arrange(a,b,ncol=2)

shapiro.test(log(datos$PUNT))#no normality as data is skewed to the right

median(datos$PUNT)
wilcox.colegios<-wilcox.test(datos$PUNT,mu=5,exact = F)
wilcox.colegios
#http://www.sthda.com/english/wiki/one-sample-wilcoxon-signed-rank-test-in-r

library(rcompanion)
wilcoxonOneSampleR(x = datos$PUNT, mu=5)#tamaño del efecto alto

c<-ggplot(datos,aes(x=Tipo,y=PUNT))+geom_boxplot(fill="grey",outlier.shape = NA)+theme_bw()+
stat_summary(fun="mean",shape=7,size=3)+geom_jitter(width=0.05)+
    ylab("puntuación") +ggtitle("a) Tipo de centro")+xlab("")

d<-ggplot(datos,aes(x=Rel,y=PUNT))+geom_boxplot(fill="grey",outlier.shape = NA)+theme_bw()+
  stat_summary(fun="mean")+ylab("puntuación") + 
    ggtitle("b) Valores religiosos")+
stat_summary(fun="mean",shape=7,size=3)+geom_jitter(width=0.05)+xlab("")

grid.arrange(c,d,ncol=2)

kruskal.test(PUNT~Tipo,data=datos)

library(rstatix)
dunn_test(PUNT~Tipo,data=datos, p.adjust.method = "bonferroni")

kruskal_effsize(PUNT~Tipo,data=datos)

library("dplyr")
group_by(datos, Tipo) %>%
  summarise(
    count = n(),
    mean = mean(PUNT, na.rm = TRUE),
    median = median(PUNT, na.rm = TRUE),
    sd = sd(PUNT, na.rm = TRUE),
    se =  sd(PUNT, na.rm = TRUE)/sqrt(n()) 
    )

wilcox.test(PUNT~Rel,data=datos,exact = F)
wilcox_effsize(PUNT~Rel,data=datos)
#The effect size r is calculated as Z statistic divided by square root of the sample size

group_by(datos, Rel) %>%
  summarise(
    count = n(),
    mean = mean(PUNT, na.rm = TRUE),
    median = median(PUNT, na.rm = TRUE),
    sd = sd(PUNT, na.rm = TRUE),
    se =  sd(PUNT, na.rm = TRUE)/sqrt(n()) 
    )


