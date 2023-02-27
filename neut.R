p <- 1 / 10
a <- 1
b <- 0
N <- 14

a * p + b * (1 - p) #expected theoretical
abs(a - b) * sqrt(p * (1-p)) #expected SD
mu <- (a * p + b * (1 - p)) * N #expected sum
se <- abs(b - a) * sqrt(p * (1-p)) * sqrt(N) #expected SE of sum
pnorm(1, mu, se)

pn <- seq(0, 10)
data.frame(pn, num = (100 - pnorm(pn, mu, se) * 100)) %>% ggplot(aes(pn, num)) + 
  geom_point() + labs() +
  ggrepel::geom_text_repel(aes(label = signif(num, 2))) +
  scale_x_continuous(breaks = c(seq(0,10))) + theme_bw() + 
  theme(panel.grid.minor = element_blank())

pnorm(1, mu, se)

neut <- readxl::read_xlsx("C:/Users/laxju/Documents/R/edX courses/neut.xlsx")
neut$logIC50 <- neut$IC50
neut$IC50 <- 2^neut$logIC50
neut$Group[c(18:26)] <- rep("CD20", 9)
names(neut)
library(tidyverse)
neut %>% 
   filter(!Group == "d106SIL12") %>%  
  ggplot(aes(IC50, Tumor_volume)) + 
  stat_smooth(method = "lm")+
  ggpubr::stat_cor(col="Black",method="pearson", size = 4.5) +
  geom_point(aes(color = Group, shape = Group), size = 3) +
  scale_y_continuous(trans = "log2") +
  scale_x_continuous(trans = "log2") +
  labs(y = expression(bold("Tumor volume (mm"^3*")")), x = "Serum IC50", color = "", shape = "") +
  scale_color_manual(values = c("Magenta3", "Red2", "Green")) +
  scale_shape_manual(values = c("triangle", "square")) + 
  theme_bw() + 
  theme(axis.text = element_text(size = rel(1.15)), 
        axis.title = element_text(size = rel(1.15), face = "bold"), 
        legend.text = element_text(size = rel(1.15)))

demo("colors")
iso <- neut %>% filter(Group == "Isotype") %>% .$IC50
cd20 <- neut %>% filter(Group == "CD20") %>% .$IC50

neut %>% 
   filter(!Group == "d106SIL12") %>%  
  ggplot(aes(IC50, Tumor_volume)) + 
  stat_smooth(method = "lm")+
  ggpubr::stat_cor(col="Black",method="spearman", size = 4.5) +
  geom_point(aes(color = Group, shape = Group), size = 3) +
  scale_y_continuous(trans = "log2", breaks = c(2^seq(2:12))) +
  scale_x_continuous(trans = "log2", breaks = c(2^seq(2:12))) +
  labs(y = expression(bold("Tumor volume (mm"^3*")")), x = "Serum IC50", color = "Depletion", shape = "Depletion") +
  scale_color_manual(values = c("#AD07E3","#FF0000", "#D38C2C")) +
  scale_shape_manual(values = c("triangle" , "square","circle")) + 
  theme_bw() + 
  theme(axis.text = element_text(size = rel(1.15)), 
        axis.title = element_text(size = rel(1.15), face = "bold"), 
        legend.text = element_text(size = rel(1.15)), 
        aspect.ratio = 1)


neut %>% 
  # filter(!Group == "d106SIL12") %>%  
  ggplot(aes(IC50, Tumor_volume)) + 
  stat_smooth(method = "lm")+
  ggpubr::stat_cor(col="Black",method="spearman", size = 4.5) +
  geom_point( size = 3) +
  scale_y_continuous(trans = "log2", breaks = c(2^seq(2:12))) +
  scale_x_continuous(trans = "log2", breaks = c(2^seq(2:11))) +
  labs(y = expression(bold("Tumor volume (mm"^3*")")), x = "Serum IC50", color = "", shape = "") +
  # scale_color_manual(values = c("#AD07E3","#D38C2C", "#FF0000")) +
  # scale_shape_manual(values = c("circle","triangle", "square")) + 
  theme_bw() + 
  theme(axis.text = element_text(size = rel(1.15)), 
        axis.title = element_text(size = rel(1.15), face = "bold"), 
        legend.text = element_text(size = rel(1.15)), 
        aspect.ratio = 1)


library(colorspace)
hsv(173/255, 226/255, 110/255)
rgb(211/255, 140/255, 44/255)
choose_palette()



###PAIRED WILCOX TEST FOR IL1B trial -------------
#make some dummy data to test
set.seed(200)
TNF <- data.frame(before = rnorm(n = 10, mean = 1), after = rnorm(n = 10, mean = 3), 
                  cytokine = rep("TNF", 10))
wilcox.test(TNF$before, TNF$after, paired = T, alternative = "two.sided")
TNF <- reshape2::melt(TNF)
IL1b <- data.frame(before = rnorm(n = 10, mean = 3), after = rnorm(n = 10, mean = 2), 
                   cytokine = rep("Il1b", 10))
IL1b <- reshape2::melt(IL1b)
cytokines <- rbind(TNF, IL1b) #merge two together

#function
long_grouped_wilcoxtest <- function(dataframe) {
  x <- dataframe
  cytokine <- names(table(x$cytokine))
  
  g <- lapply(cytokine, function(y) {
    wilcox.test(x$value[x$cytokine == y & x$variable == "before"],
                x$value[x$cytokine == y & x$variable == "after"], 
                paired = T, alternative = "two.sided")
  })
  
  pvalue <- lapply(1:length(g), function(x)
    g[[x]]$p.value) %>% setNames(nm = cytokine)
  return(pvalue)
}

long_grouped_wilcoxtest(cytokines) %>% unlist()
#---
data("ToothGrowth")
ToothGrowth
Toothgrowth <- ToothGrowth %>%
  mutate(dose = factor(dose, levels = c(0.5, 1, 2)))

ToothGrowth %>%
  mutate(dose = factor(dose, levels = c(0.5, 1, 2))) %>% 
  ggplot(aes(dose, len, fill = supp)) +
  geom_boxplot() +
  ggpubr::theme_pubr()


aov(len ~ dose, Toothgrowth) %>% summary()
TukeyHSD(aov(len ~ dose, Toothgrowth))
aov(len ~ dose + supp, Toothgrowth) %>% summary()
TukeyHSD(aov(len ~ dose * supp, Toothgrowth))$`dose:supp` %>% as.data.frame() %>% 
  arrange(rownames(.))
model.matrix(formula(Toothgrowth$len ~ Toothgrowth$dose))
