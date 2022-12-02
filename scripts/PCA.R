library(psych)
library(dplyr)
library(ggplot2)
my.df <- readRDS(here::here("data","cleaned.RDS")) %>% 
  select(-ref_date)
# componentes principais-----------------

det(cor(my.df)) > 0 # condição necessária
KMO <- KMO(my.df) # ideal é o mais próximo de 1
KMO$MSA #ok
bart <- bartlett.test(my.df) #ideal é rejeitar h0
bart$statistic #ok 
bart$p.value #ok
cortest <- cortest.bartlett(my.df) #Ho: A matriz de correlação da população é uma matriz identidade, ou seja as variáveis não são correlacionadas na população.
cortest$chisq #ok 
cortest$p.value # ok
alpha.c <- alpha(my.df, check.keys = TRUE) #O Alfa de Cronbach é um teste de consistência interna dos dados, baseada na correlação entre as variáveis observadas.
alpha.c$alpha.drop # alpha mínimo é de 0,6
mean(alpha.c$alpha.drop[,1])  # media alta
corrplot::corrplot(corr = cor(my.df), xlab = "", ylab = "") #correlação
scree(my.df) # ver o número de fatores e componentes
fa.parallel(my.df) # ver uma sugestão de componentes e fatores
#psych
my.pca <- pca(my.df, nfactors = 5)
my.pca
my.scores <- my.pca$scores
#base R
my.pca <- princomp(my.df) 
my.pca$loadings
screeplot(my.pca)
loadings <- my.pca$loadings[,1:5]  
rowname <-  row.names(loadings)
loadings <-  data.frame(loadings)
loadings %>% glimpse()
loadings$ticker <- rowname
loadings
loadings %>% gather("Component","Weight",-ticker)
loadings <- 
  loadings %>% pivot_longer(
  Comp.1:Comp.5,
  names_to = "component",
  values_to = "weight")

loadings %>% 
  ggplot(aes(x = ticker,y=weight))+
  geom_bar(stat = 'identity')+
  facet_grid(component ~.,scales = "free_y")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

library(ggcorrplot)
corr <- round(cor(my.df), 2)
ggcorrplot(corr,hc.order = TRUE, 
           type = "lower",outline.col = "white",
           colors = c("#E46726","white","#6D9EC1"))
