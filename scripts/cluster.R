library(dplyr)
library(hopkins)
library(factoextra)
library(NbClust)
library(cluster)
###
my_df <- readRDS(here::here("data","cleaned.RDS")) %>% 
  select(-ref_date)
means <-  apply(my_df,2,mean)
sds <-  apply(my_df,2,sd)
nor <-  scale(my_df,center=means,scale=sds)

### avaliação----------------
set.seed(4654)
hopkins(nor) # mais próximo de 1, melhor

## kmeans----------------
set.seed(657)
fviz_nbclust(nor,kmeans,method="wss") +
  labs(subtitle = "elbow method")

fviz_nbclust(nor,kmeans,method="silhouette") +
  labs(subtitle = "silhouette method")

gap_stat <- clusGap(nor, FUN = kmeans, nstart = 15,
                    K.max = 3, B = 100,
                    spaceH0 = "original")
fviz_gap_stat(gap_stat)


set.seed(654)
km <- eclust(nor,"kmeans",k=5,nstart=15, graph = FALSE)
pm <- pam(nor,5)
fviz_cluster(km,data=nor,
             ellipse.type = "t",#euclid
             star.plot=TRUE,
             repel=TRUE,
             ggtheme = theme_minimal())

# da pra melhorar
fviz_silhouette(km,pallete = "jco",
                ggtheme = theme_minimal())
##hclist ----------------------
distance <- dist(nor,method="euclidean")
my_clust <- hclust(distance,method="ward.D")
plot(my_clust)
member <- cutree(my_clust,5)
fviz_dend(my_clust,k=5,dex=0.5,
          color_labels_by_k = TRUE,
          rect=TRUE)

## misturas normais---------------
library(mclust)
mcl <- Mclust(nor, parameters = TRUE)
summary(mcl)
summary(mcl,parameters=TRUE)$mean
plot(mcl,what="BIC",ask=F)
plot(mcl, what = "classification")

plot(mcl, what = "uncertainty")
BIC <- mclustBIC(X)
plot(BIC)

library(fpc)
fit1 <- eclust(nor,"kmeans",k=3,nstart=60, graph = FALSE)
fit2 <- eclust(nor,"kmeans",k=2,nstart=60, graph = FALSE)
cluster.stats(distance, fit1$cluster, fit2$cluster)

## tidy alternative---------

kclusts <- 
  tibble(k = 1:5) %>%
  mutate(
    kclust = map(k, ~kmeans(my_df, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, my_df)
  )
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()


##dbscan
library(dbscan)
db <- dbscan(my_df, eps = 0.4, minPts = 4)
db
pairs(my_df, col = db$cluster + 1L)
opt <- optics(my_df, eps = 1, minPts = 4)
opt
opt <- extractDBSCAN(opt, eps_cl = 0.4)
plot(opt)
hdb <- hdbscan(my_df, minPts = 4)
hdb
plot(hdb, show_flat = TRUE)
cs  <-  cluster.stats(dist(my_df), db$cluster)
cs
cs[c("within.cluster.ss","avg.silwidth")]



### my choice----------------
my_df$cluster <- as.character(member)
my_df_plot <- 
my_df %>% 
  pivot_longer(
    cols = ends_with("SA"),
    names_to = "ticker",
    values_to = "valor")
soma <- 
  my_df %>% group_by(cluster) %>% 
  summarise_all(sum)
apply(soma, 1, sum) ### 1,2,4 = bull market
apply(soma, 1, sd) ## 3, 4 e 5 = maior volatilidade

p <- 
  my_df_plot %>% 
  filter(cluster %in% c(1,2,4)) %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  ggplot(aes(x = ticker,y = valor,
             fill = cluster))+
  geom_bar(stat = "identity")+
  facet_wrap(~cluster,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_minimal()

p %>% plotly::ggplotly()
p2 <- 
  my_df_plot %>% 
  filter(cluster %in% c(3,5)) %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  ggplot(aes(x = ticker,y = valor,
             fill = cluster))+
  geom_bar(stat = "identity")+
  facet_wrap(~cluster,scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 %>% plotly::ggplotly()
##cluster 1--------------------
## cluster 1 = bull market puxado por commodities
## nesses dias energia e comunicações desempenharam pouco
my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 1) %>% 
  slice_max(valor,n = 5)

my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 1) %>% 
  slice_min(valor,n = 5)

## cluster 2---------------------------
## empresas construtorias, consumo e logística desempenharam
## bem, provavelmente índices voltados ao consumo apresentaram
## bons resultados. Mas são dias não tão bons pra comoddities
## e holdings

my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 2) %>% 
  slice_max(valor,n = 5)

my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 2) %>% 
  slice_min(valor,n = 5)


## cluster 3---------------------------
## bear market em que o setor de construção tem pior desempenho
## nesses dias empresas mais previsíveis se sairam melhor
## caso da vivo e holdings

my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 3) %>% 
  slice_min(valor,n = 5)

my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 3) %>% 
  slice_max(valor,n = 5)



## cluster 4---------------------------
## bull market  puxado pelo setor siderúrgico
my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 4) %>% 
  slice_max(valor,n = 5)



## cluster 5---------------------------
## bear market  puxado pelo setor siderúrgico
## enquanto elétricas e  bancos mantém positivos
my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 5) %>% 
  slice_min(valor,n = 5)


my_df_plot %>% 
  group_by(cluster,ticker) %>% 
  summarise(valor = mean(valor)) %>% 
  filter(cluster == 5) %>% 
  slice_max(valor,n = 5)


### bancos e siderúrgia tem correlação negativa?
### energia e commotidies tem correlação negativa?
### consumo e holdings/comuncaçãoes tem correlação negativa?


library(corrr)
my_df %>% 
  select(-cluster) %>% 
  correlate() %>% 
  rplot()

my_df %>% 
  select(-cluster) %>% 
  correlate() %>% 
  stretch() %>% 
  filter(!is.na(r)) %>% 
  slice_min(r,n = 20)

# JHSF3.SA VALE3.SA 
# JBSS3.SA SYNE3.SA -
# SYNE3.SA JBSS3.SA
# PSSA3.SA VALE3.SA
# CYRE3.SA VALE3.SA 
# BRAP4.SA JHSF3.SA 
# JBSS3.SA MRVE3.SA
# BRAP4.SA ELET6.SA 