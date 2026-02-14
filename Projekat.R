library(readxl)
library(glmnet)
library(dplyr)

# 1 - Europe 2- NorthAmerica 3-Asia

#######################
###### Zadatak 1 ######
#######################

# Ucitavanje
df <- read_excel("Arcadis.xlsx")

# X i y
X <- as.matrix(df[, c("education","crime","health","affordability","Energy","Greenspace")])
y <- df$Tourism

# Ridge CV (lambda.min i lambda.1se)
set.seed(123)
ridge_cv <- cv.glmnet(X, y, alpha = 0, standardize = TRUE, nfolds = 10)

lambda_min <- ridge_cv$lambda.min
lambda_1se <- ridge_cv$lambda.1se

round(lambda_min, 4)  #   0.2422 e
round(lambda_1se, 4)  #   102.4271 

# Predikcije za sve gradove na lambda.min
pred <- as.numeric(predict(ridge_cv, s = "lambda.min", newx = X))
abs_err <- abs(y - pred)

top5 <- df %>%
  mutate(pred_Tourism = pred,
         abs_err = abs_err) %>%
  arrange(desc(abs_err)) %>%
  slice(1:5) %>%
  select(City, Tourism, pred_Tourism, abs_err)

top5 %>%
  mutate(across(c(Tourism, pred_Tourism, abs_err), ~round(.x, 3)))

# Podskup: Europe i Asia + crime > medijana(crime) (medijana na celom skupu)
crime_med <- median(df$crime, na.rm = TRUE)

df_sub <- df %>%
  filter(Continent %in% c(1, 3),
         crime > crime_med) %>%
  mutate(ContinentName = ifelse(Continent == 1, "Europe", "Asia"))

X_sub <- as.matrix(df_sub[, c("education","crime","health","affordability","Energy","Greenspace")])
y_sub <- df_sub$Tourism

# Ridge CV na podskupu + predikcije i abs_err
set.seed(123)
ridge_cv_sub <- cv.glmnet(X_sub, y_sub, alpha = 0, standardize = TRUE, nfolds = 10)

pred_sub <- as.numeric(predict(ridge_cv_sub, s = "lambda.min", newx = X_sub))
abs_err_sub <- abs(y_sub - pred_sub)

df_sub2 <- df_sub %>%
  mutate(pred_Tourism = pred_sub,
         abs_err = abs_err_sub)

# top 3 po kontinentu (Europe, Asia)
rez <- data.frame()

for (cont in c("Europe", "Asia")) {
  tmp <- df_sub2 %>%
    filter(ContinentName == cont) %>%
    arrange(desc(abs_err)) %>%
    slice(1:3) %>%
    select(Continent = ContinentName, City, Tourism, pred_Tourism, abs_err)
  
  rez <- bind_rows(rez, tmp)
}

rez %>%
  mutate(across(c(Tourism, pred_Tourism, abs_err), ~round(.x, 3)))


#######################
###### Zadatak 2 ######
#######################

################### 2a) 


# Podaci
str(df)

#df_ea subs
df_ea <- df %>%
  filter(Continent %in% c(1, 3)) %>%
  mutate(ContinentGroup = ifelse(Continent == 1, "Europe", "Asia"))

# Izdvajanje vrednosti po grupama
europe_val <- df_ea$Easeofdoingbusiness[df_ea$Continent == 1]
asia_val <- df_ea$Easeofdoingbusiness[df_ea$Continent == 3]

# Shapiro-Wilk test normalnosti
shapiro_europe <- shapiro.test(europe_val)
shapiro_asia <- shapiro.test(asia_val)
shapiro_europe
shapiro_asia

# Izbor testa i sprovođenje
install.packages("coin")
library(coin)

# oba normalna t-test (p > 0.05)
# Ako bar jedan nije Man Whitney (Wilcoxon)
shapiro_europe$p.value
shapiro_asia$p.value
# Zakljucak: Raspodela NIJE normalna (bar jedno p < 0.05). Koristimo Mann Whitney test,inace t-test
test_res <- wilcox.test(df_ea$Easeofdoingbusiness ~ df_ea$Continent)
test_res

################### 2b) 

#Podaci
str(df)

# Medijana Greenspace na celom skupu
gs_med <- median(df$Greenspace)
gs_med

df_sub_b <- df %>%
  filter(
    Continent %in% c(1, 3),
    Greenspace > gs_med
  )
df_sub_b

vars_to_test <- c("crime", "health", "Energy")

# Inicijalizacija praznog data frame-a za rezultate
df_results <- data.frame()

for (var in vars_to_test) {
  vals_eu <- df_sub_b[[var]][df_sub_b$Continent == 1]
  vals_as <- df_sub_b[[var]][df_sub_b$Continent == 3]
  
  shap_eu_p <- shapiro.test(vals_eu)$p.value
  shap_as_p <- shapiro.test(vals_as)$p.value
  
  if (shap_eu_p > 0.05 && shap_as_p > 0.05) {
    res <- t.test(vals_eu, vals_as)
    tn <- "t-test"
    stat <- res$statistic
    p_val <- res$p.value
  } else {
    res <- wilcox.test(vals_eu, vals_as)
    tn <- "Wilcoxon"
    stat <- res$statistic
    p_val <- res$p.value
  }
  
  # Dodavanje reda u tabelu
  df_results <- rbind(df_results, data.frame(
    Variable = var,
    Shapiro_p_Euro = round(shap_eu_p, 4),
    Shapiro_p_Asia = round(shap_as_p, 4),
    Korisceni_test = tn,
    p_value = round(p_val, 4),
    Test_statistic = round(stat, 4)
  ))
}

print(df_results)

################### 2c) 

# Q1 affordability (na celom skupu)
aff_q1 <- quantile(df$affordability, 0.25, na.rm = TRUE)

# Podskup: Europe (1) i North America (2) + affordability u prvom kvartilu
df_sub_c <- df %>%
  filter(Continent %in% c(1, 2),
         affordability <= aff_q1) 

crime_thresholds <- c(0.8, 0.9, 0.95)

# Prazna tabela za rezultate
rez_c <- data.frame()

for (thr in crime_thresholds) {
  
  df_final <- df_sub_c %>% filter(crime < thr)
  #izdvaja u vektore turizam gde je europe 
  vals_eu <- df_final$Tourism[df_final$Continent == 1]
  vals_na <- df_final$Tourism[df_final$Continent == 2]
  
  n_eu <- length(vals_eu)
  n_na <- length(vals_na)
  
  # Shapiro (samo ako n >= 3)
  shap_eu <- if (n_eu >= 3) shapiro.test(vals_eu)$p.value else NA_real_
  shap_na <- if (n_na >= 3) shapiro.test(vals_na)$p.value else NA_real_
  
  # Ako je jedna grupa prazna test nije moguc
  if (n_eu == 0 || n_na == 0) {
    test_name <- "nije moguće (n=0)"
    p_val <- NA_real_
  } else {
    # t-test samo ako su OBE grupe normalne i Shapiro nije NA
    use_ttest <- (!is.na(shap_eu) && !is.na(shap_na) && shap_eu > 0.05 && shap_na > 0.05)
    #Primena testova
    if (use_ttest) {
      res <- t.test(vals_eu, vals_na)   #  t-test 
      test_name <- "t-test"
      p_val <- res$p.value
    } else {
      res <- wilcox.test(vals_eu, vals_na, exact = FALSE)
      test_name <- "Mann–Whitney (Wilcoxon)"
      p_val <- res$p.value
    }
  }
  
  rez_c <- rbind(rez_c, data.frame(
    Crime_prag = thr,
    n_Europe = n_eu,
    n_NorthAmerica = n_na,
    Shapiro_p_Europe = shap_eu,
    Shapiro_p_NorthAmerica = shap_na,
    Korisceni_test = test_name,
    p_value = p_val,
    stringsAsFactors = FALSE
  ))
}

# Lep prikaz
rez_c <- rez_c %>%
  mutate(
    Shapiro_p_Europe = round(Shapiro_p_Europe, 4),
    Shapiro_p_NorthAmerica = round(Shapiro_p_NorthAmerica, 4),
    p_value = round(p_value, 5)
  )

rez_c


#######################
###### Zadatak 3 ######
#######################

conn_eu <- df$Connectivity[df$Continent == 1]
conn_na <- df$Connectivity[df$Continent == 2]
conn_as <- df$Connectivity[df$Continent == 3]

# Shapiro-Wilk test 
sh_eu <- shapiro.test(conn_eu)
sh_na <- shapiro.test(conn_na)
sh_as <- shapiro.test(conn_as)

sh_eu
sh_na
sh_as

# Kruskal-Wallis test, (bar 1 je < 0.05) , inc anova
kruskal.test(Connectivity ~ as.factor(Continent), data = df)

# Podskup: Greenspace > medijane (izracunata ranije)
df2 <- df[df$Greenspace > gs_med, ]

# Izdvoji Tourism po kontinentima
tour_eu <- df2$Tourism[df2$Continent == 1]
tour_na <- df2$Tourism[df2$Continent == 2]
tour_as <- df2$Tourism[df2$Continent == 3]

# Shapiro test normalnosti
sh2_eu <- shapiro.test(tour_eu)
sh2_na <- shapiro.test(tour_na)
sh2_as <- shapiro.test(tour_as)

sh2_eu
sh2_na
sh2_as

# sve normalne - ANOVA
fit_aov <- aov(Tourism ~ as.factor(Continent), data = df2)
summary(fit_aov)

# Post-Hoc test Tukey
TukeyHSD(fit_aov)


#######################
###### Zadatak 4 ######
#######################


# Ucitavanje biblioteke
library(cluster)

# Matrica indikatora + standardizacija
vars <- c("health","affordability","Energy","Easeofdoingbusiness","Connectivity")
X  <- as.matrix(df[, vars])
Xz <- scale(X)

############################################
# pam (k=3) na celom skupu
############################################
pamcluster <- pam(Xz, k = 3)  # podrazumevana metrika: Euclidean

# Broj gradova u klasterima
cl_sizes <- table(pamcluster$clustering)
cl_sizes

# Medoidi sa imenima respektivno
df$City[pamcluster$id.med]

# Silhouette metrika
sil_val <- pamcluster$silinfo$avg.width
sil_val

# Prosecan health,Energy,Connectivity
health_means <- tapply(df$health, pamcluster$clustering, mean)
energy_means <- tapply(df$Energy, pamcluster$clustering, mean)
conn_means <- tapply(df$Connectivity, pamcluster$clustering, mean)

# Najmanja prosecna vrednost health klaster

cl_min_health <- which.min(health_means)
medoid_min_health <- df$City[pamcluster$id.med[cl_min_health]]
min_health_value <- health_means[cl_min_health]

medoid_min_health
min_health_value

# Najveca prosecna vrednost energy klaster

cl_max_energy <- which.max(energy_means)
medoid_max_energy <- df$City[pamcluster$id.med[cl_max_energy]]
max_energy_value <- energy_means[cl_max_energy]

medoid_max_energy
max_energy_value

# Najmanja prosecna vrednost connectivity klaster

cl_min_conn <- which.min(conn_means)
medoid_min_conn <- df$City[pamcluster$id.med[cl_min_conn]]
min_conn_value <- conn_means[cl_min_conn]

medoid_min_conn
min_conn_value

### Tabela 1
#tourism_med i crime_med

tourism_med = median(df$Tourism)
crime_med = median(df$crime)

# subset df_sub_tour_crime
df_sub_tour_crime = df[df$Tourism > tourism_med & df$crime < crime_med,]
# pam nad df_sub_tour_crime
X_sub_tour_crime   <- as.matrix(df_sub_tour_crime[, vars])
Xz_sub_tour_crime  <- scale(X_sub_tour_crime)
pamcluster2 <- pam(Xz_sub_tour_crime, k = 3)  # podrazumevana metrika: Euclidean

# for petlja za popunjavanje tabele
tab1 <- data.frame(
  Klaster = 1:3,
  Medoid = "",
  Najudaljeniji_grad = "",
  Rastojanje_do_njega = 0,
  Broj_gradova_u_klasteru = 0,
  stringsAsFactors = FALSE
)

for (k in 1:3) {
  # izdvoji gradove u vektor koji su u datom klasteru k 
  idx_k <- which(pamcluster2$clustering == k)
  tab1$Broj_gradova_u_klasteru[k] <- length(idx_k)
  
  # indeks medoida 
  med_idx <- pamcluster2$id.med[k]
  tab1$Medoid[k] <- df_sub_tour_crime$City[med_idx]
  
  # rastojanja od medoida do svih gradova u tom klasteru (euclidean u Xz)
  med_vec <- Xz_sub_tour_crime[med_idx, ]
  dists <- sqrt(rowSums((Xz_sub_tour_crime[idx_k, , drop=FALSE] - med_vec)^2))
  
  # najudaljeniji grad
  far_pos <- which.max(dists)
  far_idx <- idx_k[far_pos]
  
  tab1$Najudaljeniji_grad[k] <- df_sub_tour_crime$City[far_idx]
  tab1$Rastojanje_do_njega[k] <- dists[far_pos]
}

tab1

###########

# health_med
health_med = median(df$health)

# subset df_sub_health
df_sub_health = df[df$health > health_med,]
df_sub_tour_crime = df[df$Tourism > tourism_med & df$crime < crime_med,]

# pam nad df_sub_health
X_sub_health  <- as.matrix(df_sub_health[, vars])
Xz_sub_health  <- scale(X_sub_health)
pamcluster3 <- pam(Xz_sub_health, k = 3)

# Tabela2 Raspodela gradova po klasterima u odnosu na pripadajuce kontinente 
cl3 <- pamcluster3$clustering
tab2 <- table(df_sub_health$Continent, cl3) # red Cont , kol cl3
tab2

# Tabela3 Dominantni klaster, udeo, n_total po kontinentu 

tab3 <- data.frame(
  Continent = c(1,2,3),
  Dominantni_klaster = 0,
  Udeo = 0,
  Ukupno_gradova_na_kontinentu = 0
)

for (cc in c(1,2,3)) {
  
  # broj gradova tog kontinenta u sva 3 klastera
  counts <- tab2[as.character(cc), ]
  
  n_total <- sum(counts)
  dom_cl <- which.max(counts)
  udeo <- max(counts) / n_total
  
  tab3[tab3$Continent == cc, "Dominantni_klaster"] <- dom_cl
  tab3[tab3$Continent == cc, "Udeo"] <- udeo
  tab3[tab3$Continent == cc, "Ukupno_gradova_na_kontinentu"] <- n_total
}

tab3


# Tabela 4: Euclidean vs Manhattan k=3


# Napravi praznu tabelu kao u zadatku
tab4 <- data.frame(
  Metrika = c("Euclidean", "Manhattan"),
  avg_sill_score = 0,
  Objective_score = 0,
  stringsAsFactors = FALSE
)

# for petlja preko 2 metrike
for (i in 1:2) {
  
  if (tab4$Metrika[i] == "Euclidean") {
    
    # Euclidean: pam direktno na Xz
    pam_tmp <- pam(Xz, k = 3, metric = "euclidean")
    
  } else {
    
    # Manhattan
    Dman <- dist(Xz, method = "manhattan")      # matrica rastojanja
    pam_tmp <- pam(Dman, k = 3, diss = TRUE)    # pam na dist matrici
    
  }
  
  # upis avg silhouette i objective u tab4
  tab4$avg_sill_score[i] <- pam_tmp$silinfo$avg.width
  tab4$Objective_score[i] <- pam_tmp$objective[2]
}

tab4

# Bolja metrika

if (tab4$avg_sill_score[1] >= tab4$avg_sill_score[2]) {
  bolja_metrika <- "Euclidean"
} else {
  bolja_metrika <- "Manhattan"
}

bolja_metrika



# Medoidi boljeg modela

if (bolja_metrika == "Euclidean") {
  pam_best <- pam(Xz, k = 3, metric = "euclidean")
} else {
  Dman <- dist(Xz, method = "manhattan")
  pam_best <- pam(Dman, k = 3, diss = TRUE)
}

medoidi_best <- df$City[pam_best$id.med]
medoidi_best

#######################
###### Zadatak 5 ######
#######################
  
library(REdaS)

data.use <- df[, c("health","affordability","Energy","Easeofdoingbusiness","Connectivity")]
matrixdata <- data.matrix(data.use)

# Bartlett test of sphericity
bart_spher(matrixdata)

# KMO (MSA)
KMOS(matrixdata)

# korelaciona matrica (FA se radi na korelaciji)
R <- cor(matrixdata)   

# eigenvalues (silazno
ev <- eigen(R)$values     

# % objasnjene varijanse (sum(ev)=broj varijabli)
prop_var <- ev / sum(ev)             
cum_var  <- cumsum(prop_var)         

tab_ev <- data.frame(
  Faktor = 1:length(ev),
  Eigenvalue = round(ev, 4),
  Proportion = round(prop_var, 4),
  Cumulative = round(cum_var, 4)
)
tab_ev

# minimalan broj faktora da kumulativno objasni >= 70%
n_factors_70 <- which(cum_var >= 0.70)[1]
n_factors_70

library(psych)

# faktorska analiza
fa4 <- fa(r = matrixdata, nfactors = 4, rotate = "varimax", fm = "ml", scores = "regression")

L <- as.matrix(unclass(fa4$loadings))
comm <- fa4$communality

# provera kako se faktori zovu i koliko ih ima
colnames(L)
dim(L)

# "MR1" = prva kolona (kako god da se zove)
MR1_name <- colnames(L)[1]

# MR1 najvece |loading|
abs_MR1 <- abs(L[, MR1_name])
var_MR1 <- names(which.max(abs_MR1))
var_MR1
val_MR1 <- L[var_MR1, MR1_name]
val_MR1

# najlosije objasnjena (min communality)
var_min_comm <- names(which.min(comm))
var_min_comm
val_min_comm <- comm[var_min_comm]
val_min_comm

# communality health i %
comm_health <- comm["health"]
comm_health
perc_health <- 100 * comm_health
perc_health

# visok MR1 skor i nizak affordability: biznis smer po korelaciji MR1 i Easeofdoingbusiness
mr1_score <- as.numeric(fa4$scores[, 1])
cor_mr1_ease <- cor(mr1_score, df$Easeofdoingbusiness)
biznis_txt <- ifelse(cor_mr1_ease > 0, "visoko razvijen za biznis", "nisko razvijen za biznis")
biznis_txt

# Faktor sa najvecom prosec. komunalnoscu + 2 top var po |loading| 

# SS loadings po faktoru
ss_load <- colSums(L^2)

# average communality po faktoru 
avg_comm_factor <- ss_load / nrow(L)
avg_comm_factor

# faktor sa max average communality
best_fac <- names(which.max(avg_comm_factor))
best_fac
best_val <- avg_comm_factor[best_fac]
best_val

# dve promenljive sa najvećim |loading| na tom faktoru
ord2 <- order(abs(L[, best_fac]), decreasing = TRUE)[1:2]
top_var1 <- rownames(L)[ord2[1]]
top_var2 <- rownames(L)[ord2[2]]
top_var1
top_var2


# Tabela: grad sa najvećim skorom za svaki faktor 

scores_df <- as.data.frame(fa4$scores)
scores_df$City <- df$City

tab_city_max <- data.frame(
  Faktor = colnames(scores_df)[1:4],   # 4 faktora kako god da se zovu
  Grad = "",
  Skor = NA_real_,
  stringsAsFactors = FALSE
)

for (i in 1:4) {
  f <- tab_city_max$Faktor[i]
  idx <- which.max(scores_df[[f]])
  tab_city_max$Grad[i] <- scores_df$City[idx]
  tab_city_max$Skor[i] <- scores_df[[f]][idx]
}

tab_city_max
tab_city_max$Faktor <- c("MR1","MR2","MR3","MR4")
tab_city_max$Skor <- round(tab_city_max$Skor, 3)
tab_city_max


# Evropa + gornji kvartil Tourism (u Evropi) + cor sa MR1..MR4

# Europe subset
df_eu <- df[df$Continent == 1, ]

# q75 unutar Evrope
q75_eu <- quantile(df_eu$Tourism, 0.75)
q75_eu

# podskup: Evropa & Tourism >= q75_eu
idx_eu_top <- which(df$Continent == 1 & df$Tourism >= q75_eu)

# skorovi na tom podskupu
scores_top <- scores_df[idx_eu_top, ]
tour_top <- df$Tourism[idx_eu_top]

# cor(Tourism, svaki faktor) for petlja
cors <- data.frame(
  Faktor = c("MR1","MR2","MR3","MR4"),
  Cor = NA_real_,
  stringsAsFactors = FALSE
)

for (i in 1:4) {
  cors$Cor[i] <- cor(tour_top, scores_top[[i]])
}

cors$Cor <- round(cors$Cor, 4)
cors

# faktor sa najvecom |cor|
best_i <- which.max(abs(cors$Cor))
best_cor_fac <- cors$Faktor[best_i]
best_cor_fac
best_cor_val <- cors$Cor[best_i]
best_cor_val


# analiza pogodnosti i faktorska analiza za svaki kontinent posebno
cont_codes <- c(1,2,3)

rez <- data.frame(
  Kontinenti = cont_codes, # 1 eu 2 na 3as
  n = NA_integer_,
  Bartlett_p = NA_real_,
  KMO_MSA_value = NA_real_,
  n_factors = NA_integer_,
  top_communality_variable = "",
  Comm_value = NA_real_,
  stringsAsFactors = FALSE
)

for (i in 1:3) {
  cc <- cont_codes[i]
  df_c <- df[df$Continent == cc, vars]
  
  rez$n[i] <- nrow(df_c)
  
  mat <- data.matrix(df_c)
  
  b <- bart_spher(mat)
  rez$Bartlett_p[i] <- as.numeric(b$p.value)
  
  k <- KMOS(mat)
  rez$KMO_MSA_value[i] <- as.numeric(k$MSA)
  
  R <- cor(mat)
  ev <- eigen(R)$values
  nf <- sum(ev > 1)
  if (nf < 1) nf <- 1
  rez$n_factors[i] <- nf
  
  fa_c <- fa(r = mat, nfactors = nf, rotate = "varimax", fm = "ml")
  comm <- fa_c$communality
  
  top_var <- names(which.max(comm))
  top_val <- as.numeric(comm[top_var])
  
  rez$top_communality_variable[i] <- top_var
  rez$Comm_value[i] <- top_val
}

rez$Bartlett_p <- round(rez$Bartlett_p, 6)
rez$KMO_MSA_value <- round(rez$KMO_MSA_value, 4)
rez$Comm_value <- round(rez$Comm_value, 4)

rez

# FA na celom skupu (nfactors=4, varimax, scores="regression" uradjeno ranije

scores_df <- as.data.frame(fa4$scores)
colnames(scores_df) <- c("MR1","MR2","MR3","MR4")

q_conn_75 <- as.numeric(quantile(df$Connectivity, 0.75))
q_aff_25  <- as.numeric(quantile(df$affordability, 0.25))

idx_sub <- which(df$Connectivity >= q_conn_75 & df$affordability <= q_aff_25)

scores_sub <- scores_df[idx_sub, ]
tour_sub <- df$Tourism[idx_sub]

mean_scores <- data.frame(
  Faktor = c("MR1","MR2","MR3","MR4"),
  MeanScore = NA_real_,
  stringsAsFactors = FALSE
)

for (i in 1:4) {
  mean_scores$MeanScore[i] <- mean(scores_sub[[i]])
}

mean_scores$MeanScore <- round(mean_scores$MeanScore, 4)
mean_scores

best_i <- which.max(abs(mean_scores$MeanScore)) # vraca index 
best_factor <- mean_scores$Faktor[best_i] # mean_scores - df Faktor je kolona 
best_factor
best_value <- mean_scores$MeanScore[best_i]
best_value

# Top 5 gradova po izabranom faktoru (smer proseka)

df_scores_all <- cbind(df, scores_df)

if (best_value >= 0) {
  top5 <- df_scores_all %>%
    arrange(desc(.data[[best_factor]])) %>%
    slice(1:5)
} else {
  top5 <- df_scores_all %>%
    arrange(.data[[best_factor]]) %>%
    slice(1:5)
}

top5_table <- data.frame(
  Rang = 1:5,
  Grad = top5$City,
  Kontinent = ifelse(top5$Continent == 1, "Eu",
                     ifelse(top5$Continent == 2, "Na", "Аzija")),
  Tourism = top5$Tourism,
  Odabrani_faktor = best_factor,
  Score = round(top5[[best_factor]], 4),
  stringsAsFactors = FALSE
)

top5_table


#######################
###### Zadatak 6 ######
#######################

library(survival)
library(survminer)
library(readxl)
library(dplyr)

# Ucitavanje
lung <- read_excel("plucaSurvival.xlsx")

# event: 1 ako je status==2, inace 0
lung$event <- ifelse(lung$status == 2, 1, 0)

# faktor za pol (1=muskarac, 2=zena)
lung$sexF <- factor(lung$sex, levels = c(1,2), labels = c("Muskarac","Zena"))


# 1) Osnovno: n, events, median

n_obs <- nrow(lung)
n_events <- sum(lung$event)

fit_all <- survfit(Surv(time, event) ~ 1, data = lung)
median_all <- summary(fit_all)$table["median"]

n_obs
n_events
median_all


# 2) KM po polu + log-rank

fit_sex <- survfit(Surv(time, event) ~ sexF, data = lung)

lr_sex <- survdiff(Surv(time, event) ~ sexF, data = lung)
chisq_lr <- lr_sex$chisq
p_lr <- 1 - pchisq(chisq_lr, df = length(lr_sex$n) - 1)

chisq_lr
p_lr

# medijane po polu (za informaciju)
summary(fit_sex)$table[, "median"]


# 3) Cox PH (jednovarijantni) - pol

cox_sex <- coxph(Surv(time, event) ~ sexF, data = lung)
sum_cox_sex <- summary(cox_sex)

hr_sex_uni <- sum_cox_sex$conf.int[1, "exp(coef)"]
p_sex_uni <- sum_cox_sex$coefficients[1, "Pr(>|z|)"]

hr_sex_uni
p_sex_uni


# 4) Cox PH (multivarijantni) - sex + age + ph.ecog

lung2 <- lung %>% filter(!is.na(ph.ecog), !is.na(age), !is.na(sex))

cox_multi <- coxph(Surv(time, event) ~ sexF + age + ph.ecog, data = lung2)
sum_multi <- summary(cox_multi)

HR_sex <- sum_multi$conf.int["sexFZena", "exp(coef)"]
HR_age <- sum_multi$conf.int["age", "exp(coef)"]
HR_ecog <- sum_multi$conf.int["ph.ecog", "exp(coef)"]

p_sex <- sum_multi$coefficients["sexFZena", "Pr(>|z|)"]
p_age <- sum_multi$coefficients["age", "Pr(>|z|)"]
p_ecog <- sum_multi$coefficients["ph.ecog", "Pr(>|z|)"]

HR_sex; HR_age; HR_ecog
p_sex; p_age; p_ecog

inc_ecog_pct <- (HR_ecog - 1) * 100
inc_ecog_pct

women_less_pct <- (1 - HR_sex) * 100
women_less_pct


# 5) AgeGroup (tercili) + KM + log-rank + tabela + "najzilavija"

q1 <- quantile(lung$age, 1/3)
q2 <- quantile(lung$age, 2/3)

lung$AgeGroup <- cut(lung$age,
                     breaks = c(-Inf, q1, q2, Inf),
                     labels = c("Low","Medium","High"),
                     right = FALSE)

fit_age <- survfit(Surv(time, event) ~ AgeGroup, data = lung)

lr_age <- survdiff(Surv(time, event) ~ AgeGroup, data = lung)
chisq_age <- lr_age$chisq
p_age_lr <- 1 - pchisq(chisq_age, df = length(lr_age$n) - 1)

chisq_age
p_age_lr

# tabela po grupama: n, events, median, S(365)
sum_age <- summary(fit_age)$table
tab1 <- data.frame(
  AgeGroup = rownames(sum_age),
  n = sum_age[, "records"],
  events = sum_age[, "events"],
  median_survival = sum_age[, "median"],
  stringsAsFactors = FALSE
)

S365 <- summary(fit_age, times = 365)$surv
grp365 <- gsub("AgeGroup=", "", summary(fit_age, times = 365)$strata)
S365_df <- data.frame(AgeGroup = grp365, S_365 = S365)

tab1 <- left_join(tab1, S365_df, by = "AgeGroup")
tab1

# najzilavija (najveca medijana)
best_group <- tab1$AgeGroup[which.max(tab1$median_survival)]
best_group

# delta i extra_days_per_month (u odnosu na druge dve)
best_med <- tab1$median_survival[tab1$AgeGroup == best_group]
others <- tab1 %>% filter(AgeGroup != best_group)

tab2 <- data.frame(
  Najzilavija_Grupa = best_group,
  Preostala_grupa = others$AgeGroup,
  Median_days_Najzilavija = best_med,
  Median_days_Preostala = others$median_survival,
  Delta_days = best_med - others$median_survival,
  Extra_days_per_month = (best_med - others$median_survival) / (others$median_survival/30)
)

tab2


# 6) Cutoff search (zene & ph.ecog<=1) + log-rank po cutoff + najbolji cutoff

sub <- lung2 %>% filter(sexF == "Zena", ph.ecog <= 1)

qs <- c(0.20,0.30,0.40,0.50,0.60,0.70,0.80)
cuts <- as.numeric(quantile(sub$age, qs))

n_min <- 5
events_min <- 5

cut_res <- data.frame(cutoff = cuts, chisq = NA_real_, p_value = NA_real_,
                      n_low = NA_integer_, ev_low = NA_integer_,
                      n_high = NA_integer_, ev_high = NA_integer_)

for (i in 1:nrow(cut_res)) {
  c0 <- cut_res$cutoff[i]
  sub$AgeBin <- ifelse(sub$age < c0, paste0("<", round(c0,3)), paste0(">=", round(c0,3)))
  
  n_low <- sum(sub$age < c0)
  n_high <- sum(sub$age >= c0)
  ev_low <- sum(sub$event[sub$age < c0])
  ev_high <- sum(sub$event[sub$age >= c0])
  
  cut_res$n_low[i] <- n_low
  cut_res$n_high[i] <- n_high
  cut_res$ev_low[i] <- ev_low
  cut_res$ev_high[i] <- ev_high
  
  if (n_low >= n_min && n_high >= n_min && ev_low >= events_min && ev_high >= events_min) {
    lr <- survdiff(Surv(time, event) ~ AgeBin, data = sub)
    chisq <- lr$chisq
    pval <- 1 - pchisq(chisq, df = 1)
    cut_res$chisq[i] <- chisq
    cut_res$p_value[i] <- pval
  }
}

cut_res

best_i <- which.max(cut_res$chisq)
best_cut <- cut_res$cutoff[best_i]
best_chisq <- cut_res$chisq[best_i]
best_p <- cut_res$p_value[best_i]

best_cut
best_chisq
best_p

# sumarno za optimalni cutoff
sub$AgeBin <- ifelse(sub$age < best_cut, paste0("age < ", round(best_cut,3)), paste0("age >= ", round(best_cut,3)))
fit_bin <- survfit(Surv(time, event) ~ AgeBin, data = sub)

tab_bin <- summary(fit_bin)$table
tab_bin2 <- data.frame(
  AgeBin = rownames(tab_bin),
  n = tab_bin[, "records"],
  events = tab_bin[, "events"],
  median_survival = tab_bin[, "median"],
  stringsAsFactors = FALSE
)

S365b <- summary(fit_bin, times = 365)$surv
grp365b <- gsub("AgeBin=", "", summary(fit_bin, times = 365)$strata)
S365b_df <- data.frame(AgeBin = grp365b, S_365 = S365b)

tab_bin2 <- left_join(tab_bin2, S365b_df, by = "AgeBin")
tab_bin2


#######################
###### Zadatak 7 ######
#######################

library(biclust)
library(BicARE)
library(foreign)

food_data <- read.spss(file.choose(), to.data.frame = TRUE)

food_data.use <- subset(food_data, select = -c(Country))
food_data.use.matrix <-as.matrix(food_data.use)
set.seed(123)
resBCCC <- biclust(food_data.use.matrix,method=BCCC(),delta=1.5,alpha=1,number=25)
summary(resBCCC)


rows_bc2 <- which(resBCCC@RowxNumber[,2])
cols_bc2 <- which(resBCCC@NumberxCol[2,])
mean(food_data.use.matrix[rows_bc2, "V17"])

rows_bc4<-which(resBCCC@RowxNumber[,4])
cols_bc4<-which(resBCCC@NumberxCol[4,])
max(food_data.use.matrix[rows_bc4,"V13"])

rows_bc5 <- which(resBCCC@RowxNumber[,5])
cols_bc5 <- which(resBCCC@NumberxCol[5,])
median(food_data.use.matrix[rows_bc5, "V7"])

rows_bc3 <- which(resBCCC@RowxNumber[,3])
cols_bc3 <- which(resBCCC@NumberxCol[3,])
min(food_data.use.matrix[rows_bc3, "V2"])


# B

biclustmember(resBCCC, food_data.use.matrix)
sum(resBCCC@NumberxCol[, 5])

ord<-bicorder(resBCCC, cols=TRUE, rev=TRUE)
biclustmember(resBCCC, food_data.use.matrix, which=ord, main= "Bicluster membership graph ordered")

sum(resBCCC@NumberxCol[, 11])

column_counts <- colSums(resBCCC@NumberxCol)
print(column_counts)





colnames(resBCCC@NumberxCol) <- colnames(food_data.use.matrix)
column_counts <- colSums(resBCCC@NumberxCol)

max_var   <- names(which.max(column_counts))
max_count <- max(column_counts)

cat(
  "\nPromenljiva", max_var,
  "se pojavljuje:", max_count, "puta\n"
)


# c
M <- resBCCC@NumberxCol
M
# true false ovo nam pokazuje pojavljivanje promenljivih u biklasterima
Mnum <- 1 * M              # TRUE/FALSE → 1/0

pair_matrix <- t(Mnum) %*% Mnum
diag(pair_matrix) <- 0     # brisemo samo parove
pair_matrix
max(pair_matrix)


max_val <- max(pair_matrix)

which(pair_matrix == max_val, arr.ind = TRUE)



# d 
# fja za afinitet
calculate_affinity <- function(biclust_result, data_matrix, cluster_number) {
  rows <- which(biclust_result@RowxNumber[, cluster_number])
  cols <- which(biclust_result@NumberxCol[cluster_number, ])
  
  bicluster_data <- data_matrix[rows, cols]
  mean_bicluster <- mean(bicluster_data)
  mean_whole <- mean(data_matrix[, cols])
  
  affinity <- mean_bicluster / mean_whole
  return(affinity)
}

# Izracunaj za svе
num_clusters <- resBCCC@Number
affinities <- numeric(num_clusters)

for (i in 1:num_clusters) {
  affinities[i] <- calculate_affinity(resBCCC, food_data.use.matrix, i)
}

# Prikazi
names(affinities) <- paste0("BC", 1:num_clusters)
print(affinities)

# Najveci
max_affinity <- max(affinities)
best_cluster <- which.max(affinities)

cat("Biklaster se najv afinitetom :", best_cluster, "\n")
cat("Vrednost afiniteta:", max_affinity, "\n")




# e
num_clusters <- resBCCC@Number

#Priprema podataka
results <- data.frame(
  cluster = 1:num_clusters,
  n_rows = numeric(num_clusters),
  n_cols = numeric(num_clusters),
  within_sd = numeric(num_clusters)
)
for (i in 1:num_clusters) {
  # Pronadji redove i kol
  rows <- which(resBCCC@RowxNumber[, i])
  cols <- which(resBCCC@NumberxCol[i, ])
  
  # br kol i redova
  results$n_rows[i] <- length(rows)
  results$n_cols[i] <- length(cols)
  
  # izvlacenje podataka iz biklast
  bicluster_data <- food_data.use.matrix[rows, cols]
  
  # stddev
  results$within_sd[i] <- sd(bicluster_data)
}

print(results)

filtrirani_rezultati<- subset(results,results$n_rows>=6 & results$n_cols>=5)
min_indeks <- which.min(filtrirani_rezultati$within_sd)
najbolji_biklaster <- filtrirani_rezultati[min_indeks, ]

najbolji_biklaster

#######################
###### Zadatak 8 ######
#######################

# Kreiranje kategorije EmploymentCategory
df$EmploymentCategory <- cut(
  df$Employment,
  breaks = c(-Inf, 0.3, 0.6, Inf),
  labels = c("low", "medium", "high")
)

# Broj instanci po kategorijama
table(df$EmploymentCategory)


# Kreiranje kategorije EducationCategory
df$EducationCategory <- cut(
  df$education,
  breaks = c(-Inf, 0.4, 0.6, Inf),
  labels = c("low", "medium", "high")
)

# Broj instanci po kategorijama
table(df$EducationCategory)

# Kreiranje binarne varijable HighEmployment
df$HighEmployment <- ifelse(df$EmploymentCategory == "high", 1, 0)

# Broj instanci po vrednostima
table(df$HighEmployment)

# Statisticki znac prediktori: 
# LR
model <- glm(HighEmployment ~ crime + Drinkingwaterandsanitation +
               Airpollution + Energy + Greenspace + EducationCategory,
             data = df, family = binomial)

# model summarry
summary(model)

# exp(3.0260) = 20.6

# Metike (threshold = 0.5)

library(caret)

# LR na ceo skup
log_model <- glm(HighEmployment ~ crime + Drinkingwaterandsanitation +
                   Airpollution + Energy + Greenspace + EducationCategory,
                 data = df, family = binomial)

# predikcija verovatnoca
pred_probs <- predict(log_model, type = "response")

# binarne klase (>0.5)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# 4) konfuziona matrica + metrike
cm <- confusionMatrix(as.factor(pred_class),
                      as.factor(df$HighEmployment),
                      positive = "1")

acc0  <- cm$overall["Accuracy"]
prec0 <- cm$byClass["Precision"]
rec0  <- cm$byClass["Recall"]
f10   <- cm$byClass["F1"]

acc0
prec0 
rec0 
f10




# 10 podela train/test (70:30), seed 123-132

rez <- data.frame(
  Seed = 123:132,
  Accuracy = NA_real_,
  Precision = NA_real_,
  Recall = NA_real_,
  F1 = NA_real_
)

n <- nrow(df)
train_n <- floor(0.7 * n)

for (i in 1:nrow(rez)) {
  set.seed(rez$Seed[i])
  
  idx_train <- sample(1:n, size = train_n, replace = FALSE)
  train <- df[idx_train, ]
  test  <- df[-idx_train, ]
  
  m <- glm(HighEmployment ~ crime + Drinkingwaterandsanitation +
             Airpollution + Energy + Greenspace + EducationCategory,
           data = train, family = binomial)
  
  p_test <- predict(m, newdata = test, type = "response")
  class_test <- ifelse(p_test > 0.5, 1, 0)
  
  cm_test <- confusionMatrix(as.factor(class_test),
                             as.factor(test$HighEmployment),
                             positive = "1")
  
  rez$Accuracy[i]  <- cm_test$overall["Accuracy"]
  rez$Precision[i] <- cm_test$byClass["Precision"]
  rez$Recall[i]    <- cm_test$byClass["Recall"]
  rez$F1[i]        <- cm_test$byClass["F1"]
}

rez_round <- rez
rez_round[,2:5] <- round(rez_round[,2:5], 3)
rez_round

# f1 je metrika za ujednaceno sagledavanje vrlina , iz tabele se vidi da je to seed 128
