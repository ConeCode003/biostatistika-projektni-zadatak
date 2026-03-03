library(biclust)
library(BicARE)
library(foreign)
library(cluster)
library(readxl)
library(glmnet)
library(dplyr)
library(REdaS)
library(psych)
library(survival)
library(survminer)
library(readxl)
library(dplyr)

# 1 - Europe 2- NorthAmerica 3-Asia

#######################
###### Zadatak 1 ######
#######################

# Ucitavanje
df <- read_excel(file.choose())

# X i y
X <- as.matrix(df[, c("education","crime","health","affordability","Energy","Greenspace")])
y <- df$Tourism

# Ridge CV (lambda.min i lambda.1se)
set.seed(123)
ridge_cv <- cv.glmnet(X, y, family = "gaussian", alpha = 0)

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
ridge_cv_sub <- cv.glmnet(X_sub, y_sub, alpha = 0, family="gaussian")

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

# Izbor testa i sprovođenje,oba normalna t-test (p > 0.05),Ako bar jedan nije Man Whitney (Wilcoxon)
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
df_final <- df_sub_c %>% filter(crime < 0.9)
df_final
n_eu <- length(vals_eu)
n_eu
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

# Ne radi se post-hoc zato sto anova nije pokazala stat znac razl (0.388)

#######################
###### Zadatak 4 ######
#######################

# Matrica indikatora + standardizacija
vars <- c("health","affordability","Energy","Easeofdoingbusiness","Connectivity")
X  <- as.matrix(df[, vars])
Xz <- scale(X)

############################################
# pam (k=3) na celom skupu
############################################
pamcluster <- pam(Xz, k = 3,stand=TRUE)  # podrazumevana metrika: Euclidean

# Broj gradova u klasterima
cl_sizes <- table(pamcluster$clustering)
cl_sizes

# Medoidi sa imenima respektivno
df$City[pamcluster$id.med]

# Silhouette metrika
sil_val <- pamcluster$silinfo$avg.width
sil_val

sil_info <- silhouette(pamcluster)
cat("Prosecna vrednost Silhouette koeficijenta:", mean(sil_info[, "sil_width"]), "\n")
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

# definisanje indikatora 
vars2 <- c("health", "affordability", "Energy", "Easeofdoingbusiness", "Connectivity")

#tourism_med i crime_med
med_tourism <- median(df$Tourism, na.rm = TRUE)
med_crime <- median(df$crime, na.rm = TRUE)

podskup1 <- df[df$Tourism > med_tourism & df$crime < med_crime, ]

# PAM klasterizacija
set.seed(123)
pam_eucl <- pam(podskup1[, vars2], k = 3, metric = "euclidean", stand = TRUE)

# pronalazenje najudaljenijih gradova
dist_matrix <- as.matrix(daisy(podskup1[, vars2], metric = "euclidean", stand = TRUE))

for (i in 1:3) {
  clanovi_klastera <- which(pam_eucl$clustering == i)
  medoid_idx <- pam_eucl$id.med[i]
  
  rastojanja <- dist_matrix[clanovi_klastera, medoid_idx]
  max_dist_idx <- clanovi_klastera[which.max(rastojanja)]
  
  cat("\nKlaster:", i)
  cat("\nMedoid:", podskup1$City[medoid_idx])
  cat("\nNajudaljeniji grad:", podskup1$City[max_dist_idx])
  cat("\nRastojanje:", round(max(rastojanja), 4))
  cat("\nBroj gradova:", length(clanovi_klastera), "\n")
}

###########

# health_med
health_med = median(df$health)
tourism_med = median(df$Tourism)

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

indicators <- df[, c("demographics", "education", "income_inequality", "work_life_balance", 
                     "crime", "health", "affordability", "Environmentalrisks", "Energy", 
                     "Greenspace", "Airpollution", "Greenhousegases", "Wastemanagement", 
                     "Drinkingwaterandsanitation", "Transportinfrastructure", 
                     "Economicdevelopment", "Easeofdoingbusiness", "Tourism", 
                     "Connectivity", "Employment")]
# Testovi pogod
print(cortest.bartlett(cor(indicators, use="complete.obs"), n = nrow(indicators)))
print(KMO(cor(indicators, use="complete.obs")))

# kum var
ev_all <- eigen(cor(indicators, use="complete.obs"))$values
cumulative_variance <- (cumsum(ev_all)/sum(ev_all))

names(cumulative_variance) <- paste0("MR", 1:length(cumulative_variance))
print(round(cumulative_variance, 4))

limit <- 0.70
n_factors <- which(cumulative_variance >= limit)[1]
print(n_factors)

# PCA
set.seed(123)
pca_res <- principal(indicators, nfactors = 4, rotate = "varimax", scores = TRUE)

print(pca_res$loadings, cut = 0)

cat("\nSortirani komunaliteti:\n")
print(sort(pca_res$communality))

cat("\nProsecna komunalnost po faktorima:\n")
print(colSums(pca_res$loadings^2) / nrow(pca_res$loadings))

skorovi_pca <- as.data.frame(pca_res$scores)
skorovi_pca$City <- df$City
for(i in 1:4) {
  f_ime <- paste0("RC", i)
  naj_grad <- skorovi_pca[order(skorovi_pca[[f_ime]], decreasing = TRUE)[1], ]
  cat(f_ime, ":", naj_grad$City, "скор:", round(naj_grad[[f_ime]], 6), "\n")
}

evropa_df <- df[df$Continent == 1, ]
q_tourism <- quantile(evropa_df$Tourism, 0.75, na.rm = TRUE)
cat("\nGornji kvartil za evropu:", q_tourism, "\n")

top_ev_mask <- df$Continent == 1 & df$Tourism >= q_tourism & !is.na(df$Tourism)
top_ev_podaci <- df[top_ev_mask, ]
top_ev_skorovi <- skorovi_pca[top_ev_mask, ]

cat("Korelacije izmedju turizma i faktora")
for(i in 1:4) {
  f_ime <- paste0("RC", i)
  kor <- cor(top_ev_podaci$Tourism, top_ev_skorovi[[f_ime]], use = "complete.obs")
  cat(f_ime, ":", round(kor, 8), "\n")
}

vars_3 <- c("health", "affordability", "Energy", "Easeofdoingbusiness", "Connectivity")
kont_mapa <- list("Europe" = 1, "North America" = 2, "Asia" = 3)

for(k_ime in names(kont_mapa)) {
  k_kod <- kont_mapa[[k_ime]]
  temp <- na.omit(df[df$Continent == k_kod, vars_3])
  
  cat("Bartlett p:", cortest.bartlett(cor(temp), n = nrow(temp))$p.value, "\n")
  cat("KMO MSA:", round(psych::KMO(cor(temp))$MSA, 3), "\n")
  
  nf <- sum(eigen(cor(temp))$values > 1) # Kaiser
  pca_k <- principal(temp, nfactors = nf, rotate = "varimax")
  top_c <- sort(pca_k$communality, decreasing = TRUE)[1]
  cat("Broj faktora:", nf, "\n")
  cat("Najveci komunalitet:", names(top_c), "=", round(top_c, 3), "\n")
}

set.seed(123)
fa_final <- fa(indicators, nfactors = 4, rotate = "varimax", fm = "minres", scores = "regression")
skorovi_fa <- as.data.frame(fa_final$scores)
skorovi_fa$City <- df$City

q_conn <- quantile(df$Connectivity, 0.75, na.rm = TRUE)
q_aff  <- quantile(df$affordability, 0.25, na.rm = TRUE)

f_idx <- which(df$Connectivity >= q_conn & df$affordability <= q_aff)
podskup_finalni <- skorovi_fa[f_idx, ]

cat("\nProsecni skorovi\n")
print(colMeans(podskup_finalni[, 1:4]))

odabrani_f <- "MR4"
top_5_final <- podskup_finalni[order(podskup_finalni[[odabrani_f]], decreasing = TRUE), ]
top_5_final <- head(top_5_final, 5)

rezultat_tab <- data.frame(
  Rang = 1:5,
  Grad = top_5_final$City,
  Kontinent = df$Continent[match(top_5_final$City, df$City)],
  Tourism = df$Tourism[match(top_5_final$City, df$City)],
  Faktor = "MR4",
  Score = round(top_5_final[[odabrani_f]], 3)
)

rezultat_tab$Kontinent <- factor(rezultat_tab$Kontinent, levels=c(1,2,3), 
                                 labels=c("Evropa", "Severna Amerika", "Azija"))


print(rezultat_tab)

#######################
###### Zadatak 6 ######
#######################

lung <- read_excel("plucaSurvival.xlsx")
lung$event <- ifelse(lung$status == 2, 1, 0)
broj_dogadjaja <- sum(lung$event)
print(paste("Ukupan broj hazardnih događaja (smrtnih ishoda):", broj_dogadjaja))

# medijalno vreme za ceo skup
fit_total <- survfit(Surv(time, event) ~ 1, data = lung)
print(fit_total)

# Log-rank test za razliku po polovima
log_rank <- survdiff(Surv(time, event) ~ sex, data = lung)
print(log_rank)

# Coxov model proporcionalnih hazarda
cox_model <- coxph(Surv(time, event) ~ sex, data = lung)
res_cox <- summary(cox_model)

print(res_cox)

hr <- res$conf.int[1]
procenat <- round((1 - hr) * 100, 2)
cat("Zene imaju", procenat, "% manji rizik od muskaraca.\n")

# kreiranje multivarijantnog Cox PH modela
cox_multi <- coxph(Surv(time, event) ~ sex + age + ph.ecog, data = lung)
res_summary <- summary(cox_multi)
print(res_summary)

# tab1

kvantili <- quantile(lung$age, probs = seq(0, 1, 1/3), na.rm = TRUE)
lung$AgeGroup <- cut(lung$age, breaks = kvantili, 
                     labels = c("Low", "Medium", "High"), 
                     include.lowest = TRUE)

# Log-rank test za AgeGroup
test_age <- survdiff(Surv(time, event) ~ AgeGroup, data = lung)
print(test_age) 

# KM analiza po grupama
fit_age <- survfit(Surv(time, event) ~ AgeGroup, data = lung)

# Verovatnoća preživljavanja u t = 365
summary_365 <- summary(fit_age, times = 365)

izvestaj_age <- summary(fit_age)$table
tabela1 <- data.frame(
  AgeGroup = c("Low", "Medium", "High"),
  n = izvestaj_age[, "records"],
  events = izvestaj_age[, "events"],
  median_survival = izvestaj_age[, "median"],
  S_365 = summary(fit_age, times = 365)$surv
)

print(tabela1)

# tab2

m_high <- tabela1$median_survival[tabela1$AgeGroup == "High"]
m_low  <- tabela1$median_survival[tabela1$AgeGroup == "Low"]
m_med  <- tabela1$median_survival[tabela1$AgeGroup == "Medium"]

# low
delta_low <- m_high - m_low
extra_low <- delta_low / (m_low / 30)

# medium
delta_med <- m_high - m_med
extra_med <- delta_med / (m_med / 30)

tabela2 <- data.frame(
  Najzilavija_Grupa = "High",
  Preostala_grupa = c("Low", "Medium"),
  Median_Najzilavija = c(m_high, m_high),
  Median_Preostala = c(m_low, m_med),
  Delta_days = c(delta_low, delta_med),
  Extra_days_per_month = c(extra_low, extra_med)
)

print(round(tabela2[, 3:6], 4))


# 2. deo nastavak
sub_data <- lung[lung$sex == 2 & lung$ph.ecog <= 1, ]
sub_data <- na.omit(sub_data)


kvantili_nivoi <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80)
cutoff_kandidati <- quantile(sub_data$age, probs = kvantili_nivoi)

najbolji_chisq <- 0
najbolji_cutoff <- 0

# petlja za trazenje najboljeg cutoff-a
for (cut in cutoff_kandidati) {
  
  privremena_grupa <- ifelse(sub_data$age < cut, "Mladji", "Stariji")
  n_mladi <- sum(privremena_grupa == "Mladji")
  n_stari <- sum(privremena_grupa == "Stariji")
  e_mladi <- sum(sub_data$event[privremena_grupa == "Mladji"])
  e_stari <- sum(sub_data$event[privremena_grupa == "Stariji"])
  
  if (n_mladi >= 5 & n_stari >= 5 & e_mladi >= 5 & e_stari >= 5) {
    
    test <- survdiff(Surv(time, event) ~ privremena_grupa, data = sub_data)
    trenutni_chi <- test$chisq
    if (trenutni_chi > najbolji_chisq) {
      najbolji_chisq <- trenutni_chi
      najbolji_cutoff <- cut
    }
  }
}

cat("Najbolji cutoff je:", najbolji_cutoff, "\n")
cat("Vrednost Chisq:", najbolji_chisq, "\n")

# rucno postavljamo AgeBin prema najboljem rez
sub_data$AgeBin <- ifelse(sub_data$age < 63.7, "Below < 63.7", "Above > 63.7")

fit_finalni <- survfit(Surv(time, event) ~ AgeBin, data = sub_data)
test_finalni <- survdiff(Surv(time, event) ~ AgeBin, data = sub_data)
statistika <- summary(fit_finalni)$table
s365_rezultat <- summary(fit_finalni, times = 365)$surv

# tab 3
tabela_rezultata <- data.frame(
  AgeBin = rownames(statistika),
  n = statistika[, "records"],
  events = statistika[, "events"],
  median_survival = statistika[, "median"],
  S_365 = s365_rezultat
)
print(tabela_rezultata)
p_vrednost <- 1 - pchisq(test_finalni$chisq, 1)
cat("P-value za log-rank test iznosi:", round(p_vrednost, 4), "\n")


#######################
###### Zadatak 7 ######
#######################



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

Arcadis_log <- df

# kreiranje kategorickih varijabli
Arcadis_log$EmploymentCategory <- cut(Arcadis_log$Employment, 
                                           breaks = c(-Inf, 0.3, 0.6, Inf), 
                                           labels = c("low", "medium", "high"),
                                           include.lowest = TRUE)

Arcadis_log$EducationCategory <- cut(Arcadis_log$education, 
                                          breaks = c(-Inf, 0.4, 0.6, Inf), 
                                          labels = c("low", "medium", "high"),
                                          include.lowest = TRUE)

# kreiranje binarne ciljne varijable
Arcadis_log$HighEmployment <- ifelse(Arcadis_log$EmploymentCategory == "high", 1, 0)

# provera instanci
cat("Broj instanci EmploymentCategory:\n")
print(table(Arcadis_log$EmploymentCategory))

cat("Broj instanci EducationCategory:\n")
print(table(Arcadis_log$EducationCategory))

cat("Broj instanci HighEmployment (0 i 1):\n")
print(table(Arcadis_log$HighEmployment))

# kreiranje modela na celom skupu
model_ceo <- glm(HighEmployment ~ crime + Drinkingwaterandsanitation + Airpollution + 
                   Energy + Greenspace + EducationCategory, 
                 data = Arcadis_log, family = binomial)

summary(model_ceo)

odds_ratios <- exp(coef(model_ceo))
print(round(odds_ratios, 4))

# evaluacija na celom skupu
probs_ceo <- predict(model_ceo, type = "response")
preds_ceo <- ifelse(probs_ceo > 0.5, 1, 0)
cm_ceo <- table(Actual = Arcadis_log$HighEmployment, Predicted = factor(preds_ceo, levels = c(0, 1)))

acc_ceo <- sum(diag(cm_ceo)) / sum(cm_ceo)
prec_ceo <- cm_ceo[2,2] / sum(cm_ceo[,2])
rec_ceo <- cm_ceo[2,2] / sum(cm_ceo[2,])
f1_ceo <- 2 * (prec_ceo * rec_ceo) / (prec_ceo + rec_ceo)

cat(paste("Accuracy:", round(acc_ceo, 3), "Precision:", round(prec_ceo, 3), 
          "Recall:", round(rec_ceo, 3), "F1:", round(f1_ceo, 3), "\n"))

# eval za 10 seedova
results_table <- data.frame(Seed=integer(), Accuracy=numeric(), Precision=numeric(), Recall=numeric(), F1=numeric())

for (s in 123:132) {
  set.seed(s)
  
  train_idx <- sample(1:nrow(Arcadis_log), round(0.7 * nrow(Arcadis_log)))
  train_set <- Arcadis_log[train_idx, ]
  test_set <- Arcadis_log[-train_idx, ]
  
  model_loop <- glm(HighEmployment ~ crime + Drinkingwaterandsanitation + Airpollution + 
                      Energy + Greenspace + EducationCategory, 
                    data = train_set, family = binomial)
  
  probs <- predict(model_loop, newdata = test_set, type = "response")
  preds <- ifelse(probs > 0.5, 1, 0)
  
  conf_matrix <- table(Actual = test_set$HighEmployment, Predicted = factor(preds, levels = c(0, 1)))
  
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[1, 2]
  FN <- conf_matrix[2, 1]
  
  acc <- (TP + TN) / sum(conf_matrix)
  prec <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  rec <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
  f1 <- ifelse((prec + rec) > 0, 2 * (prec * rec) / (prec + rec), 0)
  
  results_table <- rbind(results_table, data.frame(Seed=s, Accuracy=acc, Precision=prec, Recall=rec, F1=f1))
}

print(round(results_table, 3))

# f1 je metrika za ujednaceno sagledavanje vrlina , iz tabele se vidi da je to seed 126