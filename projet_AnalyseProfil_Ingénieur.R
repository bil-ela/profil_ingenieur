---
  title: "Engineers' Profiles in France"
output: 
  flexdashboard::flex_dashboard:
  orientation: row
vertical_layout: scroll
scroll: true
source_code: embed
theme: sandstone
runtime: shiny
---
  
  ```{r setup, include=FALSE}
library(flexdashboard)
library(rsconnect)
library(dplyr)
library(fastDummies)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(gridExtra)
library(ggcorrplot)
library(purrr)
library(ggplot2)
library(cluster)
library(rpart)
library(party)

```


# Menu {data-icon="fa-home"}

Work made by Raphaël Sève and Bilal El Abdellaoui.

Link towards other dashboards: <https://nicolassutton-charani.shinyapps.io/visu_project_manager/>{style="color:#99CC33;"}

## Row {data-height="120"}

### ***About the project*** {data-width="510"}

As part of a data collection project, we were able to build a dataset containing various information on the profiles of French engineers. From this study and the constituted dataset, we will try to distribute these various engineers in clusters in order to know the various profiles of engineers and their particularities.

Our data collection started on **17/02/2023** and lasted for **1 month**. We have received **68 answers** from engineers. 

### {data-width="200"}

![](image.jpg){width = 290, heigth = 200}

## Row {data-height="200"}

### ***Insight***

Here it is an overview of some numbers of our study

* We obtained results from **68 engineers** currently employed in France.
* **42,6%** of surveyed people are **women**, while **55,9%** are **men** (one individual didn't answer).
* The most represented age range of the engineers surveyed is **20-25 years old**.
* **51,4%** of surveyed people works in large companies (GE = +5000 salariés).
* **39,7%** of engineers who answered our poll are single while **25%** are married.
* Only **23,5%** of surveyed engineers have at least one parent who is/was an engineer.
* The 3 most represented fields in our survey are: **1** - Public sector, **2** - Pharma/Health/Chemistry, **3** - IT


# Data pre-processing {data-icon="fa-cogs"}

## Row {data-height="80"}

### [**Import**]{style="color:#99CC33;font-weight:bold;"}

First we imported the csv file containing all our data into an R dataframe. We renamed all the columns for readability and usability, and we finally deleted 2 columns that were useless: the timestamp and the emails of the people surveyed.

```{r include=FALSE}

# on importe les données
df <- read.csv('results.csv', sep=',')

# on renomme les colonnes par soucis de simplicité
anciennes_colonnes <- c("Horodateur",
              "Quel.est.votre.sexe...",
              "Quel.âge.avez.vous..",
              "Quel.est.votre.statut.social...",
              "Avez.vous.des.enfants...",
              "Dans.quel.type.de.ville.travaillez.vous..",
              "Quel.est.le.plus.haut.niveau.d.études.de.votre.mère..",
              "Quel.est.le.plus.haut.niveau.d.études.de.votre.père..",
              "Est.ce.que.l.un.au.moins.de.vos.parents.est.ingénieur..",
              "Quel.type.d.établissement.avez.vous.fréquenté.au.collège.",
              "Quel.type.d.établissement.avez.vous.fréquenté.au.lycée.",
              "Quelle.spécialité.avez.vous.suivie.au.lycée..",
              "Quel.type.d.établissement.avez.vous.fréquenté.en.enseignement.supérieur..",
              "Avez.vous.fait.une.réorientation.d.études.en.cours.de.parcours..",
              "Pensez.vous.que.vos.études.vous.servent.au.quotidien....Sur.une.échelle.de.1.à.10.",
              "Quelles.ont.été.les.activités.que.vous.avez.le.plus..pratiqué.durant.votre.enfance.adolescence.",
              "Dans.quel.secteur.travaillez.vous..",
              "Pourriez.vous.estimer.votre.quantité.de.travail.effective.hebdomadaire..",
              "Dans.quelle.type.de.structure.travaillez.vous..",
              "A.quel.niveau.hiérarchique.vous.situez.vous.au.sein.de.votre.entreprise..",
              "Depuis.combien.de.temps.travaillez.vous.dans.votre.entreprise..",
              "Au.sein.de.votre.entreprise..occupez.vous.toujours.le.poste.pour.lequel.vous.avez.été.recruté..",
              "Depuis.le.début.de.votre.carrière..avez.vous.déjà.réalisé.une.reconversion..au.sein.du.même.secteur.ou.dans.un.nouveau.secteur...",
              "Dans.quelle.tranche.de.salaire.vous.situez.vous....salaire.brut.annuel.",
              "Votre.entreprise.vous.pousse.t.elle.à.vous.former.de.manière.continue..",
              "Vous.sentez.vous.épanoui.dans.votre.travail..",
              "Quels.critères.sont.les.plus.influents.pour.vous.au.moment.de.choisir.un.poste....Choisissez.les.3.les.plus.importants.selon.vous.",
              "Concernant.vos.relations.au.travail..laquelle.de.ces.propositions.semble.vous.correspondre.le.mieux..",
              "Comment.jugez.vous.la.mixité.au.sein.de.votre.entreprise..",
              "D.après.vous..les.minorités.visibles.sont.elles.sous.représentées.dans.votre.entreprise..",
              "Selon.vous..lors.d.une.recherche.d.emploi..les.minorités.visibles.ont.elles.plus.de.difficultés.à.accéder.à.un.poste..",
              "Pensez.vous.que.les.compétences.techniques.acquises.lors.de.votre.formation.sont.utiles.dans.votre.travail.actuel..",
              "Quelles.sont.les.compétences.transverses.que.vous.pensez.utiliser.le.plus.au.travail....choisir.les.3.plus.importantes.",
              "Combien.de.langues.maitrisez.vous....connaissances.orales.et.écrites.professionnelles.",
              "Quelles.langues.parlez.vous...",
              "Le.métier.que.vous.pratiquez.actuellement.correspond.t.il.à.l.idée.et.aux.attentes.que.vous.vous.en.étiez.fait...",
              "Avez.vous.toujours.voulu.être.ingénieur..",
              "Pourquoi.avez.vous.choisi.de.devenir.ingénieur..",
              "Pensez.vous.que.vous.allez.changer.de.métier.dans.les.prochaines.années..",
              "Êtes.vous.sensibilisé.à.la.question.de.l.environnement.dans.votre.métier..",
              "Vous.interrogez.vous.sur.la.question.écologique.lors.de.vos.projets..",
              "Veuillez.laisser.votre.mail.si.vous.êtes.intéressé.par.un.retour.de.l.étude")
nouvelles_colonnes <- c("timestamps",
                        "gender",
                        "age",
                        "social_status",
                        "children",
                        "type_of_city",
                        "mother_study_lvl",
                        "father_study_lvl",
                        "engineer_parents",
                        "high_school",
                        "college",
                        "degree",
                        "postbac_study",
                        "study_reorientation",
                        "studies_utility",
                        "activities",
                        "work_field",
                        "work_time",
                        "company_size",
                        "hierarchic_position",
                        "time_in_company",
                        "same_position",
                        "professional_reconversion",
                        "salary",
                        "continuous_formation",
                        "happiness_at_work",
                        "job_criteria",
                        "work_relationships",
                        "gender_equity",
                        "under_represented_visible_minorities",
                        "visible_minorities_find_job",
                        "technical_studies_utility",
                        "transversal_skills",
                        "languages_nb",
                        "languages",
                        "job_fits_with_expectations",
                        "always_wanted",
                        "why_engineer",
                        "change_next_years",
                        "environment_sensibility",
                        "environment_project",
                        "email")


for (i in seq_along(anciennes_colonnes)) {
  names(df)[names(df) == anciennes_colonnes[i]] <- nouvelles_colonnes[i]
}

# on supprime les colonnes 'timestamps' et 'email' inutiles pour l'étude
df <- subset(df, select = -c(timestamps, email))


```


## Row {data-height="80"}

### [**Columns creation**]{style="color:#99CC33;font-weight:bold;"}

Then, as some questions were multiple choice, we sometimes had in a column several answers separated by semicolons. For example, for the question 'why did you want to become an engineer', the answer could be in the form: salary;recognition;stimulation. We then create 3 new columns: salary, recognition and stimulation, by assigning the value 1 if the person had indicated this answer, 0 otherwise.

```{r include=FALSE}

# on sépare les colonnes des questions à réponses multiples en créant de nouvelles variables binaires. On supprime ensuite les colonnes à réponses multiples
df <- df %>%
  mutate(classe_preparatoire = ifelse(grepl("Classe Préparatoire", postbac_study), 1, 0),
         IUT = ifelse(grepl("IUT", postbac_study), 1, 0),
         ecole_ing_post_prepa = ifelse(grepl("Ecole d'ingénieur post prépa", postbac_study), 1, 0),
         universite = ifelse(grepl("Université", postbac_study), 1, 0),
         ecole_ing_post_bac = ifelse(grepl("Ecole d'ingénieur post prépa", postbac_study), 1, 0),
         BTS = ifelse(grepl("BTS", postbac_study), 1, 0),
         autre_etudes = ifelse(grepl("Autre", postbac_study), 1, 0)
  )

df <- subset(df, select = -c(postbac_study))




df <- df %>%
  mutate(sports_activities = ifelse(grepl("Activités sportives", activities), 1, 0),
         musical_activities = ifelse(grepl("Activités musicales", activities), 1, 0),
         intellectual_activities = ifelse(grepl("Activités intellectuelles", activities), 1, 0),
         artistical_activities = ifelse(grepl("Activités artistiques", activities), 1, 0),
         other_activities = ifelse(grepl("Autre", activities), 1, 0)
  )

df <- subset(df, select = -c(activities))



df <- df %>%
  mutate(salary_criteria = ifelse(grepl("Salaire", job_criteria), 1, 0),
         teletravail_criteria = ifelse(grepl("Possibilité de télétravail", job_criteria), 1, 0),
         balance_criteria = ifelse(grepl("Equilibre vie professionnelle/personnelle", job_criteria), 1, 0),
         team_criteria = ifelse(grepl("Ambiance d'équipe", job_criteria), 1, 0),
         values_criteria = ifelse(grepl("Valeurs de l'entreprises", job_criteria), 1, 0),
         benefits_criteria = ifelse(grepl("Avantages", job_criteria), 1, 0),
         environment_criteria = ifelse(grepl("Cadre de travail", job_criteria), 1, 0),
         distance_criteria = ifelse(grepl("Eloignement du domicile", job_criteria), 1, 0)
  )

df <- subset(df, select = -c(job_criteria))




df <- df %>%
  mutate(creativity_skill = ifelse(grepl("Créativité", transversal_skills), 1, 0),
         organization_skill = ifelse(grepl("Organisation", transversal_skills), 1, 0),
         rigor_skill = ifelse(grepl("Rigueur", transversal_skills), 1, 0),
         communication_skill = ifelse(grepl("Communication", transversal_skills), 1, 0),
         team_spirit_skill = ifelse(grepl("Esprit d'équipe", transversal_skills), 1, 0),
         sociability_skill = ifelse(grepl("Sociabilité", transversal_skills), 1, 0),
         leadership_skill = ifelse(grepl("Leadership", transversal_skills), 1, 0),
         adaptability_skill = ifelse(grepl("Adaptabilité", transversal_skills), 1, 0)
  )

df <- subset(df, select = -c(transversal_skills))





df <- df %>%
  mutate(french_language = ifelse(grepl("Français", languages), 1, 0),
         english_language = ifelse(grepl("Anglais", languages), 1, 0),
         spanish_language = ifelse(grepl("Espagnol", languages), 1, 0),
         portugesh_language = ifelse(grepl("Portugais", languages), 1, 0),
         arabic_language = ifelse(grepl("Arabe", languages), 1, 0),
         italian_language = ifelse(grepl("Italien", languages), 1, 0),
         other_language = ifelse(grepl("Autre", languages), 1, 0)
  )


df <- subset(df, select = -c(languages))




df <- df %>%
  mutate(renommee_reason = ifelse(grepl("renommée", why_engineer), 1, 0),
         social_schema_reason = ifelse(grepl("reproduction", why_engineer), 1, 0),
         intellectual_stimulation_reason = ifelse(grepl("stimulation intellectuelle", why_engineer), 1, 0),
         salary_reason = ifelse(grepl("salaire", why_engineer), 1, 0),
         diversity_reason = ifelse(grepl("diversité", why_engineer), 1, 0),
         security_reason = ifelse(grepl("sécurité", why_engineer), 1, 0)
  )

df <- subset(df, select = -c(why_engineer))

```


## Row {data-height="80"}

### [**One-Hot Encoded Data**]{style="color:#99CC33;font-weight:bold;"}

Then, for the non-hierarchical categorical variables, it was again necessary to create new binary variables using the One-Hot Encoded data method. For example, to the question what is your sector of activity, some people answered computer science, others chemistry, others civil engineering, and so we created 3 new columns computer science, chemistry and civil engineering with the value 1 if the person works in this sector 0 otherwise.

```{r include=FALSE}

# on divise toutes nos variables catégorielles non hiérarchisée avec le package fastdummies
df <- dummy_cols(df, select_columns = "social_status")
df <- dummy_cols(df, select_columns = "degree")
df <- dummy_cols(df, select_columns = "work_field")

df <- df %>% select(-social_status)
df <- df %>% select(-degree)
df <- df %>% select(-work_field_)
df <- df %>% select(-work_field)

```


## Row {data-height="80"}

### [**Binary and Ternary variables**]{style="color:#99CC33;font-weight:bold;"}

We then encoded all the binary (True/False, Yes/No, Private/Public...) and ternary (Yes/No/Don't know) variables with 
0's, 1's (and 0.5's).

```{r include=FALSE}

# binary variables
df$engineer_parents <- ifelse(df$engineer_parents == "Oui", 1, 0)

df$same_position <- ifelse(df$same_position == "Oui", 1, 0)

df$professional_reconversion <- ifelse(df$professional_reconversion == "Oui", 1, 0)

df$happiness_at_work <- ifelse(df$happiness_at_work == "Non", 0,
                               ifelse(df$happiness_at_work == "Je ne sais pas", 0.5, 1))

df$under_represented_visible_minorities <- ifelse(df$under_represented_visible_minorities == "Oui", 1, 0)

df$gender <- ifelse(df$gender == "Masculin", 1, 0)

df$children <- ifelse(df$children == "Oui", 1, 0)

df$high_school <- ifelse(df$high_school == "Privé", 1, 0)

df$college <- ifelse(df$college == "Privé", 1, 0)

df$visible_minorities_find_job <- ifelse(df$visible_minorities_find_job == "Oui", 1, 0)

df$technical_studies_utility <- ifelse(df$technical_studies_utility == "Oui", 1, 0)

df$job_fits_with_expectations <- ifelse(df$job_fits_with_expectations == "Oui", 1,
                                        ifelse(df$job_fits_with_expectations == "Pas d'avis", 0.5, 0))

df$always_wanted <- ifelse(df$always_wanted == "Oui", 1, 0)

df$change_next_years <- ifelse(df$change_next_years == "Oui", 1,
                               ifelse(df$change_next_years == "Je ne sais pas", 0.5, 0))

df$study_reorientation <- ifelse(df$study_reorientation == "Oui", 1, 0)

df$environment_sensibility <- ifelse(df$environment_sensibility == "Oui", 1, 0)

df$environment_project <- ifelse(df$environment_project == "Oui", 1, 0)

```


## Row {data-height="80"}

### [**Hierarchical categorial variables**]{style="color:#99CC33;font-weight:bold;"}

As for the hierarchical categorical variables (small city, medium city, large city), we have encoded them with graduated scales (1, 2, 3 for example) 

```{r include=FALSE}

# on transforme toutes nos variables catégorielles hiérarchisée  en variables numériques
df$age <- ifelse(df$age == "20 - 25 ans", 0,
                 ifelse(df$age == "25 - 30 ans", 1,
                        ifelse(df$age == "30 - 40 ans", 2,
                               ifelse(df$age == "40 - 50 ans", 3,
                                      ifelse(df$age == "50 - 60 ans", 4, 5)))))

df$type_of_city <- ifelse(df$type_of_city == "Petite ville (moins de 50 000 habitants)", 0,
                          ifelse(df$type_of_city == "Ville moyenne (moins de 200 000 habitants)", 1, 3))

df$mother_study_lvl <- ifelse(df$mother_study_lvl == "Brevet", 0,
                              ifelse(df$mother_study_lvl == "Bac", 1,
                                     ifelse(df$mother_study_lvl == "Bac+3", 2,
                                            ifelse(df$mother_study_lvl == "Bac+5", 3, 4))))


df$father_study_lvl <- ifelse(df$father_study_lvl == "Brevet", 0,
                              ifelse(df$father_study_lvl == "Bac", 1,
                                     ifelse(df$father_study_lvl == "Bac+3", 2,
                                            ifelse(df$father_study_lvl == "	
Bac+5", 3, 4))))



df$work_time <- ifelse(df$work_time == "0-40 heures", 0,
                       ifelse(df$work_time == "40-60 heures", 1, 2))

df$company_size <- ifelse(df$company_size == "TPE ( 0 à 19 salariés)", 0,
                          ifelse(df$company_size == "ETI (250 à 5000 salariés)", 1,
                                 ifelse(df$company_size == "GE (+5000 salariés)", 2,
                                        ifelse(df$company_size == "Milieu scolaire et universitaire", 3,
                                               ifelse(df$company_size == "ONG", 4,
                                                      ifelse(df$company_size == "PME (20 à 249 salariés)", 5, 6))))))

df$hierarchic_position <- ifelse(df$hierarchic_position == "Collaborateur", 0,
                                 ifelse(df$hierarchic_position == "Cadre ou assimilé", 1, 2))

df$time_in_company <- ifelse(df$time_in_company == "Moins de 1 an", 0,
                             ifelse(df$time_in_company == "Entre 1 et 3 ans", 1, 2))

df$salary <- ifelse(df$salary == "Moins de 40 000 €", 0,
                    ifelse(df$salary == "De 40 000 € à 50 000 €", 1, 2))

df$continuous_formation <- ifelse(df$continuous_formation == "Non", 0,
                                  ifelse(df$continuous_formation == "Oui, mais moins d'une fois par an", 1,
                                         ifelse(df$continuous_formation == "Oui, environ une fois par an", 2, 3)))


df$work_relationships <- ifelse(df$work_relationships == "J'échange principalement sur des sujets liés à mon activité professionnelle, en évitant les sujets personnels ou sensibles (religions, politiques...)", 0,
                                ifelse(df$work_relationships == "J'aborde tout type de sujets sans complexe seulement avec mes collaborateurs directs", 1, 2))

df$gender_equity <- ifelse(df$gender_equity == "Les femmes sont sous-représentées", 0,
                           ifelse(df$gender_equity == "Les femmes sont convenablement représentées", 1,
                                  ifelse(df$gender_equity == "Les femmes et les hommes sont équitablements représentés", 2, 3)))

```

## Row {data-height="80"}

### [**Imputation of missing values**]{style="color:#99CC33;font-weight:bold;"}

We imputed the missing values (there were usually at most 2 or 3 per column) by the average of the column values (this does not pose any particular problem in the case of a clustering problem).

```{r include=FALSE}

# imputation des valeurs manquantes (dans le cas du clustering, vu que l'on est pas intéressé par l'interprétation des valeurs des variables, on peut imputer en prenant la moyenne des autres valeurs)

for (j in 1:ncol(df)) {
  col <- df[,j]
  col_mean <- mean(col, na.rm=TRUE)
  col[is.na(col)] <- col_mean
  df[,i] <- col
}

```


## Row {data-height="80"}

### [**Final dataset**]{style="color:#99CC33;font-weight:bold;"}

We finally obtained a dataset that contains only numerical values, which was the goal, and we can therefore consider a clustering problem.



# Correlation matrix {data-navmenu="Data Analysis"}

## Row {data-height="100"}

### [**Choice**]{style="color:#99CC33;font-weight:bold;"}

As we have a very important number of variables, in order to simplify the analysis and the visualization, we are going to restrict ourselves to a certain number of variables, which we chose arbitrarily according to those which interested us more particularly. Here is the list of variables that we keep: gender, age, type_of_city, engineer_parents, company_size, time_in_company, salary, languages_nb, intellectual_activities, sport_activities. 

```{r include=FALSE}

df2 <- df[, c("gender", "age", "type_of_city", "engineer_parents", "company_size", "work_time",                        "time_in_company", "salary", "languages_nb", "intellectual_activities", 
              "sports_activities")]


```

## Row {data-height="400"}

### [**Heat Map**]{style="color:#99CC33;font-weight:bold;"}

```{r}

cor_matrix <- cor(df2)
corrplot(cor_matrix, method="color", type="upper", 
         order="hclust", tl.col="black", tl.srt=45)


```

## Row {data-height="100"}

### [**Results**]{style="color:#99CC33;font-weight:bold;"}

This graph shows us which variables are correlated and also indicates the sign of the correlation. For example, we can see that the individual's age and length of service in the company are positively correlated factors with salary, which seems logical. The number of hours worked per week also appears to have a positive impact on salary, which is normal as generally, the more hours we work, the higher our salary is. We also seem to see that individuals who engaged in sports activities during their youth were less likely to engage in intellectual activities and vice versa.


# PCA {data-navmenu="Data Analysis"}

## Row {data-height="130"}

### {data-height="130"}

We will then visualize our results in the form of interactive graphs in order to observe:

- The eigenvalues of the principal components and their contribution to the total variance of the data
- The arrow plot representing the contribution of each variables
- A graph representing all our individuals on a circle and positioned close to other individuals who have characteristics close to theirs
- The complete PCA results


## Row {data-height="400"}

### [**Eigen values**]{style="color:#99CC33;font-weight:bold;"}

```{r}

# Normalisation du dataset avec la fonction scale
df2_norm <- scale(df2)
pca <- prcomp(df2_norm, scale. = TRUE)
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))


```

## Row {data-height="270"}

### [**Eigen values list**]{style="color:#99CC33;font-weight:bold;"}

```{r}
summary(pca)$importance
```

### {data-width="300"}

Here, we can see that the first four dimensions explain almost 60% of the total variance in the data. The first dimension explains the largest proportion of variance (23.57%) and is therefore considered the main dimension. The second dimension also explains a significant proportion of variance (13.52%) and is considered the second main dimension, and so on for the following dimensions, which explain decreasing proportions of variance.

The number of axes to consider in principal component analysis depends on the amount of variance you want to explain in your data. 


## Row {data-height="400"}

### [**Variables**]{style="color:#99CC33;font-weight:bold;"}

```{r}

fviz_pca_var(pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
             )

```

## Row {data-height="90"}

The variables that have a significant length on the correlation circle are the most important variables in explaining differences between observations. For instance, on the plot, e can see that salary, time_in_company, age, language_nb, company_size have a significant impact..
Variables that have vectors that are close to each other on the correlation circle are positively correlated, while variables that have vectors that are opposite to each other on the circle are negatively correlated. 
Variables that are close to an axis are strongly correlated with that axis, while those that are far away have a weak correlation.


## Row {data-height="400"}

### [**Individuals**]{style="color:#99CC33;font-weight:bold;"}

```{r}

fviz_pca_ind(pca,
             col.ind = "cos2", # Colorer par le cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
             )

```

## Row {data-height="2360"}

### [**PCA results**]{style="color:#99CC33;font-weight:bold;"}

```{r}

# Valeurs propres
eig.val <- get_eigenvalue(pca)
eig.val
  
# Résultats des variables
res.var <- get_pca_var(pca)
res.var$coord          # Coordonnées
res.var$contrib        # Contributions aux axes
res.var$cos2           # Qualité de représentation 
# Résultats des individus
res.ind <- get_pca_ind(pca)
res.ind$coord          # Coordonnées
res.ind$contrib        # Contributions aux axes
res.ind$cos2           # Qualité de représentation 

```




# Number of clusters {data-navmenu="Clustering"}

## Row {data-height="180"}

### [**K-means**]{style="color:#99CC33;font-weight:bold;"}

The basic idea behind k-means clustering consists of defining clusters so that the total intra-cluster variation (known as total within-cluster variation) is minimized. 

To use k-means clustering, the first step is to specify the desired number of clusters, denoted as k, that will be generated in the final result. Then, the algorithm randomly selects k objects from the dataset to serve as initial centers or centroids for the clusters. Each remaining object is then assigned to its closest centroid, where distance is measured using Euclidean distance. This is known as the "cluster assignment step". After this step, the algorithm computes the new mean value of each cluster, known as the "centroid update" step. The updated centroids are then used to reassign all objects to their closest cluster. These two steps are iteratively repeated until the cluster assignments stop changing, which indicates convergence, and the same clusters are obtained in consecutive iterations.

## Row {data-height="130"}

### [**Number of clusters**]{style="color:#99CC33;font-weight:bold;"}

There is three popular methods for determining the optimal clusters, which includes:

* Elbow method
* Silhouette method
* Gap statistic


## Row {data-height="320"}

### [**Elbow method**]{style="color:#99CC33;font-weight:bold;"}

There is three popular methods for determining the optimal clusters, which includes:

- Elbow method
- Silhouette method
- Gap statistic

Here, we will continue with the Elbow method.

To determine the optimal number of clusters with the Elbow method we follow this algorithm:

* Apply a clustering algorithm (such as k-means) to the dataset for different values of k, ranging from 1 to 10 clusters.
* For each value of k, calculate the total within-cluster sum of squares (WSS).
* Plot the WSS values against the number of clusters k.
* Look for a bend or "knee" in the plot, which indicates a significant reduction in the rate of decrease of WSS. This knee is typically considered as a good indicator of the appropriate number of clusters for the dataset.

Fortunately, this process to compute the “Elbow method” has been wrapped up in a single function fviz_nbclust :

## Row {data-height="400"}

### [**Number of clusters**]{style="color:#99CC33;font-weight:bold;"}

```{r}
fviz_nbclust(df2, kmeans, method = "wss")
```

## Row {data-height="30"}

The results suggest that 4 is the optimal number of clusters as it appears to be the bend in the knee.



# K-Means {data-navmenu="Clustering"}

## Row {data-height="40"}

So 4 is the number of optimal clusters according to elbow method, thus, we can perform the final analysis and extract the results using 4 clusters.

## Row {data-height="400"}

```{r include=FALSE}
set.seed(123)
final <- kmeans(df2, 4, nstart = 25)
print(final)
```


### [**K-means clustering results**]{style="color:#99CC33;font-weight:bold;"}

```{r}
fviz_cluster(final, data = df2)
```

## Row {data-height="370"}

### [**Statistics**]{style="color:#99CC33;font-weight:bold;"}

We can now extract the clusters and add them to our initial data to do some descriptive statistics at the cluster level:

```{r}
df2 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
```

As a reminder, these values are not directly interpretable because they were transformed into numerical variables during the pre-processing phase. For example, for salaries, there were three categories: 1 corresponded to less than €40,000 per year, 2 between €40,000 and €60,000, and 3 more than €60,000. Therefore, to interpret these results, one must refer to the transformations made during pre-processing.


# Performance evaluation {data-navmenu="Clustering"}

## Row {data-height="300"}

### [**Methods**]{style="color:#99CC33;font-weight:bold;"}

There are several methods to evaluate a k-means clustering algorithm. Here we will use on of them called the Silhouette analysis that can be used to determine the degree of separation between clusters. For each sample:

- Compute the average distance from all data points in the same cluster (ai).
- Compute the average distance from all data points in the closest cluster (bi).
- Compute the coefficient: bi - ai /max(ai,bi)


The coefficient can take values in the interval [-1, 1].

- If it is 0 –> the sample is very close to the neighboring clusters.
- It it is 1 –> the sample is far away from the neighboring clusters.
- It it is -1 –> the sample is assigned to the wrong clusters.


Therefore, we want the coefficients to be as big as possible and close to 1 to have a good clusters.


## Row {data-height="200"}

### [**Clusters size and Silhouette coefficients**]{style="color:#99CC33;font-weight:bold;"}

```{r include=FALSE}

sil <- silhouette(final$cluster, dist(df2))

```


```{r}
res <- fviz_silhouette(sil, plot = FALSE)
```

## Row {data-height="600"}

### [**Silhouette analysis**]{style="color:#99CC33;font-weight:bold;"}

```{r include=FALSE}
res <- fviz_silhouette(sil)
```


```{r}
plot(res)
```

## Row {data-height="100"}

### [**Silhouette analysis**]{style="color:#99CC33;font-weight:bold;"}

First we can see the size of each cluster, then we also have the silhouette coefficients for each clusters. We can see that none of our coefficients is close to 1, which suggets that our clustering is not very efficient. Nevertheless, we also see that cluster 2 has the highest average silhouette width value (0.40), which suggests that the observations in this cluster are more similar to each other than to the observations in the other clusters. The other clusters have similar average silhouette width values (0.21-0.27), which may indicate that the classification of data in these clusters is less clear.


# Classification {data-navmenu="Clustering"}

## Row {data-height="80"}

After adding a column to our dataframe in which the cluster to which each individual belongs appears, we will try to train a decision tree model allowing to classify an individual in a cluster.


## Row {data-height="400"}

```{r include=FALSE}
clusters <- final$cluster
print(clusters)

# on ajoute une colonne cluster au dataframe qui indique à quel cluster appartient un individu
df2 <- cbind(df2, cluster = clusters)

# on isole une valeur pour pouvoir tester notre arbre sur cette dernière (idealement faudrait isoler chaque valeur 1 par 1 -- > LOOCV)
donnee_de_test <- tail(df2, n=1)
cluster_de_test <- donnee_de_test$cluster
donnee_de_test <- subset(donnee_de_test, select = -cluster)
train <- df2[-nrow(df2), ]

```

```{r include=FALSE}

tree <- ctree(cluster ~ gender + age + type_of_city + engineer_parents + company_size + work_time                                         + time_in_company + salary + languages_nb + intellectual_activities                                        + sports_activities, data = train)
```

### [**Decision tree**]{style="color:#99CC33;font-weight:bold;"}

```{r}

plot(tree)

```


## Row {data-height="400"}

### [**Tree analysis**]{style="color:#99CC33;font-weight:bold;"}

(As before, the variables are still numerical and hierarchical here so when we talk about age for instance, 0 means 20-25 years old, 1 means 25-30 years old, 2 means 30-40 years old... so to understand this tree we have to refer to the transformation we made to our variables during the pre-processing work)

This decision tree is a visual representation of a predictive model that allows for classifying individuals based on several explanatory variables. In this case, the model was trained on 69 observations and takes into account the following variables: gender, age, type_of_city, engineer_parents, company_size, work_time, time_in_company, salary, languages_nb, intellectual_activities, and sports_activities.

The tree is composed of nodes, which correspond to tests on the different explanatory variables, and leaves, which correspond to the predicted classes. Tests on the explanatory variables allow for dividing the observations into subgroups based on the response to these tests. Predicted classes are determined based on the majority of observations that belong to each subgroup.

In this decision tree, the first test is performed on the "age" variable, with a threshold value of 1. If age is less than or equal to 1, the model then relies on the "company_size" variable. If the size of the company is less than or equal to 2, then the model predicts that the individual belongs to the "cluster" represented by leaf 3. If the size of the company is greater than 2, the model predicts that the individual belongs to the "cluster" represented by leaf 4.

If age is greater than 1, the model relies on the "company_size" variable. If the size of the company is less than or equal to 3, the model predicts that the individual belongs to the "cluster" represented by leaf 6. If the size of the company is greater than 3, the model predicts that the individual belongs to the "cluster" represented by leaf 7.

The number of observations corresponding to each leaf is indicated in parentheses, in the form "weights = X". This number represents the number of individuals who were classified in this leaf by the model.


## Row {data-height="140"}

### [**Prediction**]{style="color:#99CC33;font-weight:bold;"}

We now would like to try predict to which cluster an unknown individual will be assigned. We will use the test data. We predict that the cluster this individual belong to is the cluster 3. Actually, when we look at his cluster, it is indeed 3. 

```{r}

prediction <- predict(tree, newdata = donnee_de_test)

print(paste("Predicted cluster is : ", as.character(prediction),
            " and Actual cluster is : ", as.character(cluster_de_test)))


```


## Row {data-height="150"}

### [**Conclusion**]{style="color:#99CC33;font-weight:bold;"}

We have only tested our classifer on one individual and that is clearly not enough. But according to the fact that we hadn't a lot of data, if we splitted the dataset with the classical 80%/20%, our tree was very different and so we lost a lot of information. Our approach is clearly criticable on a scientific point of view but the goal of this work was to introduce ourselves to some new issues such as clustering problematic. It was the first time we dealt with clustering and so the are probably a lot of thing we didn't do correctly, such as the pre-processing step or the selection of k in the k-means procedures... But given that, it was very interesting to deal with a new model (k-means) on a dataset we have produced. It would have surely been easier to take another dataset, with already numerical variables so we don't have to transform all our categorical variables into numerical ones, but the process to work with the data we collected was interesting. Thank you for taking the time to read our dashboard.