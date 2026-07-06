library(dplyr)
library(stringr)
library(tidyr)

data2025 <- read.csv("Data//btw25_wbz//btw25_wbz_ergebnisse.csv", header = TRUE, sep = ";", skip = 4)
View(data2025)
data2021 <- read.csv("Data//btw21_wbz//btw21_wbz_ergebnisse.csv", header = TRUE, sep = ";")
View(data2021)


# Amtlichen Gemeindeschlüssel (als Key für spätere Joins) generieren:
# _ _ (Bundesland) _ (Regierungsbezirk bzw 0) _ _ (Landkreis bzw kreisfreie Stadt) _ _ _ (Gemeinde bzw. 000)
data2025 <- data2025 %>% 
  mutate(
    Gemeindeschlüssel = paste0(
      str_pad(Land, width = 2, pad = "0"),
      str_pad(Regierungsbezirk, width = 1, pad = "0"),
      str_pad(Kreis, width = 2, pad = "0"),
      str_pad(Gemeinde, width = 3, pad = "0")
    )
  )
data2021 <- data2021 %>% 
  mutate(
    Gemeindeschlüssel = paste0(
      str_pad(Land, width = 2, pad = "0"),
      str_pad(Regierungsbezirk, width = 1, pad = "0"),
      str_pad(Kreis, width = 2, pad = "0"),
      str_pad(Gemeinde, width = 3, pad = "0")
    )
  )


## gemeinsame Briefwahlbezirke unterschiedlicher Gemeinden untersuchen
## neuer Ansatz: alle künstlichen Gemeinden (mit 9 vorne) sind keine echten Gemeinden sondern zusammengelegte Briefwahlbezirke über Gemeindegrenzen hinaus
bw9 <- data2025 %>%
  filter(Bezirksart == 5,
         Gemeinde >= 900)

# Wie viele gemeinsame Briefwahlbezirke gibt es?
nrow(
  bw9 %>%
    distinct(Gemeindeschlüssel)
)

# Tabelle aller gemeinsamen Briefwahlbezirke
bw9 %>%
  distinct(
    Gemeindeschlüssel,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>% 
  View()

# Künstliche Gemeinden über Wahlkreis hinaus (1 Ausnahme:) (unterschiedlicher Gemeindename)
# erledigt: in bw_groups korrekt getrennt enthalten
data2025 %>%
  filter(Gemeinde >= 900) %>%
  group_by(
    Gemeindeschlüssel,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>%
  summarise(
    n_wahlkreise = n_distinct(Wahlkreis),
    .groups = "drop"
  ) %>%
  filter(n_wahlkreise > 1)

data2025 %>% 
  filter(Gemeindeschlüssel == "01053991") %>% 
  View()

# beteiligte Gemeinden: 9xx-Datensatz mit den normalen Gemeinden desselben Kreises 
# und derselben Briefwahlzugehörigkeit verbinden
# --> nicht nur ein Ergebnis pro Gemeinde! unterschiedlich in "Wahlbezirk" --> interessierend nur Gemeinden
# weil darüber aggregiert wird. Die Wahlbezirke für dieselbe Gemeinde müssen summiert werden

kgemeinden <- data2025 %>%
  filter(Gemeinde >= 900) %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Wahlkreis,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeindeschlüssel,
    Gemeindename
  )

egemeinden <- data2025 %>%
  filter(Gemeinde < 900) %>%
  group_by(Land, Regierungsbezirk, Kreis, Wahlkreis, Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeindename)), collapse = ", "),
    agg.schlüssel = paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
    .groups = "drop"
  )

bw_groups <- kgemeinden %>%
  left_join(
    egemeinden,
    by = c("Land", "Regierungsbezirk", "Kreis", "Wahlkreis", "Kennziffer.Briefwahlzugehörigkeit")
  )


# Anzahl problematischer Briefwahlbezirke
nrow(bw_groups)

# Anteil problematischer gemeinsamer Briefwahlbezirke
n_briefwahlbezirke <- data2025 %>%
  group_by(Land, Regierungsbezirk, Kreis) %>%
  summarise(
    n_briefwahlgruppen = n_distinct(Kennziffer.Briefwahlzugehörigkeit),
    .groups = "drop"
  ) %>%
  summarise(
    summe_briefwahlgruppen = sum(n_briefwahlgruppen)
  )
nrow(bw_groups)/n_briefwahlbezirke[["summe_briefwahlgruppen"]]

# Anzahl betroffener gemeinden 
sum(bw_groups$n_gemeinden)

# Anzahl echter Gemeinden (100 Gemeinden zu wenig als offiziell!!)
n_gemeinden <- data2025 %>%
  filter(Gemeinde < 900) %>%
  distinct(Gemeindeschlüssel) %>%
  nrow()

# Anteil betroffener Gemeinden
sum(bw_groups$n_gemeinden) / n_gemeinden

## unter Beobachtung: Wahlbezirksauszählungen durch andere Wahlbezirke
table(data2025$Kennziffer.Urnenwahlbezirke.nach...68.BWO == "0000")
# Anzahl Fälle
data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  count(Kennziffer.Urnenwahlbezirke.nach...68.BWO)
# Anzahl davon, die tatsächlich nur nullen besitzen
data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  mutate(
    leer = Gültige...Zweitstimmen == 0
  ) %>%
  count(leer)

# Shifts innerhalb und außerhalb der Gemeinde. Wichtig ist außerhalb der Gemeinde
par68 <- data2025 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  group_by(Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    gemeinden = paste(unique(Gemeindename), collapse = ", "),
    agg.schlüssel = paste(unique(Gemeindeschlüssel), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(grepl(",", agg.schlüssel))
# In Ordnung, zwar nicht immer TRUE, aber eigentlich (in Worten angegeben) enthalten


# Zusammengeführte gemeinden
alle <- c(bw_groups$agg.schlüssel, par68$agg.schlüssel)

listen <- lapply(
  alle,
  function(x) sort(unique(str_trim(strsplit(x, ",")[[1]])))
)

geaendert <- TRUE

while (geaendert) { # Mengen mit Überlappung rekursiv zusammenführen
  
  geaendert <- FALSE
  neu <- list()
  
  while (length(listen) > 0) {
    
    aktuelle <- listen[[1]]
    listen <- listen[-1]
    
    i <- 1
    while (i <= length(listen)) {
      
      if (length(intersect(aktuelle, listen[[i]])) > 0) {
        
        aktuelle <- sort(unique(c(aktuelle, listen[[i]])))
        listen <- listen[-i]
        geaendert <- TRUE
        i <- 1
        
      } else {
        
        i <- i + 1
        
      }
    }
    
    neu[[length(neu) + 1]] <- aktuelle
  }
  
  listen <- neu
}

joinedG <- data.frame(
  agg.schlüssel = sapply(
    listen,
    function(x) paste(x, collapse = ", ")
  ),
  stringsAsFactors = FALSE
)

# Ergebnis gleich zur bw_groups Aggregation, siehe:
identisch <- mapply(
  function(x, y) {
    setequal(
      strsplit(x, ",\\s*")[[1]],
      strsplit(y, ",\\s*")[[1]]
    )
  },
  joinedG$agg.gemeindeschlüssel,
  bw_groups$agg.gemeindeschlüssel
)

all(identisch)

###############################################################################
#Mapping-Datensatz erstellen
###############################################################################
mapping25 <- data2025 %>% 
  select(Wahlkreis, Gemeinde, Gemeindeschlüssel) %>% 
  distinct()

mapping25 <- mapping25 %>%
  left_join(
    bw_groups %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c(
      "Wahlkreis",
      "Gemeindeschlüssel"
    )
  )

bw_lookup <-
  bw_groups %>%
  distinct(agg.schlüssel) %>%
  mutate(
    Gemeindeschlüssel =
      strsplit(agg.schlüssel, ",\\s*")
  ) %>%
  tidyr::unnest(Gemeindeschlüssel)

mapping25 <-
  mapping25 %>%
  left_join(
    bw_lookup,
    by = "Gemeindeschlüssel",
    suffix = c("", ".bw")
  ) %>%
  mutate(
    
    agg.schlüssel =
      case_when(
        
        # künstliche Gemeinde:
        Gemeinde >= 900 ~ agg.schlüssel,
        
        # echte Gemeinde mit Briefwahlaggregation:
        !is.na(agg.schlüssel.bw) ~ agg.schlüssel.bw,
        
        # normale Gemeinde:
        TRUE ~ Gemeindeschlüssel
        
      )
    
  ) %>%
  select(-agg.schlüssel.bw)






















# Bereinigung Bundestagswahl 2021
data2021 <- data2021%>% 
  mutate(
    Gemeindeschlüssel = paste0(
      str_pad(Land, width = 2, pad = "0"),
      str_pad(Regierungsbezirk, width = 1, pad = "0"),
      str_pad(Kreis, width = 2, pad = "0"),
      str_pad(Gemeinde, width = 3, pad = "0")
    )
  )


# Künstliche Gemeinden über Wahlkreis hinaus (1 Ausnahme:) (unterschiedlicher Gemeindename)
# erledigt: in bw_groups korrekt getrennt enthalten
data2021 %>%
  filter(Gemeinde >= 900) %>%
  group_by(
    Gemeindeschlüssel,
    Kennziffer.Briefwahlzugehörigkeit
  ) %>%
  summarise(
    n_wahlkreise = n_distinct(Wahlkreis),
    .groups = "drop"
  ) %>%
  filter(n_wahlkreise > 1) %>% 
  View()

# beteiligte Gemeinden: 9xx-Datensatz mit den normalen Gemeinden desselben Kreises 
# und derselben Briefwahlzugehörigkeit verbinden
# --> nicht nur ein Ergebnis pro Gemeinde! unterschiedlich in "Wahlbezirk" --> interessierend nur Gemeinden
# weil darüber aggregiert wird. Die Wahlbezirke für dieselbe Gemeinde müssen summiert werden

kgemeinden21 <- data2021 %>%
  filter(Gemeinde >= 900) %>%
  distinct(
    Land,
    Regierungsbezirk,
    Kreis,
    Wahlkreis,
    Kennziffer.Briefwahlzugehörigkeit,
    Gemeindeschlüssel,
    Gemeinde.Name
  )

egemeinden21 <- data2021 %>%
  filter(Gemeinde < 900) %>%
  group_by(Land, Regierungsbezirk, Kreis, Wahlkreis, Kennziffer.Briefwahlzugehörigkeit) %>%
  summarise(
    n_gemeinden = n_distinct(Gemeinde),
    gemeinden = paste(sort(unique(Gemeinde.Name)), collapse = ", "),
    agg.schlüssel = paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
    .groups = "drop"
  )

bw_groups21 <- kgemeinden21 %>%
  left_join(
    egemeinden21,
    by = c("Land", "Regierungsbezirk", "Kreis", "Wahlkreis", "Kennziffer.Briefwahlzugehörigkeit")
  )


# Anzahl problematischer Briefwahlbezirke
nrow(bw_groups21)

# Anteil problematischer gemeinsamer Briefwahlbezirke
n_briefwahlbezirke21 <- data2021 %>%
  group_by(Land, Regierungsbezirk, Kreis) %>%
  summarise(
    n_briefwahlgruppen = n_distinct(Kennziffer.Briefwahlzugehörigkeit),
    .groups = "drop"
  ) %>%
  summarise(
    summe_briefwahlgruppen = sum(n_briefwahlgruppen)
  )
nrow(bw_groups21)/n_briefwahlbezirke21[["summe_briefwahlgruppen"]]

# Anzahl betroffener gemeinden 
sum(bw_groups21$n_gemeinden)

# Anzahl echter Gemeinden (100 Gemeinden zu wenig als offiziell!!)
n_gemeinden21 <- data2021 %>%
  filter(Gemeinde < 900) %>%
  distinct(Gemeindeschlüssel) %>%
  nrow()

# Anteil betroffener Gemeinden
sum(bw_groups21$n_gemeinden) / n_gemeinden21


## unter Beobachtung: Wahlbezirksauszählungen durch andere Wahlbezirke
table(data2021$Kennziffer.Urnenwahlbezirke.nach...68.BWO == "0000")
# Anzahl Fälle
data2021 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  count(Kennziffer.Urnenwahlbezirke.nach...68.BWO)
# Anzahl davon, die tatsächlich nur nullen besitzen
data2021 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  mutate(
    leer = E_Gültige == 0
  ) %>%
  count(leer)

# Shifts innerhalb und außerhalb der Gemeinde. Wichtig ist außerhalb der Gemeinde
par6821 <- data2021 %>%
  filter(Kennziffer.Urnenwahlbezirke.nach...68.BWO != "0000") %>%
  group_by(Kennziffer.Urnenwahlbezirke.nach...68.BWO) %>%
  summarise(
    gemeinden = paste(unique(Gemeinde.Name), collapse = ", "),
    agg.schlüssel = paste(unique(Gemeindeschlüssel), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(grepl(",", agg.schlüssel))
# In Ordnung, zwar nicht immer TRUE, aber eigentlich (in Worten angegeben) enthalten


# Zusammengeführte gemeinden
alle21 <- c(bw_groups21$agg.schlüssel, par6821$agg.schlüssel)

listen21 <- lapply(
  alle21,
  function(x) sort(unique(str_trim(strsplit(x, ",")[[1]])))
)

geaendert <- TRUE

while (geaendert) { # Mengen mit Überlappung rekursiv zusammenführen
  
  geaendert <- FALSE
  neu <- list()
  
  while (length(listen21) > 0) {
    
    aktuelle <- listen21[[1]]
    listen21 <- listen21[-1]
    
    i <- 1
    while (i <= length(listen21)) {
      
      if (length(intersect(aktuelle, listen21[[i]])) > 0) {
        
        aktuelle <- sort(unique(c(aktuelle, listen21[[i]])))
        listen21 <- listen21[-i]
        geaendert <- TRUE
        i <- 1
        
      } else {
        
        i <- i + 1
        
      }
    }
    
    neu[[length(neu) + 1]] <- aktuelle
  }
  
  listen21 <- neu
}

joinedG21 <- data.frame(
  agg.schlüssel = sapply(
    listen21,
    function(x) paste(x, collapse = ", ")
  ),
  stringsAsFactors = FALSE
)

# Ergebnis gleich zur bw_groups Aggregation, siehe:
identisch21 <- mapply(
  function(x, y) {
    setequal(
      strsplit(x, ",\\s*")[[1]],
      strsplit(y, ",\\s*")[[1]]
    )
  },
  joinedG21$agg.gemeindeschlüssel,
  bw_groups21$agg.gemeindeschlüssel
)

all(identisch21) # muss TRUE sein für nachfolgenden Code

###############################################################################
#Mapping-Datensatz erstellen
###############################################################################
mapping21 <- data2021 %>% 
  select(Wahlkreis, Gemeinde, Gemeindeschlüssel) %>% 
  distinct()

mapping21 <- mapping21 %>%
  left_join(
    bw_groups21 %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c(
      "Wahlkreis",
      "Gemeindeschlüssel"
    )
  )

bw_lookup21 <-
  bw_groups21 %>%
  distinct(agg.schlüssel) %>%
  mutate(
    Gemeindeschlüssel =
      strsplit(agg.schlüssel, ",\\s*")
  ) %>%
  tidyr::unnest(Gemeindeschlüssel)

mapping21 <-
  mapping21 %>%
  left_join(
    bw_lookup21,
    by = "Gemeindeschlüssel",
    suffix = c("", ".bw")
  ) %>%
  mutate(
    
    agg.schlüssel =
      case_when(
        
        # künstliche Gemeinde:
        Gemeinde >= 900 ~ agg.schlüssel,
        
        # echte Gemeinde mit Briefwahlaggregation:
        !is.na(agg.schlüssel.bw) ~ agg.schlüssel.bw,
        
        # normale Gemeinde:
        TRUE ~ Gemeindeschlüssel
        
      )
    
  ) %>%
  select(-agg.schlüssel.bw)










# Abchecken: einheitliche Gruppierungen der Wahlgebiete von 2021 zu 2025

mapping21_echt <-
  mapping21 %>%
  filter(substr(Gemeindeschlüssel,
                nchar(Gemeindeschlüssel)-2,
                nchar(Gemeindeschlüssel)-2) != "9")

mapping25_echt <-
  mapping25 %>%
  filter(substr(Gemeindeschlüssel,
                nchar(Gemeindeschlüssel)-2,
                nchar(Gemeindeschlüssel)-2) != "9")

anti_join( # Fehlende Gemeinden: 57 fehlend im Vergleich zu 21 (nrow(mapping25_echt) = 10721)
  mapping21_echt,
  mapping25_echt,
  by="Gemeindeschlüssel"
) %>% 
  nrow()

anti_join( # Zusätzliche Gemeinden: 91 zusätzlich im Vergleich zu 21 (nrow(mapping25_echt) = 10721)
  mapping25_echt,
  mapping21_echt,
  by="Gemeindeschlüssel"
) %>% 
  nrow()


# Aggregationen prüfen
vergleich <-
  mapping21 %>%
  select(
    Gemeindeschlüssel,
    agg21 = agg.schlüssel,
    Wahlkreis
  ) %>%
  inner_join(
    mapping25 %>%
      select(
        Gemeindeschlüssel,
        agg25 = agg.schlüssel,
        Wahlkreis
      ),
    by= c("Gemeindeschlüssel", "Wahlkreis")
  )

vergleich %>%
  filter(agg21 != agg25) %>% # klappt da Gemeindeschlüssel in string sortiert wurden
  View()



# Daten auf gemeinsame Gemeinden beschränken
# echte Gemeinden identisch sind. (sind sie nicht, wir ignorieren diese und entfernen echte gemeinden die nicht in beiden jahren vorkommen. diese müssen nicht nur als gemeindeschlüssel sondern auch alle gemeindeschlüssel, die in den strings der agg.schlüssel die fehlende/zusätzliche gemeinde haben müssen entfernt werden.)

g21 <- mapping21 %>%
  filter(Gemeinde < 900) %>%
  pull(Gemeindeschlüssel) %>%
  unique()

g25 <- mapping25 %>%
  filter(Gemeinde < 900) %>%
  pull(Gemeindeschlüssel) %>%
  unique()

entfernen <- union(
  setdiff(g21, g25),
  setdiff(g25, g21)
)

enthaelt_gemeinde <- function(x, entfernen){
  
  sapply(strsplit(x, ",\\s*"), function(v){
    
    any(v %in% entfernen)
    
  })
  
}


mapping21_clean <-
  mapping21 %>%
  filter(
    !enthaelt_gemeinde(agg.schlüssel, entfernen)
  )

mapping25_clean <-
  mapping25 %>%
  filter(
    !enthaelt_gemeinde(agg.schlüssel, entfernen)
  )




# Für spätere Aggregation entsprechende ergebnisse aus den Wahldaten entfernen
###############################################################################
# Gültige künstliche Gemeinden bestimmen
###############################################################################

kuenstlich21_ok <- mapping21_clean %>%
  filter(Gemeinde >= 900) %>%
  distinct(Wahlkreis, Gemeindeschlüssel)

kuenstlich25_ok <- mapping25_clean %>%
  filter(Gemeinde >= 900) %>%
  distinct(Wahlkreis, Gemeindeschlüssel)

data21_clean <-
  data2021 %>%
  anti_join(
    tibble(
      Wahlkreis = data2021$Wahlkreis[data2021$Gemeinde < 900 & data2021$Gemeindeschlüssel %in% entfernen],
      Gemeindeschlüssel = data2021$Gemeindeschlüssel[data2021$Gemeinde < 900 & data2021$Gemeindeschlüssel %in% entfernen]
    ) %>% distinct(),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  ) %>%
  anti_join(
    data2021 %>%
      filter(Gemeinde >= 900) %>%
      distinct(Wahlkreis, Gemeindeschlüssel) %>%
      anti_join(kuenstlich21_ok,
                by = c("Wahlkreis", "Gemeindeschlüssel")),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  )

data25_clean <-
  data2025 %>%
  anti_join(
    tibble(
      Wahlkreis = data2025$Wahlkreis[data2025$Gemeinde < 900 & data2025$Gemeindeschlüssel %in% entfernen],
      Gemeindeschlüssel = data2025$Gemeindeschlüssel[data2025$Gemeinde < 900 & data2025$Gemeindeschlüssel %in% entfernen]
    ) %>% distinct(),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  ) %>%
  anti_join(
    data2025 %>%
      filter(Gemeinde >= 900) %>%
      distinct(Wahlkreis, Gemeindeschlüssel) %>%
      anti_join(kuenstlich25_ok,
                by = c("Wahlkreis", "Gemeindeschlüssel")),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  )






## Aggregation anpassen
# 1) Alle eindeutigen Aggregationen aus beiden Jahren
mapping2125 <-
  mapping21_clean %>%
  select(
    Gemeindeschlüssel,
    Wahlkreis,
    agg21 = agg.schlüssel
  ) %>%
  inner_join(
    mapping25_clean %>%
      select(
        Gemeindeschlüssel,
        Wahlkreis,
        agg25 = agg.schlüssel
      ),
    by = c("Gemeindeschlüssel", "Wahlkreis")
  )

split_keys <- function(x) unique(strsplit(x, ",\\s*")[[1]])

collapse_keys <- function(x) paste(sort(unique(x)), collapse = ", ")

mapping21_new <- mapping21_clean
mapping25_new <- mapping25_clean

geaendert <- TRUE

while(geaendert){
  
  geaendert <- FALSE
  
  mapping2125 <-
    mapping21_new %>%
    select(
      Gemeindeschlüssel,
      Wahlkreis,
      agg21 = agg.schlüssel
    ) %>%
    inner_join(
      mapping25_new %>%
        select(
          Gemeindeschlüssel,
          Wahlkreis,
          agg25 = agg.schlüssel
        ),
      by = c("Gemeindeschlüssel","Wahlkreis")
    )
  
  for(i in seq_len(nrow(mapping2125))){
    
    if(mapping2125$agg21[i] == mapping2125$agg25[i])
      next
    
    neu <-
      collapse_keys(
        c(
          split_keys(mapping2125$agg21[i]),
          split_keys(mapping2125$agg25[i])
        )
      )
    
    neue_keys <- split_keys(neu)
    
    ## solange sich durch bereits vorhandene Aggregationen noch neue Gemeinden
    ## ergeben, erweitern
    ## transitive Erweiterung
    repeat{
      
      alt_keys <- sort(neue_keys)
      
      ## alle bisherigen Aggregationen finden,
      ## die mindestens eine der Gemeinden enthalten
      agg_strings <-
        c(
          
          mapping21_new %>%
            filter(
              Wahlkreis == mapping2125$Wahlkreis[i],
              Gemeindeschlüssel %in% neue_keys
            ) %>%
            pull(agg.schlüssel),
          
          mapping25_new %>%
            filter(
              Wahlkreis == mapping2125$Wahlkreis[i],
              Gemeindeschlüssel %in% neue_keys
            ) %>%
            pull(agg.schlüssel)
          
        ) %>%
        unique()
      
      ## daraus sämtliche Gemeinden einsammeln
      neue_keys <-
        agg_strings %>%
        lapply(split_keys) %>%
        unlist() %>%
        unique()
      
      ## falls neue Gemeinden hinzugekommen sind,
      ## wiederholen
      if(identical(sort(neue_keys), alt_keys))
        break
    }
    
    neu <- collapse_keys(neue_keys)
    
    mapping21_new <-
      mapping21_new %>%
      mutate(
        agg.schlüssel =
          if_else(
            Wahlkreis == mapping2125$Wahlkreis[i] &
              agg.schlüssel %in% agg_strings,
            neu,
            agg.schlüssel
          )
      )
    
    mapping25_new <-
      mapping25_new %>%
      mutate(
        agg.schlüssel =
          if_else(
            Wahlkreis == mapping2125$Wahlkreis[i] &
              agg.schlüssel %in% agg_strings,
            neu,
            agg.schlüssel
          )
      )
    
    geaendert <- TRUE
  }
}


## checks
# Sind die einzigartigen Aggregationen identisch? (Nein)
agg21 <- unique(mapping21_new$agg.schlüssel)
agg25 <- unique(mapping25_new$agg.schlüssel)
setequal(agg21, agg25)
setdiff(agg21, agg25) %>% 
  View()
setdiff(agg25, agg21)

# Differenzierende betrachten
diff21 <- setdiff(
  unique(mapping21_new$agg.schlüssel),
  unique(mapping25_new$agg.schlüssel)
)
diff25 <- setdiff(
  unique(mapping25_new$agg.schlüssel),
  unique(mapping21_new$agg.schlüssel)
)
mapping21_new %>%
  filter(agg.schlüssel %in% diff21) %>%
  arrange(agg.schlüssel) %>% 
  View()
mapping25_new %>%
  filter(agg.schlüssel %in% diff25) %>%
  arrange(agg.schlüssel) %>% 
  View()

# wegen falsche Reihenfolge in character-IDs? (Nein, immer noch bestehendes Problem)
normalize <- function(x) {
  paste(
    sort(unique(strsplit(x, ",\\s*")[[1]])),
    collapse = ", "
  )
}
mapping21_test <- mapping21_new %>%
  mutate(agg_norm = sapply(agg.schlüssel, normalize))
mapping25_test <- mapping25_new %>%
  mutate(agg_norm = sapply(agg.schlüssel, normalize))
setequal(
  unique(mapping21_test$agg_norm),
  unique(mapping25_test$agg_norm)
)

# Konsistenzcheck (eindeutiger künstlicher gemeindecode check) (passt da 0 zeilen)
mapping21_new %>%
  group_by(Wahlkreis, Gemeindeschlüssel) %>%
  summarise(
    n = n_distinct(agg.schlüssel),
    .groups = "drop"
  ) %>%
  filter(n > 1)
mapping25_new %>%
  group_by(Wahlkreis, Gemeindeschlüssel) %>%
  summarise(
    n = n_distinct(agg.schlüssel),
    .groups = "drop"
  ) %>%
  filter(n > 1)

# Hat jede echte Gemeinde dieselbe Aggregation? Antwort: Ja
vergleich <-
  mapping21_new %>%
  filter(Gemeinde < 900) %>%
  select(
    Gemeindeschlüssel,
    Wahlkreis,
    agg21 = agg.schlüssel
  ) %>%
  inner_join(
    mapping25_new %>%
      filter(Gemeinde < 900) %>%
      select(
        Gemeindeschlüssel,
        Wahlkreis,
        agg25 = agg.schlüssel
      ),
    by = c("Gemeindeschlüssel", "Wahlkreis")
  )
vergleich %>%
  filter(agg21 != agg25)

# Kommt ein Gemeindeschlüssel in mehreren Aggregationen vor? Antwort: Nein
lookup <-
  mapping21_new %>%
  distinct(Wahlkreis, agg.schlüssel) %>%
  tidyr::separate_rows(
    agg.schlüssel,
    sep = ",\\s*"
  ) %>%
  rename(
    Gemeindeschlüssel = agg.schlüssel
  )
lookup %>%
  count(
    Wahlkreis,
    Gemeindeschlüssel
  ) %>%
  filter(n > 1)






# Aggregation der Wahldaten
id_vars <- c(
  "Wahlkreis",
  "Land",
  "Regierungsbezirk",
  "Kreis",
  "Verbandsgemeinde",
  "Gemeinde",
  "Kennziffer.Urnenwahlbezirke.nach...68.BWO",
  "Kennziffer.Briefwahlzugehörigkeit",
  "Gemeindename",
  "Gemeinde.Name",
  "Wahlbezirk",
  "Bezirksart",
  "Ungekürzte.Wahlbezirksbezeichnung",
  "Bezeichnung.des.Wahlbezirkes.gemäß.Anlage.30.zur.BWO",
  "Gemeindeschlüssel"
)

num_vars25 <- setdiff(
  names(data2025)[sapply(data2025, is.numeric)],
  id_vars
)

num_vars21 <- setdiff(
  names(data2021)[sapply(data2021, is.numeric)],
  id_vars
)

data25_agg <- data25_clean %>%
  left_join(
    mapping25_new %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  )

data21_agg <- data21_clean %>%
  left_join(
    mapping21_new %>%
      select(
        Wahlkreis,
        Gemeindeschlüssel,
        agg.schlüssel
      ),
    by = c("Wahlkreis", "Gemeindeschlüssel")
  )

wahldaten2025 <- data25_agg %>%
  group_by(agg.schlüssel) %>%
  summarise(
    
    Wahlkreis = first(Wahlkreis),
    
    Gemeinden =
      paste(sort(unique(Gemeindename)), collapse = ", "),
    
    Gemeindeschlüssel =
      paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
    
    across(
      all_of(num_vars25),
      ~sum(.x, na.rm = TRUE)
    ),
    
    .groups = "drop"
  )

wahldaten2021 <- data21_agg %>%
  group_by(agg.schlüssel) %>%
  summarise(
    
    Wahlkreis = first(Wahlkreis),
    
    Gemeinden =
      paste(sort(unique(Gemeinde.Name)), collapse = ", "),
    
    Gemeindeschlüssel =
      paste(sort(unique(Gemeindeschlüssel)), collapse = ", "),
    
    across(
      all_of(num_vars21),
      ~sum(.x, na.rm = TRUE)
    ),
    
    .groups = "drop"
  )






# Mapping Größe untersuchen
wahldaten2025 %>%
  summarise(
    n        = n(),
    Mittel   = mean(Gültige...Erststimmen),
    Median   = median(Gültige...Erststimmen),
    SD       = sd(Gültige...Erststimmen),
    Varianz  = var(Gültige...Erststimmen),
    Minimum  = min(Gültige...Erststimmen),
    Q1       = quantile(Gültige...Erststimmen, 0.25),
    Q3       = quantile(Gültige...Erststimmen, 0.75),
    Maximum  = max(Gültige...Erststimmen),
    IQR      = IQR(Gültige...Erststimmen),
    CV       = sd(Gültige...Erststimmen) / mean(Gültige...Erststimmen)
  )

options(scipen = 999)
hist(
  wahldaten2025$Gültige...Erststimmen,
  breaks = 100,
  main = "Histogramm der gültigen Erststimmen",
  xlab = "Gültige Erststimmen"
)




# Bundestagswahl 2025 abspeichern

# Mapping speichern

# Abspeichern Bundestagswahl 2021