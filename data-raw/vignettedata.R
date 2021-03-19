setwd("~/Git/uk-covid-datatools/")
devtools::load_all("~/Git/uk-covid-datatools/")
ukcovidtools::setup("~/Git/uk-covid-datatools/config.yml")
setwd("~/Git/arear/")
usethis::proj_set()

## SARI ITU cases outcode to trust Id
sariLL = dpc$spim$getSARI()
sariLL2 = sariLL %>% filter(admittedtoicu=="Yes") %>% select(trustId = trustcode,outcode = postcode)
sariLL2 %>% readr::write_csv("vignettes/non-public-data/outcodes-of-itu-admissions.csv")

##  postcode assignments for
postcodes <- readxl::read_excel("./data-raw/Locality Postcodes.xlsx")
tmp = dpc$postcodes$getFullONS()
devonlocality = postcodes %>% left_join(tmp %>% select(pcds,code=lsoa11),by=c("Postcode"="pcds")) %>% select(locality, code, pcds = Postcode) %>% distinct()
devonlocality = devonlocality %>% mutate(
  trustName = case_when(
    locality=="Northern locality" ~ "Northern Devon Healthcare NHS Trust",
    locality=="Eastern locality" ~ "Royal Devon and Exeter NHS Foundation Trust",
    locality=="Western locality" ~ "University Hospitals Plymouth NHS Trust",
    locality=="Southern locality" ~ "Torbay and South Devon NHS Foundation Trust",
    TRUE ~ "other"
  ),
  codeType = "LSOA11"
)

devonlocality = devonlocality %>% left_join(arear::surgecapacity %>% as_tibble() %>% select(trustName,trustId) %>% distinct(), by="trustName")

usethis::use_data(devonlocality, overwrite = TRUE)
