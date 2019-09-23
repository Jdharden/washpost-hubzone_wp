head(FPDS_HUB_Database)
str(FPDS_HUB_Database)
summarize(FPDS_HUB_Database)
glimpse(FPDS_HUB_Database)

FPDS_HUB_Database <- read.csv("Complete_FPDS_DC_HUB_Database.csv", stringsAsFactors=FALSE, na.strings="#VALUE!", header = TRUE)

FPDSpivot <- FPDS_HUB_Database %>% 
  select(Global.Vendor.Name, Action.Obligation....) %>%
  group_by(Global.Vendor.Name, Action.Obligation....) %>%
  summarise(TotalContract = sum(Action.Obligation....), 
            AvgContract = mean(Action.Obligation....,na.rm = TRUE),
            NumContracts = length(!is.na(Action.Obligation....)))

FPDS_HUB_Database %>% group_by(Vendor.Name,) %>%
  count() %>%
  arrange(desc(n))
 
FPDS_HUB_Database %>% filter(Vendor.Name == "NEAL R GROSS AND COMPANY INC") %>%
  group_by(Contracting.Agency) %>%
  summarise(
    AvgContract = mean(Action.Obligation...., na.rm = TRUE), 
    MedianContract = median(Action.Obligation...., na.rm = TRUE),
    TotalContract = sum(Action.Obligation...., na.rm = TRUE)
  )

vendors <- FPDS_HUB_Database %>% group_by(Vendor.Name) %>%
  summarise(
    AvgContract = mean(Action.Obligation...., na.rm = TRUE), 
    MedianContract = median(Action.Obligation...., na.rm = TRUE),
    TotalContract = sum(Action.Obligation...., na.rm = TRUE),
  )  %>%
  top_n(30)

ggplot(vendors, aes(x = TotalContract, y = reorder(Vendor.Name, TotalContract)  )) +
  geom_point() +
  labs( x = "Contract Amounts", y = "Vendors")
