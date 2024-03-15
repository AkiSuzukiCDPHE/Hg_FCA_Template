

# NPK 

HgDataNPK <- HgData_5 %>% filter(Species == "Northern Pike")

NPKSummary <- data.frame(unclass(summary(HgDataNPK$Length_Inches)))

print(NPKSummary)


hist(HgDataNPK$Length_Inches)

# Lake Trout(Mackinaw)

HgDataMAC <- HgData_5 %>% filter(Species == "Lake Trout(Mackinaw)")

MACSummary <- data.frame(unclass(summary(HgDataMAC$Length_Inches)))

hist(HgDataMAC$Length_Inches)

# Largemouth Bass

HgDataLMB <- HgData_5 %>% filter(Species == "Largemouth Bass")

LMBSummary <- data.frame(unclass(summary(HgDataLMB$Length_Inches)))

hist(HgDataLMB$Length_Inches)

