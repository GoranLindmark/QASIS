# är det bara 135 audits gjorda
# Trendanalys
# Insikter om data.
# En audit kan utföras på flera datum. Det betyder att vi måste välja
# om vi ska aggregera per audit eller på datum
#
# Om vi definerar ett audit tillfälle som en kombination av datum och audit nummer.
# Jämförelse av de "audits" som är helt korrekta dvs saknar "Chritical", "Major" eller "Minor" faults.
# Denna jämförelse ger vid handen att trenden bland felaktiga är ökande medan trenden för korrekta är vikande.
#


d <-
  qasis %>%
  select(audit, `audit type`, rating, num_rating) %>%
  group_by(audit) %>%
  summarize(nrIssues = n(),
            num_rating_p_audint = sum(num_rating))


# på hur många datum är en audit genomförd
#

classDistribution(qasis$rating)
d <-
  qasis %>%
  select(audit, date, rating) %>%
  group_by(audit, date, rating) %>%
  summarize( antal = n(),
            .groups = "drop") %>%
  mutate(auditOccation = 1:nrow(.))

noFaults <-
  d %>%
  filter( !(rating %in% c("Chritical", "Major", "Minor")) ) %>%
  mutate(faultFree  = 1) %>%
  select(auditOccation, faultFree)

d <- left_join(d, noFaults, by = (c("auditOccation" = "auditOccation")))

d %>%
  group_by(faultFree) %>%
  summarize(antal = n(),
            pct = antal/nrow(.)*100)

summary(d)

s <-
  d %>%
  select(date, auditOccation, faultFree, antal) %>%
  mutate(faultFree = ifelse( is.na(faultFree), "Correct", "fault")) %>%
  pivot_wider(names_from = "faultFree", values_from = "antal")

s[is.na(s)] <- 0


series <- tsibble(s, index = auditOccation)

series %>%
  autoplot(fault)

series %>%
  autoplot(Correct)

dcmp <-
  series %>%
  model(stl = STL(fault))
components(dcmp) %>% autoplot()+
  labs(caption = "Faulty")

dcmp <-
  series %>%
  model(stl = STL(Correct))
components(dcmp) %>% autoplot() +
  labs(caption = "Correct")

