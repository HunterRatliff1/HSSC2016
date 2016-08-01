# 
# 
# # Density + Histogram of percentage correct
# PreT %>% 
#   group_by(SID) %>%
#   summarise(Score = sum(isCorrect)/n()) %>%
#   ggplot(aes(x=Score)) +
#   geom_histogram(aes(y=..density..), binwidth=0.06, alpha=0.75) +
#   geom_density() + 
#   scale_x_continuous(labels=percent) + 
#   theme_ + labs(subtitle="High School (2016)") +
#   labs(y="", title="Pre-Test Score Distribution")
# 
# # Question Distribution (bars)
# PreT %>% ggplot(aes(x=QID, fill=Score)) +
#   geom_bar(position="fill") + 
#   scale_fill_fivethirtyeight(name="") +
#   scale_y_continuous(labels=percent, name="Percentage of Responses") +
#   theme_ + labs(subtitle="High School (2016)") +
#   labs(x="", title="Question Distribution :: Pre-Test")
# ## Facet by topic
# last_plot() + facet_wrap("Topic", scales="free") + 
#   labs(title="Question Distribution :: Pre-Test (Facet by Topic)")
#   
# # counts by gender & race
# PreI %>% count(Race, Gender, sort=T) %>% 
#   ggplot(aes(x=Race, y=n, fill=Gender)) + 
#   geom_bar(stat="identity") + coord_flip() + 
#   scale_fill_fivethirtyeight() +
#   theme_ + labs(subtitle="High School (2016)") +
#   labs(title="Students by race & gender ID")
# 
# PreI %>% ggplot(aes(x=EO.sum, fill=Edu)) + 
#   geom_histogram(binwidth=1, color="black") +
#   theme_ + labs(subtitle="High School (2016)") +
#   labs(title="Students by EO Indicator & Parents Edu") 
# 
# 
# PreI %>% ggplot(aes(x=EO.sum, y=Score, color=Race)) +
#   geom_jitter() + scale_y_continuous(labels=percent) +
#   scale_x_sqrt() +
#   theme_ + labs(subtitle="High School (2016)") +
#   labs(title="Pre-Test Scores by sum of EO Indicator") 
# 
# 
# PreI %>% ggplot(aes(x=Edu, y=Score)) +
#   geom_violin(aes(fill=Edu)) +
#   geom_jitter(alpha=0.5) +
#   geom_boxplot(alpha=0.25, fill="white") +
#   scale_y_continuous(labels=percent) +
#   guides(fill=F) + coord_flip() +
#   theme_ + labs(subtitle="High School (2016)") +
#   labs(title="Pre-Test Scores by Parents Edu")
# 
# 
# 
# 
# # PreT %>% 
# #     group_by(SID, Topic) %>%
# #     summarise(Score = sum(isCorrect)) %>%
# #     ggplot(aes(x=Score, fill=Topic)) +
# #     geom_density(alpha=0.3) + 
# #     theme_ + labs(subtitle="High School (2016)") +
# #     labs(y="", title="Pre-Test Percentages") + 
# #     facet_wrap("Topic")
# 
#   

Dems %>% mutate(delta = Score2 - Score) %>% 
  ggplot() + geom_density(aes(x=Score), fill="grey") +
  geom_density(aes(x=Score2), fill="red", alpha=.5) 

last_plot() + facet_wrap("Edu")
  
  
Dems %>% 
  filter(!is.na(Edu)) %>%
  ggplot(aes(x=Edu, y=Score, color=Edu), ) +
  # geom_point(aes(y=Score), color="grey", position=position_jitter(height=0)) + 
  # geom_point(aes(y=Score2), position=position_jitter(height=0)) + 
  geom_curve(aes(y=Score, yend=Score2, xend=Edu), position="jitter",
              curvature = -0.1, arrow = arrow(length = unit(0.3,"cm"))) +
  scale_y_continuous(labels=percent) + coord_flip() +
  guides(color=F) +#guide_legend(nrow=2)) +
  # scale_color_brewer(name="Parent's Education", palette = "Spectral") +
  theme_ + labs(subtitle="High School (2016)") +
  labs(title="Pre-Test Scores by sum of EO Indicator")   