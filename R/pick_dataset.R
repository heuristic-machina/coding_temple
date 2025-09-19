#module 1 picking a dataset:  skills from Indeed.com over a 4 year time span

#drill down
dat_sg<-public_use_industry_skills_needs |>
  filter(skill_group_category=='Tech Skills') |>
  summarize(year, industry_name,
            skill_group_category,
            skill_group_name,
            skill_group_rank)

#workable format
tibble(dat_sg)
# A tibble: 754 × 5
#year industry_name           skill_group_category skill_group_name     skill_group_rank
#  <dbl> <chr>                   <chr>                <chr>                 <dbl>
#1 2015 Mining & Metals         Tech Skills          Digital Literacy        8
#2 2015 Oil & Energy            Tech Skills          Digital Literacy        6

#observe skill names
dat_sg |> distinct(skill_group_name)
# A tibble: 18 × 1
#skill_group_name                      
#<chr>                                 
#1 Digital Literacy                      
#2 Scientific Computing   

#visual plot
library(plotly)

plot1 <- dat_sg |>
  count(year, skill_group_name) |>
  ggplot(aes(x = year, y = n, color = skill_group_name, text = skill_group_name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Tech Skill Group Trends Over Time",
    x = "Year",
    y = "Frequency",
    color = "Skill Group"
  ) +
  theme_minimal()

ggplotly(plot1, tooltip = "text")
