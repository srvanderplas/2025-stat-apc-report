
library(dplyr)
library(magrittr)
library(readxl)
library(ggplot2)

metrics <- read_xlsx("data-analysis/data/tabled_department_metrics.xlsx", sheet="Detail")
options(knitr.kable.NA = '')


ggplot(metrics, aes(research_awards_growth_inc_nuf_percent_of_total, p1_expenditures_normalized)) + geom_point()


ggplot(metrics, aes(awards_normalized, p1_expenditures_normalized)) + geom_point() +
  geom_text(data = filter(metrics, awards_normalized > 0.05), aes(label = department))

ggplot(metrics, aes(awards_normalized,books_normalized)) + geom_point() +
  geom_text(data = filter(metrics, awards_normalized > 0.02), aes(label = department))

ggplot(metrics, aes(x = sri)) + geom_dotplot()


metrics2 <- read_xlsx("data-analysis/data/tabled_department_metrics.xlsx", sheet=1)

instructional_metrics_long <- select(metrics2, department, lowest_level_short_name, lowest_level_name, starts_with("z")) |>
  pivot_longer(-c(1:3), names_to = "metric", values_to = "value")

research_metrics_long <- metrics |>
  filter(acad_end_year==2024) |>
  select(department, lowest_level_short_name, lowest_level_name, matches("z_score|sri_aau_public_peers")) |>
  select(-research_avg_z_score_equally_weighted) |>
  mutate(sri_aau_public_peers_z_score = (sri_aau_public_peers-mean(sri_aau_public_peers, na.rm = T))/sd(sri_aau_public_peers, na.rm = T)) |>
  select(-sri_aau_public_peers_z_score) |>
  pivot_longer(-c(1:3), names_to = "metric", values_to = "value")

metrics_long <- bind_rows(instructional_metrics_long, research_metrics_long)

metrics_long_sum <- metrics_long |>
  group_by(metric) |>
  summarise(mean = mean(value, na.rm = T), NAs = sum(is.na(value)), woebegone = mean(value<0, na.rm = T)*100)

ggplot(metrics_long, aes(x = value)) +
  facet_wrap(~metric, ncol = 3) + geom_histogram(aes(y = after_stat(ncount)), binwidth=.1) +
  geom_text(data = metrics_long_sum, aes(x = Inf, y = Inf, label = sprintf("Mean: %.02f\nNAs: %d\n%0.0f%% below 0", mean, NAs, woebegone)), hjust = 1, vjust =1)
ggsave("Lake_Woebegone_Effect.png", width = 6, height = 9)

research_dept_sum <- research_metrics_long |>
  group_by(department, lowest_level_short_name, lowest_level_name) |>
  summarize(num_research_sub_zero=sum(value < 0, na.rm = T), num_research_NA = sum(is.na(value)))

instructional_dept_sum <- instructional_metrics_long |>
  group_by(department, lowest_level_short_name, lowest_level_name) |>
  summarize(num_instr_sub_zero=sum(value < 0, na.rm = T), num_instr_NA = sum(is.na(value)))

dept_sum <- full_join(research_dept_sum, instructional_dept_sum)

ggplot(dept_sum, aes(x = num_research_sub_zero)) + geom_bar() + ggtitle("Distribution of # Sub-Zero Research Metrics") + xlab("# Metrics")
ggsave("Dist-Subzero-Research.png", width = 6, height = 4)
ggplot(dept_sum, aes(x =  num_instr_sub_zero)) + geom_bar()+ ggtitle("Distribution of # Sub-Zero Instructional Metrics") + xlab("# Metrics")
ggsave("Dist-Subzero-Instr.png", width = 6, height = 4)


dept_sum <- dept_sum |>
  mutate(as_or_more_extreme_stats = (num_research_sub_zero >= 4 & num_instr_sub_zero >= 4))

write.csv(dept_sum, "Departments as or more extreme than statistics.csv")
