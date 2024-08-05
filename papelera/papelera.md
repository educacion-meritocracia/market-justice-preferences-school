# papelera

----

Esto enfatiza demasiado diferencias entre países, que no son el foco del paper; guardar para otro sobre comparación internacional:

To what extents are individuals willing to justify unequal access to welfare services based on market justice principles? Influenced by theories of policy feedback, which suggest that social welfare policies can reinforce (positive feedback) or undermine (negative feedback) previous policy trajectories [@fernandez_positive_2013; @pierson_increasing_2000; @weaver_paths_2010], citizens’ beliefs about market justice are likely also shaped by the institutional and social contexts they encounter. In a study by @soler-martinez_concerns_2023 using Latinobarómetro 2020 data across 18 Latin American countries, show evidence of large differences amogn countries with regard to the where concerns about health and education access predominated over income inequality. Research using 2009 ISSP data   shows that, despite a general lower support for market justice in education and health in 17 OECD countries,  Despite a general disagreement with market justice principles,  as shown in @fig-issp using data from the 2019 International Social Survey Program (ISSP). Although in general it is considered unjust that education and health are conditional to payment capacity, still there is an important part of the respondents that consider market distribution as just. Besides, this varies according to policy domain, being the justification for market inequality grater in education than in healthcare.

```{r echo=FALSE}
pacman::p_load(dplyr, ggplot2, haven)
load("input/data/issp_rec.RData")
```

```{r echo=FALSE}
#| label: fig-issp
#| fig-cap: "Average of market justice preferences by country"
#| fig-asp: 0.65

base$country <- as.character(base$country)
base$country <- substr(base$country, 4, nchar(base$country))
base$country <- as_factor(base$country)

base %>% ggplot(aes(mean, country)) +
  theme_bw() +
  geom_point(aes(color = justice, shape = justice), size = 2) +
  stat_summary(aes(xintercept = after_stat(x), y = 0), fun = mean, geom = "vline", orientation = "y", colour = "green") +
  xlab("") +
  ylab("Country")+
  scale_x_continuous(limits = c(1, 5),
                     breaks = seq(1, 5, by = 1),
                     label = c("Very unjust,\n definitely wrong", 
                               "Somewhat unjust,\n wrong", 
                               "Neither just nor\n unjust, mixed feelings", 
                               "Somewhat just,\n right", 
                               "Very just,\n definitely right")) +
  scale_color_discrete(name = NULL) +
  scale_shape_discrete(name = NULL) +
  theme(text = element_text(size = 9.35),
#        legend.position = "bottom",
        plot.margin = margin(b = 0))
```

----
Acá mucho bla bla, redundante

The concept of meritocracy frequently appears nowadays when it comes to analyze cultural determinants of social inequalities. In general, it is mentioned as a value associated with justice, as it would link efforts and talents with rewards in an equitable manner. This normative sense is quite far from its original formulation by @young_rise_1958 in the satirical novel "The Rise of Meritocracy", where it represented a mechanism for reproducing the inequalities of origin. The meritocratic ideal remained relatively unchallenged until a series of recent publications turned into its potential consequences for maintaining social inequality. Perhaps one of the most recent sources in this line is Michael Sandel's "The Tyranny of Merit", where he strongly questions the implications of carrying out the principle of merit in societies that do not guarantee equal opportunities and that generate a feeling of scarce recognition and appreciation of those who receive lesser rewards: "In society's eyes, and perhaps also their own, their work no longer signified a valued contribution to the common good." [@sandel_tyranny_2020, pp.].

---

The differences in economic understanding across age groups are consistent with research on cognitive development [@choudhury_social_2006]. Adolescents with mature socio-cognitive abilities tend to express stronger preferences for fairness than infants and children [@wynn_not_2018]. As children grow older, they become more likely to behave fairly, with their early-emerging strict egalitarianism being replaced by an increasing endorsement of fairness principles and engagement in collaborative activities [@huppert_development_2019; @mcauliffe_developmental_2017]. In these activities, their fairness views consider individual contributions, merits, and circumstances [@almas_fairness_2010; @huppert_development_2019; @sigelman_development_1991]. @engelmann_children_2019 claim that children’s sense of fairness emerges at three years old, and we can observe it in collaborative activities, where they accept inequality if the procedure gives everyone an equal chance. Therefore, children at this age respond to unequal distributions based on interpersonal concerns, as they already demand equal respect. In any case, between 3 and 8 years of age, inequitable and anti-meritorious allocations are evaluated more negatively, but equitable and meritorious allocations are not evaluated more positively [@elenbaas_unfairness_2019].

----

Some research shows that the social environment in which children develop, such as family and school, is associated with their prosocial behaviors by playing an essential role in the transmission of equity norms [@schunk_fairness_2023; @kosse_prosociality_2020]. In fact, 


---
[Studies about meritocratic beliefs and inequality justification at school age is rather scarce, revealing a wide research gap as schools are one of the primary socialization institutions where achievement based on merit is related to success [@erivwo_meritocracy_2021]. A number of studies in this area have been developed by Clara Sabbagh and Nura Resh in Israel. Using justice in grade obtention as a measure of distributive justice and meritocracy ]




Using data for 41 countries from the International Social Survey Programme (ISSP),@garcia-sanchez_attitudes_2020 found that the perceived size of the income gap correlated positively with support for progressive taxation, an association that was weaker among those who endorsed meritocratic and equal opportunity beliefs. Along the same line, experimental research by @durante_preferences_2009 showed that subjects support less redistribution when the initial distribution is determined according to task performance.
