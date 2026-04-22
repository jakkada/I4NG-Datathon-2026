# Critical Thinking, Media, and Democracy in an Age of Unchecked Technological Development

A policy brief and reproducible data analysis prepared for the [I4NG Datathon: Resilient Europe](https://infra4nextgen.com/events/datathon-resilient-europe/).

## Summary

This project investigates what influences Europeans' support for teaching critical thinking, media, and democracy in compulsory education, in a time of rapid technological development. Using data from the CRONOS3 and ESS10-11 surveys across 11 European countries, we identify key predictors and formulate policy recommendations.

### Key findings

-   **Education** is the strongest individual-level driver: each additional year of schooling increases support by 7%.
-   People who **expect technology to bring negative societal changes** are 67% more likely to support this type of education.
-   **Reacting to social media** (e.g. commenting) increases support by 44%, while passive scrolling decreases it by 25%.
-   Preferences for more **STEM education** are associated with lower support: those wanting more digital tools & programming are 49% less likely, and those wanting more maths & sciences 35% less likely, to also support critical thinking, media and democracy education.
-   **Country context matters**: support is substantially lower in Hungary, Czechia, Poland, and Portugal compared to Austria.

### Policy recommendations

1.  Involve scientific and expert communities in public debates on technology's societal impact.
2.  Integrate critical thinking into STEM curricula and technology literacy into civic education.
3.  Limit youngest users' social media access and pair relaxed access over time with responsible engagement training.

## Interactive visualisation

An interactive plot covering all 21 studied predictors, including country effects [is available](https://rawcdn.githack.com/jakkada/I4NG-Datathon-2026/fd1b6cf441be7ba63a5edc4665fa16d0f188ac5a/supl_p3_interactive.html).

## Data sources

- [CRONOS3 Wave 1](https://doi.org/10.21338/cronos3-w1)
- [CRONOS3 Wave 2](https://doi.org/10.21338/cronos3-w2)
- [CRONOS3 Wave 3](https://doi.org/10.21338/cronos3-w3)
- [CRONOS3 Wave 4](https://doi.org/10.21338/cronos3-w4)
- [CRONOS3 Wave 5](https://doi.org/10.21338/cronos3-w5)
- [ESS10](https://doi.org/10.21338/ess10e03_3)
- [ESS10sc](https://doi.org/10.21338/ess10sce03_2)
- [ESS11](https://doi.org/10.21338/ess11e04_1)

Countries covered: Austria, Belgium, Czechia, Finland, France, Hungary, Iceland, Poland, Portugal, Slovenia, and the United Kingdom. Total unweighted sample: n = 10,081.

## Reproducibility

The analysis is fully reproducible in GNU R. The repository contains:

-   `1-load-data.R` — data loading
-   `2-data-preparation.R` — data preparation
-   `3-modeling.R` — logistic regression modelling
-   `4-results-brief.Rmd` — policy brief (knits to PDF)

## Authors

**Jakub Adamski** — [jakub.adamski\@uni.lodz.pl](mailto:jakub.adamski@uni.lodz.pl)\
**Remigiusz Żulicki** — [remigiusz.zulicki\@uni.lodz.pl](mailto:remigiusz.zulicki@uni.lodz.pl)\
[Faculty of Economics and Sociology](https://www.eksoc.uni.lodz.pl/en/about-the-faculty/get-to-know-eksoc), University of Lodz, Poland

## License

This work is licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

![](by.png)
