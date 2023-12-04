################################################################################
#### WEIGHTING A 2016 PRE-ELECTION SURVEY ######################################
################################################################################

## SETUP

### Packages
library(tidyverse)                      # for useful utilities
library(survey)                         # for analyzing complex surveys

### Function for creating targets from auxiliary information and formula
create_targets <- function (target_design, target_formula) {
    target_mf <- model.frame(target_formula, model.frame(target_design))
    target_mm <- model.matrix(target_formula, target_mf)
    wts <- weights(target_design)
    colSums(target_mm * wts) / sum(wts)
}

## SURVEY DATA (PEW)

### Load
pew <- readRDS("../data/pew.rds")

### Make survey design 
pew_srs <- svydesign(ids = ~1, data = pew)

### Unweighted survey estimates of presidential vote
svymean(~recode_vote_2016, design = pew_srs)
vote_contrast <- quote((recode_vote_2016Democrat - recode_vote_2016Republican) /
                       (recode_vote_2016Democrat + recode_vote_2016Republican))
svycontrast(svymean(~recode_vote_2016, pew_srs), vote_contrast)

### Auxiliary variables
svymean(~recode_female, design = pew_srs)
svymean(~recode_age_bucket, design = pew_srs)
svymean(~recode_race, design = pew_srs)
svymean(~recode_region, design = pew_srs)
svymean(~recode_educ, design = pew_srs)

## AUXILIARY INFORMATION (CCES)

### Load
cces <- readRDS("../data/cces.rds")

### Drop invalid cases
cces <- cces %>%
    filter((CC16_401 == "I definitely voted in the General Election.") &
           !is.na(commonweight_vv_post))

### Make survey design
cces_awt <- svydesign(ids = ~1, weights = ~commonweight_vv_post, data = cces)

### Presidential vote estimates
#### National
svymean(~recode_vote_2016, design = cces_awt, na.rm = TRUE)
svycontrast(svymean(~recode_vote_2016, cces_awt, na.rm = TRUE), vote_contrast)
#### State
svyby(~I(as.numeric(recode_vote_2016 == "Democrat")), ~recode_inputstate,
      design = cces_awt, svymean, na.rm = TRUE, keep.var = FALSE)

### Auxiliary variables
svymean(~recode_female, design = cces_awt)
svymean(~recode_age_bucket, design = cces_awt)
svymean(~recode_race, design = cces_awt)
svymean(~recode_region, design = cces_awt)
svymean(~recode_educ, design = cces_awt)

## POPULATION TARGETS

### Formulas for auxiliary vector

#### (1) Marginal distributions of age, female, race, and region
formula_1 <- ~recode_age_bucket + recode_female + recode_race + recode_region
#### (2) Marginal distributions of age, female, race, region, and education
formula_2 <- ~recode_age_bucket + recode_female + recode_race + recode_region +
    recode_educ
#### (3) Joint distribution of age (coarsened), female, race, region, and
#### education (coarsened)
formula_3 <- ~recode_age_3way * recode_female * recode_race *
    recode_region * recode_educ_3way
table(cces$recode_age_3way, cces$recode_age_bucket)
table(cces$recode_educ_3way, cces$recode_educ)
#### (4) Marginal distributions of age, female, and race and, among whites,
#### the joint distribution of region and education
#### (recode_race_educ_reg = race * educ * reg if race == "white" and
#### race * reg otherwise)
formula_4 <- ~recode_age_bucket + recode_female + recode_race_educ_reg

### Population targets
targets_1 <- create_targets(cces_awt, formula_1)
targets_2 <- create_targets(cces_awt, formula_2)
targets_3 <- create_targets(cces_awt, formula_3) # will have to modify below
targets_4 <- create_targets(cces_awt, formula_4)

### Weighted survey designs
#### (1)
pew_lwt_1 <- calibrate(design = pew_srs,
                       formula = formula_1,
                       population = targets_1,
                       calfun = "linear")

#### (2)
pew_lwt_2 <- calibrate(design = pew_srs,
                       formula = formula_2,
                       population = targets_2,
                       calfun = "linear")

#### (3)
##### Can't compute below because some cells are empty.
try(pew_lwt_3 <- calibrate(design = pew_srs,
                           formula = formula_3,
                           population = targets_3,
                           calfun = "linear"),
    silent = TRUE)
##### So instead we use the `postStratify` function with `partial = TRUE`, which
##### ignores empty cells.
formula_3_ps <- as.formula(str_replace_all(formula_3, "\\*", "+"))
targets_3_ps <- svytable(formula = formula_3_ps, design = cces_awt)
sum(svytable(formula_3_ps, pew_srs) == 0) # 265 empty cells
pew_ps_3 <- postStratify(design = pew_srs,
                         strata = formula_3_ps,
                         population = targets_3_ps,
                         partial = TRUE) # ignores empty cells

#### (4)
pew_lwt_4 <- calibrate(design = pew_srs,
                       formula = formula_4,
                       population = targets_4,
                       calfun = "linear")

## Examine weights

#### Summarize and compare 
wts <- data.frame(wt1 = weights(pew_lwt_1) / mean(weights(pew_lwt_1)),
                  wt2 = weights(pew_lwt_2) / mean(weights(pew_lwt_2)),
                  wt3 = weights(pew_ps_3) / mean(weights(pew_ps_3)),
                  wt4 = weights(pew_lwt_4) / mean(weights(pew_lwt_4)))

sapply(wts, summary)
sapply(wts, sd)

#### Boxplots
wts %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = name, y = value)) +
    geom_boxplot()

#### Negative weights 
pew_lwt_2$variables %>%
    filter(weights(pew_lwt_2) < 0) %>%
    group_by(recode_race_educ) %>%
    summarise(count = n())
pew_lwt_4$variables %>%
    filter(weights(pew_lwt_4) < 0) %>%
    group_by(recode_race_educ) %>%
    summarise(count = n())

## Compare marginal distributions across weighting schemes

aux_comp <- data.frame(
    cces = svymean(formula_2, design = cces_awt),
    pew0 = svymean(formula_2, design = pew_srs),
    pew1 = svymean(formula_2, design = pew_lwt_1),
    pew2 = svymean(formula_2, design = pew_lwt_2),
    pew3 = svymean(formula_2, design = pew_ps_3),
    pew4 = svymean(formula_2, design = pew_lwt_4))
print(aux_comp, digits = 2)

var_rex <- paste0("(", all.vars(formula_2), ")", collapse = "|")
aux_comp %>%
    select_at(vars(-matches("SE"))) %>%
    rename_all(function (x) str_remove_all(x, ".mean")) %>%
    rename_all(function (x) str_replace(x, "pew", "Pew ")) %>%
    rownames_to_column("Margin") %>%
    mutate(Variable = as.character(str_extract_all(Margin, var_rex)),
           Variable = str_remove_all(Variable, "(recode\\_)|(\\_bucket)"),
           Margin = str_remove_all(Margin, var_rex)) %>%
    mutate_if(is.numeric, list(~format(round(100 * ., 1), nsmall = 1))) %>%
    write.csv("aux_comp.csv", row.names = FALSE, quote = FALSE)

## Presidential vote estimates

### Actual results
pres <- readRDS("../data/election.rds")

natl_margin <- pres %>%
    summarise(margin = (sum(demtotal) - sum(reptotal)) /
                  (sum(demtotal) + sum(reptotal))) %>%
    as.numeric()
natl_margin

### Compare estimates
comp_df <- data.frame(
    CCES = svycontrast(svymean(~recode_vote_2016, cces_awt, na.rm = TRUE),
                       vote_contrast),
    Pew_0 = svycontrast(svymean(~recode_vote_2016, pew_srs, na.rm = TRUE),
                        vote_contrast),
    Pew_1 = svycontrast(svymean(~recode_vote_2016, pew_lwt_1, na.rm = TRUE),
                        vote_contrast),
    Pew_2 = svycontrast(svymean(~recode_vote_2016, pew_lwt_2, na.rm = TRUE),
                        vote_contrast),
    Pew_3 = svycontrast(svymean(~recode_vote_2016, pew_ps_3, na.rm = TRUE),
                        vote_contrast),
    Pew_4 = svycontrast(svymean(~recode_vote_2016, pew_lwt_4, na.rm = TRUE),
                        vote_contrast)) %>%
    pivot_longer(cols = everything(),
                 names_to = c("source", ".value"), 
                 names_pattern = "(.*)\\.(.*)") %>%
    rename(est = nlcon) %>%
    mutate(err = est - natl_margin,
           source = str_replace(source, "_", " "))
comp_df

### Plot results 
pdf("../results/national_margin.pdf", width = 5, height = 4)
comp_df %>%
    ggplot() +
    aes(x = source, y = est, ymin = est - 1.96*SE, ymax = est + 1.96*SE) +
    geom_pointrange() +
    geom_hline(yintercept = c(0, natl_margin),
               linetype = c("solid", "dashed")) +
    scale_y_continuous(breaks = c(natl_margin, seq(-.05, .1, .05)),
                       minor_breaks = NULL,
                       labels = scales::percent_format()) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = NULL, y = "Estimated Margin (95% CI)") +
    ggtitle("Estimates of Clinton National Popular Vote Margin")
dev.off()


