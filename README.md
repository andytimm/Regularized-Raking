# Regularized-Raking

This repo contains materials for my talk at the [New York Open Statistical Programming](https://nyhackr.org/) meetup. I'll update this page with a link to the talk recording once it's online.

**Raking** is among the most commonly used algorithms for building survey weights such that a unrepresentative sample can be used to make inferences about the general population.

**Regularized Raking** extends this framework,
allowing for more explicit and granular tradeoffs to be made on properties of the weights set. These properties include how closely the (weighted) sample adheres to population totals, by which distance "closeness" is measured, and how strongly regularized the weights distribution is.

---

There's a PDF of the slides, but to reproduce the code and follow along:
1. Install rsw using the instructions here: https://github.com/cvxgrp/rsw/tree/master
2. Grab the entire reproduction capsule from [Chapter 4 of "Target Estimation and Adjustment Weighting for Unrepresentative Survey Samples"](https://codeocean.com/capsule/4173151/tree/v1), from which I (very gratefully) borrow some example polling data and preprocessing functions. Place it in the themed folder as is.
3. Run `search_lambdas.qmd`, optionally modifying the parameters to determine how much search is done if you're curious.
4. Run `themed.qmd` to reproduce the full analysis and slides.


