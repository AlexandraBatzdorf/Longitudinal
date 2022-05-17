## Alexandra Batzdorf, Analysis of Longitudinal Data in R Example, 2022
# Required R packages: suddengains, reshape2, dplyr, lme4, ggplot2, glmmTMB, 
# sjPlot, MASS, lattice, cowplot, ggpubr, multcomp, viridis



# Load longitudinal example data meant to illustrate changes in depression levels
# throughout treatment among people with major depressive disorder (MDD) from the 
# suddengains package. Create a dataframe with a subset of the dataset including 
# Beck Depression Inventory (BDI) scores from four study visits.
{if (!require(suddengains)) {install.packages("suddengains"); library(suddengains)}
  sg.example.bdi <- data.frame(ID=c(sgdata$id), baseline=c(sgdata$bdi_s0),
                               FL1=c(sgdata$bdi_s3), FL2=c(sgdata$bdi_s6),
                               FL3=c(sgdata$bdi_s12))}
# Transform the dataframe from wide format to long format. Add a column specifying
# the visit in number of months from baseline--e.g., 0, 6, 12, and 24 months for
# baseline, follow-up 1, follow-up 2, and follow-up 3, respectively. Export this 
# as a .csv file.
{if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
  sg.example <- melt(sg.example.bdi, id.vars="ID")
  colnames(sg.example) <- c("ID", "timepoint", "BDI")
  factor(sg.example$timepoint, ordered=T, levels=c("baseline", "FL1", "FL2", "FL3"))
  if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  sg.example <- 
    sg.example %>%
    mutate(visit_months=case_when(timepoint=="baseline" ~ 0, timepoint=="FL1" ~ 6,
                                  timepoint=="FL2" ~ 12, timepoint=="FL3" ~ 24), 
           .after=timepoint)
  dir.create("~/Longitudinal_Analysis_Output")
  output.path <- "~/Longitudinal_Analysis_Output"; setwd(output.path)
  write.csv(sg.example, file=file.path(paste0(output.path, 
                                       "/SG_Example_Long_RawValues.csv")), 
            na="", row.names=F)
  head(sg.example)}


# Fit a linear mixed-effects model optimizing the restricted maximum likelihood 
# (REML) criterion, modeling visit timepoint as a fixed effect and participant 
# as a random effect. Using na.exclude, pad missing values with NA so that pairwise 
# deletion is utilized within each contrast, rather than deleting the entire row of 
# observations for a given participant (listwise deletion).
{if (!require(lme4)) {install.packages("lme4"); library(lme4)}
bdi.model <- lmer(BDI ~ timepoint + (1|ID), data=sg.example, na.action=na.exclude)}


# Before evaluating the models, check that the data do not violate model 
# assumptions.
# Set plot theme elements.
{if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
  Theme <- theme_classic() + 
    theme(text=element_text(size=10, family="sans", color="black", face="bold"),
          plot.title=element_text(hjust=0.5), strip.background=element_blank(),
          strip.text.x=element_text(size=10, family="sans", color="black",
                                    face="bold", hjust=0.5),
          line=element_line(color="black"), legend.position=c(0,1.03),
          axis.text=element_text(size=10, family="sans", color="black",
                                 face="bold"), legend.title=element_blank(), 
          axis.ticks=element_line(size=1, color="black"),
          panel.border=element_rect(color="black", fill=NA, size=2), 
          legend.key=element_blank(), legend.background=element_blank(),
          legend.text=element_text(size=10, family="sans", color="black",
                                   face="bold"),
          legend.justification=c(0,1), aspect.ratio=1, 
          plot.caption=element_text(size=10, family="sans", color="black",
                                    face="bold", hjust=0))}


# Plot the fitted values against the residuals to evaluate linearity and 
# homoscedasticity.
{if (!require(glmmTMB)) {install.packages("glmmTMB"); library(glmmTMB)}
  if (!require(sjPlot)) {install.packages("sjPlot"); library(sjPlot)}
  bdi.diag <- get_model_data(bdi.model, type="diag", pred.type="re")
  bdi.fitres <-
  ggplot(bdi.diag[[4]], aes(x=fitted, y=res)) +
    geom_smooth(na.rm=T, size=1.5, color="black", fullrange=T) +
    geom_point(na.rm=T, size=2, color="#7F0001", fill="red", pch=21, alpha=0.7) +
    Theme +
    scale_x_continuous(expand=c(0, 0), limits=c(7, 40.25)) +
    scale_y_continuous(expand=c(0, 0), limits=c(-19, 19)) +
    labs(x="Fitted Values (Beck Depression Inventory)", y="Residuals", 
         title="Linear and Homoscedastic")
  bdi.fitres}
# View a density histogram of the residuals, with a normal distribution curve
# overlaid for reference.
{if (!require(MASS)) {install.packages("MASS"); library(MASS)}
  resids <- data.frame(res=c(na.exclude(residuals(bdi.model))))
  params <- as.list(MASS::fitdistr(resids$res, "normal")$estimate)
  bdi.histres <-
  ggplot(bdi.diag[[3]], aes(x=res)) +
    geom_density(fill="red", color="#7F0001", alpha=0.5, na.rm=T, size=1, adjust=1) +
    stat_function(aes(linetype="Normal Distribution"), fun=dnorm, 
                  args=list(mean=params$mean, sd=params$sd), size=1.5) +
    Theme + 
    scale_x_continuous(expand=c(0, 0), limits=c(-19, 19)) +
    scale_y_continuous(expand=c(0, 0), limits=c(-0.001, 0.101)) +
    labs(x="Residuals", y="Density", title="Normally Distributed") +
    scale_linetype_manual(labels="Normal Distribution", values=2, name="") +
    guides(linetype=guide_legend(override.aes=list(linetype=c(3), size=c(1)))) 
  bdi.histres}
# For a better sense of whether the residuals are normally distributed, view a
# quantile-quantile plot.
{if (!require(lattice)) {install.packages("lattice"); library(lattice)}
  std.res <- qqmath(bdi.model)
  bdi.qqres <-
  ggplot(as.data.frame(std.res$panel.args[[1]]), aes(sample=x)) +
    geom_qq_line(aes(linetype="Normal Distribution"), fullrange=T, size=1.5, 
                 color="black", na.rm=T) +
    geom_qq(size=2, color="#7F0001", fill="red", pch=21, alpha=0.7, na.rm=T) +
    Theme + 
    labs(x="Normal Theoretical Quantiles", y="Standardized Residuals", 
         title="Normally Distributed, Slightly Dispersed") +
    scale_x_continuous(limits=c(-3.1, 3.1), expand=c(0, 0)) +
    scale_y_continuous(limits=c(-3.1, 3.1), expand=c(0, 0)) +
    scale_linetype_manual(labels="Normal Distribution", values=2, name="") +
    guides(linetype=guide_legend(override.aes=list(linetype=c(3), size=c(1)))) 
  if (!require(cowplot)) {install.packages("cowplot"); library(cowplot)}
  bdi.assumptions <- align_plots(bdi.fitres, bdi.histres, bdi.qqres, 
                                       align="hv", axis="tblr")
  if (!require(ggpubr)) {install.packages("ggpubr"); library(ggpubr)}
  bdi.model.assumptions <- ggarrange(bdi.assumptions[[1]], 
                                     bdi.assumptions[[2]],
                                     bdi.assumptions[[3]], nrow=1)
  ggexport(bdi.model.assumptions, filename="BDI_Model_Assumptions.pdf", 
           height=4, width=12)
  bdi.qqres}


# The quantile-quantile plot indicates that the model residuals are slightly more
# dispersed than a normal distribution, but more or less normally distributed.


# Evaluate the general linear hypotheses of the model. Test the contrasts between
# consecutive visits (e.g., between follow-up visit 1 and follow-up visit 2) and
# overall (i.e., between baseline and follow-up visit 3). 
# Utilize a single-step procedure to generate adjusted P-values and simultaneous 
# 95% CIs for the model based on its familywise error rate.
{if (!require(multcomp)) {install.packages("multcomp"); library(multcomp)}
  bdi.glht <- glht(bdi.model, linfct=mcp(timepoint=c("FL1-baseline=0", 
                                                     "FL2-FL1=0",
                                                     "FL3-FL2=0", 
                                                     "FL3-baseline=0")))
  p.vals <- c(format.pval(summary(bdi.glht)$test$pvalues, digits=2, eps=0.001,
              nsmall=2))
  conf.ints.wide <- data.frame(confint(bdi.glht)$confint)
  conf.ints.wide <- format(round(conf.ints.wide, digits=2), nsmall=2)
  conf.ints <- c(paste(conf.ints.wide$lwr, conf.ints.wide$upr, sep=", "))
  within.pt <- data.frame(Months=c("0 to 6", "6 to 12", "12 to 24", "0 to 24"), 
                          PValue=c(p.vals),
                          Confidence_Interval=c(conf.ints))
  write.csv(within.pt, file=file.path(paste0(output.path, 
                                              "/Within_Participant_Results.csv")), 
            na="", row.names=F)
  print(within.pt)}


# Create a spaghetti plot of patients' raw BDI scores over time, with each colored 
# line representing one participant. Superimpose a summary line (black) of the 
# mean BDI score at each timepoint, with error bars representing 95% CIs.
{caption <- expression(bold('Error bars indicate mean +/- 95% ')*bolditalic('CI.'))
  if (!require(viridis)) {install.packages("viridis"); library(viridis)}
  patient.bdi.scores <-
  ggplot(sg.example, aes(x=visit_months, y=BDI)) +
    geom_path(aes(group=ID, color=ID), alpha=0.6, na.rm=T) +
    geom_point(aes(color=ID), alpha=0.6, na.rm=T) +
    stat_summary(fun="mean", geom="path", size=2, na.rm=T) +
    stat_summary(fun="mean", size=0.7, na.rm=T) +
    scale_color_viridis(option="inferno", begin=0.1, end=0.8) +
    Theme +
    theme(legend.position="none") +
    scale_x_continuous(breaks=c(0, 6, 12, 24), limits=c(-1, 25), expand=c(0, 0)) +
    scale_y_continuous(limits=c(-1, 61), expand=c(0, 0)) +
    labs(x="Visit (Months)", y="Beck Depression Inventory Score",
         caption=caption) 
  patient.bdi.scores <- add_summary(patient.bdi.scores, fun="mean_ci", 
                                    error.plot="errorbar") 
  ggexport(patient.bdi.scores, filename="Patient_BDI_Scores.pdf", 
           height=3.5, width=3.5)
  patient.bdi.scores}
  

# Compare longitudinal patient data to healthy control data collected during a
# single study visit.
# Generate example data for ~43 healthy controls.
{bdi.controls <- round(rnorm(43, mean=7, sd=3))
  bdi.controls <- bdi.controls[bdi.controls>=0]
  length(bdi.controls) <- max(length(bdi.controls), length(sg.example.bdi$ID))
  sg.example.bdi$controls <- bdi.controls
  sg.example.ctrls <- melt(sg.example.bdi, id.vars="ID")
  colnames(sg.example.ctrls) <- c("ID", "timepoint", "BDI")
  bdilabs <- c('baseline'="0 Months", 'FL1'="6 Months", 'FL2'="12 Months", 
               'FL3'="24 Months", 'controls'="Controls")
  head(sg.example.bdi)}


# Compare the patient data at each timepoint with the healthy control data using
# Student's t-test (or Welch's, in cases with unequal variances).
# First, check if the data meet t-test assumptions.
{t.test.dists <-
  ggplot(sg.example.ctrls, aes(x=BDI)) +
    geom_density(fill="red", color="#7F0001", alpha=0.5, na.rm=T, size=1) +
    facet_wrap(~timepoint, labeller=as_labeller(bdilabs)) +
    geom_line(aes(y=dnorm(BDI, mean=tapply(BDI, timepoint, mean, na.rm=T)[PANEL],
                          sd=tapply(BDI, timepoint, sd, na.rm=T)[PANEL]),
                  linetype="Normal Distribution"), na.rm=T, size=1) +
    Theme + 
    labs(x="Beck Depression Inventory Score", y="Density", 
         title="Reasonably Normally Distributed") +
    scale_x_continuous(limits=c(-1, 61), expand=c(0, 0)) +
    scale_y_continuous(limits=c(-0.003, 0.203), expand=c(0, 0)) +
    scale_linetype_manual(labels="Normal Distribution", values=2, name="") +
    guides(linetype=guide_legend(override.aes=list(linetype=c(3), size=c(1)))) 
  ggexport(t.test.dists, filename="t_Test_Assumptions.pdf", 
           height=6, width=8)
  t.test.dists + theme(legend.text=element_text(size=8))}


# The sample distributions appear reasonably normal. Additionally, evidence 
# suggests that the population distribution of BDI scores among those with MDD 
# is likely normal (e.g., Veerman et al., 2018).


# Conduct the t-tests.
{bl <- t.test(sg.example.bdi$baseline, bdi.controls)
  fl1 <- t.test(sg.example.bdi$FL1, bdi.controls)
  fl2 <- t.test(sg.example.bdi$FL2, bdi.controls)
  fl3 <- t.test(sg.example.bdi$FL3, bdi.controls)}


# For use with a large number of outcome measures, calculate the false discovery
# rate to adjust P-values for multiple comparisons. 
{p.vals.orig <- c(bl$p.value, fl1$p.value, fl2$p.value, fl3$p.value)
  p.vals.fdr <- p.adjust(p.vals.orig, method="fdr")
  p.comp <- cbind(Month=c(0, 6, 12, 24), Original.P.Value=p.vals.orig, 
                  Adjusted.P.Value=p.vals.fdr)
  p.vals.fdr <- c(format.pval(p.vals, digits=2, eps=0.001, nsmall=2))
  print(p.comp, digits=3)}


# Calculate the false coverage-statement rate (FCR) to adjust corresponding 
# confidence intervals.
# In this case, all t-tests yield significant results after adjustment 
# (meaning there is an equal number of selected and considered parameters), so 
# the FCR-adjusted confidence level is just 95%.
{adj.conf.level <- 1 - (4 * 0.05 / 4)
  bl.adj <- t.test(sg.example.bdi$baseline, bdi.controls, 
                   conf.level=adj.conf.level)
  fl1.adj <- t.test(sg.example.bdi$FL1, bdi.controls, 
                    conf.level=adj.conf.level)
  fl2.adj <- t.test(sg.example.bdi$FL2, bdi.controls, 
                    conf.level=adj.conf.level)
  fl3.adj <- t.test(sg.example.bdi$FL3, bdi.controls, 
                    conf.level=adj.conf.level)
  conf.ints.adj.wide <- rbind(bl.adj$conf.int, fl1.adj$conf.int, 
                                   fl2.adj$conf.int, fl3.adj$conf.int)
  conf.ints.adj.wide <- data.frame(format(round(conf.ints.adj.wide, digits=2), 
                                          nsmall=2))
  conf.ints.adj <- c(paste(conf.ints.adj.wide$X1, conf.ints.adj.wide$X2, sep=", "))
  t.stats <- c(bl.adj$statistic, fl1.adj$statistic, fl2.adj$statistic, 
               fl3.adj$statistic)
  t.stats <- data.frame(tStatistic=format(round(t.stats, digits=2), nsmall=2))
  between.pt <- data.frame(Comparison=c("0 Months vs. Controls", 
                                       "6 Months vs. Controls", 
                                       "12 Months vs. Controls", 
                                       "24 Months vs. Controls"), 
                           tStatistic=t.stats,
                           PValue=c(p.vals.fdr),
                           Confidence_Interval=c(conf.ints))
  write.csv(between.pt, file=file.path(paste0(output.path, 
                                             "/Between_Participant_Results.csv")), 
            na="", row.names=F)
  print(between.pt)}


# Plot the results.
# First, calculate each participant's percent difference from baseline for each 
# study visit, for ease of comparing measures when using multiple outcome 
# variables (i.e., to create comparable y-axes between graphs).
{FL1.perc.diff <- (sg.example.bdi$FL1 - sg.example.bdi$baseline)/
    abs(sg.example.bdi$baseline)*100
  FL2.perc.diff <- (sg.example.bdi$FL2 - sg.example.bdi$baseline)/
    abs(sg.example.bdi$baseline)*100
  FL3.perc.diff <- (sg.example.bdi$FL3 - sg.example.bdi$baseline)/
    abs(sg.example.bdi$baseline)*100
  ctrl.perc.diff <- (mean(bdi.controls, na.rm=T) - 
                       mean(sg.example.bdi$baseline, na.rm=T))/
    abs(mean(sg.example.bdi$baseline, na.rm=T))*100
  ctrl.perc <- data.frame(Months=c(0, 6, 12, 24), BDI.perc=c(ctrl.perc.diff))
  bdi.perc.wide <- data.frame(ID=c(sg.example.bdi$ID), baseline.perc=c(0), 
                              FL1.perc=c(FL1.perc.diff), FL2.perc=c(FL2.perc.diff),
                              FL3.perc=c(FL3.perc.diff))
  bdi.perc <- melt(bdi.perc.wide, id.vars="ID")
  colnames(bdi.perc) <- c("ID", "Months", "BDI.perc")
  bdi.perc <- 
    bdi.perc %>%
    mutate(Months=case_when(Months=="baseline.perc" ~ 0, Months=="FL1.perc" ~ 6,
                            Months=="FL2.perc" ~ 12, Months=="FL3.perc" ~ 24))
  write.csv(bdi.perc, file=file.path(paste0(output.path, 
                                     "/SG_Example_Long_PercentDifference.csv")), 
            na="", row.names=F)
  head(bdi.perc)}


# Create the graphs, with points and error bars representing means and 95% CIs. 
# Add a red reference line for healthy Controls values.
{perc.diff.bdi <-
  ggplot(bdi.perc, aes(x=Months, y=BDI.perc)) +
    stat_summary(aes(color="Patients", linetype="Patients"), fun.data=mean_se, 
                 geom="line", na.rm=T, size=1) +
    geom_line(data=ctrl.perc, aes(color="Controls", linetype="Controls"), size=1) +
    Theme + theme(legend.position=c(1, 1.05), legend.justification=c(1, 1)) +
    scale_x_continuous(breaks=c(0, 6, 12, 24), expand=c(0.0035, 0.0035), 
                       limits=c(0, 24.9)) +
    scale_linetype_manual(name="Group", values=c("Patients"=1, "Controls"=2)) +
    scale_color_manual(name="Group", values = c("Patients"="black", 
                                                "Controls"="red")) +
    scale_fill_manual(name="Group", values = c("Patients"="red", "Controls"=NA)) +
    scale_alpha_manual(name="Group", values=c("Patients"=1, "Controls"=0)) +
    guides(linetype=guide_legend(override.aes=list(linetype=c(1, 3),
                                                           fill=c("red", NA),
                                                           shape=c(21, NA)))) +
    labs(y="Beck Depression Inventory Score,\nPercent Difference from Baseline", 
         x="Visit (Months)", caption=caption) 
perc.diff.bdi <- add_summary(perc.diff.bdi, color="black", fun="mean_ci", 
                               error.plot="errorbar") 
  perc.diff.bdi <- perc.diff.bdi + 
    stat_summary(data=subset(bdi.perc, Months>0), fun.data=mean_se, geom="point", 
                 aes(fill="Patients", color="Patients"), na.rm=T, size=2.25, pch=21)
  ggexport(perc.diff.bdi, filename="Percent_Difference_BDI.pdf", 
           height=3.5, width=3.5)
  perc.diff.bdi}







