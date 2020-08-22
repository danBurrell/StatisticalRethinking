# The Golem of Prague {#theGolemOfPrague}

Statistical models are like powerful yet dangerous robots. They will do their task, but they have no discernment as to whether their task is appropriate. For their particular task, they are good. But set to work on a task they're not intended for, they yield untrustworthy results. 

Classical statistical methods are fragile and inflexible. They are inflexible in that they are exhibit very limited adaptability to unique research contexts. They are fragile in that they fail in unpredictable ways when applied to new contexts. This is important because at the cutting edge of research it's rarely ever clear which procedure is appropriate. 

McElreath's point is that classical statistical tools are not diverse enough to handle many common research problems. Moreover, statistical tools on their own only understand association, and can tell us nothing about cause and effect. So, rather than having a tool kit of statistical "robots" and risk their misapplication, what is needed is a unified set of engineering principles for designing, constructing and refining purpose-built statistical procedures. 

Some thoughts on this:

* You need to understand how a statistical procedure processes information in order to be able to reasonably interpret its output; 
* detailed knowledge is a requirement; 
* getting greasy under the hood is how you cultivate this sort of deep understanding --- doing the computations the hard way (at least initially);
* we also need a statistical epistemology --- an appreciation of how statistical models relate to hypotheses and the natural mechanisms of interest. 
* we need to do away with the null hypothesis significance testing mindset that is born from unscrupulous application of Popper's falsificationism; good science is done by developing statistical procedures that can falsify hypotheses, but this ought to be done thoughtfully and with adequate insight into the limitations of deductive falsification. 

McElreath argues that deductive falsification is impossible in nearly every scientific context because:

1. Hypotheses are not models. There is not a one-to-one correspondence between hypotheses and models, so that strict falsification is impossible. 
2. Measurement matters. Even when we think our data do falsify a model, the trustworthiness (or representativeness) of the data is always up for debate.

The upshot is that the scientific method cannot be reduced to a statistical procedure; and hence our scientific methods ought not pretend. That's the arrow shot at the heart of null hypothesis significance testing (NHST), which is often identified with falsificationist, or Popperian, philosophy of science. NHST is used to falsify a null hypothesis, not the actual research hypothesis, so the "falsification" doesn't even truly pertain to the explanatory model; a kind of reversal of Popper's philosophy. 

## Hypotheses are not models

We may wish to test a hypothesis but we actually substitute the hypothesis with a surrogate: a model that attempts to make operational, insofar as is possible, the essence of the hypothesis. Models are not truth, they merely attempt to capture aspects of nature so as to reflect back useful information. If models are false (albeit perhaps useful), what does it even mean to falsify a model? By working with models we are in no position to conclude anything about the truth or falsity of the hypotheses underlying them; we can only say that a model supports the hypothesis or it doesn't. 

A model of the process that moves one from a hypothesis to a statistical model capable of being confronted with data is as follows:

1. Hypothesis: characterized by vague boundaries that begin as verbal conjectures
2. Process model: making choices about competing processes that fall within the vague boundaries of the verbal conjecture. Importantly, process models express causal structure. 
3. Statistical model: to challenge a process model with data it needs to be translated into statistical models, and a side-effect of this is that statistical models do not embody specific causal relationships, only associations among variables.This translation from process to statistical model is many-to-one: many different process models may be consistent with a single statistical model. Moreover, statistical models can be confused by unobserved variables (mediators) and sampling bias. Process models allow us to design statistical models with these problems in mind - we need both process and statistical model.

Uncomfortable lesson:

1. Any given statistical model (M) may correspond to more than one process model (P).
2. Any given hypothesis (H) may correspond to more than one process model (P).
3. Any given statistical model (M) may correspond to more than one hypothesis (H). 

Many common distributions --- exponential family distributions --- are maximum entropy distributions. 

## Measurement matters

The logic of falsificationism is simple: we have a hypothesis H, and we show that it entails some observation D. Then we look for D. If we don't find it, we must conclude that H is false --- logically this is *modus tollens* (the method of destruction). In contrast, find D tells us nothing about H, because other hypotheses might also predict D. 

Seeking disconfirming evidence is important, but it suffers other problems in addition to the correspondence problems among hypotheses and models discussed earlier. In particular, observations are prone to error, and most hypotheses live on a continuum of degree, rather than on a discrete dichotomous space of absolutes. 

### Observaqtion error

Especially at the edges of scientific knowledge, the ability to measure a hypothetical phenomenon is often in question as much as the phenomenon itself. Finding disconfirming cases is complicated by the difficulties of observation: false positives (mistaken confirmations) and false negatives (mistaken disconfirmations). This induces a key dilemma: whethr or not a falsification is real or spurious. Measurement is complicated in both cases, but in quite different ways, rendering both true-detection and false-detection plausible. 

### Continuous hypotheses

Most interesting hypotheses are not of an absolute nature, but concern degrees: not "all swans are white", but "80\% of swans are white", or "balck swans are rare". The task is not to prove or disprove but to estimate and explain the distribution of swan coloration as accurately as we can. This problem prevents strict application of *modus tollens*.  Nearly everyone agrees that it is a good practice to design experiments and observations that can differentiate competing hypotheses, but in many cases, the comparison must be probabilistic, a matter of degree, not kind. 

## Tools for golem engineering

We want to use our models for several distinct purposes: designing inquiry, extracting information from data, and making predictions. Tools for this are:

1. Bayesian data analysis: essentially counting the number of ways the data could happen, according to our assumptions; things that can happen more ways are more plausible. 
2. Model comparison: based on expected predictive accuracy (cross-validation and information criteria); overfitting issue
3. Multilevel models: It's parameters all the way down: any particular parameter can be usefully regarded as a placeholder for a missing model; given some model of how the parameter gets its value, it's simple enough to embed the new model inside the old one --- resulting in a model with multiple levels of uncertainty that trickle down the levels (also called hierarchical; random effects; varying effects; mixed effects models); incidentally, while corss-validation and information criteria measure overfitting risk, multilevel models help do something about it via partial pooling of information across the data in order to produce better estimates for all units. Multilevel regression deserves to be the default form of regression. 
4. Graphical causal models: A statistical model is an amazing association engine, but is insufficient for inferring cause. Models that are causally incorrect can make better predictions than those that are causally correct. Hence, focussing on prediction can systematically mislead us. We need a causal model that can be used to design one or more statistical models for the purpose of causal identification. Graphical causal models allow us to deduce which statistical models can provide valid causal inferences, assuming the DAG is true. 









You can label chapter and section titles using `{#label}` after them, e.g., we can reference Chapter \@ref(intro). If you do not manually label them, there will be automatic labels anyway, e.g., Chapter \@ref(methods).

Figures and tables with captions will be placed in `figure` and `table` environments, respectively.


```r
par(mar = c(4, 4, .1, .1))
plot(pressure, type = 'b', pch = 19)
```

<div class="figure" style="text-align: center">
<img src="01-theGolemOfPrague_files/figure-epub3/nice-fig-1.png" alt="Here is a nice figure!" width="80%" />
<p class="caption">(\#fig:nice-fig)Here is a nice figure!</p>
</div>

Reference a figure by its code chunk label with the `fig:` prefix, e.g., see Figure \@ref(fig:nice-fig). Similarly, you can reference tables generated from `knitr::kable()`, e.g., see Table \@ref(tab:nice-tab).


```r
knitr::kable(
  head(iris, 20), caption = 'Here is a nice table!',
  booktabs = TRUE
)
```



Table: (\#tab:nice-tab)Here is a nice table!

| Sepal.Length| Sepal.Width| Petal.Length| Petal.Width|Species |
|------------:|-----------:|------------:|-----------:|:-------|
|          5.1|         3.5|          1.4|         0.2|setosa  |
|          4.9|         3.0|          1.4|         0.2|setosa  |
|          4.7|         3.2|          1.3|         0.2|setosa  |
|          4.6|         3.1|          1.5|         0.2|setosa  |
|          5.0|         3.6|          1.4|         0.2|setosa  |
|          5.4|         3.9|          1.7|         0.4|setosa  |
|          4.6|         3.4|          1.4|         0.3|setosa  |
|          5.0|         3.4|          1.5|         0.2|setosa  |
|          4.4|         2.9|          1.4|         0.2|setosa  |
|          4.9|         3.1|          1.5|         0.1|setosa  |
|          5.4|         3.7|          1.5|         0.2|setosa  |
|          4.8|         3.4|          1.6|         0.2|setosa  |
|          4.8|         3.0|          1.4|         0.1|setosa  |
|          4.3|         3.0|          1.1|         0.1|setosa  |
|          5.8|         4.0|          1.2|         0.2|setosa  |
|          5.7|         4.4|          1.5|         0.4|setosa  |
|          5.4|         3.9|          1.3|         0.4|setosa  |
|          5.1|         3.5|          1.4|         0.3|setosa  |
|          5.7|         3.8|          1.7|         0.3|setosa  |
|          5.1|         3.8|          1.5|         0.3|setosa  |

You can write citations, too. For example, we are using the **bookdown** package [@R-bookdown] in this sample book, which was built on top of R Markdown and **knitr** [@xie2015].
