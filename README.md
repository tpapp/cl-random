[![Project Status: Unsupported – The project has reached a stable, usable state but the author(s) have ceased all work on it. A new maintainer may be desired.](http://www.repostatus.org/badges/latest/unsupported.svg)](http://www.repostatus.org/#unsupported) This library is [**unsupported**](https://tpapp.github.io/post/orphaned-lisp-libraries/).

## Introduction

cl-random is a library for (1) generating random draws from various
commonly used distributions, and (2) calculating statistical functions,
such as density, distribution and quantiles for these distributions.

In the implementation and the interface, our primary considerations were

1.  **Correctness.** Above everything, all calculations should be
    correct. Correctness shall not be sacrificed for speed or
    implementational simplicity. Consequently, everything should be
    unit-tested all the time.

2.  **Simple and unified interface.** Random variables are instances
    which can be used for calculations and random draws, for example,

```lisp
(let ((rv (r-normal 13 2)))
  (pdf rv 15d0)             ; density
  (cdf rv 13d0)             ; CDF
  (draw rv))                ; a random draw
```

1.  **Speed and exposed building blocks on demand.** You can obtain the generator function for random draws as a closure using the accessor "generator" from an rv. In addition, the package exports independent building blocks such as draw-standard-normal, which can be inlined into your code if absolutely necessary.

Implementation note: Subclasses are allowed to calculate intermediate values (eg to speed up computation) any time, eg right after the initialization of the instance, or on demand. The consequences or changing the slots of RV classes are UNDEFINED, but probably quite nasty. Don't do it. **Note: lazy slots are currently not used, will be reintroduced in the future after some serious profiling/benchmarking.**

## To do list

### Roadmap

1.  Sketch the interface.

2.  Some basic functionality. We are currently here, eg exponential, normal and gamma distributions are partially implemented.

3.  Keep extending the library based on user demand.

4.  Optimize things on demand, see where the bottlenecks are.

### Specific planned improvements, roughly in order of priority

-   more serious testing. I like the approach in Cook (2006): we should transform empirical quantiles to z-statistics and calculate the p-value using chi-square tests

-   (mm rv x) and similar methods for multivariate normal (and maybe T)

### Coding guidelines

Always try to implement state-of-the-art generation and calculation methods. If you need something, read up on the literature, the field has developed a lot in the last decades, and most older books present obsolete methods. Good starting points are Gentle (2005) and Press et al (2007), though you should use the latter one with care and don't copy algorithms without reading a few recent articles, they are not always the best ones (the authors admit this, but they claim that some algorithms are there for pedagogical purposes).

Always document the references in the docstring, and include the full citation in doc/references.bib (BibTeX format).

Do at least basic optimization with declarations (eg until SBCL doesn't give a notes any more, notes about return values are OK). Benchmarks are always welcome, and should be documented.

Document doubts and suggestions for improvements, use `!!` and `??`, more
marks mean higher priority.

The naming convention for building blocks is something like
`(draw|cdf|pdf|quantile|...)-(standard-)?distribution-name(possible-suffix)?`,
eg `pdf-standard-normal` or `draw-standard-gamma1`.
