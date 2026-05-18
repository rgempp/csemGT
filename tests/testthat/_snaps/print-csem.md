# print.csem display is stable (analytical, smoothed, cutpoint)

    Code
      print(fit)
    Output
      ----------------------------------------------------------------
      Conditional SEMs in Generalizability Theory
      ----------------------------------------------------------------
      Design          :  univariate single-facet (p x i, crossed)
      Persons (n_p)   :  90
      G-study items   :  16
      D-study items   :  16
      Method          :  all
      SE method       :  analytical
      Smoothing       :  quadratic on observed score
      Cutpoint        :      0.500000
      ANOVA table
      ----------------------------------------------------------------
        Effect    df              SS              MS         sigma^2
      ----------------------------------------------------------------
        p           89       98.811806        1.110245      0.058359
        i           15       25.315972        1.687731      0.016792
        pi        1335      235.621528        0.176496      0.176496
      D-study error variances and SEMs (n_i' = 16)
      ----------------------------------------------------------------
        sigma^2(Delta) =   0.012080      sigma(Delta) = 0.109911  (absolute)
        sigma^2(delta) =   0.011031      sigma(delta) = 0.105028  (relative)
      Reliability-like coefficients
      ----------------------------------------------------------------
        Generalizability coef.    E rho^2     =   0.8410
        Dependability coef.       Phi         =   0.8285
        Dep. coef. for cutpoint   Phi(lambda) =   0.8270   (lambda =  0.500)
      Quadratic smoothing fits  (y = b0 + b1*score + b2*score^2)
      --------------------------------------------------------------------------
        Quantity              b0         b1         b2        R^2       RMSE
      --------------------------------------------------------------------------
        abs_ev                0.00000    0.06667   -0.06667     1.0000    0.00000
        rel_ev_full           0.00087    0.05666   -0.05700     0.8471    0.00171
        rel_ev_la             0.00087    0.05529   -0.05563     0.8437    0.00169
        rel_ev_unc           -0.00105    0.06667   -0.06667     1.0000    0.00000
      Mean variance of estimator across persons
      ----------------------------------------------------------------
        Quantity              Analytical
      ----------------------------------
        abs_ev              4.647618e-04
        rel_ev_full         5.991964e-04
        rel_ev_la           5.821768e-04
        rel_ev_unc          5.395546e-04

# print.csem display is stable (no smoother)

    Code
      print(fit)
    Output
      ----------------------------------------------------------------
      Conditional SEMs in Generalizability Theory
      ----------------------------------------------------------------
      Design          :  univariate single-facet (p x i, crossed)
      Persons (n_p)   :  90
      G-study items   :  16
      D-study items   :  16
      Method          :  n/a (absolute error only)
      SE method       :  analytical
      ANOVA table
      ----------------------------------------------------------------
        Effect    df              SS              MS         sigma^2
      ----------------------------------------------------------------
        p           89       98.811806        1.110245      0.058359
        i           15       25.315972        1.687731      0.016792
        pi        1335      235.621528        0.176496      0.176496
      D-study error variances and SEMs (n_i' = 16)
      ----------------------------------------------------------------
        sigma^2(Delta) =   0.012080      sigma(Delta) = 0.109911  (absolute)
        sigma^2(delta) =   0.011031      sigma(delta) = 0.105028  (relative)
      Reliability-like coefficients
      ----------------------------------------------------------------
        Generalizability coef.    E rho^2     =   0.8410
        Dependability coef.       Phi         =   0.8285
      Mean variance of estimator across persons
      ----------------------------------------------------------------
        Quantity              Analytical
      ----------------------------------
        abs_ev              4.647618e-04

# print.csem display is stable (bootstrap, both SE sources)

    Code
      print(fit)
    Output
      ----------------------------------------------------------------
      Conditional SEMs in Generalizability Theory
      ----------------------------------------------------------------
      Design          :  univariate single-facet (p x i, crossed)
      Persons (n_p)   :  60
      G-study items   :  12
      D-study items   :  12
      Method          :  all
      SE method       :  both
      Smoothing       :  quadratic on observed score
      ANOVA table
      ----------------------------------------------------------------
        Effect    df              SS              MS         sigma^2
      ----------------------------------------------------------------
        p           59       44.666667        0.757062      0.048365
        i           11       20.666667        1.878788      0.028368
        pi         649      114.666667        0.176682      0.176682
      D-study error variances and SEMs (n_i' = 12)
      ----------------------------------------------------------------
        sigma^2(Delta) =   0.017088      sigma(Delta) = 0.130719  (absolute)
        sigma^2(delta) =   0.014724      sigma(delta) = 0.121340  (relative)
      Reliability-like coefficients
      ----------------------------------------------------------------
        Generalizability coef.    E rho^2     =   0.7666
        Dependability coef.       Phi         =   0.7389
      Quadratic smoothing fits  (y = b0 + b1*score + b2*score^2)
      --------------------------------------------------------------------------
        Quantity              b0         b1         b2        R^2       RMSE
      --------------------------------------------------------------------------
        abs_ev                0.00000    0.09091   -0.09091     1.0000    0.00000
        rel_ev_full           0.00099    0.06937   -0.06714     0.6654    0.00337
        rel_ev_la             0.00101    0.06670   -0.06451     0.6552    0.00331
        rel_ev_unc           -0.00236    0.09091   -0.09091     1.0000    0.00000
      Mean variance of estimator across persons
      ----------------------------------------------------------------
        Quantity              Analytical         Bootstrap
      ----------------------------------------------------
        abs_ev              8.432168e-04      3.974634e-04
        rel_ev_full         9.959658e-04      3.507425e-04
        rel_ev_la           9.581419e-04      3.408455e-04
        rel_ev_unc          1.041053e-03      5.071434e-04

