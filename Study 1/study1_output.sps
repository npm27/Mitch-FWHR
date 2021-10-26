* Encoding: UTF-8.
*Omnibus analysis for Study 1; Hi = High-fWHR, Lo = Low-fWHR, N = Nurturance, P = Protection.
GLM HighN HighP LowN LowP BY Sex
  /WSFACTOR=hi_lo 2 Polynomial n_p 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=hi_lo n_p hi_lo*n_p
  /EMMEANS=TABLES(hi_lo*n_p)COMPARE(hi_lo)
  /EMMEANS=TABLES(hi_lo*n_p)COMPARE(n_p)
  /DESIGN=Sex.
