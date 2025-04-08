# A list of TODOs to revisit

**[1] Add support for `^n (ListN)` iterator**

  -- replace `$repeat_` and `$init_` list helpers
  -- need more thought in binding analysis though

**[2] Implement capture-avoiding type substitution**

  -- when substituting a function parameter `(def $f<X>(...))[theta]`,
     take care of the situation where `theta` captures `X`

**[3] Maybe add a validation pass post-elaboration**

  -- to check if elaboration is really working as intended

**[4] Tweak parser to properly parse hint expressions where commented out**

**[5] Add subscripted turnstile `|-_`**

  -- this would de-necessiate markers like `consctxt`

**[7] Of course, big ones:**

  -- LaTeX renderer
  -- Prose generator
