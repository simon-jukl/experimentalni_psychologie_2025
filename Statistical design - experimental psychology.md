
# Basic linear regression

$$
\displaylines{
response_i \sim Normal( \beta_0 + \beta_1 \cdot beat\_frequency + \beta_2 \cdot interval\_length + \beta_3 \cdot group, 
\sigma)
}$$

```
bayesformula(

  response ~ beat_frequency + interval_length + group
  
)
```


# Linear regression with interactions

$$
\displaylines{
response_i \sim Normal( \beta_0 + \beta_1 \cdot beat\_frequency + \beta_2 \cdot interval\_length + \beta_3 \cdot group + \\ 
\beta_4 \cdot beat\_frequency \cdot group + \beta_5 \cdot interval\_length \cdot group + \beta_6 \cdot beat\_frequency \cdot interval\_length, 
\sigma)
}$$

```
bayesformula(

  response ~ beat_frequency + interval_length + group + beat_frequency:group + interval_length:group + beat_frequency:interval_length

)
```


# Hierarchical regression with (some) interactions

$$
\displaylines{
response_i \sim Normal( \beta_0 + u_{subject[i], 0} + (\beta_1 + u_{subject[i], 1}) \cdot beat\_frequency + \\
(\beta_2 + u_{subject[i], 2}) \cdot interval\_length + \beta_3 \cdot group + \\ 
\beta_4 \cdot beat\_frequency \cdot group + \beta_5 \cdot interval\_length \cdot group, 
\sigma)
}$$

```
bayesformula(

  response ~ beat_frequency + interval_length + group + 
  beat_frequency:group + interval_length:group + beat_frequency:interval_length +
  (1 + beat_frequency + interval_length | subject)

)
```