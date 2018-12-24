# CPDS-OPTY
Implementation of the OPTY algorithm in Erlang

## HOW TO RUN
Compile all modules, then run into the erlang interpreter and execute the following command:
```
opty:start(3,10,3,2,3).
```

Changing the parameters for that command (more likely the last one) may lead to errors like ``Too many processes``. Still have to check this.