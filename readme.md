z system
---

The z system is a program/workbench/system/environment that has was
created in the 2005-2013 timeframe off and on. It uses a genetic
programming concept, along with the Common Lisp capability for dynamic code
generation to directly manipulate the genetics of the forms under examination.

The Z system is designed to
generate a model for functions of one variable such as:

```
y = f(x)
```

It answers, to an approximation- given the above, what is f? The Z
system generates a set of possible answers.

It is designed to be live-managed in the CL REPL mode with the code under the `workbench` section.


Behaviors
---

- Functions that yield underflow/overflow on the test points are
  cheerfully considered and discarded. Same for other errors. Same for
  functions that generate complex numbers.

- Functions of multiple numbers (e.g., `y=f(x,w)`) will not work.

- The vocabulary is limited to log abs square sqrt cos sin exp and the
  arithmetics. Check the DEFCONSTANTS and modify to taste...

- The fitness function seeks to minimize the sum-of-squares difference
  from the target data. A clever hacker could probable make that more
  awesome.

- An academic or other skilled person would be able to determine
  better evolutionary models.


Contributions
---
If you want to work on this, go ahead. Send in the PR. I'd love to see the ideas.

License
---
AGPL3
