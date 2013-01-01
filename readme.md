z system
---

The z system is a program/workbench/system/environment that has gotten
created over the past 7+ years off and on. It uses a genetic
programming concept, along with the Common Lisp capability for dynamic code
generation to directly manipulate the genetics of the forms under examination.

But what does it it *do*, I hear you cry. The Z system is designed to
generate a model for functions of one variable such as:

```
y = f(x)
```

It answers, to an approximation- given the above, what is f? The Z
system generates a set of possible answers.


Issues
---

Issues will of course exist and unsurprisingly do:

- Functions that yield underflow/overflow on the test points are
  cheerfully considered and discarded. Same for other errors. Same for
  functions that generate complex numbers.

- Functions of multiple numbers (e.g., `y=f(x,w)`) will not work. This
  should be a minor technical challenge for a hardworking person with
  a free weekend.

- The vocabulary is limited to log abs square sqrt cos sin exp and the
  arithmetics. Check the DEFCONSTANTS and modify to taste...

- The fitness function seeks to minimize the sum-of-squares difference
  from the target data. A clever hacker could probable make that more
  awesome.

- An academic or other skilled person would be able to determine
  better evolutionary models.

- Etc.


Notus Bene.
---

This is a *hobby* project, and I license as AGPL3. It will be
maintained as the fit takes me. I think this kind of tech could be
absurdly useful, and am willing to relicense it given the right
reasons.
