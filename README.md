# Elea - a supercompiler for theorem provers

[![Build Status](https://travis-ci.org/wsonnex/elea.svg?branch=master)](https://travis-ci.org/wsonnex/elea)

Currently under development, but almost finished!


<!--
## What is a supercompiler?

A supercompiler is a tool which rewrites terms into *fixed-point promoted form*.
A term is in fixed-point promoted form when recursive function calls have only variable arguments, 
all of which are unique, and none of which are free within the definition of the recursive function.

Here're some terms in fixed-point promoted form:

    (add x y)
    (reverse xs)
    
Here're some terms **not** in fixed-point promoted form:

    (add (add x y) z)  ;; one argument is not a variable, it's a function call1
    (add x x)          ;; the x argument is repeated

Elea can rewrite terms such as those above, for example:

    >>> (reverse (reverse xs))
    xs

In the above example, Elea is able to rewrite the term `reverse (reverse xs)` to just `xs`,
removing the recursion entirely.

## Why do I need a supercompiler?

If you've ever thought:

 "I wish I could integrate automated proof by (fixed-point) (co-)induction 
 into my theorem prover, but it seems like a lot of effort."

then *Elea* is the tool for you!

Rewriting your functional language terms into fixed-point promoted form means that you (often) don't need
automated proof by induction, co-induction, or any of the other cyclic, second-order proof techniques
to prove your properties. In the next few sections we give examples of how using Elea to
supercompile your terms can remove the need for cyclic techniques.


## Proving equations using Elea





### Proving `A = B` with the help of Elea

Let's say you want to prove `A = B`, for some terms `A` and `B`. You can run Elea on the 
`A` term, hopefully rewriting `A` to something in fixed-point promoted form, viz. something of the 
shape `((fix f C[f]) x1 x2 ... xn)` where `F[_]` is a term context, and `x1` to `xn` 
are the free variables of `A`. So our property is now:

    ((fix f F[f]) x1 x2 ... xn) = B
    
Through beta-abstraction, the above property is equivalent to:

    ((fix f F[f])) = (lambda x1 x2 ... xn B)
 
Since fixed-points are least, if we are in a total language (or if `F[_]` is well-founded or productive)
we can prove the above by showing:

    y = F[(fun x1 x2 ... xn B)] = (lambda x1 x2 ... xn B)
 
Et voila! The recursion has disappeared, and hence so has the need for a cyclic proof technique, like induction.

That's how it works for equality, but you can also show other types of property, such as numerical
in-equalities, as shown in the next section.


### Proving `A < B` with the help of Elea

Let's say `A` and `B` are terms of integer type, and we'd like to prove `A < B`. 
As in the previous section, we run Elea on `A`, hopefully giving aN equivalent term of the shape
`((fix f C[f]) x1 x2 ... xn)`. 




let's assume that you are writing a theorem prover. 

Terms in fixed-point promoted form do not require induction or co-induction to reason about,
for any chain-complete property, e.g. equality, or numerical in-equality.






### Total mode vs. partial mode

The default functional language that Elea supercompiles is partial, meaning that functions can fail to terminate, 
and that variables can contain `_|_`. 
However, if you require supercompilation of a total language, you can add `(mode total)` to the top of your 
program in order to switch Elea to total mode. In total mode, all functions are assumed to be terminating and all
free variables are assumed to not contain `_|_`. Elea does *not* check totality for you - if you give it a non-total
function and switch total mode on, Elea no longer guarantees semantics preserving transformations.

For example, let's try to simplify the term `add x x`, first in the default partial mode, then in total mode.

    (defcodata Nat (Zero) (Suc Nat))
    (defun add x y (match x (Zero -> y) (Suc x' -> Suc (add x' y))))
    
    >>> ... 
    
    (mode partial)  ;; optional
    (defun double x (add x x))
    
    >>> (defun double x (add x x))
    
As you can see, the term `add x x` cannot be simplified in partial mode.  If we switch to total mode though:
    
    (mode total)
    (defun double x (add x x))
    
    >>> (defun double x (match x (Zero -> Zero) (Suc x' -> Suc (Suc (add x')))))
    
In this example, the term `add x x` term was able to be simplified    

3. [Call-by-value vs. call-by-name mode](#cbv-vs-cbn)

### Call-by-value vs. call-by-name
<a name="cbv-vs-cbn"></a>

So far, Elea has only been proven sound for call-by-name semantics 
Most programming languages are call-by-value, meaning function arguments are evaluated 
to a value before they are passed to the function. 
The default mode for Elea is call-by-value semantics.
However, if you are proving properties of a call-by-name or call-by-need language, like Haskell, you can switch
Elea to call-by-name mode by adding the declaration `(semantics call-by-name)` to your code.

For example:
-->


