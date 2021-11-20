# Horth
A stack based, **VERY WIP** language written in Haskell and inspired by [Tsoding's Porth](https://github.com/tsoding/porth). Compiled using Haskell Stack.  
### How does one use the compiler?  
Simply write your code into horth.hth and execute using the command `stack run` from your terminal. You could alternatively import all the source into GHCi and run `main` in Main.hs. 
### Okay but what's the syntax like?  
Fairly similar to Forth, albeit without the ability to define words in its current state.  
Note that Horth, like Forth, uses reverse Polish notation meaning that words and operations are written after their respective arguments, this avoids the need for parentheses and makes the code very easy to parse (for a computer, it can be quite confusing for humans).  Any number of spaces or new lines can separate words and values.
A code sample:
```
12 4 dup .s
* *
.
```
has output:
```
head -> [4,4,12]
192 
```
The compiler reads 12 as an int and adds it to the stack, then reads 4, then duplicates 4, then displays the stack, then multiplies all numbers together, then prints the head of the stack and dumps it.  
phew.
Anyway here's the current list of functions:  
|Word(s)|Number of Args|Function|
|-------|--------------|--------|
|`.`|1|Prints the head of the stack and dumps it|
|`.s`|0|Displays the stack in its current state|
|`dup`|1|Duplicates the most recent element in the stack|
|`cp`|>=2|Takes one integer argument and copies the element in the stack that many indices before it to the front e.g. `1 2 3 1 cp .s --> head -> [2,3,2,1] `|
|`dump`|1|Removes the head of the stack|
|`+ - * / ^`|2|Self explanatory: note that for non-commutative operators the first element in the stack is always applied to the next e.g. ```2 4 / --> 0.5```|
|`< > <= >= ==`|2|Equivalence operators: take two numbers each and returns a boolean, either `True` or `False`|
|`&& \|\|`|2|And and Or respectively, take two booleans and retun another|
|`¬`|1|Not: takes one boolean and returns the inverse|
|`if then else endif`|1, before `then`|Conditional routines: takes a boolean before `then` and if true evaluates the block between `then` and `else` otherwise evaluates the latter block between `else` and `endif`|


### Planned features:
There will be plans
