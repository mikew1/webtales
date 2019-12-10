
This is my code from [lisp webtales](https://leanpub.com/lispwebtales) and the result of the 
approximately four days it took me to study the book and build the application described in it. 
I had to fix a few problems with the code in the book. What these were and how I resolved them 
is noted in the commit messages. 

My best guess as to why the code in the book didn't work for me as written 
is that perhaps the [restas](https://github.com/archimag/restas) 
source has changed a bit since the book was published.

My Lisp adventures have as ever been greatly inspired and helped by the wonderful CL folks on stackoverflow
with special thanks to Rainer Joswig, Renzo & coredump. It's always an inspiration that such experienced
and knowledgeable folks give their time and expertise so freely to the interested learner.

## Rationale

Having completed more or less all of Paul Graham's book ANSI Common Lisp, I needed to find ways
to get more practice. Since I have close familiarity with the problem web applications solve from my day to day work
in other languages, seeing how these same problems are conquered in LISP seemed like a good plan. 

## Main points - web programming in Lisp

- The common languages used for web programming all seem to create a bunch of DSLs which we must navigate.
  The most obvious are those for templating and validation. (My point is that those are different from the core language, 
  i.e. they are different syntactically). Personally I only really became aware of this from the perspective of lisp. 
- Lisp tends to not do that and it's only when you've seen that it's possible do you realise what barriers
  those other systems actually erect to your thinking.
- To take a different example, the framework here, Restas, uses generic functions to achieve what in OO is called 
  Inversion of control. It does it with virtually no ceremony which could not be said of OO code. Reason is the powerful
  constructs in the language. In this case, generic functions.
- Freer organisation. MVC is just one option. Because of the lack of DSLs as mentioned, you're much freer to code in a way 
  that suits the task at hand.
- Code is about 50% shorter for the same tasks.
- Easier to understand (once you've learnt Lisp, of course).


## Details if you want to run the code

I used [CCL](https://ccl.clozure.com/) for this, as [asdf](https://common-lisp.net/project/asdf/) is 
needed and [CLisp](https://clisp.sourceforge.io/) which I'd been using has an older version of asdf which
I didn't bother trying to upgrade.  [Quicklisp](https://www.quicklisp.org/beta/) must be installed, 
and this repo should be placed in your `local-projects` directory of Quicklisp. 
The final commit demonstrates extracting a restas module, whereby the auth system becomes a
separate package. Therefore to run that, you need [this](https://github.com/mikew1/authdemo) additional 
repo in your `local-projects` directory, which is the extracted auth system as a module. 
If you don't care about having the auth system as a separate module to start with, (a good idea when
looking at the code for the first time), then check out an earlier commit of this repo, then you won't
need the aforementioned `authdemo` at all, as in those earlier commits, that code is still in the main 
app.
