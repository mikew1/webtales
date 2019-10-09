
This is my code from [lisp webtales](https://leanpub.com/lispwebtales) and the result of the 
approximately four days it took me to study the book and build the application described in it. 
I had to fix a few problems with the code in the book. What these were and how I resolved them 
is noted in the commit messages. 

My best guess as to why the code in the book didn't work for me as written 
is that perhaps the [restas](https://github.com/archimag/restas) 
source has changed a bit since the book was published.

My Lisp adventures have as ever been greatly inspired and helped by the wonderful CL folks on stackoverflow
with special thanks to Rainer Joswig, Renzo & coredump. I am always amazed at how such experienced
and knowledgeable folks give their time and expertise so freely to the interested learner.
You folks are an inspiration on many levels.

# Rationale

Having completed more or less all of Paul Graham's book ANSI Common Lisp, I needed to find ways
to get more practice. (I haven't done [Siebel's book](http://www.gigamonkeys.com/book/) yet, but 
may do that soon).

Since I have close familiarity with the problem web applications solve from my day to day work
in another lesser language, seeing how these same problems are conquered 
in LISP seemed like a good plan. 

# Outcome

The book Lisp webtales by Pavel Penev takes you step by step through building a simple web application 
and shows you how minimal and yet powerful a web framework can be in a Lisp. 
It delivers on that promise admirably. I was not disappointed.

Here's what I got from it

- Insight into what web application code can look like in a Lisp.

- Correspondingly, how verbose and inflexible the solution to the same problem actually
  is in another language I am familiar with.

Note that while the tool here is Common Lisp, these points are directly applicable to Clojure too.

### Main points - some thoughts on web programming in Lisp

- You can pick between what we know from every other web framework as a templating language,
  and another option, seemingly unique to lisp, an html generator.
  With the latter, your "templates" become **~50% less LOC** and easier to read.
  
- I noticed how restas here uses the core facilities of Lisp, and conversely how
  **other frameworks erect their own OO derived structure** which you have no sensible 
  choice other than to learn & live with. **Lisp is minimal, and easier to understand**, because
  there's less to remember - once you know Lisp itself. You do have to invest in that.
   
- For example, restas uses generic functions to achieve what's called IoC (Inversion
  of control) in other frameworks. It does it in a much simpler way and with much less
  code to type. 

- You can write in MVC **if you want**, but that is just one of many options you have.
  You use the core facilities of the language itself. There is no gigantic structure
  erected by framework authors which you have to accomodate to.
  
- I feel sure that coding this way, whether it ends up being CL itself or Clojure
  (any platform will have [its own deployment issues](https://tech.grammarly.com/blog/running-lisp-in-production))
  can result in **much greater agility**. 

- That means if you base your business on this you can [win](http://www.paulgraham.com/avg.html).

----

#### Details if you want to run the code

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
