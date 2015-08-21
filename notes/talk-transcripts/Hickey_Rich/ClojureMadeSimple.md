# Clojure, Made Simple

* **Speaker: Rich Hickey**
* **Conference: [JavaOne 2014](https://www.oracle.com/javaone/sessions.html) - September / October 2014**
* **Video: [https://www.youtube.com/watch?v=VSdnJDO-xdg](https://www.youtube.com/watch?v=VSdnJDO-xdg)**

Slide1 - 0:00

I want thank everybody for coming. The title of this talk is "Clojure, Made Simple" on a brochoure they left out the comma. So that's "Clojure Made Simple" another words a tutorial in Clojure, an easy explanation of Clojure. It is a comprehensive explanation of Clojure at all, but I look on slice what Clojure is about a way of thinking about why you might want to use it.

Slide2 - 0:25

So I am the person who made Clojure. I currently work on a database called Datomic, which is kind of functional database, which is written in Clojure and runs on JVM architecture. I am co-founder of Cognitect which builds Datomic and sponsors the development and stewardship of Clojure. But the main point that I want to make about myself to this audience, because in this talk I might sound kind of skeptical about Java and Object Oriented Programming, is that I done absolutely ton of that. That's what I did for two decades, before I said if I still want to be a programmer I don't want to do this anymore. So I know exactly how apps are built using Java and C++, and C#, because that's what I used to do. That doesn't mean that I think about them is correct but that's my experience.

Slide3 - 1:22

But I am wondering about you. How many people are programming in Java here? How many people are happy about that? How many people are actively unhappy about that and looking for alternatives? Okay. Great. How many people tried Clojure at all? How many people never heard of Clojure and are in the wrong room? How many people have tried Clojure and are trying to get to this at work, but not yet? A few. Maybe this talk will give you some ways how of talking value proposition that will help you. How many people actively use Clojure? And somehow are accidentaly on JavaOne. And rest couldn't get into Brian Goetze's talk. I shouldn't even mention Brian Goetze's talk right now because we falling. Or maybe you just tired of Brian get about immutability, in which case you are definitely in the wrong room. No, I like Brian, is a good friend and his talks are great. So I appreciate you being in this one.

Slide4 - 2:21

So, there are very few people never heard Clojure, so I not gonna spend a lot of time on it, oh I've got one more question. How many have seen my talk "Simple made Easy"? How many have not? Okay a few. So, I may spent a minute describing what I mean simple. Clojure is a programming language which runs on JVM and JavaScript, and pretty much a substancial part of Clojure runs on JavaScript. So it is a programming language with which you can target both. Originally you can target JVM and CLR. There is still a port to the CLR that is maintained but this is not seem wide use. I release first in 2007. Its had, a surprising adoption especially from my perspective since then giving its characterictics. Because it is a Lisp, it is functional, it is data oriented and has a lot of things that make it seem not like the kind of language that would succeed. And this talk really be about data orientation of Clojure.

Slide5 - 3:27

So, "A lot of best programmers and the most productive programmers I know are writting everything in 'blank' and swearing by it, and then just producing ridiculously sophisticated things in a very short time. And that programmer productivity matters.". So Adrian Cockcroft was Cloud Architect at Netflix, now is a Battery Ventures. How many think Java goes into blank? So we know, we know this, there is something about Java that makes it not suitable for this blank.

Slide5b - 4:04

So maybe tease that a part, just of course he is saying Clojure. And this talk is about maybe why. Why this could be true? What is that makes the Clojure different and possibly better fit for that blank.

Slide6 - 4:19

So the first thing I want to talk about is that I think we have this tendency in programming to think about ourselved just to tone and our languages, our tools, and our techniques, and "Me, Me, Me, Me, Me", as what we're doing whatever, we lose of track of the fact we all working for somebody else, or ourselves but for a business or an organization which is trying to accomplish something in the world and the software is completely secondary to that task. Right? It should be measured always in terms of cost/benefit ratio, ROI, how quickly we can get product to market, and is what we're doing profitable. Right? If we're not doing that, we are not being good participants in our businesses and organizations.

Slide7 - 5:05

So what do the stakeholders want? They really want two things, they want something good and they want it soon.

Slide8 - 5:18

So, something good. We think we know what something good, we know how to makes things good, we've these techniques. And some things are good when the techniques are successfull with them, right? When the types check, and our tests pass we have something good. But of course we all know that with our best efforts in those things, which I'm not saying that are not bad things BTW, but no matter what we do there, we end up with programs that don't work. We all know we have that type check and tests pass and they don't work. They don't work from the perspective the stakeholder - there as, they don't do with the program was supposed and what's supposed to do was something that was conveyed between people or through documentation or papers, things that are not in programming languages. They have to meet operation requirements and they've to be flexible, okay? There are some times when people just want something soon and they don't want something good. There are actually better languages than Clojure for that, right? Just give me something fast that I'm absolutely, definitely gonna throw away, will not grow, will not expand, will not take me further. So this first two things are means, they're good but they're only good in so far sa they help insure those latter three things.

*TODO* - Review things below.

Slide9 - 6:35

so we break it down what is it supposed
to do again it's a perspective thing at the stake all those things so it's supposed to do they're fine of course they're gonna have expressed um concerns about software was supposed to do it should do this my purse this but lemme and they're gonna have unexpressed presume things like it should be secure it shouldn't you know cause the computers go fire cuz it's so slow it shouldn't require three new data centers 'em it should keep running and not stop for an hour every day well those are the unstated presumptions of something being good but it ends up that if you build large elaborate stay for programs it's extremely difficult to ascertain whether or not they are going to do what they're supposed to do and in fact if you build any one of those things i'm just still the very large program or a very elaborate program or very stable program it will be as hard to figure out if its what if its gonna do is supposed to do so why things closures oriented at is up making it easier to understandwhether or not your program its gonna do is supposed to do mostly by making it substantially smallerat all so by making it more functional

Slide10 - 7:43

concerns about racial requirements as a
boatload of things as a boatload of fun
stated requirements of software
you know to our there can I deploy it in
the normal way with all my stuff but the
people who know how to run my machines
and everything else
and and that's where the targets a
closer
closures meant to be hosted it's just a
jar it runs in the environment season 2
sneak and
rates just at the store one large are
the
and they were and they were running but
but it's not a small thing rate if you
wanted to adopt
Common Lisp say or how school
you be asking your um your ops team
and their deployment same to start
manipulating something completely alien
whose characters as they don't
understand lines are the security
everything that's available for the JVM
for security is available
by to closure I'm and their performances
other concerns
I'm a very important thing though is
that now we can also reach the browser
so I think you how he will write
applications were some part of the
overall system
such as a browser yeah so
right now use two different things I
almost definitely is two different
things
and I think it's a straight the closure
that we're delivering the same value
proposition
in both places both on the server and
the client even if they're separate
those
a home the value proposition is
necessary to mall was more so
in the browser which is one of the most
complex places
ever I'll in terms of performance
a you know a lot so it might look at a
dynamic languages lol you know how could
I be whatever but you know
closes right down there with the fast
languages I'll
on admittedly the benchmark game which
is a benchmark in a game
so but it says we can reach that
a on JavaScript we have a very
interesting results here's a home is a
closure script library suppose a skirt
disclosure on
on JavaScript and home is a library that
actually wraps
react which is the new hotness what's
the really interesting things here
all raps react and then spank said
and performance how's that possible
it ends up that that the always use a
persistent data structures which will
talk about in a minute
up make it faster there react because
the big four reactors
doing a change detection and change
detection for mutable things is
identity comparison so
super fast and in fact they are we are
protecting their whole thing to use
persistent data structures and
JavaScript now
the reacts the dev came up to me at
strains have been searching by hand and
said you're saving us a ton of money
up because we're switching so that
strategy so the other party the value
proposition is that was flexibility
right people that that Ste you know
have a stake in software no that they're
building a system now button it tomorrow
things are going to change requires
coaches address do something different
change is inevitable
so can we change the program can we make
it more flexible
and ends up there's a lot we can learn
from
bigger system design in the small to
make this the
subcomponents have systems more flexible
and of course this is the old is all
this thing right
loose coupling but but we talk and talk
and talk about a week continues
techniques that's what it
like every single day plot a technique
could do something that's that makes
this harder
I'll so what makes it easier
the so
the my talk was done and somebody said
you have this quote from Walmart labs
guy
you might wanna put in here talkin it
was blank wow this is great
because he stole my story I was closer
we get to market faster with better
quality
we avoid unanswered unintended
interruptions in jobs will go to one
area and
the application another closure shrinks
are code base about one-fifth the size
with
have have written in Java I
and these are the points of my talk but
this is what I want this is a
stakeholder
saying is what we get by by Chism
closure faster time to market
better-quality right we avoid
coupling problems that make it difficult
for us to change
we have a smaller code base so how does
go to do this
there are many many different
characteristics the closer I only wanna
talk about
sir to today mostly one one is and
because this is one
I think that's that's closer apart a
makes a
somewhat different is that has dado
orientation no
through and through and the other is
simplicity so
when I say simplicity I mean the
absolute complexity I do not mean he's
I mean closures and easy language that
easy only
union type and everything magically
happens or some sort using this
metric the complex things are
intertwined and simple things arse
not right there more independent they're
separate
even if they have as many things going
on this is simpler than this
by four things like this is complex I'm
for things like this is simple
that's what I mean when I say Sabal a
untangled
so the thing is that
you know all these languages can do
everything right we can do the same
stuff and people like what you know you
I know I can do the same stuff in C
sharp in Java and
and Scala and and closer and you know it
per any by any general purpose language
you can accomplish the same things that
the other day right so what
differentiates languages what they make
practical and what they make idiomatic
enclosure we focus on something
making something idiomatic that I think
I'm is not and should be more so
that's this before we had all this
highfalutin
you know
opinions of ourselves as programmers and
computer scientists and stuff like that
programming is because data processing
how many people actually do data
processing in the program's
you can raise your hands we all to rate
this is what most programs there
it takes some information and some a
type some stuff so I sent you a message
you put it somewhere later you try to
find it
you put on the screen is sent it to
somebody else
that's what most programs to most the
time sure the computational ass but the
program's
this quality but when Titian issue so
this but there's nothing wrong with
saying
programs process data because status
information information system this is
should be what we're doing
right where the stewards the world's
information
and information is just data it's not a
complex
thing that's an elaborate thing it's a
simple thing
until we programmer start touching it so
we have data processor processing most
programs that this is very few girls
adult
right the and data is
a Splenda medley simple thing data is
just
raw immutable information so that's the
first point date is immutable
if you make a data structure you can
start messing with that
but actual data is is immutable so if
you have a representation for that's
also immutable
you're capturing its essence better then
if you start
fiddling around and that's what happens
languages fiddle around
Friday elaborate un data they had types
they had methods to make data active
they make data
mutable they make data movable the
they turn into you an agent or some
active thing
um at that point they're ruining it a
allister moving away from what it what
it is a
and an object orientation I think this
is rapid because our fundamentals
constructs the object conflates two
things that doesn't give us two separate
ways to talk about process
for which objects are okay approach
and information for which there are
terrible terrible terrible
coach right but where we don't have a
problem not agree make a new object to
make a new class
making a sensational something by and
and this makes our programs more about
themselves again
and less about the informational a
programs are more
increasingly about code and decreasing
the about data
and I think that's a mistake so closer
embraces data that's like the simplest
idea like lawyers just
there's nothing wrong with data data has
this great property is
a let's use a in fact let's make it
really important first class
in your face kinda thing so close
embrace it
first and foremost to having strong data
liberals which also you in a minute
'em and they're playing
make plain it just means flat level
unadorned no extra stuff and
code and this is an old list thing
enclosure is represented as data
that's important for a lotta reasons
that enables macros and a lot a
sophisticated program transformation
things
but also means that you don't have
different stuff right
the majority of functions enclosure just
take data
and by that I mean immutable unadorned
stuff and the return that same thing
giant library functions hundreds and
hundreds and hundreds of data
manipulation functions
ready to go they take data they were
turned a
so if you have anything that state a you
can use all those functions on it
which tends to pressure you towards
making everything data
because then you have this giant library
which learn once
and you can apply to every problem you
have and in particular
every time we encountered information
enclosure systems causes different parts
of systems rate
the party system that manipulates
information as a partner system that
sort of
plumbing right or the machinery every
program like
you with a socket or communications and
point is more like a machine
than it is like information right so
they can be active part severe program
but whenever you're done with the
partner program that's just about
representing
fax and informational we will always in
closer to use plain data to do that
so there there's a small Saturday to
liberals and closer
um their relatively obvious ready to
rate injures on all the way in doubles a
long way in big dust bowls with them at
the end
their ratios and these are proper ratios
a don't lose precision
strings are in double quotes and strings
are
java.lang.string when you're right that
you get a java.lang.string canceled just
a little frustrating
their laurels for characters um because
we use data structures for programming
right there's a couple extra things we
need but if you look at Java code
is a written all with quotes around
every every word
no so job I need something besides
strings in order to be a successful
programming language
needs symbols enemies identifiers
up so if you're gonna properly represent
your code is data
those things have to be first-class data
structures
or atomic data types that are different
from strains
so there are two in in closer weather
symbols
and the other keywords and their their
use in closer which I'm not gonna
you know dive into is that symbols are
generally used to reference something
else
so like variables and things like that
the name something
and keywords name themselves the right
enormous if you up I'm they're very
useful as keys
in maps for insults well that's what I
call that their
durables Boolean literals and there's no
which is no
as a say that's a job in all in those
billions a job in there Java
bully in those characters are Java
characters
et cetera et cetera and there's also the
rules for for rent checks
and we have data structures some
fundamental data structures
we have the singly linked list by itself
friends
those are all this there listed numbers
listed symbols
a list that has a simple and then some
numbers that's okay can be her genius
and they grow at the front and they have
linked list kinda performance
characteristics which means it's fast to
put something at the front
it slow to find the fifties 7,000 think
linear time to find stuff in the middle
we have doctors
there in square brackets
right again they can be heterogeneous
the sums be one with numbers along with
symbols but
you can answer makes them they grow at
the end in constant time but they also
offer
fast access anywhere in the middle so
they're different from link list them
and then there are maps their key value
key-value key-value
the commas are optional the key is not
need not be keywords for this first one
uses keywords mapping two integers on
the second uses and just happened to
strings
but you can be heterogeneous in both the
key and the value
and then we have sets
which are just curly's preceded by hash
and you have such again heterogeneous
have anything and follow the stuff nests
when the important things about this is
that these maps
they scale and are efficient from the
very small all the way to the very large
sick and use them as sorta pseudo
objects in the small and help you know
45
fields if you will or or or entries are
you can have
a giant map that has you know lillian's
tens of millions hundreds of millions of
things that the same data structures is
throughout thats range enclosure we
don't distinguish those two uses
at all so all the data structures I
showed you are immutable
there's no way to change them just like
there's no way to change 42
there's no way to change a vector where
you can do is make a new vector
thats slightly different from the back
to start with and you have both
factors um and
there's a technique called persistent
data structures which makes this
the making Avenue slightly different
version efficient
the two versions will have us a
substantial amount of structural sharing
going on under the hood
and that's possible because there are
mutable by they can share structure
because no one can change them
I'll and that's what makes it practical
to use immutable data structures all the
way through the range from tiny things
through very very large sales
a is not copy on write way
these new modifications that you make
still comply with the Big O
expectations you have for the data
structure and head
a and this is the key to practical
functional programming is having us
so the idea behind closure or 10 the
idea is moving closer is just the Hess
a I I'd done so much object-oriented
programming
and like is so much busier and so much
extra stuff
and when I finally later in my career
learned less
I saw people building very very
interesting systems at a much
much simpler stuff and I tried it
and guess what you can build exactly the
same systems that are much much simpler
stuff
and I said well i cant repeated
but I was very unhappy basically
something to the effect the
I have been wasting my time in my career
doing what I've been doing
I need to do something I need to change
what I'm doing because I'm wasting my
time I'm wasting my life doing it this
way
because you can build programs the same
programs
better programs to do the same things
was substantially suppose up in fact you
can build the matter the data structures
I just showed you
plus pure functions to take those things
a return those things
most severe programming can build our
way a little tiny bits your program
you'll have state you'll have
communication you'll have the other
the other aspects I like runtime:
polymorphism enclosure has it all talk
about in the second
but you can build programs %uh this and
you can build big programs that about
you can build databases at a
by I've done a lot built a database at
it when I'm a closure
I was targeting being able to do
everything that I used to do
in Java and C plus plus side bill
broadcast automation systems
scheduling systems yield management
systems
up election projections systems annex a
poll systems
and although they are simple spas in
Java and C sharp I
I believe that I could have use closer
to build anything I ever built
but was maybe a tiny little bit have
lower level code
and small places but that's the target
for closure
I wanted to replace what I was doing
a.m. I think the program's
are substantially smaller simpler and
they're much more robust
an object program %uh or others are into
programming I was doing
took up the same jobs sorry I didn't
closures
let's make that the Ematic let's make
that the first cuz we do things
so the syntax a closer there's no more
than you just saw
is those data structures that setup data
structures and those
the those fundamental things are a4a
called Eden
which stands for extensible data
notation but it's just
a grown-up version %uh s expressions
which have been used in the list
community for years
basically any bill programs that I've
data structures
and also use some other second so the
data structures are the code
the syntax is not based on characters is
based around data structures
know the definition of what a function
is as the list whose head is a symbol
called
you know fun and his next thing in the
list as a vector up
arguments which are themselves symbols
that servicers are above syntax
is described in terms of data structures
I'll
it's not like there's no syntax but does
that's where the interpretation happens
and everything that would be special in
ordinary programming language
decorations control structures function
calls
operators et cetera et cetera they're
all just represented as lists with the
bar about the front
that's a that's the Lisp way I'm
also everything as an expression and
that's typical functional parameters
that's about all I'm gonna say about
closure language
though but I'm gonna show you a lot more
about the use of these
aspects of it I love or later so
Eden this successful the rotation I
showed you some built-in things there's
also a way to extend its head
new tags that are namespace allow you to
describe something new
in terms of anything that's already know
to can make up archery stuff you can say
will
build the new parser to parse characters
and get this thing but you can say
have a new interpretation a vector have
two numbers and it we're gonna call that
a point
and so on and so forth and you can
cascade is
extensions and build richer thanks but
one of the important things about you in
this is meant to be useful
as for data and code
for so is on a previous light I said
commas are optional
that's kinda critical matching have been
between you know
for headlines in between everything you
said in job yet but I'll
who would like that now at the awful
right so that you can have stuff like
that so I mean having people over
used any system that tried to encode
programming
in like j-son or XML yeah
how fun is that yeah
so this is a little bit like I am it a
view as cloud formation has some
functions
in their syntax which is normally
declarative
so this is a nested function call we
can't you can't do this right
care program like this so you get a
program using their structures have to
a day is such a format that's amenable
to that so this were closer looks like I
really expected be able the
to read it but I just told you before
that Solis trade
Kirk you know because up a friend
a the
death on rate it defines a function the
name the functions words
takes are you recall texting is
implementation and so on and so on and
so forth so this is a
port over Peter Norbeck spy phone code
it's a short as the Python program right
and and that's not like a contest
thank its it it's more about this
everything that's in here is about the
problem like
all the words you're reading into
everything it is all about the problem
is not
extra stuff there's no static import
blah
up type this type that yeah I
extra control stuff it's all about the
problem
100 percent about the problem and that's
what you up
right so it short it's free of ceremony
but most important is it's about the
problem which means
it's a lot easier to look at now and
later and see what you're trying to do
what you're trying to accomplish so
once we have this format needn't we is a
for code
we can %ah missy is a for data is a for
everything
I so let's say you want to have a DSL to
represent
HTML because we know a salon so that's
kinda gross
to generate and manipulate us as XML
plus some
randomness a so this is one of many
DS sells for representing is selling
closure but its even this is the same
stuff that the program was made at a
the same reader reads this if I if I
call read
which is available ala carte closer I
get a vector
that as a keyword that then a vector
that has a keyword a vector
bother like it did I know exactly when I
get the dishes I may get
have a special thing I have a DOM blah
blah blah
not you know I get data what every this
that's great
means it's easy for you to process is
easy for me to write a program that
produces this
so I can produce his cell with an extra
special different thing
of course we would use this for our
configuration files right
why not we can read it we can generate
it we can process a week manipulated
everything we know all those hundreds of
functions going to do we can use the
less
now add up the stuff in Java how much
have you got to have syntax
for which you use I don't know what some
parcels thingy
right for that for Java C++ or something
to manipulate your annotations but yes
Jason here you have extra mile there
right may be right here on DSL use
antler something
keep setting up more more more different
things and if you read stuff with an API
what do you get
some API authors idea at the time for
this kind of thing
right what do you get when you read XML
but spends a highly readable you get
this you know machine right
the call you back every time he gets a
new element well
this is a the new programming
Netflix has quite probably is closure
to build very succinct the hell up to
your cell for doing
I'm I do and big data processing it
looks exactly like closure
it is closer so even better than closure
a
nothing is you can run this locally and
push a button
and it will distributed over your do
cluster right there
same thing and you can run your own
closer functions there too and will ship
them and everything else
so we just do this we do this everywhere
every what there's a type that's a
station system proposal uses data
there's a schema language for closure
that uses data there are
many kinds of logic the S l's all
news data other as a representation and
and this allows you to do something
that's very interesting which is a good
write and edit DSL
how people bring a DSL and had more more
pressure
to make it or incomplete and and you
know general purpose
you start with the DSL the tax on line
that people like dizzy
could have conditionals could have case
could gamble
you know there was one more stuff um so
when the
cool things about doing D S l's in a
language like closures that you can sort
i've
kohut co-op all up closure inside your
DSL
like oh you whether it was short you all
you have to do is expose something you
know you to flow through to closure and
let closer develop
I'll so it's very powerful you keep
doing this
the the other big point to programs as
late as it allows you to write
program generating programs and that
takes a few forms
in the small there's a yeah capability
in closer and other lists called macros
they're nothing like see macros full
from your past maybe
thing I told your um their
functions have data structures to data
structures for basically it says
the compiler for closure says if you
declare something as a macro
thereby see that in the program I will
call your code
I will give you the form as data that I
encountered
and you give me back a different for you
can do any transformation you up
she's you can build your own syntax you
can build
constructs you can extend the language
sorry what we do not need to wait for me
or for anybody else for for the Java
JSR whatever just go
though so we're we're doing all this up
with raw data structures 0
and and how do we contrast that with
objects
right the rain to parts of
this warning in advance
the so objects like marionettes
every as the burrito analogy but I can
Eivissa not like burritos
objects like marionettes hope they have
all these methods I'll
rape and anybody who has a house access
the object it's like they have that
that control stick thing wikipedia says
that the person has called the
mastermind her
I can buy a males who agree with them
but that's a cool name
like you can remote control the other
which you can write if you have a
reference the object no sabe you can
call any of its members where you feel
like it
and whatever thread you want right just
have added you can do anything you up
because you can call those things
so whoever writes the object to class
that's defend against that because
what's gonna happen when the real
program
use our pet passing around references
that object well now you have more than
one master minder
they had as many mass reminders as
you've shared references to think
and anybody can call something at any
time and what he end up doing end up
saying
well maybe their friends and their
standing next to each other and the like
okay let's make the
fancy force you know dance and I'll do
the front legs in the back legs
and that happens in puppetry right up
but a programming
you know sometimes they just go off in
like sum is trying to make the from the
horse go this way in some ways to make
the back the worse go that way
and it doesn't work and so you have to
have a all kinds a protection and you
can't you actually care
effectively do this Java and languages
like it
don't give you an effective way to do
this so you're suffering from this
constantly as soon as you have a
reference sleep
your suffering
90 you say all I is value objects from
below in with you
don't everywhere you could but you don't
and situ Matic and still hard and you
still don't even know
like if somebody gives you a reference
right to an interface
can you know that they will start
dancing on you
is there anything in Java that will tell
you
it's not gonna move around that anyway
any concert anything the type system
anything no nothing
so everyday you have this
unhappy face and then I'm
the final problem and but we'd how many
people rated program is just a program
like it says by itself
and it reads you know center dance but
standout prep
compiler writers write and there it is
great language is a really good at that
it but that's not the real world we're
hoping to rate programs at other
programs
routinely yeah how we do it all the time
how people put
objects on the wire Wow
has terrifying
a alright well just so
you so you know a long time ago we
decided I was a bad idea
so I don't know we got them now or
whatever but
a yeah objects to travel a la raza you
can take it you can pretend you can make
all these elaborate things but
I'm a most the most these things have
have failed in practice right you can
you can get away with it
in small circumstances but it's not how
things work
but we don't actually give somebody a
reference to something allow them to
sort remote controls
right so nose was saying before
if your API takes an object especially
by a reference on interface right so
you're on the country classes
definitions or
make you feel better about not changing
do you know if it's going to take right
can you type system help you with us
which is really it's such an important
thing for the robust secure program to
control us
the answer is no you getting no help at
all so you have defaulted helms
leave you completely on your own to deal
with these problems and really
in a position to encourage it encourages
you to create his problems just back sir
it the other thing is so let's say
you know let's say our allies not
allowed at your company
how you build are not allowed to use our
might accompany
a result allowed to the desired
well love you
go to yeah that's actually not as bad
them but
but let's let let's say you're allowed
right and so is as I want to this
service eyes are things how many people
would shoes are on my over something
else
like a stupid here anything else
by nobody nobody would right
because and then what you have to do are
you have this interface you said I took
an object they expected all these
methods on the object and now
I have to talk over a wire what you have
to do map
mapping break object-relational mapping
object blossom at object something map
every time you want to get to and from
the outside world
especially stuff you're not writing so
are my eyes out other questions right
you're going to get like some webs
website to like
acceptor are michael's or make are
michael's the right
um anytime you have to go outside up
your box out on your own worldview
garages worldview you have to map bed so
you know at Java programmers and I was
going to propose a bill I kept checking
sickle say all require such a racial
mapping and that's like a problem sequel
no it's a pilot objects but thats
others are not the way the world works
nothing in the world works that way
people that hand their strings out to
other people like star yanking on a man
like that so we're going to build this
is that so we're going to have like a
soccer team
right is like I was gonna reference to
somebody else and like you know you call
pass to me it like
were built to specs be getting nightmare
that's not how the world works it's
completely not the way the world works
is that help is it's worth
so we say objects in the way to model
the real world it's not at all
that's a complete programming
fabrication it's not very realistic
it's not a good fit for almost anything
that's in the outside world so you can
build your own
world where all the stuff makes sense
but it's inherently
I would call idiosyncratic but in
particular
it's not the way that systems work but
systems in the large
I solar systems
well the words as the means to cause to
stand
I love that idea I mean I always think
I'm there with his legs or
self assembling to try to make something
that stands up
this independent parts that you can act
together
substantially independent parts right
because you need to cause something the
standards like one thing that already
has three
three legs are not you know causing it
to to stand up
it's two independent so the things that
matters and
in general we try to build systems in a
way that makes them
independent right to me wanna care of
another services in the same programming
language that we are
by the same runtime the same version was
our on site where the same type system
we want a bill this is like that where
we care
no we know why because it's gonna be
brutal rape
it's gonna be hard to make changes ran
to agree with the other person I
tomorrow we're gonna have this thing and
everything's every different note 32
well you know we don't do that the
Internet
doesn't work that way no
by it we don't we know the specifics of
we do general stuff we try to be as a
independent as possible
most things out between systems is one
of two techniques
right these are PC with playing data
Outback
or they use cues we send data survey
shows up later and
gets the data just sending just flow
data around
this is the way the systems a bill big
systems big successful systems like the
Internet
and most systems a and those this is a
flexible
you measure the flexibility in terms of
how much independence they support
can you independently developed as part
of a system can somebody upgrade their
part the
system and not mess up the other person
I
because like Twitter is not gonna tell
your web browser when
they change the homepage in like to have
Safari you know do something special
that's not how it works mary has
independent
development independent I'm friends then
the other thing is critical to this is
that if somebody else on the other end
is gonna change
yet to be tolerant of things being
different are you can say well here's
our contract you know then I heard
things out to be exactly this way and
I'll work on that will work
then if we're gonna change anything we
have to you know
have lunch and
have a meeting and and and decide the
stuff again
yet to be tolerant and accepting some
more things we don't have to change the
locks up
and so these systems are inherently
dynamic and they're inherently
extensible
but that's what it does I just said
systems are made with dynamic types
extensible types I they can accept data
that we're expecting to see it won't
make them fall over and hopefully
they'll do a good job of
propagating it I they're all made this
way
so that the other fundamental idea of
closure
we should build inside the bar systems
like we build
outside the bar systems all those value
propositions that accrue to systems
we want them how many people want to
have a meeting every time they change a
class
or subsystem how people have meetings
every time they change across
yeah you have the right discover break
such gonna break
so we should we should communicate using
immutable data
insider systems for the same reason we
do outside it makes our systems
more robust make some easier to change
it makes the independent parts
separate makes it easier to move them
around by
we get loose coupling get subsystem
independence for your flexibility
and what's the mapping well there's no
real mapping range
this doesn't need mapping by RPC becomes
PC right we can we can call functions
we work all functions before there was
are right then they had our
we had PC before we had our pissing we
can go back to PC
we can do that but we can pass data to
functions get data back
we used to be able to do it that we had
all Slabbert stuff
I forgot how to do it by and we can
implement cues and flow
insider insider programs using cues or
channels or things like that
says the other key idea now
there's going to be process and stay
bank closures not a close as a practical
language of course
you know process and state but there's
no reason to run your program I just
picked up your hot and cold
right you gonna have stated the facts
but this is another area where we're
left to stew poorly with nothing
in inch in object-oriented languages
like Java you have nothing here
make there are very fancy functional
languages that have
purity although where they will force
you through the type system
to identify Eisley all the possible
program I could do I owe
or have any kind of effect they'll have
to do it by a purity or via fax systems
right and then there's the alternative
to that is
nothing by muscle most people have is
absolutely nothing
and then you could also have re if I'd
constructs at least make
X a state change explicit and that's
where closure says
because in Java and C pas plus in
c-sharp you have nothing
miss got nothing yep some really rock
concerts like
new taxes and you know a pat on the back
and good luck buddy and re brian's book
the
it so so closure doesn't have any purity
to it but it has explicit concerts for
state
these are like you can imagine being
variables that have semantics to them so
that just like a break and come in a
whack on the spiraling time and said you
say
I think if you function and you somehow
apply the function
to that variable to move from one state
to another the
but in doing so you can ensure it's free
of conflict for free races as they were
gonna become half but thing
and these variables always referred to
values
so you're always able to observe them or
dereference them
and get out a value there's nothing else
there's only
these reference cells the point to
values and values
there's no mutable object to happen
which can be whatever you like and a
classy could set the month of things
like that
right a date is a value you have a
reference to a date
you made a reference point to another
day we can change it a
a sip things you can change which are
references and their values which are
not
you can change dates a more you can
change 42 baby can change his references
but their atomic this point to one thing
so between those you get a whole bunch
of different
variance rate CAS implements
a successor should follow you say only
make this new thing if it was the thing
I
you know that my presumption still valid
it's just the tiniest percent
i jus a construct that wraps that so you
don't have to write that right to lube
raping else
so here's my function apply to the
insider that newscasts
to the lib forming and I know I guess a
clear successor should
there with no race and no conflicts
there's also an STM enclosure that
allows for
bigger transactional skynews a
modifications to occur
but the point is this construct is doing
the job and the concert is calling out
here's where the mutation is in the
system here's where the status
and the thing is you have a way to get
out a bit thank if I give you something
that didn't
don't know if it's going to change how
can you see if its value
like I give you referenced by an in
interface
to some composite type you don't know if
it could mutate
how can you save its value what's the
safe way to do that
well you know the clothes I work at all
sorry does not work what else what's
wrong with gets skate this get that you
got nothin you had absolutely nothing
so this is like a critical thing for
making a system that works
you have absolutely nothing to do this
with but you have to build up your own
commensurate on this
so like anticipate when you see other
habits was a concert or go all the way
to Haskell
has everything in between is cash for
free I'll
and that we have a in close we have
something call core a sink
which is a channel model deliver richer
than um
a cues because you have to set up the
threads and do all that the microarrays
they have semantics that are based
around
something called communicating
sequential sequential processes
but the basic idea is that you're gonna
try to encourage especially when you're
trying to convey values through a system
instead of saying I'll put the acorn
behind this tree
and you come by later and find it behind
the tree you say I'm gonna put the Acorn
on the conveyor belt
and you can take it off the conveyor
belt and there's a big difference
between those two things because if you
put something on a conveyor belt
and then go back to it what to expect
nothing age to compare both moving and
that's that's it
the flood so you can't right kinda the
logic you can write with variables
going back n reexamining a place to
update in place and you're trying to
read it again
it flows and so flow is a much more
robust way to build a system data flows
a much more robust way than variables
so we want to emphasize flow over places
so
program size matters rate smaller is
better
but they're there's an app this one the
few areas where we have like research
right people done research and said
smaller programs have fewer bugs
it's just that simple it doesn't matter
what programming languages
smaller programs of your books but
bigger programs are more bucks
longer time to market their harder to
maintain and they're more brittle
way but what i think is interesting is
that there's two flavors a
small they have a lot of languages focus
on concessional
rights which is size in the small like
how small is your if statement you know
how small as a function call how you
tinier constructs
how much overhead I'm a syntactic stuff
is there and there's a lotta languages
that focus on the Ruby and Python
a lotta languages are actually very good
at concession
'em but the the bigger impact
on a program overall I is not moving
from you know forty two characters to 20
characters that only gets you to acts
right
the biggest thing is moving from more
specificity which
below to your program two more
generality which shrinks it
that's the big payoff that's the kinda
that's the area where you gonna get
a payoff much higher than 2x so what are
the other things I think we suffer from
in other transitions death by
specificity right
all the time we have a new thing we have
a new idea new piece today to buoy have
a new class
get this get that get whatever ok after
Vitesse whatever
right it's just a glorified map except
you can even use it as a map in Java
but so generic way to manipulates up
that says get this get that cat that's a
new type
new language cuz own little bit
capillary it
so
a you're going to have more coding have
much less for use you have more coupling
bankers essentially what's happening is
every object has called a little
language
my class by interface my language this
is my the my biggest pet peeve
I want to get away from this we start
writing get this cat that's like this is
there's no purpose to this is just life
sucking so let's look at life sucking
in there in this is this is just a tiny
part actually skipped
this is just the servlet request and I
have a little bit %uh viscous or who has
a truly
doubles the size of this thing but my
question to you is
how many naps do you see here were like
give a name yet about you I got one I'll
and when i text you can't do that
auction airstrike hurry up
year Heights OH
for so this game is heartbreak there
there some a I got I got three inside in
the overall thing is a nap too
cya for right of by
help picking it apart what's really
interesting is look at these map
interfaces
they're all ad hoc guess what else
they're all different one has said
anything when you can actually get the
map
when you can get a list so you can get
out with types this 4
different maps in this one class this
crazy
enclosure we just use maps right
this stuff came over wire in St Pete as
text
how do we turn it into this what
happened II
what happened why you know this is crazy
now consider I told you the curly braces
are maps open .coms want to hear you
seal the Masters
like this because the store laps right
if you gonna write code that manipulates
said other stuff
every single unico Dios gonna be special
has the you know use the whatever job X
surly blah by entering code explicitly
to this thing
it is another way to do is to peanuts at
the resort was a probably the only way
to do is to pay
but there was another way jewish to pay
will you be able to reuse echoed
no it's orenthal hard-wired to this
person or persons idea Lake
one ish few requesters it so yeah
okay there's a tiny little benefit right
top works in your ID
will you oh my goodness cuz I could
never remember that so I'm
but something better happen when I press
stop because yeah
I'm doomed otherwise because I could
look at the SFI spec annealed like we
could agree on his name still
I just don't get it you know you can
tell a kid
not to put up spoon in a blender in turn
up and like
they will remember that their entire
lives they'll never make that mistake
but grow up a dull programmers like we
need we need protection
right for from the stuff up but the
protection we get this really minimal
what's the cost it's huge but that's it
idiot in inconsistent interface
it's incredibly idiosyncratic the
interface is huge so if you want to like
have a second plantation
you get to work think there's a ton more
code section summit
you can't use any libros already have by
with the closure version
all the Mapco class at those hundreds of
functions they work on this
you can create this with them you can
read this without you can merge to these
with that
act like you have no new code to
manipulate us know new code
all the front is already know manipulate
a spirit bus
assays are represented as data which
they were by the way before
we met them right that
testing rate it's easier to test data
can you make a program that makes this
Yakima for the next one of these
mmm yeah
okay and this is another problem is
your typical Java program has 223
orders of magnitude more aback more
about this
right 100 classes to couple hundred how
to develop programs that more than a
thousand classes
yeah
at a party alright
so it's ok order for has a smaller both
ways
they're more concise and they support
generic programming cuz we just program
what is data structures we represent
information is playing data
so I in this is always the biggest
reservation I love my types I like my
dot
I like what I D E you know I i cant
I can tell something a little less and
it's true by if you have
a the types of Java right not to answer
to Spencer tyson's can do more but the
other types are some Java
you can you can catch note I puzzle
pressing international surpass
strings but its really likely that your
tests
or your rebel its russians gonna catch
the stuff that is not a quality metric
that is not sufficient for quality
part applauding right no typos but it's
not sufficient for quality
but the quality is in all this other
stuff you had that would we say we have
no way to do a statement as red
worker straight highly couple programs
were inflexible
we're not meeting the customer
stakeholder quality metric at all in
fact were pointed against it
time and time again or point at the
wrong thing
and because our code is so huge we can't
even really understand what it does
anymore so the biggest
source about some programs which is
misconceptions rate everybody gets a row
I talked to stake although they told me
this I don't think at 1.i think
situations when I wrote the program
that's the Actos is the real bugs in
programs
by everything else a superficial but
those are the real bugs they're harder
to say
so I think this the full details are big
one I look estimates I was there was
interesting to me because I was like to
look up words what this economic mean
and actually means relating to household
management
it so the idea of home economics is
kinda redundant
that's what the word means and I would
say that you know Server programming
house
is just like a it's like a hoarders the
light
everything there's too much stuff in it
everything is too big we need to many
people to do basic things
um there's a lot more to closure this
was not a tutorial on closer but the
important thing is that most visible
libraries closure grows by a library is
the core
is really pretty vigorously protect
against growth
it's not like the no
experiment the language design the
so the one other particles I'd like to
talk about his polymorphism
and it just because it's another example
simply haven't talked a lot about simple
but when the cool things about closure
is up I morphism
is independent the words it doesn't
require
inheritance you can imagine closure
something called protocols there a set
of functions
that work together um their polymorphic
on the first argument
so it's like the same person will
dispatch your havin Java
so you can imagine I miss interfaces but
they don't require inheritance
and the beautiful thing about not
requiring inheritances you can have a
protocol
you can extend it to something that's
finished know something that
son wrote a long time ago and is never
gonna change in certainly is never going
to implement their interface
right you can also take stuff from two
vendors right
because what we usually have we have the
framework problem is a privilege
framework in Java
the one that comes with that and people
implement those interfaces
but if you have a Pisa suffer from
vendor a as an interface
and stuff from vendor be like others
from Ben Derby that you want to use
how do you have been Derby table
inventor is interfaces
it just doesn't happen was even worse
than a simple spots were open in Java we
saw this problem
there's a privilege framework the people
but the interest is an otherwise
implement
it interfaces are parochial a small
so we have play more physical closures
as
ala carte and that reduces couple
coupling proceed only to derive
alright I know well mabel may be closer
seems more interesting
so it's not just about technology right
saving about this programming thing it's
also about because some things like that
and the first thing that's great about
using closure is that
you get to keep connections to the
kansas may already know
not only the runtime and that deployment
environment but those libraries
by to interrupt in both cases is
extremely good a fault those closer data
structures I sho do they all implement
take appropriate Java you till map
whatever they hope to implement all the
job interests you can just take one of
those things you wrote in square
brackets that was pretty easy
passive something expects a
java.util.list that the infamous random
access
right there works
ready to go um closer is also very
stable
you know I value that in Java I think
it's important part of why Java grew
and closer takes the same approach is
not like kids on github hacking away
adding every new idea or
as I think tank experiment I'm it's made
for production use
and it's very stable all the programs
from a long time ago still run
there are books if you want to get
started there's a lot of books though
for closure I spoke your last five years
ago
and people never heard a closer look now
those planning there are tools to look
does an idea though it's like a clips
with cook code highlighting for closer
in like structural Navin
who that's intelligent a same thing
break points look at you type and it
starts poppin stuff up
this is good we're good and you have a
rebel down there which is even better
what should get used to that
does it I love tools um in various areas
a there are lots of libraries that says
12,000
repose and get up there are lots of
users
the mailing list is almost 10,000 users
on it and they're all happy nice people
I promise who they are I think that
matters if you've ever seen the old list
community
they were all have been nice people the
closures are happy and nice people
a decir closures at a red months where
language is that we will
way up there this old functional list
what is happening in the world
look at it there is on the tech radar
adopters actually gone off the tech
writer like of course you should be
using close already
and and they're right but people are
using those already
a lot of people are already is a closer
banks is closer
plan services closer play Analytic
Services closure
I'll so
up people are being successful with that
so
I think that's a short message for today
the idea behind closures to get you to
better and more flexible programs sooner
and
the way it approaches that is by being
did oriented and simple
and i really appreciate your time thanks
