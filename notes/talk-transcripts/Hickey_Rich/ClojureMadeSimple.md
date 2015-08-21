# Clojure, Made Simple

* **Speaker: Rich Hickey**
* **Conference: [JavaOne 2014](https://www.oracle.com/javaone/sessions.html) - September / October 2014**
* **Video: [https://www.youtube.com/watch?v=VSdnJDO-xdg](https://www.youtube.com/watch?v=VSdnJDO-xdg)**

Slide1

I want thank everybody for coming. The title of this talk is "Clojure, Made Simple" on a brochoure they left out the comma. So that's "Clojure Made Simple" another words a tutorial in Clojure, an easy explanation of Clojure. It is a comprehensive explanation of Clojure at all, but I look on slice what Clojure is about a way of thinking about why you might want to use it.

Slide2

So I am the person who made Clojure. I currently work on a database called Datomic, which is kind of functional database, which is written in Clojure and runs on JVM architecture. I am co-founder of Cognitect which builds Datomic and sponsors the development and stewardship of Clojure. But the main point that I want to make about myself to this audience, because in this talk I might sound kind of skeptical about Java and Object Oriented Programming, is that I done absolutely ton of that. That's what I did for two decades, before I said if I still want to be a programmer I don't want to do this anymore. So I know exactly how apps are built using Java and C++, and C#, because that's what I used to do. That doesn't mean that I think about them is correct but that's my experience.

Slide3

But I am wondering about you. How many people are programming in Java here? How many people are happy about that? How many people are actively unhappy about that and looking for alternatives? Okay. Great. How many people tried Clojure at all? How many people never heard of Clojure and are in the wrong room? How many people have tried Clojure and are trying to get to this at work, but not yet? A few. Maybe this talk will give you some ways how of talking value proposition that will help you. How many people actively use Clojure? And somehow are accidentaly on JavaOne. And rest couldn't get into Brian Goetze's talk. I shouldn't even mention Brian Goetze's talk right now because we falling. Or maybe you just tired of Brian get about immutability, in which case you are definitely in the wrong room. No, I like Brian, is a good friend and his talks are great. So I appreciate you being in this one.

Slide4

So, there are very few people never heard Clojure, so I not gonna spend a lot of time on it, oh I've got one more question. How many have seen my talk "Simple made Easy"? How many have not? Okay a few. So, I may spent a minute describing what I mean simple. Clojure is a programming language which runs on JVM and JavaScript, and pretty much a substancial part of Clojure runs on JavaScript. So it is a programming language with which you can target both. Originally you can target JVM and CLR. There is still a port to the CLR that is maintained but this is not seem wide use. I release first in 2007. Its had, a surprising adoption especially from my perspective since then giving its characterictics. Because it is a Lisp, it is functional, it is data oriented and has a lot of things that make it seem not like the kind of language that would succeed. And this talk really be about data orientation of Clojure.

Slide5

So, "A lot of best programmers and the most productive programmers I know are writting everything in 'blank' and swearing by it, and then just producing ridiculously sophisticated things in a very short time. And that programmer productivity matters.". So Adrian Cockcroft was Cloud Architect at Netflix, now is a Battery Ventures. How many think Java goes into blank? So we know, we know this, there is something about Java that makes it not suitable for this blank. So maybe tease that a part, just of course he is saying Clojure. And this talk is about maybe why. Why this could be true? What is that makes the Clojure different and possibly better fit for that blank.

Slide6

So the first thing I want to talk about is that I think we have this tendency in programming to think about ourselved just to tone and our languages, our tools, and our techniques, and "Me, Me, Me, Me, Me", as what we're doing whatever, we lose of track of the fact we all working for somebody else, or ourselves but for a business or an organization which is trying to accomplish something in the world and the software is completely secondary to that task. Right? It should be measured always in terms of cost/benefit ratio, ROI, how quickly we can get product to market, and is what we're doing profitable. Right? If we're not doing that, we are not being good participants in our businesses and organizations.

Slide7

So what the stakeholders want? They really want two things, they want something good and they want it soon.

Slide8

So, something good. We think we know what something good, we know how to makes things good, we've these techniques. And some things are good when the techniques are successfull with them, right? When the types check, and our tests pass we have something good. But of course we all know that with our best efforts in those things, which I'm not saying that are not bad things BTW, but no matter what we do there, we end up with programs that don't work. We all know we have that type check and tests pass and they don't work. They don't work from the perspective the stakeholder - there as, they don't do with the program was supposed and what's supposed to do was something that was conveyed between people or through documentation or papers, things that are not in programming languages. They have to meet operation requirements and they've to be flexible, okay? There are some times when people just want something soon and they don't want something good. There are actually better languages than Clojure for that, right? Just give me something fast that I'm absolutely, definitely gonna throw away, will not grow, will not expand, will not take me further. So this first two things are means, they're good but they're only good in so far sa they help insure those latter three things.

Slide9 - 6:35
