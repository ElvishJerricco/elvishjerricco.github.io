---
layout: post
title:  "Automatically Generating Magic Decks Using Deck Building Strategies"
date:   2015-09-24
categories:
---
Magic deck building is no easy task. You have to identify a strategy,
choose cards that go well together, and make sensible cuts.
With all the cards available in a given format, this can be a daunting task.

The other night, I was trying to add a Cryptic Command to my Modern
[Grixis Pyromancer](http://decks.deckedbuilder.com/d/108894) brew,
and rather than trying to guess which card to cut,
I decided to try something new.
I sorted the deck from best to worst card.
Now, doing this requires that you treat additional copies of a card as separate
cards, with different values to the deck.
So for example, the fourth Remand is worse in the deck than the third,
and they’re probably not adjacent in the sorted list.
So you have to be careful with how everything is sorted.

After finishing the sort,
I looked at the list and cut the worst card (that silly 4th Remand).
Then I had an idea. What if I just took all the playable cards in a format,
threw them into one multi-hundred card deck, and sorted it?
I could remove the worst card,
re-sort it (to account for changing composition of the deck), and repeat.
Eventually, as long as I’m making sure to rank cards that don’t fit the emerging
strategy on the low end of the deck,
I should come to something kind of playable.
Obviously this would be a very long process to do manually,
so I decided to write a program for it.

So how does a computer know how to rank how good a card is?
I decided to use popularity.
This reminded me of [Frank Karsten’s aggregation algorithm](http://www.channelfireball.com/articles/magic-math-a-new-way-to-determine-an-aggregate-deck-list-rg-dragons/)
(which was implemented [here](http://www.empirimancy.net/metadeck/)).
TL;DR, Karsten outlines a method for taking various decklists for a specific
deck archetype (from [mtgtop8](http://mtgtop8.com/) or wherever),
and aggregating them into one smooth, averaged list devoid of awkward inclusions
some random guy made.
His algorithm works by ranking each card by how many of the input decks included
it (again, treating different copies of the same card as different cards),
then cutting out the lowest ranked cards.
What remains is an effective aggregate.

The problem with this approach is that it only works for aggregating
an archetype. You can’t give it the lists of, say, the top 32 of a GP and have
it build a deck.
It would just build a list of the most used cards,
without any regard for staying in color and having a coherent strategy.

Karsten’s algorithm uses what I call a **first order ranking system**.
An *Nth* order ranking system looks at all combinations of cards with size *N*
or less in all the input decks and ranks those combinations by how many of the
input decks run them.
For example, Snapcaster Mage + Lightning Bolt would be a second order
combination that would be fairly highly ranked,
due to the number of decks that run the pair.
So when Karsten sorts by each card’s popularity on its own,
he’s looking at combinations of size 1; that is, individual cards.
Instead, by ranking combinations of cards in addition to individual cards,
the algorithm can reason about how well different cards go together,
and build a deck based on effective *combinations* of cards.

---

The New algorithm
---

For an *Nth* order aggregation:

- Create a ranking structure.
  - This structure keeps track of the number of times any combination of cards
    appears in any of the input decks.
  - For each input deck D:
    - For each combination C of N or less cards in D, add 1 to C‘s rank in the
      ranking structure.
- Merge the input decks into a collective
  - Remember, the 3rd Lightning Bolt is a different card from the 4th one.
  - But the 3rd Bolt is the same card no matter which deck it’s in.
  - So if every deck has 4 Bolts, the collective will still only have 4 Bolts.
  - But if every deck has 2 Path to Exiles, except one has 3, the deck will have
    3 Path to Exiles.
- Repeat the following until the collective only has 60 cards left.
  - For each combination C in the ranking structure:
    - If C is not contained completely in the collective, skip C.
    - Otherwise, for each card in C, add the following to the card’s ranking:
      - (rank of C in ranking structure) * 1 / (2 ^ size of C)
    - This way, the lower the order of C, the more important it is to rankings.
      Thus, the individual popularity of a card is more important than it’s
      interaction with other cards.
    - What this does is rank each card in the collective by the sum of its
      combinations that are present in the collective.
      Thus, cards are ranked by how well they go with the remainder of the deck.
  - Remove the lowest ranked card from the collective.

The nice thing about this algorithm is that you can specify whatever order you
want. The higher the order, the longer the program takes to run,
because it has to generate all combinations of size N
(which grows extremely rapidly).
But higher orders will obviously produce better results.

An interesting fact about this algorithm is that when it’s run using only first
order rankings, it will produce the exact same result as Karsten’s algorithm.
This is because, as I said, Karsten’s algorithm is a first order ranking,
so the two are equivalent.

---

Showtime
---

[I have written a program for my algorithm in Haskell](https://github.com/ElvishJerricco/MTGBuilder).
I fed the top 33 deck lists from GP OKC 2015 into it as my test.
With 2nd order rankings,
[this is the list it came up with](http://decks.deckedbuilder.com/d/109287).
Affinity! Ok, it’s a little weird.
It’s a pretty stock affinity list, but with 4 Bolts and a Forest thrown in.
I ran the program again using 3rd order rankings, and it came up with
[exactly the same thing](http://decks.deckedbuilder.com/d/109293),
except the Forest became a Stomping Ground.
I suspect this was caused by the abundance of Affinity and Naya decks in the
top 33. Since all the affinity lists ran 4 Galvanic Blasts and a Mountain,
I think my algorithm correlated the Mountain with 4 Bolts.
Then because of the Naya decks,
it correlated the Bolts and Mountain with the Forest.
And in 3rd order, it found that the Stomping Ground fit better than the Forest
(which it does, I guess; but there are still no green cards in the deck).

I was surprised to see it choose the same list in both 2nd and 3rd order.
But I suppose it makes sense, with 4 Affinity decks in the input.
Removing the Naya decks from the input fixes things a bit.
It came up with [this list](http://decks.deckedbuilder.com/d/109289).
It basically just cut the Forest for a Spellskite.
Good change! But it still has those 4 Bolts,
which I believe still comes from the basic Mountain.

Overall, this is impressive.
It was able to produce a mostly appropriate mana base with a coherent,
strong strategy in the deck.
Who knows; maybe Bolt *would* be a good inclusion in Affinity.
But I wanted to see what it would do if I tried it with the Affinity decks
removed, and again with both Affinity and Naya removed.

With Affinity removed, Naya back in, and using 2nd order rankings,
it produced this
[fascinating 4 color Collected Company deck](http://decks.deckedbuilder.com/d/109291).
The mana base is a little off,
but I’m impressed that it handled a 4 color mana base so well.
The only blue cards in the deck are 2 copies of Snapcaster Mage.
I believe this was a result of a correlation between Snap and Bolt
(which, by the way, was the only red card).
Every Grixis deck ran 4 Snapcasters and 4 Bolts,
so those cards should be strongly paired in this algorithm.
Still, it’s impressive that it recognized that Snapcaster
made the deck require blue lands.
Maybe using 3rd order rankings will help remove the awkward 4-color-ness of
this.

[Better](http://decks.deckedbuilder.com/d/109299)!
Still not ideal.
It could use more copies of Voice of Resurgence.
But it’s good, and only 3 colors!
And the mana base works as well.
Now, it’s a little awkward that it’s running 12 fetch lands,
one of which is a Scalding Tarn.
But it’s technically fine
(although strictly worse than just running the 4th Arid Mesa).
And the rest of the mana base is pretty good!
The deck itself is nice. Basically just big, big zoo.

Removing both Affinity and Naya, using 2nd order rankings,
it made this [almost-Twin deck](http://decks.deckedbuilder.com/d/109294).
This time it only used 3 colors, which is nice.
The mana base is very good, as well.
Not ideal (most would run two more Islands, and cut the Blackcleave Cliffs),
but very very good.
And the deck is pretty great too.
It did a great job building a good Grixis list.
The problem is that although it runs 4 Splinter Twins,
it only has 1 Deceiver Exarch.
It successfully included the 2 Pestermites,
but it left out 3 copies of an important combo piece, in favor of Thought Scour,
which many other Grixis lists were running to support Angler
(which didn’t end up in this build).
Again, maybe 3rd order rankings will fix this.

[4 Deceiver Exarchs](http://decks.deckedbuilder.com/d/109304)!
Success! The 3 inquisitions are a little unconventional.
And again, the mana base is really close, but not quite correct.
Still, this is really impressive for a deck that was built by a computer.
It has the combo right.
It has good cards filling out all the slots.
It all seems to work together quite nicely.

Small note about performance: In my testing on my laptop,
2nd order takes about a minute to run, while 3rd order takes about half an hour.
You can imagine how long 4th order would take,
so I haven’t spent the time to test that yet.

---

As an Aggregation Algorithm
---

What if we used this algorithm for Karsten’s original purpose of making aggregates of an archetype instead of making decks out of a diverse top 32? How does it compare to Karsten’s algorithm? To test this, I pulled 20 lists of Affinity and 20 of Twin from mtgtop8 and used them to make Twin and Affinity aggregates.

- Affinity
  - [Karsten](http://decks.deckedbuilder.com/d/109342)
  - [3rd Order](http://decks.deckedbuilder.com/d/109343)
- Twin
  - [Karsten](http://decks.deckedbuilder.com/d/109340)
  - [3rd Order](http://decks.deckedbuilder.com/d/109341)

With Affinity, both algorithms produced the exact same things
(except of course Karsten’s algorithm produced a sideboard,
which my algorithm isn’t tuned to do yet).

With Twin, they’re one card different.
The 3rd order ranking produced a deck with one more land (23 total),
while Karsten’s algorithm produced a deck with
an extra dispel in place of that land.
I think this change is indicative of my algorithm’s ability to
recognize patterns better; as Twin traditionally runs 23 lands, not 22.
It’s pretty rare that I see a Twin deck with any other number of lands than 23.
Using 3rd order rankings allowed my algorithm to see this for itself.

Sidenote: The previous tests had almost 800 unique cards to examine.
These aggregates have around 120, despite using 20 different lists.
Because of how much smaller the input is,
aggregates with 3rd order rankings take about a minute,
instead of the ~30+ minutes they were taking for the previous tests.
Feeling ambitious, I tried it with 4th order rankings.
This took about 10 minutes, and produced the exact same lists as 3rd order.

---

Problems With the Algorithm
---

#### Bolt in Every Deck

Bolt is so heavily played,
this algorithm refuses to build a deck that doesn’t play 4 of them.
This isn’t necessarily an issue, because Bolt does build good decks.
But it excludes or warps non-Bolt archetypes.
For instance, Affinity isn’t seen as a deck that has any business running Bolt.

What this means for the algorithm is that if the input decks tend to favor one
card, the algorithm only cares about decks which run that card.
Again, this isn’t necessarily an issue.
If everyone plays that card, it is because it’s a good card,
and the best deck will likely run that card.
It’s just a little disheartening.

#### Premature Removal

I believe the reason that the 2nd order Twin deck didn’t have all the Exarchs
was because they were cut too early, before it realized it was building Twin,
and the algorithm didn’t have a chance to see if adding them back in would
improve the deck.
It’s possible that the fix to this is to just never go below 3rd order.
But I believe there could be a way to let the algorithm reevaluate previously
removed cards.

#### Sideboarding

The algorithm so far doesn’t support sideboards.
In fact, sideboard cards are discarded *at the parsing phase*,
because I don’t know how to make the algorithm use them.
This is by far the biggest issue with it.
And I have no idea how to fix it.
All of the ideas I’ve had about this have one common flaw:
It is easy to produce a mainboard,
and then build a sideboard that goes well with the mainboard,
but it would be much better if sideboards were considered for the entire
process.
There are decks that are made noticeably better by their sideboards,
and I’d rather not let the algorithm stray away from those just because it won’t
consider their sideboards.

---

Goals for the Algorithm
---

I have two main goals for this algorithm going forward.
First, I need to be able to produce sideboards.
But I’ve already talked about that.
The other thing I want to do is provide some way for users to change how a card
is weighted.
For example, there should be a way to make it gravitate towards Grixis decks,
or force it to build a Tarmogoyf deck.

---

Conclusions
---

This algorithm is a sophisticated method of creating a deck out of a list of
input decks.
It is good for two purposes.

- Looking at a meta and producing a good, playable deck.
  - This is good for exploring which cards made decks successful,
    and seeing how those cards interact from a playability standpoint.
  - For example, I never would have seriously considered 2 Snapcasters in
    Collected Company decks, but it makes a lot of sense,
    if you’re ok with the painful mana.
- Looking at an archetype and producing a high quality aggregate of that
  archetype.
  - This is good for producing a starting point for an archetype.
    If you’re interested in some archetype X,
    you can aggregate it and start from that list,
    rather than choosing an arbitrary list from a random player.
  - I’ve used Karsten’s algorithm in the past for producing entire gauntlets of
    aggregates, in order to get a statistically accurate testing experience.
    This algorithm is a slight improvement on Karsten’s,
    so my gauntlets can be improved.

All in all, it’s a cool program doing something really cool.
It’s pretty impressive that it can narrow down a playable deck from ~800 loosely
related cards.
Let me know if you have any thoughts on the problems I detailed,
or the algorithm in general.

**Update**: MTGBuilder is now on cabal. If you have the haskell platform
installed, you can install this program with cabal install MTGBuidler.

**Update 2**: I’ve released another update.
I won’t go into detail here, but it uses an additive algorithm,
which is very different from what’s outlined above.
It uses the same ranking concept though.
