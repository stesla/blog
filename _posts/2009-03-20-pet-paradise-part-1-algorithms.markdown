---
layout: post
wordpress_id: 137
title: "Pet Paradise Part 1: Algorithms"
wordpress_url: http://blog.alieniloquent.com/?p=137
---
So [Jay Hannah][1] over at [ODynUG][2] has issued a [programming
challenge][3]. Far be it from me to turn that kind of thing down. I am going
to be doing this in Erlang.

Before I dive into coding on this, I want to give some thought to the
algorithms I want to use in order to solve the problem. The way I tend to
approach this is to consider what other problems I've solved, and see which
this one looks the most like. Given my strong mathematics background, I tend
to draw on theory-laden ideas. In this particular case I'm going to use graph
theory. I plan to construct an [interval graph][4], do some processing on it,
and then find a [coloring][5]. I'll then examine each color class, and find
the one that produces the largest revenue.

The first step is to turn the raw demand data into an interval graph. In this
graph, each individual demand will be represented by a vertex. An edge will be
added to the graph between each demand that overlaps in time. At this point,
we're paying no attention to cats, dogs, small cages or large cages. All we
want to see is how the demands overlap in time.

The second step is to group together the vertices that represent demands that
can be met simultaneously. So picking a vertex, we'll traverse all of its
edges, and if we can combine two vertices in to one that represents them both,
then we'll do that. This presents an interesting problem, as the order in
which we traverse the vertices makes a difference as to how many can be
combined. However, it's more likely that we'll be able to combine demands if
they have fewer pets or if their pets are smaller, so I'm going to traverse
them in increasing order of pet quantity and size.

Next, we'll find a coloring for the graph using the [greedy coloring
algorithm][6]. To use this algorithm we need to pick a vertex ordering. Since
the number of colors used can at most be one more than degree of the most
connected vertex in the graph, and the greedy coloring algorithm relies on
looking at the colors of previously colored neighbors, it makes sense to order
the vertices in decreasing order of degree.

Armed with a coloring, we simply sum the rates for all of the demands in each
color class, and then select the color that gives us the biggest revenue. The
demands in that color are the ones that get accepted, the rest get rejected.

There are some drawbacks to this approach. For one, the graph coloring problem
is [NP-complete][7], so we cannot always be certain that the coloring we
produce is the _optimal_ coloring. Also, because of how we're combining
demands together, it is possible that a different combination would yield
higher revenue. However, this algorithm should perform fairly well.

   [1]: http://jays.net

   [2]: http://odynug.kicks-ass.org

   [3]: http://github.com/stesla/odynug/blob/3d3d75cc4d70b189c625579ce841de3ce7ada68b/pet_paradise/README

   [4]: http://en.wikipedia.org/wiki/Interval_graph

   [5]: http://en.wikipedia.org/wiki/Graph_coloring

   [6]: http://en.wikipedia.org/wiki/Greedy_coloring

   [7]: http://en.wikipedia.org/wiki/NP-complete

