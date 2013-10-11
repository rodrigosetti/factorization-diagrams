# Factorization Diagrams

Nothing new here. Inspired by [this
post](http://mathlesstraveled.com/2012/10/05/factorization-diagrams/) I decided
to implement myself - for self educational purposes - the factorization diagram
algorithm.

The factorization diagram for a given integer is a representation (by grouping)
of it's prime factors.

For example, see the following diagram for the number `210`, which prime
factors You can see that there are clusterings for all it's prime factors: `2 *
3 * 5 * 7`. The smaller factors have the biggest (out most) clusters.

![210](examples/210.png?raw=true)

Here are the factorization diagrams for the numbers 1 to 36 (from left to
right, and then from top to bottom):

![1-36](examples/1-36.png?raw=true)

Prime numbers, naturally, just have one factor: themselves. Therefore, their
factorization diagram will look like a single cluster. See for example for the
prime `331`:

![331](examples/331.png?raw=true)

If you multiply `331` by `2`, you get `662` (with now two prime factors):

![662](examples/662.png?raw=true)

Powers of two (e.g. `2^10 = 1024`) forms the [Cantor Dust
Fractal](http://en.wikipedia.org/wiki/Cantor_set#Cantor_dust):

![1024](examples/1024.png?raw=true)

Powers of three (e.g. `3^8 = 6561`) forms the [Sierpinski Triangle
Fractal](http://en.wikipedia.org/wiki/Sierpinski_triangle):

![6561](examples/6561.png?raw=true)

Powers of five (e.g. `5^6 = 15625`) forms the [Koch Snowflake
Fractal](http://en.wikipedia.org/wiki/Koch_snowflake):

![15625](examples/15625.png?raw=true)

