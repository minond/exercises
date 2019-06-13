#| Write a function 𝚙𝚊𝚕𝚒𝚗𝚍𝚛𝚘𝚖𝚒𝚌 that takes a list of numbers and evaluates to a
   list of numbers of the same length, where each element is obtained as
   follows: the first element should be the sum of the first and the last
   elements of the original list, the second one should be the sum of the
   second and second to last elements of the original list, etc. Example:
   (𝚙𝚊𝚕𝚒𝚗𝚍𝚛𝚘𝚖𝚒𝚌 (𝚕𝚒𝚜𝚝 𝟷 𝟸 𝟺 𝟾)) evaluates to (𝚕𝚒𝚜𝚝 𝟿 𝟼 𝟼 𝟿). |#


#| Define a stream 𝚏𝚒𝚋𝚘𝚗𝚊𝚌𝚌𝚒, the first element of which is 0, the second one
   is 1, and each successive element is the sum of two immediately preceding
   elements. |#

#| Write a function 𝚜𝚝𝚛𝚎𝚊𝚖-𝚞𝚗𝚝𝚒𝚕 that takes a function 𝚏 and a stream 𝚜, and
   applies 𝚏 to the values of 𝚜 in succession until 𝚏 evaluates to #𝚏. |#

#| Write a function 𝚜𝚝𝚛𝚎𝚊𝚖-𝚖𝚊𝚙 that takes a function 𝚏 and a stream 𝚜, and
   returns a new stream whose values are the result of applying 𝚏 to the values
   produced by 𝚜. |#

#| Write a function 𝚜𝚝𝚛𝚎𝚊𝚖-𝚣𝚒𝚙 that takes in two streams 𝚜𝟷 and 𝚜𝟸 and returns
   a stream that produces the pairs that result from the other two streams (so
   the first value for the result stream will be the pair of the first value of
   𝚜𝟷 and the first value of 𝚜𝟸). |#

#| Thought experiment: Why can you not write a function 𝚜𝚝𝚛𝚎𝚊𝚖-𝚛𝚎𝚟𝚎𝚛𝚜𝚎 that is
   like Racket's 𝚛𝚎𝚟𝚎𝚛𝚜𝚎 function for lists but works on streams. |#

#| Write a function 𝚒𝚗𝚝𝚎𝚛𝚕𝚎𝚊𝚟𝚎 that takes a list of streams and produces a new
   stream that takes one element from each stream in sequence. So it will first
   produce the first value of the first stream, then the first value of the
   second stream and so on, and it will go back to the first stream when it
   reaches the end of the list. Try to do this without ever adding an element
   to the end of a list. |#

#| Define a function 𝚙𝚊𝚌𝚔 that takes an integer 𝚗 and a stream 𝚜, and returns a
   stream that produces the same values as 𝚜 but packed in lists of 𝚗 elements.
   So the first value of the new stream will be the list consisting of the
   first 𝚗 values of 𝚜, the second value of the new stream will contain the
   next $\verb|n|$$ values, and so on. |#

#| We'll use Newton's Method for approximating the square root of a number, but
   by producing a stream of ever-better approximations so that clients can
   "decide later" how approximate a result they want: Write a function
   𝚜𝚚𝚛𝚝-𝚜𝚝𝚛𝚎𝚊𝚖 that takes a number 𝚗, starts with 𝚗 as an initial guess in the
   stream, and produces successive guesses applying fn(x)=12((x+nx) to the
   current guess. |#

#| Now use 𝚜𝚚𝚛𝚝-𝚜𝚝𝚛𝚎𝚊𝚖 from the previous problem to define a function
   𝚊𝚙𝚙𝚛𝚘𝚡-𝚜𝚚𝚛𝚝 that takes two numbers 𝚗 and 𝚎 and returns a number x such that
   x⋅x is within 𝚎 of 𝚗. Be sure not to create more than one stream nor ask for
   the same value from the stream more than once. Note: Because Racket defaults
   to fully precise rational values, you may wish to use a floating-point
   number for 𝚗 (e.g., 10.0 instead of 10) as well as for 𝚎. |#

#| Write a macro perform that has the following two forms:

       (𝚙𝚎𝚛𝚏𝚘𝚛𝚖 𝚎𝟷 𝚒𝚏 𝚎𝟸)

       (𝚙𝚎𝚛𝚏𝚘𝚛𝚖 𝚎𝟷 𝚞𝚗𝚕𝚎𝚜𝚜 𝚎𝟸)

   𝚎𝟷 should be evaluated (once) depending on the result of evaluating 𝚎𝟸 --
   only if 𝚎𝟸 evaluates to #𝚏 in the latter case, and only if it doesn't in the
   former case. If 𝚎𝟷 is never evaluated, the entire expression should evaluate
   to 𝚎𝟸. Neither 𝚎𝟷 nor 𝚎𝟸 should be evaluated more than once in any case. |#
