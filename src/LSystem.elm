module LSystem exposing (Rule, apply, print)

{-| Implementation of a L-System.

L-Systems are algorithms originally developed to simulate plants, now
widely adopted by the generative geometry community.

The core concept of a L-system is to rewrite states based on a previous state,
using a set of user-defined rules. For example:

    type State
        = A
        | B

    rule : LSystem.Rule State
    rule state =
        case state of
            A ->
                [ A, B ]

            B ->
                [ A ]

The result is concatentated after applying the rules to each state in the list.
If we start with the state `[A]` and apply the rule a few times, we would have:

`[A]` => `[A, B]`
`[A, B]` => `[A, B, A]`
`[A, B, A]` => `[A, B, A, A, B]`

So why is this useful for generative graphics? As it turns out, L-System states
can be thought of as drawing commands. Here is an example of a "turtle graphics
system":

    type State
        = D -- | Draw forward by 1 unit
        | S -- | Skip foward 1 unit without drawing
        | L -- | Turn left by 90 degrees
        | R -- | Turn right by 90 degrees

    rule : LSystem.Rule State
    rule state =
        case state of
            D ->
                [ D, D, R, D, L, D, R, D, R, D, D ]

            S ->
                [ S ]

            L ->
                [ L ]

            R ->
                [ R ]

Experiment with the starting state `[D, R, D, R, D, R, D]`, which draws a
square, you can try it out by pen and paper, and see that the rule draws a more
interesting square with rectangles within. You can see an example of the above
after applying the rule 3 iterations [here](https://twitter.com/Xavier_Ho/status/995651719064252416).

If you are after a very comprehrensive and dense introductory textbook,
[The Algorithm Beauty of Plants](http://algorithmicbotany.org/papers/abop/abop.pdf)
is a great introduction.

-}


type alias Rule a =
    a -> List a


apply : Rule a -> List a -> List a
apply rule states =
    List.concatMap (\s -> rule s) states


print : List a -> String
print state =
    String.join "" (List.map toString state)
