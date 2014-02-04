Lub is an experiment in computing least upper information bounds on (partially defined) functional values.
It provides a `lub` function that is consistent with the unamb operator but has a more liberal precondition.
Where `unamb` requires its arguments to equal when neither is bottom, `lub` is able to synthesize a value from the partial information contained in both of its arguments, which is useful with non-flat types.
For more info on `unamb`, see the unamb package package [on Hackage](http://hackage.haskell.org/package/lub) and [on GitHub](http://github.com/conal/unamb).

I got inspired for this package after [stimulating discussions](http://tunes.org/~nef/logs/haskell/08.11.17) with Thomas Davie, Russell O'Connor others in the #haskell gang.
