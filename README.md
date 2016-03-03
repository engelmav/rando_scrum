Haskell Determines Scrum Master


When Matt left, we found ourselves trying to determine who'd be the one to hit the gong for the scrum.

It occurred to me this might be an innocuous opportunity to "write some Haskell." I googled around for good ways to do randomizations in Haskell.

## The code

After a little bit of googling, I ended up more or less blatantly ripping this off (https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms):

```haskell
import System.Random
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
 
data Gonger = Person1 | Person2 | Person3 | Person4 | Person5 deriving (Show, Enum, Bounded)
 
instance Random Gonger where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g
 
main = do
  g <- newStdGen
  print $ (randoms g :: [Gonger]) !! 0
```

Now one thing about Haskell that I love (and maybe this is just a beginner's thing), is that there is, up front, so much to explain about this.

Let me see if I can catch all the "new stuff".

## The Gonger sum type.

There are sum types. And they can be used in unexpected ways.
This is interesting because the Gonger sum type is used instead of, say, an array index, or a list of names. What makes this usable is the deriving of Enum and Bounded typeclasses (explained here in LYAH).

## Typeclasses and their instances

Ah, our own instance of a typeclass. Kind of forces you to gaze at it and see WTF it's for, what it achieves.
LYAH's best two sentences on this are here:

...we first make our data type and then we think about what it can act like. If it can act like something that can be equated, we make it an instance of the Eq typeclass. If it can act like something that can be ordered, we make it an instance of the Ord typeclass.


So the typeclass instance we defined for Gonger means that Gonger is a randomizable thing. And we proceed to define how it gets randomized inside of the typeclass instance. Sounds like a Java interface, right?
And the reason we do all of that is because we're going to pass Gonger to a randomizing function.

## Hackage helps

You can use hackage to determine how the random functions are implemented (https://hackage.haskell.org/package/random-1.0.0.2/docs/System-Random.html), if you care. Actually, knowing this is what allows us to...


* Understand why you would need a Gonger instance of the Random typeclass
* Reveal the infinite recursion of the polymorphic list a that results from the ``randoms`` function
More on this below.


## Type signatures, polymorphic return types

How to return a Gonger. This was slightly weird to me. Note the code below:

```haskell
randoms g :: [Gonger]
```

What the heck is going on?

I asked in #haskell on freenode. It turns out this is what is called a polymorphic return type.

You can see this by looking at the type of the randoms function.


```haskell
ghci> :t randoms
(RandomGen g, Random a) => g -> a
(notice the a)
```

The full definition of ``randoms`` is a recursive use of ``random``, which is itself implemented in another instance of the ``Random Int`` typeclass. (This seems kind of insane to me. Like, very convoluted. Maybe it just takes getting used to?).

Here's the definition from hackage:

``randoms g = (\(x,g') -> x : randoms g') (random g)``

So ``randoms g :: Gonger``. The purpose of this ``:: Gonger`` type annotation is to tell the Haskell compiler that we want the ``randoms`` function to return a list of ``Gongers``, as opposed to a list of any other possible things. And it's only able to do that because we defined a {{Gonger instance of the Random typeclass.


Full transcript of irc chat.
Person1
hello. quick question. is there a particular name for when one appends the type signature at the end of function application (e.g. not definition, but application). For example, LYAH has an example "read "Just 't'" :: Maybe Char"
merijn
Person1: They're type annotations
merijn
Technically type signatures are also just type annotations
Person1
thanks merijn. so appending a type annotation at the moment of application, does this assist haskell in determining which implementation of a function should be used? (for example, in the case that there are instances of typeclasses with different implementations)
mniip
technical notice: different instances, implementations do not matter at all
merijn
Person1: It specifies what type an expression should have (which should be equal to, or a more specific version of all it's possible types). The instance selection can happen based on types, yes.
merijn
Sometimes (especially when using polymorphic return types) it can't always be possible to decide which instance to pick, so an annotation helps
merijn
i.e. print (read "Just 't'")
:t print
lambdabot
Show a => a -> IO ()
aweinstock
> 0 :: Float
10:04:39
lambdabot
0.0
merijn
Person1: Since "print" can print any type that is an instance of Show and read returns any type that is an instance of Read, the compiler has a problem
aweinstock
> 0 :: Int
lambdabot
0
mizu_no_oto
> print (read "Just 't'")
lambdabot
<IO ()>
merijn
Person1: There's multiple types that are instances of Show and Read and it can't possibly know which one you meant
aweinstock
> (read "Just 't'") :: Maybe Char
lambdabot
Just 't'
Person1
thanks guys. here is something that appears (to me) to be slightly different in purpose. perhaps i'm wrong. I found it in an fpcomplete post. the line i'm interested in is this one: "print . take 10 $ (randoms g :: Coin)" (from this page: https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/randoms). The interesting thing to me is that we are applying "randoms" to a seed `g`, and telling it we want a return type of Coin, where Coin = Heads | Tails deriving (Show, Enum, Bounded)
Person1
i guess my question is, how do we get from a seed `g`, to one of the options in Coin?
mizu_no_oto
:t randoms
lambdabot
(RandomGen g, Random a) => g -> a
maerwald
Person1: Coin has an instance of Random... but afais you could delay the coin type annotation in that example
merijn
Person1: Coin has to have a Random instance for this to work
Person1: The reason the annotation is there is because if it wasn't you have a similar problem to the Read/Show one
mizu_no_oto
:t randoms $ mkStdGen 42
lambdabot
Random a => a
merijn
Person1: i.e. print wants a type that is a Show instance, and randoms returns a type that is a Random instance and there's multiple types that are both Show and Random instances
merijn
So by annotating you remove the ambiguity by telling GHC "while this CAN produce any random instance, I want you to produce Coins"
mizu_no_oto
Person1: Basically, it's the exact same issue as with 'show . read'
Person1
i am starting to get it, slowly
mizu_no_oto
Person1: typeclasses are interesting because you can be polymorphic on any part of the type, including the return type
Person1
part of the difficulty for me is that when i'm looking at the instance on the fpcomplete page, i don't see `randoms` directly, but instead see `randomR` and `random` (one of which, i can imagine, calls `randoms`)
mizu_no_oto https://hackage.haskell.org/package/random-1.0.0.2/docs/System-Random.html
mizu_no_oto
randoms is defined in terms of randoms
geh. In terms of random
Person1
ah, randoms g = ((x,g') -> x : randoms g') (random g)
10:16:12
Person1
hopefully this type of thing will be easier on my brain eventually
Recursion and Lazy Infinite Lists
The randoms function is an infinite list. Note carefully:
randoms g = (\(x,g') -> x : randoms g') (random g)
This is non-terminating. It takes some variable x and appends the result of randoms g' to it, which itself is the same function, thus creating an infinite list.
This gives us an example of lazy evaluation. This bit right here (specifically, the use of !! 0 at the end) shows that Haskell is lazily evaluating up to the 0th location of this infinite list:
print $ (randoms g :: [Gonger]) !! 0
But is it fair?
I think so. On two sets of runs:
One night:
$ cat log | sort | uniq -c | sort -n -k 1
1826 Person2
1868 Person4
1890 Person1
1901 Person5
1914 Person3
Another night night:
$ cat log2 | sort | uniq -c | sort -n -k 1
2291 Person4
2333 Person3
2339 Person2
2372 Person5
2397 Person1
Looks fair.
There is a monad here.
It's the "do notation" here:
main = do
  ...
I think it's a monad because the type that is returned from newStdGen is wrapped in an IO type. And we need to do some function chaining or composition or something, with other functions that do not take values wrapped in IO. Not sure yet.
Why the do notation?
We're doing something with IO. Where does the IO come in?
newStdGen :: IO StdGen
newStdGen = atomicModifyIORef theStdGen split
atomicModifyIORef, according to Hoogle, "atomically modifies the contents of an 'IORef'." I don't know what this means. What's an IORef? Moving on...
theStdGen,
theStdGen :: IORef StdGen
theStdGen  = unsafePerformIO $ do
   rng <- mkStdRNG 0
   newIORef rng
Gah, another do notation with another IO function (mkStdRNG). Let's look up mkStdRNG.
mkStdRNG :: Integer -> IO StdGen
mkStdRNG o = do
    ct          <- getCPUTime
    (sec, psec) <- getTime
    return (createStdGen (sec * 12345 + psec + ct + o))
Ah, there's the IO (getCPUTime and getTime). I'm not going to look up their definitions.
