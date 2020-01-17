---
title: An overview of property testing in Haskell
---

# Making sure that something works

- Use the right tools
- Verification?
- Testing!

# The problems with testing

```
myFunction exampleInput `shouldBe` expectedOutput
```

* Coverage
* Price


# Property testing

``` haskell
type UnitTest = IO ()
type PropertyTest a = a -> IO ()
```

* Exhaustive
* Randomised

Example:

``` haskell
(\str -> reverse (reverse str) == str) :: String -> Bool
```

# Generators

```
  Seed
   |
   v
+-------+    +---+
| Gen A | -> | A |
+-------+    +---+
   ^
   |
  Size
```

# Shrinking

What happens if a property test fails?

. . .

```
-- With shrinking turned off:
quickCheck $ \s -> all isUpper $ map toUpper s
*** Failed! Falsifiable (after 1 test):
"x\CAN\32937J\ENQ^\DC1?\FS\96943\&0\74134V+"
```

. . .

- Try to shrink: `A -> [A]`

<!--

  When a test fails, QuickCheck will try to 
  shrink the counterexample to something more manageable

-->

. . .

```
quickCheck $ \s -> all isUpper $ map toUpper s
*** Failed! Falsifiable (after 3 tests and 1 shrink):
"1"
```


# Libraries

* QuickCheck
* Hedgehog
* Validity

* Leanspec 
* Smallcheck

* QuickSpec
* Speculate
* EasySpec

* Extrapolate
* Fitspec

# QuickCheck

``` haskell
class Arbitrary a where
```

. . .

``` haskell
  arbitrary :: Gen a -- A generator for A
```

. . .

``` haskell
  shrink :: a -> [a] -- A shrinking function far A
```

. . .

``` haskell
λ> quickCheck $ \(str :: String) -> reverse (reverse str) == str
+++ OK, passed 100 tests.
```

# Property combinators for non-specialised randomised property testing

Test whether `<|*|>` is reflexive:

``` haskell
\x -> x <|*|> x
```

. . .

Property combinator

``` haskell
reflexive :: Arbitrary a => (a -> a -> Bool) -> Property
reflexive rel = \x -> x `rel` x
```

. . .

Only for non-specialised property testing

``` haskell
> 2 :% 2 == (1 :% 1 :: Rational)
False
```


# No laws or semantics for `Arbitrary`

`Gen Rational`

- `0 :% 0` ?

. . .

- `1 :% 0` ?

. . .

- `5 :% -1` ?

. . .

- `-5 :% 1` ?

. . .

- Never generate un-normalised values? !> never test these edge-cases

. . .

- Generate un-normalised values?       !> `(==)` breaks


# Newtypes for guiding generators

``` Haskell
instance Arbitrary Rational
```

. . .


``` Haskell
newtype Normalised a = Normalised a
instance Arbitrary (Normalised Rational)
```

. . .


``` Haskell
reflexive (\(Normalised nn1) (Normalised nn2) -> nn1 == nn2)
```

. . .

!> Cannot compose these

# Expensive generators and shrinking

```
There is no generic arbitrary implementation included because
we don’t know how to make a high-quality one. If you want one,
consider using the testing-feat or generic-random packages.
```

!> No default implementation of `arbitrary`
!> Default shrinking: `shrink _ = []`

# Orphan instances

Where do you put `Arbitrary` instances?

- Next to data     !> depend on `QuickCheck`

. . .

- In test suite    !> Rewrite for every test suite

. . .

- `quickcheck-instances` !> Extra dependencies

. . .

- Seperate library !> Maintenance overhaid

# Hedgehog

``` haskell
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

# Hedgehog generators

- Value-level generators, no type class

. . .

- Built-in (monadic!) shrinking via lazy shrink trees

. . .

- More general -> monadic effects

# Sub-optimal shrinking

Hedgehog:

``` haskell
λ > let gen = Gen.enum 'a' 'b'
λ > Gen.printTree (Gen.filter (uncurry (==)) ((,) <$> gen <*> gen))
=== Outcome ===
('b','b')
=== Shrinks ===
('a','a')
('a','a')
('a','a')
```

. . .

Quickcheck:

``` haskell
λ filter (uncurry (==)) $ QC.shrink ('b','b')
[]
```

Validity:

``` haskell
λ filter (uncurry (==)) $ shrinkUnchecked ('b', 'b')
[('a','a')]
```

# Even more expensive generators

!> No type class at all

# Cumbersome property combinators

``` haskell
associative :: Gen a -> (a -> a -> a) -> Property
```

!> Need to string generators around

# Validity-based testing

---
title: Validity and Validity-based testing
author: Tom Sydney Kerckhove
---

# Cheaper testing: My wish for sensible functions

``` haskell
data Tag = Tag
  { tagName :: Text -- Invariant: No whitespace
  , tagValue :: Text -- Invariant: No newlines
  } deriving (Show, Eq, Generic)
                        
                            
                        
 


doSomethingUseful :: Tag -> Tag

spec :: Spec
spec = 
    <SOMETHING CHEAP>




```

# Cheaper testing: Validity for sensible functions

``` haskell
data Tag = Tag
  { tagName :: Text -- Invariant: No whitespace
  , tagValue :: Text -- Invariant: No newlines
  } deriving (Show, Eq, Generic)

instance Validity Tag where
  validate = [...] -- Explain the invariants
instance GenUnchecked Tag
instance GenValid Tag

doSomethingUseful :: Tag -> Tag

spec :: Spec
spec = 
  describe "Tag" $
    describe "doSomethingUseful" $
      it "produces valid values" $
        producesValidsOnValids doSomethingUseful
```


# Cheaper testing: My wish for sensible instances

``` Haskell
data MyType = MyType
  { myBool :: Bool
  , myDouble :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON MyType where
  parseJSON = [...]
instance ToJSON MyType where
  toJSON = [...]

                        
                            
                        

spec :: Spec
spec =
   <SOMETHING EASY>                    
                           
```

# Cheaper testing: Validity for sensible instances

``` Haskell
data MyType = MyType
  { myBool :: Bool
  , myDouble :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON MyType where
  parseJSON = [...]
instance ToJSON MyType where
  toJSON = [...]

instance Validity MyType
instance GenUnchecked MyType
instance GenValid MyType

spec :: Spec
spec =
  describe "MyType" $
    jsonSpecOnValid @MyType

```

# Cheaper testing: My wish for sensible monads

``` Haskell
data MStop a
    = Stop
    | Continue a
    deriving (Show, Eq, Generic)

instance Functor MStop where
  [...]
instance Applicative MStop where
  [...]
instance Monad MStop where
  [...]

                                         
                                                 
                                         

spec :: Spec
spec = do




```

# Cheaper testing: Validity for sensible monads

``` Haskell
data MStop a
    = Stop
    | Continue a
    deriving (Show, Eq, Generic)

instance Functor MStop where
  [...]
instance Applicative MStop where
  [...]
instance Monad MStop where
  [...]

instance Validity a => Validity (MStop a)
instance GenUnchecked a => GenUnchecked (MStop a)
instance GenValid a => GenValid (MStop a)

spec :: Spec
spec = do
  functorSpec @MkStop
  applicativeSpec @MkStop
  monadSpec @MkStop
```

# Cheaper testing: My wish for sensible lenses

``` Haskell

data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
  } deriving (Show, Eq, Generic)

myRationalL :: Lens' MyType Rational
myRationalL = lens myRational $ \mt d -> mt {myRational = d}

                        
                            
                        

spec :: Spec
spec =
    <SOMETHING CHEAP THAT TESTS EVERYTHING ABOUT LENSES>                       
                             
```

# Cheaper testing: Validity for sensible lenses

``` Haskell

data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
  } deriving (Show, Eq, Generic)

myRationalL :: Lens' MyType Rational
myRationalL = lens myRational $ \mt d -> mt {myRational = d}

instance Validity MyType
instance GenUnchecked MyType
instance GenValid MyType

spec :: Spec
spec =
  describe "myRationalL" $
    lensSpecOnValid myRationalL
```


# What do we need for cheap testing:

* Property tests: 100 tests for the price of one
* Cheap generators: free?
* Cheap shrinking: free?
* Cheap properties: property combinators

# Cheap generators: Arbitrary

What if not all values are valid

* Only generate valid values?
  No, then you are missing crucial values for safety.
* Generate unchecked values?
  No, then you cannot test functions that only handle valid values.
* Have seperate generators for each?
  No, then you lose the composability of combinators: type classes

## WE NEED SEMANTICS!

# Cheap generators: GenUnchecked

``` haskell
class GenUnchecked a where
  genUnchecked :: Gen a
  shrinkUnchecked :: a -> [a]
```

Generic implementation: FREE!

What about values with invariants?


# Validity: Invariant on data type

``` haskell
module Prime (Prime, unPrime) where

-- INVARIANT: isPrime
newtype Prime = Prime { unPrime :: Int }
```

# Validity: Explicit invariant

``` haskell
class Validity a where
  isValid :: a -> Bool

instance Validity Prime where
  isValid (Prime p) = isPrime p
```


# Validity: Why is this invalid?

``` haskell
λ isValid (Prime 6)
False

λ isValid [Prime 2, Prime 3, Prime 5, Prime 6]
False -- But why?
```

# Validity: Validation

``` haskell
class Validity a where
  validate :: a -> Validation -- Check _why_ it is valid
```

```
λ prettyValidation [Prime 2, Prime 3, Prime 5, Prime 6]
The element at index 3 in the list
  \ Violated: Prime -- Aha!
```

# Cheap generators: GenValid

``` haskell
class GenValid a where
  genValid :: Gen a
  genValid = genUnchecked `suchThat` isValid
  shrinkValid :: a -> [a]
  shrinkValid = filter isValid . shrinkUnchecked
```

Default implementation: FREE!


# Cheap properties: Property combinators (1)

``` Haskell
failsOnInvalid
  :: (Show a, Show b, GenInvalid a)
  => (a -> Maybe b) -> Property

validIfSucceeds
  :: (Show a, Show b, GenUnchecked a, Validity b)
  => (a -> Maybe b) -> Property

succeedsOnValid
  :: (Show a, Show b, GenValid a)
  => (a -> Maybe b) -> Property
```

# Cheap properties: Property combinators (2)

``` Haskell
equivalentOnValid
  :: (Show a, Eq a, GenValid a, Show b, Eq b)
  => (a -> b) -> (a -> b) -> Property

associativeOnValids
  :: (Show a, Eq a, GenValid a)
  => (a -> a -> a) -> Property

inverseFunctionsIfSucceed
  :: (Show a, Eq a, GenUnchecked a, CanFail f, CanFail g)
  => (a -> f b) -> (b -> g a) -> Property
```

# Cheap properties: Test suite combinators: Ord

``` Haskell
{-# LANGUAGE TypeApplications #-}

ordSpec :: forall a.
     (Show a, Ord a, Typeable a, GenUnchecked a)
  => Spec

ordSpecOnValid :: ...
```

# Cheap properties: Test suite combinators: Ord

```
λ> ordSpec @Int

  Ord Int
    (<=) :: Int -> Int -> Bool
      is reflexive
      is antisymmetric
      is transitive
      is equivalent to (\a b -> compare a b /= GT)
    (>=) :: Int -> Int -> Bool
      is reflexive
      is antisymmetric
      is transitive
      is equivalent to (\a b -> compare a b /= LT)
    (<) :: Int -> Int -> Bool
      is antireflexive
      is transitive
      is equivalent to (\a b -> compare a b == LT)
    (>) :: Int -> Int -> Bool
      is antireflexive
      is transitive
      is equivalent to (\a b -> compare a b == GT)
```


# Cheap properties: Test suite combinators: Functor

``` Haskell
{-# LANGUAGE TypeApplications #-}

functorSpec :: forall f.
    ( Eq (f Int), Show (f Int)
    , Functor f, Typeable f, GenUnchecked (f Int)
    )
    => Spec

functorSpecOnValid :: ...
```

# Cheap properties: Test suite combinators: Functor

```
functorSpec @[]

Functor []
    fmap :: (a -> b) -> [] a -> [] b
      satisfies the first Fuctor law:
        'fmap id == id'
      satisfieds the second Functor law:
        'fmap (f . g) == fmap f . fmap g'
    (<$) :: a -> [] b -> [] a
      is equivalent to its default implementation
```


# Cheap properties: Test suite combinators: aeson

``` Haskell
jsonSpec :: forall a.
  ( Show a, Eq a, Typeable a,
    GenUnchecked a, FromJSON a, ToJSON a)
  => Spec

jsonSpecOnValid :: ...
```


# Cheap properties: Test suite combinators: aeson

```
λ> jsonSpec @Int

  JSON Int (unchecked)
    encode :: Int -> Data.ByteString.Lazy.ByteString
      never fails to encode
    decode :: Int -> Data.ByteString.Lazy.ByteString
      ensures that encode and decode are inverses
```

# Cheap properties: Test suite combinators: lens

```
λ> lensSpecOnValid ((_2) :: Lens' (Double, Double) Double)

  lensSpecOnValid
    satisfies the first lens law
    satisfies the second lens law
    satisfies the third lens law
    gets valid values
    produces valid values
```


# Cost

```
data MyType = MyType Int Text
  deriving (Show, Eq, Generic)
instance GenUnchecked MyType
instance Validity MyType
instance GenValid MyType
```

# Companion libraries

```
genvalidity-hspec
genvalidity-hspec-aeson
genvalidity-hspec-binary
genvalidity-hspec-cereal
genvalidity-hspec-hashable
genvalidity-hspec-optics
```


