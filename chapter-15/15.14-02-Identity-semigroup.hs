-- import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck

data Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  (<>) _ _ = Identity

instance Arbitrary Identity where
  arbitrary = return Identity

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc = Identity -> Identity -> Identity -> Bool

main = quickCheck (semigroupAssoc :: IdentityAssoc)
