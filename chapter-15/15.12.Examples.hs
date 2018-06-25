import Data.Monoid
import Data.Semigroup hiding ((<>))

data Booly a = False' | True' deriving (Eq, Show)

instance Monoid (Booly a) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) Nada = Only a
  mappend Nada (Only b) = Only b
  mappend (Only a) (Only b) = Only (a `mappend` b)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <> adv <> " as he jumped into his car " <> noun <> " and drove off with his " <> adj <> " wife."

madlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin e adv noun adj = mconcat [
  e,
  "! he said ",
  adv,
  " as he jumped into his car ",
  noun,
  " and drove off with his ",
  adj,
  " wife."
  ]

