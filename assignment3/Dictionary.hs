module Dictionary (T, empty, lookup, insert) where
import Prelude hiding (lookup)
import qualified Prelude

newtype T a b = Dictionary [(a, b)] deriving (Show)

empty :: (Eq a, Ord a) => T a b
empty = Dictionary []

lookup :: (Eq a, Ord a) => a -> T a b -> Maybe b
lookup a (Dictionary d) = Prelude.lookup a d

insert :: (Eq a, Ord a) => (a, b) -> T a b -> T a b
insert p (Dictionary d)  = Dictionary (p:d)
