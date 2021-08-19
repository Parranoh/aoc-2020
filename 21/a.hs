import Data.List (nub,intersect)
import qualified Data.Map.Strict as M

newtype Ingredient = Ingredient String deriving (Show,Eq)
newtype Allergen = Allergen String deriving (Show,Eq,Ord)
data Food = Food [Ingredient] [Allergen] deriving Show

main = interact $ show . solve . input

input :: String -> [Food]
input = map parseFood . lines

parseFood s =
    let (is, as) = break (== '(') s
        ingredients = map Ingredient . words $ is
        allergens = map (Allergen . init) . tail . words $ as
    in Food ingredients allergens

solve :: [Food] -> Int
solve fs =
    let is = concatMap (\(Food is _) -> is) $ fs
        a2is = ingredientsByAllergen fs
        badIs = nub . concatMap snd $ a2is
        goodIs = filter (`notElem` badIs) is
    in length goodIs

ingredientsByAllergen :: [Food] -> [(Allergen,[Ingredient])]
ingredientsByAllergen = M.toList . foldr f M.empty
    where
        f (Food is as) m = foldr (M.alter g) m as
            where
                g Nothing = Just is
                g (Just is') = Just $ intersect is is'
