import Data.List (nub,intersect,sortOn,partition)
import qualified Data.Map.Strict as M

newtype Ingredient = Ingredient String deriving (Show,Eq)
newtype Allergen = Allergen String deriving (Show,Eq,Ord)
data Food = Food [Ingredient] [Allergen] deriving Show

main = interact $ output . solve . input

input :: String -> [Food]
input = map parseFood . lines

parseFood s =
    let (is, as) = break (== '(') s
        ingredients = map Ingredient . words $ is
        allergens = map (Allergen . init) . tail . words $ as
    in Food ingredients allergens

solve :: [Food] -> [Ingredient]
solve fs =
    let a2is = ingredientsByAllergen fs
        a2i = ingredient2Allergen a2is
    in map snd . sortOn fst $ a2i

ingredientsByAllergen :: [Food] -> [(Allergen,[Ingredient])]
ingredientsByAllergen = M.toList . foldr f M.empty
    where
        f (Food is as) m = foldr (M.alter g) m as
            where
                g Nothing = Just is
                g (Just is') = Just $ intersect is is'

ingredient2Allergen :: [(Allergen,[Ingredient])] -> [(Allergen,Ingredient)]
ingredient2Allergen [] = []
ingredient2Allergen a2is =
    let (oneToOne,other) = partition ((== 1) . length . snd) a2is
        known = map unpack oneToOne
    in known ++ ingredient2Allergen (update (map snd known) other)
    where
        unpack (a,[i]) = (a,i)

        update :: [Ingredient] -> [(Allergen,[Ingredient])] -> [(Allergen,[Ingredient])]
        update is = map . fmap $ filter (`notElem` is)

output :: [Ingredient] -> String
output = tail . concatMap (\(Ingredient i) -> ',':i)
