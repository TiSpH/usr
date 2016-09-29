import Data.Char
import Data.Ratio

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname










move a (b, c)
     | a == b = c
     | a == c = b
     | otherwise = a








race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g
     | v1 >= v2 = Nothing
     | otherwise = Just (h, mn, s) where
          total =  (3600 * g) `div` ( v2 - v1 )
          h     =  div total 3600
          mn    =  div ( total - 3600 * h ) 60
          s     =  total - 3600 * h - 60 * mn







revRot :: [Char] -> Int -> [Char]
revRot strng 0 = ""
revRot "" sz = ""
revRot strng sz
    | sz > length strng = ""
    | otherwise = foldl1 (++) (map change (chunk strng sz))

chunk xs sz
    | sz > length xs = []
    | otherwise = fst ( splitAt sz xs ) : chunk ( snd ( splitAt sz xs ) ) sz

cubes [] = 0
cubes (x:xs) = x^3 + cubes xs

change xs
    | even $ cubes $ map digitToInt xs = reverse xs
    | otherwise = tail xs ++ [head xs]








sum' xs = sum [a % b | (a,b) <- xs]











convertFracs xs = [ (tou, xiao) | tou <- tous] where
       xiao = foldl1 lcm [ b | (a, b) <- xs ]
       tous = [ (xiao `div` b) * a | (a, b) <- xs]







presses :: String -> Int
presses = sum . map (`finder` keypads)

finder  key ((x,y):xys)
     | elem (toUpper key) x        =  y
     | otherwise                   =  finder key xys

keypads = [("",0),("1ADGJMPTW* #",1),("BEHKNQUX0",2),("CFILORVY",3),("23456R8Z",4),("79",5)]
