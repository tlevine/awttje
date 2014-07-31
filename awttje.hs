import qualified Data.Map as M
import qualified Data.Set as S

data Level = Nominal | Ordinal | Interval | Ratio
type Column = [String]
type Histogram a = M.Map a Integer
type Feature a = String -> a

nChar :: Feature Integer
nChar = length

nNonNumericChar :: Feature Integer
nNonNumericChar x = length $ filter (notElem "0123456789.,") x


data Sign = Increasing | Decreasing

signedSorted :: Sign -> Column -> Bool
signedSorted _ [] = True
signedSorted _ x:[] = True
signedSorted Increasing x:y:[] = x <= y
signedSorted Decreasing x:y:[] = x => y
signedSorted sign x:y:zs = signedSorted sign [x,y] && signedSorted sign y:zs

sorted :: Column -> Bool
sorted column = ((signedSorted Increasing column) || (signedSorted Decreasing column))

sequential :: Column -> Bool
sequential [] = True
sequential column = correctRange && correctCount
  where
    s = S.fromList column
    correctRange = 1 + (S.findMax s) - (S.findMin s) == length column
    correctCount = S.size s == length column
