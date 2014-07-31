import qualified Data.Map as M
import           Control.Arrow  (&&&)

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
sorted = ((signedSorted Increasing) &&& (signedSorted Decreasing)) 3
