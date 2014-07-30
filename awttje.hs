import qualified Data.Map as M

data Level = Nominal | Ordinal | Interval | Ratio
type Column = [String]
type Histogram a = M.Map a Integer
