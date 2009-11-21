import Data.List
import Debug.Trace
(=-=) :: String -> String -> Bool

s =-= g = matchGlob s g 
matchGlob ss ('*':[])       = True
matchGlob ss ('*':gs)       = True && matchGlob (dropWhile ((/=) $ head gs) ss) gs
matchGlob (s:ss) ('?':gs)   = True && matchGlob ss gs

matchGlob ss ('[':gs)       = (charClass gs ss)

matchGlob (s:ss) (g:gs)     = (s == g) && matchGlob ss gs

matchGlob (s:ss) []         = False
matchGlob [] []             = True

charClass :: String -> String -> Bool
charClass  gs (s:ss)   = (s `elem` (cClass gs)) && (matchGlob ss gs'')
    where   cClass (']':gs')    = ']':(takeWhile (/= ']') gs')
            cClass gs'          = takeWhile (/= ']') gs'
            gs''                = drop ((length $ cClass gs)+1) gs

test' = zipWith (matchGlob) searchStrs globs

globs       = ["f[il]le.*", "file?[aA][12]", "*.??[]aabc]c", "*.abc"]
searchStrs  = ["file.xyz", "file.A.2", "asdf.ab]c", "asdf.dge"]
