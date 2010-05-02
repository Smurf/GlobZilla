module GlobMatcher ((=-=)) where

import Data.List
import Debug.Trace

(=-=) :: String -> String -> Bool
s =-= g = matchGlob s g

matchGlob ss ('*':[])       = True
matchGlob ss ('*':gs)       = True && matchGlob (dropWhile ((/=) $ head gs) ss) gs
matchGlob (s:ss) ('?':gs)   = True && matchGlob ss gs

matchGlob [] ('%':_)        = False
matchGlob (s:ss) ('%':[])   = alphaNum s
matchGlob (s:ss) ('%':gs)   = alphaNum s && matchGlob (dropWhile ((/=) $ head gs) ss) gs


matchGlob ss ('[':gs)       = (charClass gs ss)

matchGlob (s:ss) (g:gs)     = (s == g) && matchGlob ss gs

matchGlob (s:ss) []         = False
matchGlob [] []             = True
matchGlob [] _              = True

charClass :: String -> String -> Bool
charClass  gs (s:ss)   = (s `elem` (cClass gs)) && (matchGlob ss gs'')
    where   cClass (']':gs')    = ']':(takeWhile (/= ']') gs')
            cClass gs'          = takeWhile (/= ']') gs'
            gs''                = drop ((length $ cClass gs)+1) gs

alphaNum s = or $ concat $ [checkAlpha, checkNum]
    where   checkAlpha  = map (==s) $ concat [['a'..'z'],['A'..'Z']]
            checkNum    = map (==s) ['0'..'9']


test' = zipWith (matchGlob) searchStrs globs
--Should return [True, False, True, False]
globs       = ["f[il]le.*", "file?[aA][12]", "*.??[]aabc]c", "*.abc"]
searchStrs  = ["file.xyz", "file.A.2", "asdf.ab]c", "asdf.dge"]

