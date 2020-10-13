module Lzw (lzwCode, lzwCodeSimple, lzwDecode) where
import Data.List


applyN :: (Eq t, Num t) => t -> (b -> b) -> b -> b
applyN 0 f = id
applyN n f = f . applyN (n-1) f

extGloss :: Eq a => [[a]] -> [[a]] -> [a] -> ([[a]], [[a]])
extGloss [] a inseq = (a ++ [inseq],[])
extGloss bgloss egloss inseq = let lg = last bgloss 
                                   llg = length lg
                                 in
                                   if lg == take llg inseq
                                     then (bgloss ++ egloss ++ [take (llg+1) inseq], [take llg inseq,drop llg inseq])
                                     else extGloss (init bgloss) ([last bgloss] ++ egloss) inseq

lzwStep :: Eq a => ([[a]], [[a]]) -> ([[a]], [[a]])
lzwStep (gloss,[[]]) = (gloss,[[]])
lzwStep (gloss,inseq) = fmap ((++) (init inseq)) (extGloss gloss [] (last inseq))

lzwCodeMisc (gloss,inseq) = if (last inseq == [])
                          then (gloss,inseq)
                          else lzwCodeMisc $ lzwStep (gloss,inseq)

unJust Nothing = -1
unJust (Just x) = x

makeInitGlos a = map (\x->[x]) (nub a)

codeToInt (gloss,code) = init $ map (unJust . (flip elemIndex gloss)) code
------------------------------------------------------------------------------

lzwCode gloss a = codeToInt $ lzwCodeMisc (gloss, [a])

lzwCodeSimple a = let
                    initGl = makeInitGlos a
                    t = lzwCodeMisc (initGl, [a])
                  in
                    (codeToInt t, (initGl, fst t))

------------------------------------------------------------------------------

buildGlos initGlos [] = initGlos
buildGlos initGlos [a] = initGlos ++ [(initGlos!!a)++[head (initGlos!!a)]]
buildGlos initGlos code = let ind = head code
                              indNext = head $ tail code
                              l = length initGlos
                          in
                            if (ind < l) && (indNext < l) 
                              then buildGlos (initGlos ++ [(initGlos!!ind)++[head (initGlos!!indNext)]]) (tail code) 
                              else buildGlos (initGlos ++ [(initGlos!!ind)++[head (initGlos!!ind)]]) (tail code)
--------------------------------------------------------------------------------

lzwDecode initGlos code = foldr (\acc x -> acc ++ x) "" $ map ((buildGlos initGlos code) !!) code


