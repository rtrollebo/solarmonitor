{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Client
    (sendRequest
    ) where


import Network.Wreq
import Control.Lens
import GeosXray


sendRequest :: IO ()
sendRequest = do
    r <- asJSON =<< get sourceUrl :: IO (Response [ProductGoesXray])
    let result = r ^. responseBody 
    let events = getXrayEvents result
    putStrLn (
        strBlock "Date" 25 ++ (strRepl " " 3) ++ 
        strBlock "Class" 4 ++ (strRepl " " 3) ++ 
        strBlock "Max Flux [Watts m^-2]" 5 ++ (strRepl " " 3) ++ 
        (toPlainTxt events "") ++ "\n") 

toPlainTxt :: [XrayEvent ] -> String -> String
toPlainTxt [] p = p
toPlainTxt (x:xs) p = p ++ toPlainTxt xs (eventToPlainTxt x 3) 


eventToPlainTxt :: XrayEvent -> Int -> String
eventToPlainTxt (XrayEvent v t spec) spacing = 
    strBlock (show t) 25 ++ (strRepl " " spacing) ++
    strBlock (show spec) 4 ++ (strRepl " " spacing) ++
    strBlock (show v) 5 ++ (strRepl " " spacing) ++ "\n"



strBlock :: String -> Int -> String
strBlock x max 
    | length x >= max = take max x 
    | otherwise = x ++ (strRepl " " blocksize)
    where blocksize 
            | n > 0 = n
            | otherwise = 0
          n = max - (length x)

strRepl :: String -> Int -> String
strRepl s i = concat $ replicate i s

