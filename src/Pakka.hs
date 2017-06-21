module Pakka (Maa, KortinNo, Pakka, PakanTila, TeePakka, mahdArvot, mahdPisteet, ota, teePakka, otaSatunnainenKortti, otaXKortti, sekoitaPakka) where

import System.Random
import Control.Monad.ST
import Control.Monad.Trans.State
import Data.Ix --ehkä turha

--Data ja tyypit
data Maa = Pata | Risti | Ruutu | Hertta deriving (Show, Enum)
data KortinNo = Assa | Kaksi | Kolme | Nelja | Viisi | Kuusi | Seitseman | Kahdeksan | Yhdeksan | Kymmenen | Jatka | Rouva | Kuningas deriving (Eq, Ord, Bounded, Enum)

instance Show KortinNo where
    show a = case a of
        Assa      -> "1"
        Kaksi     -> "2"
        Kolme     -> "3"
        Nelja     -> "4"
        Viisi     -> "5"
        Kuusi     -> "6"
        Seitseman -> "7"
        Kahdeksan -> "8"
        Yhdeksan  -> "9"
        Kymmenen  -> "10"
        Jatka     -> "11"
        Rouva     -> "12"
        Kuningas  -> "13"

type Kortti = (Maa, KortinNo)
data Pakka = Pakka
   { pakka :: [Kortti]
   , gen :: StdGen }
   deriving (Show)

type PakanTila s = StateT Pakka s

--Ottaa kortin pakan päältä
ota :: PakanTila -> Kortti
ota = otaXKortti 0

--Tekee pakan korteista
teePakka :: StdGen -> Pakka
teePakka = [(maa, kortinNo) | maa <- [Pata .. Hertta], kortinNo <- [1 .. 13]]

--Pakan korttien piste arvot
mahdArvot :: Kortti -> [Int]
mahdArvot (maa, numero)
    | numero == 1 = [1, 11]
    | elem numero [11, 12, 13] = 10
    | otherwise = fromEnum numero

--Pakan korttien pisteet
mahdPisteet :: [Kortti] -> [Int]
mahdPisteet kasi = nub $ map sum $ mapM mahdArvot kasi

--Satunnainen kortti pakasta
otaSatunnainenKortti :: PakanTila -> Kortti
otaSatunnainenKortti = do
    curr <- get
    let pituus = length $ pakka curr
        (i, gen') = randomR (0, pituus) $ gen curr
    kortti <- otaXKortti i
    put curr { gen = gen' }
    return kortti

--Kortin otaamiseen pakasta
otaXKortti :: Int -> PakanTila Kortti
otaXKortti x = do
    curr <- get
    let (pakka', pakka'') = splitAt (i + 1) $ pakka curr
        kortti = last pakka'
        uusiPakka = init pakka' ++ pakka''
    put curr { pakka = uusiPakka }
    return kortti

--Pakan sekoittamiseen
sekoitaPakka :: PakanTila ()
sekoitaPakka = do
    curr <- get
    sekoitus <- replicateM 52 otaSatunnainenKortti
    put curr { pakka = sekoitus }

--getPakanTila :: PakanTila
