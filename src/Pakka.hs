module Pakka (Maa, KortinNo, Pakka, PakanTila, TeePakka, mahdArvot, mahdPisteet) where --lisää kaikki moduulit

import System.Random
import Control.Monad

import Data.Range --ehkä turha

--Data ja tyypit
data Maa = Pata | Risti | Ruutu | Hertta deriving (Show, Enum)
data KortinNo = [] deriving (Show, Enum)
--Rangen parsinta ongelman kiertämiseen
KortinNo = range 1 1 13

instance Show Value where
	show 1 = "Ässä"
	show 2 = "Kaksi"
	show 3 = "Kolme"
	show 4 = "Neljä"
	show 5 = "Viisi"
	show 6 = "Kuusi"
	show 7 = "Seitsemän"
	show 8 = "Kahdeksan"
	show 9 = "Yhdeksän"
	show 10 = "Kymmenen"
	show 11 = "Jätkä"
	show 12 = "Kuningatar"
	show 13 = "Kuningas"
type Kortti = (Maa, KortinNo)
type Pakka = [Kortti]
type PakanTila s = State Pakka s

--Tekee pakan korteista
teePakka :: Pakka
teePakka = [(Maa, KortinNo) | Maa <- [Pata .. Hertta], KortinNo <- [1 .. 13]]

--Pakan korttien piste arvot
mahdArvot :: Kortti -> [Int]
mahdArvot kortti
	| kortti == 1 = [1, 11]
	| elem kortti [11, 12, 13] = 10
	| otherwise = fromEnum kortti

--Pakan korttien pisteet
mahdPisteet :: [Kortti] -> [Int]
mahdPisteet kasi = nub $ map sum $ mapM mahdArvot kasi

--Satunnainen kortti pakasta
otaSatunnainenKortti :: PakanTila Kortti
otaSatunnainenKortti = do
	curr <- get
	let pituus = length $ Pakka curr
		(i, gen') = randomR (0, pituus) $ gen curr
	Kortti <- otaXKortti i
	put curr { gen = gen' }
	return Kortti

--Kortin otaamiseen pakasta
otaXKortti :: Int -> PakanTila Kortti
otaXKortti x = do
	curr <- get
	let (Pakka', Pakka'') = splitAt (i + 1) $ Pakka curr
		Kortti = last Pakka'
		uusiPakka = init Pakka' ++ Pakka''
	put curr { Pakka = uusiPakka }
	return Kortti

getPakanTila :: 

--Pakan sekoittamiseen
sekoitaPakka :: pakkaTila ()
sekoitaPakka = do
	curr <- get
	sekoitus <- replicateM 52 otaSatunnainenKortti
	put curr { Pakka = sekoitus }

--Jakaa pakasta päälimmäisen kortin
--jaaKortti ::

--Satunnaisen kortin jakamiseen
--satunnainenMaa x = randomRIO (1, 4)
--satunnainenNo x = randomRIO (1, 13)
--satunnainenKortti

--Jaetun kortin poistaminen pakasta
--poistaNoPakasta x = x
--poistaMaaPakasta x = x
--poistaKortti

--Ässän pisteiden määrän valintaan
--valitseAssanArvo :: Bool -> pisteArvot
--valitseAssanArvo b = if b == True then 1
--	else if b == False then 11

--Kuvakortin tunnistamiseen
--onkoKuvakortti :: KortinNo -> Bool
--onkoKuvakortti b = if b > 10 then True
--	else False

--Ässän tunnistamiseen
--onkoAssa :: KortinNo -> Bool
--onkoAssa b = if b == 1 then True
--	else False
