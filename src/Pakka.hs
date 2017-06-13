module Pakka (Maa, KortinNo, Pakka, PakanTila, TeePakka) where --lisää kaikki moduulit

import System.Random

--Data ja tyypit
data Maa = Pata | Risti | Ruutu | Hertta deriving (Show, Enum)
data KortinNo = [1..13] deriving (Show, Enum)
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

mahdPisteet :: [Kortti] -> [Int]
mahdPisteet = go [0] where
        go ns (1:rest) = go (map ((+) 1) ns ++ map ((+) 11) ns) rest
        go ns (2:rest) = go (map ((+) 2) ns) rest
        go ns (3:rest) = go (map ((+) 3) ns) rest
        go ns (4:rest) = go (map ((+) 4) ns) rest
        go ns (5:rest) = go (map ((+) 5) ns) rest
        go ns (6:rest) = go (map ((+) 6) ns) rest
        go ns (7:rest) = go (map ((+) 7) ns) rest
        go ns (8:rest) = go (map ((+) 8) ns) rest
        go ns (9:rest) = go (map ((+) 9) ns) rest
        go ns (10:rest) = go (map ((+) 10) ns) rest
        go ns (11:rest) = go (map ((+) 10) ns) rest
        go ns (12:rest) = go (map ((+) 10) ns) rest
        go ns (13:rest) = go (map ((+) 10) ns) rest

--sekoitaPakka :: pakkaTila ()
--sekoitaPakka = do
  --todo
  --curr <- get
  --shuffled <- replicateM 52 takeRandomCard
  --put curr { cards = shuffled }
  --putStrLn "Pakka sekoitettu. "


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
--    else if b == False then 11

--Kuvakortin tunnistamiseen
--onkoKuvakortti :: KortinNo -> Bool
--onkoKuvakortti b = if b > 10 then True
--    else False

--Ässän tunnistamiseen
--onkoAssa :: KortinNo -> Bool
--onkoAssa b = if b == 1 then True
--    else False
