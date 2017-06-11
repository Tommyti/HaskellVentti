module Pakka (maa, kortinNo, pakka, pakanTila, teePakka, gen, mkDeck, draw, shuffle, takeRandomCard, takeCardAt) where

import System.Random

--Data ja tyypit
data maa = Pata | Risti | Ruutu | Hertta deriving (Show, Enum)
data kortinNo = [1 .. 13] deriving (Show, Enum)
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
type kortti = (maa, kortinNo)
type pakka = [kortti]
type pakanTila s = State pakka s

teePakka :: pakka
teePakka = [(maa, kortinNo) | maa <- [Pata .. Hertta], kortinNo <- [1 .. 13]]

mahdPisteet :: [kortti] -> [Int]
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

sekoitaPakka :: pakkaTila ()
sekoitaPakka = do
  --todo
  --curr <- get
  --shuffled <- replicateM 52 takeRandomCard
  --put curr { cards = shuffled }

jaaKortti :: osallistuja
jaaKortti do
    curr <- get
    let n = length $ cards curr
        (i, gen') = randomR (0, n) $ gen curr
    card <- takeCardAt i
    put curr { gen = gen' }
    return card


--Satunnaisen kortin jakamiseen
--satunnainenMaa x = randomRIO (1, 4)
--satunnainenNo x = randomRIO (1, 13)
satunnainenKortti

--Jaetun kortin poistaminen pakasta
poistaNoPakasta x = x
poistaMaaPakasta x = x
poistaKortti

--Ässän pisteiden määrän valintaan
valitseÄssänArvo :: Bool -> pisteArvot
valitseÄssänArvo b = if b == True then 1
    else if b == False then 11-

--Kuvakortin tunnistamiseen
onkoKuvakortti :: kortinNo -> Bool
onkoKuvakortti b = if b > 10 then True
    else False

--Ässän tunnistamiseen
onkoAssa :: kortinNo -> Bool
onkoAssa b = if b == 1 then True
    else False








        -- |'takeRandomCard' will pick one random card from the deck and remove it.
        -- It is a helper-function used by 'shuffle'.
        takeRandomCard :: DeckS Card
        takeRandomCard = do
          curr <- get
          let n = length $ cards curr
              (i, gen') = randomR (0, n) $ gen curr
          card <- takeCardAt i
          put curr { gen = gen' }
          return card

        -- |'takeCardAt' will pick the card at the given index and remove it from the
        -- deck.
        takeCardAt :: Int -> DeckS Card
        takeCardAt i = do
          curr <- get
          let (cards', cards'') = splitAt (i + 1) $ cards curr
              card              = last cards'
              newCards          = init cards' ++ cards''
          put curr { cards = newCards }
          return card
