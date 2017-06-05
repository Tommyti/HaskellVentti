import System.random

osallistuja = [Pelaaja, Jakaja]
maa = [Pata, Risti, Ruutu, Hertta]
kortinNo = [1 .. 13]
kortit = [maa, kortinNo]
pisteArvot = [1, 2, .. 10]
pelaajaNo = [1, 2]

data Pelaaja = Pelaaja {
    kortit[pelaajaNo, kortit]
} deriving (Eq, Show)

head osallistuja = Pelaaja {
    pelaajaNo = 1
}
tail osallistuja = Pelaaja {
    pelaajaNo = 2
}

-- main
main = do
	putStrLn "Pelin aloitus "
	jaa2korttiaP1
    jaa2korttiaPC
	-- Käyttäjän syöte
	tila <- getLine


jaa2korttiaP1 :: 

jaa2korttiaPC :: 

--Satunnaisen kortin jakamiseen
satunnainenMaa x = randomRIO (1, 4)
satunnainenNo x = randomRIO (1, 13)
satunnainenKortti

--Jaetun kortin poistaminen pakasta
poistaNoPakasta x = x
poistaMaaPakasta x = x
poistaKortti

--Ässän pisteiden määrän valintaan
valitseÄssänArvo :: Bool -> pisteArvot
valitseÄssänArvo b = if b == True then 1
    else if b == False then 11

--Kuvakortin tunnistamiseen
onkoKuvakortti :: kortinNo -> Bool
onkoKuvakortti b = if b > 10 then True
    else False

--Ässän tunnistamiseen
onkoAssa :: kortinNo -> Bool
onkoAssa b = if b == 1 then True
    else False

pyydaKortti

skippaa

vuoronVaihto

--Pelin voittajan päättämiseen
lopullisetPisteet :: pisteArvot -> pisteArvot -> Int
lopullisetPisteet x y = if x > y && x <= 21 then p1Win
    else if y > x && y <=21 then pcWin
        else if x == y then pcWin

p1Win = "Pelaaja voitti pelin!"
pcWin = "Tietokone voitti pelin!"


--Jakaja jakaa aluksi kaksi korttia sekä pelaajalle että itselleen.
--Kuvakortit (kuningas, kuningatar ja jätkä) ovat numeroarvoltaan 10, ässä pelaajan valinnan mukaan joko 1 tai 11 ja muiden korttien numeroarvo on normaali.
--Ensimmäisen jaon jälkeen pelaaja voi vuorollaan joko pyytää lisäkortin tai tyytyä nykyisiin.
--Jakajan on pakko ottaa uusi kortti, mikäli hänellä on kasassa pisteitä 16 tai vähemmän, ja hänen on pakko pysähtyä, mikäli hänellä on pisteitä 17 tai enemmän.
--Mikäli pelaaja saa ventin eli tasan 21 pistettä, hän voittaa. Mikäli hän ylittää ventin, hän häviää.
--Mikäli pelaaja pääsi lähemmäs venttiä (sitä kuitenkaan ylittämättä), hän voittaa.
--Mikäli pelaaja ja jakaja päätyivät tasapeliin, jakaja voittaa.
--Toteuta järjestelmä, jossa käyttäjä pääsee pelaamaan venttiä tietokonejakajaa vastaan.