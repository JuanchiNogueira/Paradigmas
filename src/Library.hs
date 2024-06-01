module Library where
import PdePreludat
import qualified Control.Applicative as FALSE



--a
data Postre = UnPostre {sabores :: [String]
                       ,peso :: Number
                       ,temperatura :: Number}


modificarTemperatura :: Number -> Hechizo
modificarTemperatura delta postre = postre{temperatura = temperatura postre + delta}

modificarPesoPorcentual :: Number -> Hechizo
modificarPesoPorcentual por postre = modificarPeso (div (por * peso postre) 100) postre

modificarPeso :: Number -> Hechizo
modificarPeso delta postre = postre{temperatura = peso postre + delta}

agregarSabor :: String -> Hechizo
agregarSabor sabor postre = postre {sabores = sabores postre ++ [sabor]}

perderSabores :: Hechizo
perderSabores postre = postre {sabores = []}

bizcochuelo :: Postre
bizcochuelo =  UnPostre {sabores=["Vainilla","Chocolate"], peso = 30, temperatura = 50}

--b
type Hechizo = Postre->Postre

incendio :: Hechizo
incendio =  modificarTemperatura 1.modificarPesoPorcentual 5

immobulus :: Hechizo
immobulus postre = postre {temperatura = 0}

wingardumLeviosa :: Hechizo
wingardumLeviosa = modificarPesoPorcentual 10. agregarSabor "concentrado"

diffindo :: Number -> Hechizo
diffindo corte = modificarPeso (-corte)

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedabra :: Hechizo
avadaKedabra = immobulus.perderSabores

--c

estaCongelado :: Postre -> Bool
estaCongelado p = temperatura p >= 0

pesoPostre :: Postre -> Number
pesoPostre = peso

tieneSabor :: Postre -> Bool
tieneSabor p = not (null (sabores p))

estaListo :: Postre -> Bool
estaListo p = pesoPostre p > 0 && tieneSabor p && not (estaCongelado p)

postresListos :: Hechizo -> [Postre] -> Bool
postresListos hechizo = all (estaListo.hechizo)

--d

promedio :: [Number] -> Number
promedio nums = sum nums / length nums

pesoPromedioListo :: [Postre] -> Number
pesoPromedioListo = promedio . map peso . filter estaListo

--2
data Mago = UnMago {hechizos::[Hechizo],
                    horrocruxes :: Number}

aprenderHechizo :: Hechizo -> Mago -> Mago
aprenderHechizo hechizo mago = mago{hechizos = hechizos mago ++ [hechizo]}

esAvadaKedabra :: Postre -> Bool
esAvadaKedabra p = (temperatura p == 0) && null (sabores p)

sumarHorrocrux :: Mago -> Mago
sumarHorrocrux m = m{horrocruxes = horrocruxes m + 1}

modificarHorrocrux :: Hechizo -> Postre -> Mago -> Mago
modificarHorrocrux hechizo postre mago
                                    |esAvadaKedabra (hechizo postre) = sumarHorrocrux mago
                                    |otherwise = mago

claseDefensa :: Hechizo -> Postre -> Mago -> Mago
claseDefensa hechizo postre = modificarHorrocrux hechizo postre.aprenderHechizo hechizo

--b

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre h1 h2 = (length.sabores.h1) postre > (length.sabores.h2) postre

elMejorEntre :: Postre -> Hechizo -> Hechizo -> Hechizo
elMejorEntre postre h1 h2
                            |esMejor postre h1 h2 = h1
                            |otherwise = h2

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (elMejorEntre postre) (hechizos mago)

--3

--a
postresInfinitos :: [Postre]
postresInfinitos = repeat bizcochuelo

alberto :: Mago
alberto = UnMago{hechizos = repeat avadaKedabra , horrocruxes = 1}

{-
--b

Si, cualquier hechizo que haga que alguno de los postres de la lista infinita no este listo me podra devolver FALSE esto
debido a que Haskell trabaja con Lazy evaluation y al identificar un elemento que no cumpla con la condicion puede cortar 
con la ejecucion

--c

No, en este caso si es imposible encontrar al mejor hechizo ya que la funcion foldl1 debe aplicarse entre todos los elementos
de la lista para finalmente quedarse con el mejor de los hechizos, si esta lista es infinita lo unico que podra devolvernos es 
un stack overflow 

-}
doble :: Number -> Number
doble numero = numero + numero