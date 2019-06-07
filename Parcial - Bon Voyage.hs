--Modelado de Datos --1--
data Reserva = Reserva {
   pasajeros :: [String],
   tramos :: [Tramo],
   agregados :: [Agregados],
   costoBase :: Costo
} --deriving (Show)

--type Tramo = (Origen, Destino, Duracion)
data Tramo = Tramo {
   origen :: Origen,
   destino :: Destino,
   duracion :: Duracion
} deriving  (Show)

type Origen = String
type Destino = String
type Duracion = Int

type Agregados = Reserva -> Costo

type Costo = Float

--2--
reservaLarga = Reserva ["Tom B.", "Frank G."] [Tramo "BA" "Sao Pablo" (6*60), Tramo "Sao Pablo" "Londres" (10*60)] [utensilios 3] 45000

reservaCorta = Reserva ["Cesar F."] [Tramo "BA" "Chascomus" 15] [utensilios 6, lunchCompleto, equipajeExtra 3] 50000

--3--
lunchCompleto = (*0.15).costoBase

menuEspecial descripcion _ = 10 * length descripcion

equipajeExtra cantidad _ = (*200) cantidad

utensilios cantidad reserva = 0.01 * cantidad * (costoBase reserva)

polizon _ _ = 0

--Funcion a utilizar si el porcentaje del utensilio se aplica sobre el costo recien calculado
{-utensilios 0 reserva = costoBase reserva
utensilios cantidad reserva = (costoBase reserva + (costoBase reserva * 0.01)) + utensilios (cantidad - 1) reserva
utensilios cantidad reserva = costoBase reserva + calcularPorcentaje cantidad (costoBase reserva) 0.01

calcularPorcentaje 0 _ _ =  0
calcularPorcentaje cantidad costo porcentaje = costo * porcentaje + calcularPorcentaje (cantidad - 1) (costo + costo * porcentaje) porcentaje
-}

--4--
sumarCosto costo reserva = reserva {costoBase = costoBase reserva + costo}
sumarAgregado agregado reserva = reserva {agregados = agregados reserva ++ [agregado]}
sumarTramo tramo reserva = reserva {tramos = tramos reserva ++ [tramo]}

--5--
precioTotal reserva = costoBase reserva + costoAgregados reserva
 where costoAgregados reserva = sum.pam (agregados reserva) $reserva
 
pam funciones valor = map ($valor) funciones

--6--
esLarga = (>15*60).sum.map duracion.tramos

--7--
nuevaEscala tramo costo = sumarTramo tramo.sumarCosto costo

--8--
estaBienConstruida :: Reserva -> Bool
estaBienConstruida = verificarTramos.tramos

verificarTramos [] = True
verificarTramos [_] = True
verificarTramos (t1:t2:ts)
   |destino t1 == origen t2 = verificarTramos (t2:ts)
   |otherwise = False
   
--9--
{-
*Main> precioTotal.sumarAgregado (utensilios 2) $reservaLarga
47250.0
-}
{-
*Main> estaBienConstruida.nuevaEscala (Tramo "Brc" "Roma" (2*60)) 4000.nuevaEscala (Tramo "Miami" "Brc" (11*60)) 15000 $reservaLarga
False
-}

--10--
personas reservas = map pasajeros reservas
masViajador reservas = foldl1 mayorCantidadTramos (concat . map pasajeros $reservas)
   where mayorCantidadTramos p1 p2 = mayorCantidadTramos' p1 p2 reservas
      
mayorCantidadTramos' p1 p2 reservas  
   |cantTramos p1 reservas > cantTramos p2 reservas = p1 
   |otherwise = p2

cantTramos pasajero reservas = sum.map (length.tramos) . filter (elem pasajero . pasajeros) $reservas