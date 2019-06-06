import Text.Show.Functions

data Zombie = Zombie {
   nombre :: String,
   accesorios :: [String],
   poderMordida :: Int,
   nivelDeMuerte :: Int
} deriving (Show)

data Planta = Planta{
   especie :: String,
   puntosVida :: Int,
   solesProducidos :: Int,
   poderAtaque :: Int,
   ataque :: (Zombie -> Zombie)
} deriving (Show)

data LineaDeDefensa = LineaDeDefensa{
plantas :: [Planta],
zombies :: [Zombie]
} deriving (Show)

data Jardin = Jardin {lineas :: [LineaDeDefensa] } deriving (Show)

--Plantas
peaShooter = Planta {
   especie = "PeaShooter",
   puntosVida = 5,
   solesProducidos = 0,
   poderAtaque = 2,
   ataque = ataquePlanta peaShooter
}

repeater = Planta {
   especie = "Repeater",
   puntosVida = puntosVida peaShooter,
   solesProducidos = solesProducidos peaShooter,
   poderAtaque = poderAtaque peaShooter * 2,
   ataque = ataquePlanta repeater
}

sunFlower soles = Planta {
   especie = "SunFlower",
   puntosVida = 7,
   solesProducidos = soles,
   poderAtaque = 0,
   ataque = ataquePlanta.sunFlower $soles
}

solesProducidosBase :: Int
solesProducidosBase = 1

nut = Planta {
   especie = "Nut",
   puntosVida = 100,
   solesProducidos = 0,
   poderAtaque = 0,
   ataque = ataqueNut
}

cactus = Planta {
   especie = "Cactus",
   puntosVida = 5,
   solesProducidos = 0,
   poderAtaque = 0,
   ataque = ataqueCactus
}

--Zombies
zombieBase = Zombie {
   nombre = "Zombie",
   accesorios = [],
   poderMordida = 1,
   nivelDeMuerte = nivelDeMuerteDe.nombre $ zombieBase
}

balloonZombie = Zombie {
   nombre = "Pepe Colgado",
   accesorios = ["Globo"],
   poderMordida = poderBalloon.accesorios $ balloonZombie,
   nivelDeMuerte = nivelDeMuerteDe.nombre $ balloonZombie
}

newspaperZombie = Zombie {
   nombre = "Beto el chismoso",
   accesorios = ["Diario"],
   poderMordida = length.concat.accesorios $ newspaperZombie,
   nivelDeMuerte = nivelDeMuerteDe.nombre $ newspaperZombie
}

gargantuar = Zombie {
   nombre = "Gargantuar Hulk Smash Puny God",
   accesorios = ["Poste", "Zombie Enano"],
   poderMordida = (+) 30.length.accesorios $ gargantuar,
   nivelDeMuerte = nivelDeMuerteDe.nombre $ gargantuar
}

--Determinar Nivel de Muerte
nivelDeMuerteDe :: String -> Int
nivelDeMuerteDe nombre = length nombre

--2--
--a--Defino especialidad
especialidad :: Planta -> String
especialidad planta
 |solesProducidos planta > 0 = "Proveedora"
 |((*2) . poderAtaque $ planta) > (puntosVida planta) = "Atacante"
 |otherwise = "Defensiva"
 
--b--Defino peligro
esPeligroso :: Zombie -> Bool
esPeligroso zombie = (length.accesorios $ zombie) > 1 || nivelDeMuerte zombie > 10

--3--

--Genero las lineas
linea1 = LineaDeDefensa{
plantas = [sunFlower solesProducidosBase, sunFlower solesProducidosBase, sunFlower solesProducidosBase],
zombies = []
}

linea2 = LineaDeDefensa{
plantas = [peaShooter, peaShooter, sunFlower solesProducidosBase,nut],
zombies = [zombieBase, newspaperZombie]
}

linea3 = LineaDeDefensa{
plantas = [sunFlower solesProducidosBase, peaShooter],
zombies = [gargantuar,zombieBase,zombieBase]
}

linea4 = LineaDeDefensa{
plantas = [peaShooter],
zombies = [zombieBase]
}

--3a--
agregarPlanta :: Planta -> LineaDeDefensa -> LineaDeDefensa
agregarPlanta planta linea = linea{plantas = agregarAFila (plantas linea) planta}

agregarZombie :: Zombie -> LineaDeDefensa -> LineaDeDefensa
agregarZombie zombie linea = linea{zombies = agregarAFila (zombies linea) zombie}

agregarAFila :: [pz] -> pz -> [pz]
agregarAFila plantasOZombies plantaOZombie =  plantasOZombies ++ [plantaOZombie]

--3b--
estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = (poderTotalDeAtaqueDe linea < poderTotalDeMordiscosDe linea ) || (sonTodosZombiesPeligrososDe linea && hayUnZombieEn linea )

poderTotalDeAtaqueDe :: LineaDeDefensa -> Int
poderTotalDeAtaqueDe linea = sum.(map poderAtaque) $ plantas linea

poderTotalDeMordiscosDe :: LineaDeDefensa -> Int
poderTotalDeMordiscosDe linea = sum.(map poderMordida) $ zombies linea

sonTodosZombiesPeligrososDe :: LineaDeDefensa -> Bool
sonTodosZombiesPeligrososDe linea = all esPeligroso (zombies linea)

hayUnZombieEn :: LineaDeDefensa -> Bool
hayUnZombieEn linea = (>0).length $ (zombies linea)

--3c--
necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all ((=="Proveedora").especialidad) (plantas linea)

--3d--
--REHACER

--4a--
esMixta :: LineaDeDefensa -> Bool
esMixta linea = sonPlantasMixtas (plantas linea) && lineaTiene2Plantas (plantas linea)

--Verifico si la lista de plantas es mixta
sonPlantasMixtas :: [Planta] -> Bool
sonPlantasMixtas [planta] = True
sonPlantasMixtas (planta1:planta2:plantas)
 |especialidad planta1 == especialidad planta2 = False
 |otherwise = sonPlantasMixtas (planta2:plantas)

--Verifico si la lista tiene al menos 2 plantas
lineaTiene2Plantas :: [Planta] -> Bool
lineaTiene2Plantas (planta1:planta2:plantas) = True
lineaTiene2Plantas [planta] = False
lineaTiene2Plantas [] = False

--5a-- Ataque de Planta a Zombie

ataquePlanta :: Planta -> Zombie -> Zombie
ataquePlanta planta zombie = zombie {
   nombre = drop (poderAtaque planta) (nombre zombie),
   nivelDeMuerte = nivelDeMuerteDe . drop (poderAtaque planta) $ nombre zombie
}

--5b-- Ataque de Zombie a Planta
ataqueZombie :: Zombie -> Planta -> Planta
ataqueZombie zombie planta = planta{
   puntosVida = plantaDanada (puntosVida planta) (poderMordida zombie)
}

plantaDanada puntosVida poderMordida
   |puntosVida - poderMordida <=0 = 0
   |otherwise = puntosVida - poderMordida
   
--2da entrega
--1--
ataqueNut :: Zombie -> Zombie
ataqueNut zombie
   |(poderMordida zombie) > 1 = zombie{poderMordida = (poderMordida zombie) - 1}
   |otherwise = zombie
   
ataqueCactus :: Zombie -> Zombie
ataqueCactus zombie
   |elem "Globo" (accesorios zombie) = zombie{accesorios = quitar "Globo" (accesorios zombie),
                                              poderMordida = poderBalloon (quitar "Globo" (accesorios zombie))}
   |otherwise = zombie

quitar _ [] = []
quitar e (x:xs)
   |e == x = xs
   |otherwise = quitar e xs
   
poderBalloon :: [String] -> Int
poderBalloon accesorios
   |elem "Globo" accesorios = 5
   |otherwise = 2

--3--
cumplenPlantas :: (Planta -> Bool) -> LineaDeDefensa -> [Planta]
cumplenPlantas condicion linea = filter condicion (plantas linea)

--3b--
--i			cumplenPlantas ((>0).solesProducidos) linea3
--ii		cumplenPlantas (\x -> ((=='P').head.especie $x) && ((>4).puntosVida $x )) linea2
--iii		all (==(especie.head.plantas $linea2)) (map especie . cumplenPlantas (\x -> (solesProducidos x > 0) && (puntosVida x >0) ) $linea2)


--4--
miJardin :: Jardin
miJardin = Jardin {lineas = [linea1,linea2,linea3,linea4]}

type Potenciador = (Jardin -> Jardin)
navidadZombie accesorio jardin = jardin {lineas = map (agregarAccesorioLinea accesorio) (lineas jardin)}
catenaccio jardin = jardin {lineas = map (agregarPlanta nut) (lineas jardin)}
riego vida criterio jardin = jardin {lineas = map (aumentarVidaSegunLinea vida criterio) (lineas jardin)}

aumentarVida :: Int -> Planta -> Planta
aumentarVida vida planta = planta {puntosVida = puntosVida planta + vida}

aumentarVidaSegun :: Int -> (Planta -> Bool) -> [Planta] -> [Planta]
aumentarVidaSegun vida criterio plantas = map (aumentarVida vida) (filter criterio plantas)

aumentarVidaSegunLinea :: Int -> (Planta -> Bool) -> LineaDeDefensa -> LineaDeDefensa
aumentarVidaSegunLinea vida criterio linea = linea {plantas = aumentarVidaSegun vida criterio (plantas linea)}

agregarAccesorioLinea :: String -> LineaDeDefensa -> LineaDeDefensa
agregarAccesorioLinea accesorio linea = linea {zombies = map (agregarAccesorioZombie accesorio) (zombies linea) }

agregarAccesorioZombie :: String -> Zombie -> Zombie
agregarAccesorioZombie accesorio zombie = zombie {accesorios = accesorios zombie ++ [accesorio],
                                                  poderMordida = calcularPoderMordida zombie accesorio}

calcularPoderMordida zombie accesorio
   | nombre zombie == nombre zombieBase = 1
   | nombre zombie == nombre balloonZombie = poderBalloon . (++) (accesorios zombie)  $[accesorio]
   | nombre zombie == nombre newspaperZombie = length.concat.(++) (accesorios zombie) $[accesorio]
   | nombre zombie == nombre gargantuar = (+)30.length.(++) (accesorios zombie) $[accesorio]
   
potenciar :: Jardin -> [Potenciador] -> Jardin
potenciar jardin [] = jardin
potenciar jardin (p:ps) = flip potenciar ps . p $jardin

--5to punto--
--a--
mejorValorada criterio planta1 planta2
   |criterio planta1 >= criterio planta2 = planta1
   |otherwise = planta2

--b--
mostValuablePlant criterio plantas = foldl1 (mejorValorada criterio) plantas

--c--
--mostValuablePlant (puntosVida) (concatMap plantas (lineas miJardin))

--6to punto--
todasAUno :: [Planta] -> [Zombie] -> [Zombie]
todasAUno [] zombies = zombies
todasAUno _ [] = []
todasAUno (p:ps) (z:zs)
   |(==0).nivelDeMuerte.ataquePlanta p $z = todasAUno ps zs
   |otherwise = todasAUno ps ((ataquePlanta p z):zs)

todasAUnoLinea linea = linea {zombies = todasAUno (plantas linea) (zombies linea)}   
   
atacaUltima [] _ = []
atacaUltima plantas [] = plantas
atacaUltima plantas (z:zs)
   |puntosVida (ataqueZombie z $last plantas) == 0 = init plantas
   |otherwise = init plantas ++ [ataqueZombie z (last plantas)]

atacaUltimaLinea linea = linea {plantas = atacaUltima (plantas linea) (zombies linea)}

ataqueCompleto = atacaUltimaLinea . todasAUnoLinea

ataqueMasivo linea
   |length (zombies linea) == 0 || length (plantas linea) == 0 = linea
   |otherwise = ataqueMasivo . ataqueCompleto $linea

   
theZombiesAteYourBrains jardin = any (==0) (filter (==0) (map length (map plantas (map ataqueMasivo (lineas jardin)))))