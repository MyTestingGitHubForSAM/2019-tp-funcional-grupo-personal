--Modelado genérico del tipo Planta
data Planta = Planta{
especie :: String,
puntosVida :: Int,
cantSoles :: Int,
poderAtaque :: Int
} deriving (Show)

--peaShooter
peaShooter = Planta {
especie = "peaShooter",
puntosVida = 5,
cantSoles = 0,
poderAtaque = 2
}

--Repeater
repeater = Planta {
especie = "Repeater",
puntosVida = puntosVida peaShooter,
cantSoles = cantSoles peaShooter,
poderAtaque = poderAtaque peaShooter * 2
}

--SunFlower
sunFlower = Planta {
especie = "SunFlower",
puntosVida = 7,
cantSoles = 1,
poderAtaque = 0
}

--Nut
nut = Planta {
especie = "Nut",
puntosVida = 100,
cantSoles = 0,
poderAtaque = 0
}


--Modelado generico del tipo Zombie
data Zombie = Zombie {
nombre :: String,
accesorios :: [String],
daño :: Int,
nivelDeMuerte :: Int
} deriving (Show)

--Zombie Base
zombieBase = Zombie {
nombre = "Zombie",
accesorios = [],
daño = 1,
nivelDeMuerte = (nivelDeMuerteDe.nombre) zombieBase
}

--Balloon Zombie
balloonZombie = Zombie {
nombre = "Pepe Colgado",
accesorios = ["Globo"],
daño = daño zombieBase,
nivelDeMuerte = (nivelDeMuerteDe.nombre) balloonZombie
}

--Newspaper Zombie
newsPaperZombie = Zombie {
nombre = "Beto el chismoso",
accesorios = ["Diario"],
daño = 2,
nivelDeMuerte = (nivelDeMuerteDe.nombre) newsPaperZombie
}

--Gargantuar
gargantuar = Zombie {
nombre = "Gargantuar Hulk Smash Puny God",
accesorios = ["Poste", "Zombie Enano"],
daño = 30,
nivelDeMuerte = (nivelDeMuerteDe.nombre) gargantuar
}

--Determinar Nivel de Muerte
nivelDeMuerteDe :: String -> Int
nivelDeMuerteDe nombre = length nombre

--2--
--a--Defino especialidad
especialidad :: Planta -> String
especialidad planta
 |cantSoles planta > 0 = "Proveedora"
 |poderAtaque planta *2 > puntosVida planta = "Atacante"
 |otherwise = "Defensiva"
 
--b--Defino peligro
esPeligroso :: Zombie -> Bool
esPeligroso zombie = (length.accesorios) zombie > 1 || nivelDeMuerte zombie > 10

--3--
--Defino listas Plantas y Zombies
data LineaDeDefensa = LineaDeDefensa{
plantas :: [Planta],
zombies :: [Zombie]
} deriving (Show)

--Genero las lineas
linea1 = LineaDeDefensa{
plantas = [sunFlower, sunFlower, sunFlower],
zombies = []
}

linea2 = LineaDeDefensa{
plantas = [peaShooter, peaShooter, sunFlower,nut],
zombies = [zombieBase, newsPaperZombie]
}

linea3 = LineaDeDefensa{
plantas = [sunFlower, peaShooter],
zombies = [gargantuar,zombieBase,zombieBase]
}

linea4 = LineaDeDefensa{
plantas = [peaShooter],
zombies = [zombieBase]
}

agregarPlantaALinea planta linea = LineaDeDefensa{
plantas = plantas linea ++ [planta],
zombies = zombies linea
}

agregarZombieALinea zombie linea = LineaDeDefensa{
plantas = plantas linea,
zombies = zombies linea ++ [zombie]
}

--estaEnPeligro :: LineaDeDefensa -> Bool