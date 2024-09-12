module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right
        
        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio = \_ -> []

procId :: Procesador a a
procId = \x -> [x]

procCola :: Procesador [a] a
procCola [] = []
procCola (x:xs) = xs

procHijosRose :: Procesador (RoseTree a) (RoseTree a) -- devuelve una lista de RoseTree (Proc)
procHijosRose (Rose _ hijos) = hijos 

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern v u d t) = [u, d, t]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo Nothing _) = []
procRaizTrie (TrieNodo (Just x) _) = [Just x]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ []) = []
procSubTries (TrieNodo _ hijos) = hijos


--Ejercicio 2

foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b -- es como el de foldAEB pero con un hijo mas por nodo
foldAT cTern cNil at = case at of
                Nil -> cNil
                Tern v u d t -> cTern v (foldAT cTern cNil u) (foldAT cTern cNil d) (foldAT cTern cNil t)

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose cRose (Rose n hijos) = cRose n (map rec hijos)
                      where rec = foldRose cRose

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie cTrie (TrieNodo v hijos) = cTrie v (map (\(char, hijo) -> (char, foldTrie cTrie hijo)) hijos)


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = map (\ x -> [x])

sufijos :: Procesador [a] [a]
sufijos xs = rec xs []
      where rec [] acc = acc
            rec ys acc = ys : rec (tail ys) acc

--Ejercicio 4
preorder :: AT a -> [a]
preorder = foldAT (\v u d t -> [v] ++ u ++ d ++ t) [] -- valor inicial "[]"

inorder :: AT a -> [a]
inorder = foldAT (\v u d t -> u ++ d ++ [v] ++ t) []

postorder :: AT a -> [a]
postorder = foldAT (\v u d t -> u ++ d ++ t ++ [v]) []

--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\v hijos -> v : concat hijos)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\v hijos -> if null hijos then [v] else concat hijos)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\v hijos -> if null hijos then [[v]] else map (v: ) (concat hijos))


--Ejercicio 6

caminos :: Trie a -> [String] -- es como hacer un dump de todas las claves posibles
caminos = foldTrie (\_ paresCs -> [""] ++ concatMap (\(c, chars) -> map (c :) chars) paresCs)


--Ejercicio 7

palabras :: Trie a -> [String]
palabras = foldTrie palabrasFold

palabrasFold :: Maybe a -> [(Char, [String])] -> [String]
palabrasFold v cs = case v of
                Nothing -> concatMap (\(c, strings) -> map (c: ) strings) cs
                Just _  -> "" : concatMap (\(c, strings) -> map (c: ) strings) cs


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc pred p1 p2 = \x -> if pred x then p1 x else p2 x

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) p1 p2 = \x -> p1 x ++ p2 x

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) p1 p2 = \x -> mapProcesador p1 (p2 x) -- habria que armar un mapProcesador

mapProcesador :: Procesador a b -> [a] -> [b]
mapProcesador _ [] = []
mapProcesador p1 (x:xs) = p1 x ++ mapProcesador p1 xs

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

testsEj1 = test [ 
 procVacio 0 ~=? ([]:: [Int]),
  procVacio (-15) ~=? ([]:: [Int]),
  procVacio True ~=? ([]::[Bool]),
  --procVacio "string" ~=? ([]::[char]),
  procVacio ([] :: [Int]) ~?= ([] :: [Int]),

  procId 0 ~=? [0],
  procId True ~=? [True],
  --procId ([]::Int) ~=? ([[]]:: [[Int]]),
  procId  "string" ~=? ["string"],
  procId [[1,3],[2,4]] ~=? [[[1,3],[2,4]]],

  procCola [0] ~=? ([]:: [Int]),
  procCola [1,2] ~=? [2],
  procCola "string" ~=? "tring",
  procCola [True, False, True] ~=? [False, True],
  procCola [[1,2,4],[1,5,6]] ~=? [[1,5,6]],

  --procHijosRose Rose 0 [Rose 5 [], Rose 1 []] ~=? [Rose 5 [], Rose 1 []],
  --procHijosRose Rose 1 [Rose 6 [], Rose 9 [Rose 4 [], Rose 3 []]] ~=? [Rose 6 [], Rose 9 [Rose 4 [], Rose 3 []]],
  --procHijosRose Rose 0 [] ~=? [],
  --procHijosRose Rose True [Rose False []] ~=? [Rose False []],
  --procHijosRose Rose "Selección Argentina" [Rose "Selección Francesa" []] ~=? [Rose "Selección Francesa" []],


  procHijosAT (Tern 0 (Tern 5 Nil Nil Nil) (Tern 1 Nil Nil Nil) Nil) ~=? [Tern 5 Nil Nil Nil, Tern 1 Nil Nil Nil, Nil],
  procHijosAT (Tern True (Tern False Nil Nil Nil) Nil Nil) ~=? [Tern False Nil Nil Nil, Nil, Nil],
  procHijosAT (Tern 0 Nil Nil Nil) ~=? [Nil, Nil, Nil],
  procHijosAT (Tern "Selección Argentina" (Tern "Selección Francesa" Nil Nil Nil) (Tern "Selección de Países Bajos" Nil Nil Nil) (Tern "Selección de Uruguay" Nil Nil Nil)) ~=? [Tern "Selección Francesa" Nil Nil Nil, Tern "Selección de Países Bajos" Nil Nil Nil, Tern "Selección de Uruguay" Nil Nil Nil],

  procRaizTrie (TrieNodo (Just 20) [('a', TrieNodo Nothing [])]) ~=? [Just 20],
  --procRaizTrie (TrieNodo Nothing []) ~=? [],
  procRaizTrie (TrieNodo (Just False) []) ~=? [Just False],
  procRaizTrie (TrieNodo (Just "Selección Argentina") [('f', TrieNodo (Just "Selección Francesa") [])]) ~=? [Just "Selección Argentina"],

  --procSubTries (TrieNodo Nothing []) ~=? [],
  procSubTries (TrieNodo (Just 5) [('a', TrieNodo Nothing []), ('b', TrieNodo (Just 3) [])]) ~=? [('a', TrieNodo Nothing []), ('b', TrieNodo (Just 3) [])],
  procSubTries (TrieNodo (Just 'p') [('l', TrieNodo (Just 'l') [('p', TrieNodo (Just 'p') [])])]) ~=? [('l', TrieNodo (Just 'l') [('p', TrieNodo (Just 'p') [])])],
  procSubTries (TrieNodo Nothing [('y', TrieNodo (Just True) [])]) ~=? [('y', TrieNodo (Just True) [])]                                                      
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  (0,0)       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  'a'      -- Caso de test 1 - expresión a testear
    ~=? 'a'            -- Caso de test 1 - resultado esperado
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  ""       -- Caso de test 1 - expresión a testear
    ~=? ""                             -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  0       -- Caso de test 1 - expresión a testear
    ~=? 0                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  False       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
"palabras de un Trie vacío"  palabras (TrieNodo Nothing []) ~=? [],
"una palabra de una letra"  palabras (TrieNodo Nothing [('f', TrieNodo (Just True) [])]) ~=? ["f"],
"palabras con ramas" palabras (TrieNodo Nothing [ ('a', TrieNodo Nothing [ ('b', TrieNodo Nothing [ ('c', TrieNodo (Just 'abc') []) ])])]) ~=? ["abc"],
"varias palabras" palabras TrieNodo Nothing[ ('h', TrieNodo Nothing[ ('o', TrieNodo Nothing[ ('l', TrieNodo (Just 'hol') []) ])]), ('a', TrieNodo (Just 'a') []) , ('m', TrieNodo Nothing [ ('u', TrieNodo Nothing [ ('n', TrieNodo (Just 'mun') []) ])]) , ('d', TrieNodo Nothing[ ('o', TrieNodo (Just 'do') []) ])] ~=? ["hol", "a", "mun","do"]                                         -- Caso de test 1 - resultado esperado
  ]

testsEj8a = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
