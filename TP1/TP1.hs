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
procId = undefined

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

--foldTrie :: undefined
foldTrie = undefined


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno [ ] = []
unoxuno (x:xs) = [x] : unoxuno xs

sufijos :: Procesador [a] [a]
sufijos [] = [[]]
sufijos (x:xs) = (x:xs) : sufijos xs


--Ejercicio 4
preorder :: AT a -> [a]
preorder Nil = []
preorder (Tern v u d t) = [v] ++ preorder  u ++ preorder d ++ preorder t

inorder :: AT a -> [a]
inorder Nil = []
inorder (Tern v u d t) =
    inorder u ++ inorder d ++ [v] ++ inorder t

postorder :: AT a -> [a]
postorder Nil = []
postorder (Tern v u d t) = postorder u ++ postorder d ++ postorder t ++ [v]

--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose (Rose v hijos) = v : concatMap preorderRose hijos

hojasRose :: Procesador (RoseTree a) a
hojasRose (Rose v []) = [v]
hojasRose (Rose v hijos) = concatMap hojasRose hijos

ramasRose :: Procesador (RoseTree a) [a]
ramasRose (Rose v []) = [[v]]
ramasRose (Rose v hijos) = case hijos of
                    [] -> [[v]] -- tiene una hoja (seria como la raiz)
                    _ -> concatMap (\hijo -> map (v :) (ramasRose hijo)) hijos -- se puede usar composicion tambien


--Ejercicio 6

caminos :: Trie a -> [String] -- es como hacer un dump de todas las claves posibles
caminos trie = buscarEnTrie trie -- deberia buscar desde la raiz todos los caminos posibles

buscarEnTrie :: Trie a -> [String]
buscarEnTrie (TrieNodo _ hijos) = [""] : concatMap (\(c, hijo) -> map (c :) (buscarEnTrie hijo)) hijos -- "c" es el char del nodo


--Ejercicio 7

palabras :: Trie a -> [String] -- seria la lista de claves validas
palabras trie = tail (clavesValidas "" trie) -- la raiz siempre es ""?

clavesValidas :: String -> Trie a -> [String]
clavesValidas char (TrieNodo Nothing hijos) = concatMap (\(c, hijo) -> clavesValidas (char ++ [c]) hijo) hijos
clavesValidas char (TrieNodo (Just v) hijos) = char : concatMap (\(c, hijo) -> clavesValidas (char ++ [c]) hijo) hijos


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc pred p1 p2 = \x -> if pred x then p1 x else p2 x

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) = undefined

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) = undefined

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

testsEj1 = test [ -- Casos de test para el ejercicio 1
  0             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  1     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
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
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
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
