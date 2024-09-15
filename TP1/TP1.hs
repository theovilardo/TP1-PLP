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
(.!) p1 p2 = \x -> concatMap p1 (p2 x)

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
  procVacio ["string"] ~=? ([] :: [String]), -- cmabie de [char] a [String] (en realidad deberia ser lo mismo pero haskell rompe las bolas)
  procVacio ([] :: [Int]) ~?= ([] :: [Int]),

  procId 0 ~=? [0],
  procId True ~=? [True],
  procId ([]:: [Int]) ~=? ([[]]:: [[Int]]),
  procId  "string" ~=? ["string"],
  procId [[1,3],[2,4]] ~=? [[[1,3],[2,4]]],

  procCola [0] ~=? ([]:: [Int]),
  procCola [1,2] ~=? [2],
  procCola "string" ~=? "tring",
  procCola [True, False, True] ~=? [False, True],
  procCola [[1,2,4],[1,5,6]] ~=? [[1,5,6]],

  procHijosRose (Rose 0 [Rose 5 [], Rose 1 []]) ~=? [Rose 5 [], Rose 1 []],
  procHijosRose (Rose 1 [Rose 6 [], Rose 9 [Rose 4 [], Rose 3 []]]) ~=? [Rose 6 [], Rose 9 [Rose 4 [], Rose 3 []]],
  procHijosRose (Rose 0 []) ~=? [],
  procHijosRose (Rose True [Rose False []] )~=? [Rose False []],
  procHijosRose (Rose "Selección Argentina" [Rose "Selección Francesa" []]) ~=? [Rose "Selección Francesa" []],


  procHijosAT (Tern 0 (Tern 5 Nil Nil Nil) (Tern 1 Nil Nil Nil) Nil) ~=? [Tern 5 Nil Nil Nil, Tern 1 Nil Nil Nil, Nil],
  procHijosAT (Tern True (Tern False Nil Nil Nil) Nil Nil) ~=? [Tern False Nil Nil Nil, Nil, Nil],
  procHijosAT (Tern 0 Nil Nil Nil) ~=? [Nil, Nil, Nil],
  procHijosAT (Tern "Selección Argentina" (Tern "Selección Francesa" Nil Nil Nil) (Tern "Selección de Países Bajos" Nil Nil Nil) (Tern "Selección de Uruguay" Nil Nil Nil)) ~=? [Tern "Selección Francesa" Nil Nil Nil, Tern "Selección de Países Bajos" Nil Nil Nil, Tern "Selección de Uruguay" Nil Nil Nil],

  procRaizTrie (TrieNodo (Just 20) [('a', TrieNodo Nothing [])]) ~=? [Just 20],
  procRaizTrie (TrieNodo Nothing []) ~=? ([] :: [Maybe Int]), --fixed? (borrar este comentario si se arreglo)
  procRaizTrie (TrieNodo (Just False) []) ~=? [Just False],
  procRaizTrie (TrieNodo (Just "Selección Argentina") [('f', TrieNodo (Just "Selección Francesa") [])]) ~=? [Just "Selección Argentina"],

  procSubTries (TrieNodo Nothing []) ~=? ([] :: [(Char, Trie Int)]) --fixed? (borrar este comentario si se arreglo)
  procSubTries (TrieNodo (Just 5) [('a', TrieNodo Nothing []), ('b', TrieNodo (Just 3) [])]) ~=? [('a', TrieNodo Nothing []), ('b', TrieNodo (Just 3) [])],
  procSubTries (TrieNodo (Just 'p') [('l', TrieNodo (Just 'l') [('p', TrieNodo (Just 'p') [])])]) ~=? [('l', TrieNodo (Just 'l') [('p', TrieNodo (Just 'p') [])])],
  procSubTries (TrieNodo Nothing [('y', TrieNodo (Just True) [])]) ~=? [('y', TrieNodo (Just True) [])]                                                      
  ]

testsEj2 = test [ 
 foldAT (\v u d t -> v + u + d + t) 0 (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)) ~=? 10,
 foldAT (\_ _ _ _ -> 1) 0 Nil ~=? 0,

 foldRose (\n hijosSuma -> n + sum hijosSuma) (Rose 5 [Rose 1 [], Rose 6 [], Rose 12 []])  ~=? 24,
 foldRose (\_ _ -> 1) (Rose 0 []) ~=? 1

 foldTrie (\_ hijos -> 1 + sum (map snd hijos)) (TrieNodo (Just 'a') [('b', TrieNodo (Just 'c') []), ('d', TrieNodo (Just 'e') [])]) ~=? 3
 foldTrie (\v _ -> maybe 0 (const 1) v) (TrieNodo (Just 0) []) ~=? 1
 foldTrie (\_ _ -> 1) (TrieNodo Nothing []) ~=? 1      
  ]

testsEj3 = test [ 
 unoxuno [1,2,3,4] ~=? [[1],[2],[3],[4]],
 unoxuno ([]:: [Int]) ~=? ([[]]:: [[Int]]),
 unoxuno ("String1") ~=? ["h","o","l","a"]
 unoxuno (["String2"]) ~=? [["string"]],
 unoxuno ([[True, False], [True, True]]) ~=? [[[True,False]],[[True,True]]],

 sufijos [1,2,3,4] ~=? [[1,2,3,4],[2,3,4],[3,4],[4]],
 sufijos ([]:: [Int]) ~=? ([[]]:: [[Int]]),
 sufijos ("char") ~=? ["char","har","ar","r"],
 sufijos sufijos [[1,3],[4,5],[6,7]] ~=? [[[1,3],[4,5],[6,7]],[[4,5],[6,7]],[[6,7]]],
 sufijos ([True, False]) ~=? [[True,False],[False]]
  ]

AT Int :: arbolTVacio = Nil
AT Int :: arbolT1 = Tern 16 (Tern 1 9 7 2) (Tern 14 0 3 6) (Tern 10 8 5 4)
AT Int :: arbolT2 = Tern 1 (Tern 2 3 4 Nil) (Tern 5 Nil 6 Nil) (Tern 7 8 Nil 9)
AT Int :: arbolT3 = Tern 3 (Tern 16 27 Nil (Tern 38 14 12 Nil)) Nil (Tern 49 21 16 7)

testsEj4 = test [
  preorder (arbolTVacio) ~=? [],
  preorder (arbolT1) ~=? [16,1,9,7,2,14,0,3,6,10,8,5,4],
  preorder (arbolT2) ~=? [1,2,3,4,5,6,7,8,9],
  preorder (arbolT3) ~=? [3,16,27,38,14,12,49,21,16,7],

  postorder (arbolTVacio) ~=? [],
  postorder (arbolT1) ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16],
  postorder (arbolT2) ~=? [3,4,2,6,5,8,9,7,1],
  postorder (arbolT3) ~=? [27,14,12,38,16,21,16,7,49,3],
  
  inorder (arbolTVacio) ~=? [],
  inorder (arbolT1) ~=? [9,7,1,2,0,3,14,6,16,8,5,10,4],
  inorder (arbolT2) ~=? [3,4,2,6,5,1,8,7,9],
  inorder (arbolT3) ~=? [27,16,14,12,38,3,21,16,49,7]
  ]

RoseTree Int :: roseVacio = Rose 0 []
RoseTree Int :: rose1 = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 []]
RoseTree Int :: rose2 = Rose 10 [Rose 5 [Rose 1 [], Rose 3 []], Rose 6 [Rose 4 [Rose 2 []]], Rose 9 []]
RoseTree Int :: rose3 = Rose 20 [Rose 19 [Rose 14 [Rose 6 [Rose 5 []]]], Rose 18 [Rose 12 [Rose 4 [], Rose 3 []], Rose 11 [Rose 2 []], Rose 10 [Rose 1 []]], Rose 17[Rose 9 []], Rose 16 [], Rose 15 [Rose 8 [], Rose 7 []]]

testsEj5 = test [
  preorderRose (roseVacio) ~=? [],
  preorderRose (rose1) ~=? [1,2,3,4],
  preorderRose (rose2) ~=? [10,5,1,3,6,4,2,9],
  preorderRose (rose3) ~=? [20,19,14,6,5,13,18,12,4,3,11,2,10,1,17,9,16,15,8,7],

  hojasRose (roseVacio) ~=? [],
  hojasRose (rose1) ~=? [2,3,4],
  hojasRose (rose2) ~=? [1,3,2,9],
  hojasRose (rose3) ~=? [5,4,3,2,1,9,16,8,7],

  ramasRose (roseVacio) ~=? [],
  ramasRose (rose1) ~=? [[1,2],[1,3],[1,4]],
  ramasRose (rose2) ~=? [[10,5,1],[10,5,3],[10,6,4,2],[10,9]],
  ramasRose (rose3) ~=? [[20,19,14,6,5],[20,19,13],[20,18,12,4],[20,18,12,3],[20,18,11,2],[20,18,10,1],[20,17,9],[20,16],[20,15,8],[20,15,7]]
  ]

t = TrieNodo Nothing [ ('a', TrieNodo (Just True) []) , ('b', TrieNodo Nothing [ ('a', TrieNodo (Just True) [ ('d', TrieNodo Nothing []) ])]) , ('c', TrieNodo (Just True) [])]
vacio = TrieNodo Nothing []
unico = TrieNodo Nothing [('f', TrieNodo (Just True) [])]
completa = TrieNodo Nothing [ ('a', TrieNodo Nothing [ ('b', TrieNodo Nothing [ ('c', TrieNodo (Just 'abc') []) ])])]
holamundo = (palabras TrieNodo Nothing[ ('h', TrieNodo Nothing[ ('o', TrieNodo Nothing[ ('l', TrieNodo (Just 'hol') []) ])]), ('a', TrieNodo (Just 'a') []) , ('m', TrieNodo Nothing [ ('u', TrieNodo Nothing [ ('n', TrieNodo (Just 'mun') []) ])]) , ('d', TrieNodo Nothing[ ('o', TrieNodo (Just 'do') []) ])])


testsEj6 = test [ -- Casos de test para el ejercicio 6
"ejemplo" caminos t ~=? ["", "a", "b", "ba", "bad", "c"],
"vacio" caminos vacio ~=? [],
"una palabra" caminos unico ~=? ["", "f"],
"palabra completa" caminos completa ~=? ["", "a", "ab", "abc"],
"varias palabras" caminos holamundo ~=? ["", "h", "ho", "hol", "a", "m", "mu", "mun", "d", "do"]                  
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
"ejemplo" palabras t ~=? ["a", "ba", "c"],
"palabras de un Trie vacío"  palabras vacio ~=? [],
"una palabra de una letra"  palabras unico ~=? ["f"],
"palabras con ramas" palabras completa ~=? ["abc"],
"varias palabras" palabras holamundo ~=? ["hol", "a", "mun", "do"]
   ]

pvacio _ = []
psuma1 x = [x + 1]
pconalgo _ = [1, 2, 3]
phasta x = [0..x]

testsEj8a = test [ -- Casos de test para el ejercicio 8a
"Debe usar el primer procesador si el predicado es verdadero" (ifProc (not . null) (\x -> [1, 2, 3]) (\x -> [4, 5, 6]) [1, 2, 3]) ~=? [1, 2, 3],
"Debe usar el segundo procesador si el predicado es falso"  (ifProc (not . null) (\x -> [1, 2, 3]) (\x -> [4, 5, 6]) []) ~=? [4, 5, 6],
"Usa el primer procesador si el predicado es verdadero" (ifProc (const True) psuma1 pvacio 5) ~=? [6],
"Usa el segundo procesador si el predicado es falso" (ifProc (const False) psuma1 pconalgo 5) ~=? [1, 2, 3],
"Devuelve una lista vacía si ambos procesadores devuelven vacías"  (ifProc (null) pvacio pconalgo [2,7,9]) ~=? [1,2,3],
"Debe devolver una lista vacía si ambos procesadores devuelven vacías" (ifProc (const True) (const []) (const []) [1, 2, 3]) ~=? [],
"Usa el primer procesador cuando el predicado es verdadero" (ifProc (>0) psuma1 phasta 3) ~=? [4],                     
"Debe devolver la lista del primer procesador si el predicado es verdadero" (ifProc (const True) (\x -> [1, 2, 3]) (\x -> [4, 5, 6]) [10]) ~=? [1, 2, 3]
   ]


testsEj8b = test [ -- Casos de test para el ejercicio 8b
"Concatena dos procesadores que devuelven listas vacías" (pvacio ++! pvacio) ~=? [],
"Concatena un procesador vacío con uno no vacío" (pvacio ++! (\_ -> [4, 5, 6])) ~=? [4,5,6],
"Concatena un procesador vacío con uno no vacío" ((pvacio ++! pconalgo) [7,9,10]) ~=? [1, 2, 3],
"Concatena dos procesadores que devuelven listas no vacías" ((\_ -> [1, 2, 3]) ++! (\_ -> [4, 5, 6])) ~=? [1,2,3,4,5,6],
"Concatena dos procesadores no vacíos" ((psuma1 ++! pconalgo) [1,1,1]) ~=? [6, 1, 2, 3],
"Concatena preorder y postorder para un árbol ternario" ((postorder ++! preorder) at) ~=? [1, 2, 3, 4, 2, 3, 4, 1],
"Concatena `phasta` y `psuma1`" ((phasta ++! psuma1) 5) ~=? [0, 1, 2, 3, 4,5,6]
  ]


testsEj8c = test [ -- Casos de test para el ejercicio 8c
"Aplicar dos procesadores vacíos" ((pvacio .! pvacio) [1, 2, 3]) ~=? [],
"Aplicar primer procesador vacío, segundo no vacío" (pvacio .! (\x -> [x, x + 1]) [1, 2]) ~=? [],
"Aplica el primer procesador vacío y el segundo no vacío" ((pvacio .! psuma1) 10) ~=? [],
"Aplicar dos procesadores no vacíos" ((\z -> [0..z]) .! (\x -> [x, x + 1]) [1, 3]) ~=? [0, 1, 2, 0, 1, 2, 3, 4],
"Aplica dos procesadores no vacíos" ((psuma1 .! phasta) 0) ~=? [1],
"Aplicar incrementar después de duplicar" ((incrementar .! duplicar) [1, 2]) ~=? [2, 2, 4, 4, 6, 6],
"Aplica `phasta` después de `pconalgo`" ((phasta .! pconalgo) 5) ~=? [0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]
  ]
