# Trabajo Práctico N° 1 - Programación Funcional

## Paradigmas de Lenguajes de Programación - Segundo Cuatrimestre 2024

### Introducción
En este trabajo práctico exploramos el concepto de 'procesadores', funciones que permiten recorrer y procesar estructuras de datos definidas. Un procesador se define como `type Procesador a b = a -> [b]`, donde `a` es el tipo de la estructura y `b` es el tipo de los resultados a generar.

### Estructuras a Utilizar
Para este trabajo utilizaremos las siguientes estructuras de datos:

- **Árboles Ternarios (`AT a`)**: Representa un árbol con tres ramas en cada nodo.
- **Rose Trees (`RoseTree a`)**: Representa un árbol donde cada nodo puede tener múltiples hijos.
- **Tries (`Trie a`)**: Implementación naive para recuperación de información, utilizada como conjunto o diccionario.

### Ejercicios Principales
#### Procesamiento Básico de Estructuras
1. **Implementación de Funciones Procesadoras**
   - `procVacio :: Procesador a b`
   - `procId :: Procesador a a`
   - `procCola :: Procesador [a] a`
   - `procHijosRose :: Procesador (RoseTree a) (RoseTree a)`
   - `procHijosAT :: Procesador (AT a) (AT a)`
   - `procRaizTrie :: Procesador (Trie a) (Maybe a)`
   - `procSubTries :: Procesador (Trie a) (Char, Trie a)`

#### Uso de Esquemas de Recursión
2. **Implementación de Funciones de Plegado**
   - `foldAT :: (b -> b -> b -> a -> b) -> b -> AT a -> b`
   - `foldRose :: (a -> [b] -> b) -> RoseTree a -> b`
   - `foldTrie :: (Maybe a -> [(Char, Trie a)] -> b) -> Trie a -> b`

#### Procesamiento de Listas, Árboles y Tries
3. **Procesamiento Específico de Listas**
   - `unoxuno :: Procesador [a] [a]`
   - `sufijos :: Procesador [a] [a]`

4. **Recorridos sobre Árboles Ternarios**
   - `preorder :: AT a -> [a]`
   - `postorder :: AT a -> [a]`
   - `inorder :: AT a -> [a]`

5. **Recorridos sobre RoseTrees**
   - `preorderRose :: Procesador (RoseTree a) a`
   - `hojasRose :: Procesador (RoseTree a) a`
   - `ramasRose :: Procesador (RoseTree a) [a]`

6. **Operaciones sobre Procesadores**
   - `ifProc :: (a -> Bool) -> Procesador a b -> Procesador a b -> Procesador a b`
   - `(++!) :: Procesador a b -> Procesador a b -> Procesador a b`
   - `(.!) :: Procesador b c -> Procesador a b -> Procesador a c`

### Pautas de Entrega
El trabajo se entregará en un único archivo llamado `tp1.hs` a través de la plataforma del curso. Debe incluir tests que verifiquen las funciones implementadas y ser ejecutable en Haskell2010.

### Referencias
- [The Haskell 2010 Language Report](http://www.haskell.org/onlinereport/haskell2010)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
- [Real World Haskell](http://book.realworldhaskell.org/read)
- [Hoogle](http://www.haskell.org/hoogle)
- [Hayoo!](http://holumbus.fh-wedel.de/hayoo/hayoo.html)

---
