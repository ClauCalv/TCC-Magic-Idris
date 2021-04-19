module Magic.Data.UniqueDict where

import qualified Data.Map.Strict as SM
--import qualified Data.IntMap.Strict as SIM
--import qualified Data.HashMap.Strict as SHM --Não consigo fazer esse pacote ser importado, meu stack build quebra

{-@ measure replaceMe :: a -> TT @-} --placehoder "true" para lugares que queremos refinar algo

-- Qual estrutura de dados usar? Achei que lista de tuplas era muito fraco '[(k,v)]' e eu teria que refinar eu mesmo
-- Se eu usar 'Map k v' eu posso refinar 'k' pra ser menor que nextKey, e Map já está refinado.
-- Se eu usar 'IntMap v' eu tenho uma estrutura especializada em inteiros não refinados, mas IntMap já está refinado.
-- Se eu usar 'Hashmap k v' eu tenho a melhor estrutura possível, mas hashmap não está refinado ainda pelo LiquidHaskell

--Aqui podemos ver que alguns pacotes de containtes tem sua versão refinada (liquid-X)
--https://hackage.haskell.org/package/liquid-containers-0.6.2.1 

-- Porque eu não posso usar o nome qualificado dentro da anotação (SM.Map)? Como ele decide a referência certa?
-- Como forçar a invariante de que todas as keys são menores que nextKey?
{-@ data UDict a = UDict { nextKey :: ID,
                           dict :: Map { k : ID | k < nextKey} a} 
@-}
data UDict a = UDict Int (SM.Map Int a)

{-@ type ID = Nat @-}
type ID = Int


-- Construção


-- Qual a pós-condição de empty? 'null v'? 'size v == 0'? Qual a measure que criaremos?
-- Seria mais fácil se soubéssemos o refinamento da estrutura de dados, pra imitar
-- Mas só encontro um módulo fantasma que reexporta outro: 
{- https://github.com/ucsd-progsys/liquidhaskell/blob/develop/liquid-containers/src/Data/Map/Strict.hs
module Data.Map.Strict (module Exports) where
import "containers" Data.Map.Strict as Exports 
-}
{-@ empty :: {v : UDict a | replaceMe v} @-}
empty :: UDict a
empty = UDict 0 SM.empty

-- Nenhuma pré-condição de a, certo?
-- Pós-condição de v é 'not (null v)'? 'size v == 1'?
{-@ singleton :: a -> {v : UDict a | replaceMe v} @-}
singleton :: a -> UDict a
singleton a = UDict 1 (SM.singleton 0 a)

-- Nenhuma pré-condição de x, certo?
-- Pós-condição de v é 'size v == size x'?
-- Se eu troco lenght xs por 0, ele ainda compila, mesmo exigindo que 'k < nextKey'
{-@ fromList :: x : [a] -> {v : UDict a | replaceMe v} @-}
fromList :: [a] -> UDict a
fromList [] = empty
fromList xs = UDict (length xs) (SM.fromAscList (zip xs [0..])) -- solução O(n)


-- Modificação


-- Nenhuma pré-condição de a, certo?
-- Pós-condição de v é 'size v == size x + 1'?
-- Pós-condição de k é 'k = member v && k = nextKey x'?
{-@ insert :: a -> x : UDict a -> (k : ID, {v : UDict a | replaceMe v}) @-}
insert :: a -> UDict a -> (ID, UDict a)
insert x (UDict next xs) = (next, UDict (next + 1) (SM.insert next x xs))

-- Alguma pré-condição sobre ID estar no Map? 
-- Seria legal fazer k < nextKey x, mas como sem inverter a ordem dos argumentos?
-- Pós-condição de v é 'size v == if k member x then size x - 1 else size x'?
{-@ delete :: k : ID -> x : UDict a -> {v : UDict a | replaceMe v} @-}
delete :: ID -> UDict a -> UDict a
delete k (UDict n xs) = UDict n (SM.delete k xs)

-- Alguma pré-condição sobre ID estar no Map? 
-- Seria legal fazer k < nextKey x, mas como sem inverter a ordem dos argumentos?
-- Pós-condição de v é 'size v == size x'?
{-@ adjust :: (a -> a) -> ID -> x : UDict a -> {v : UDict a | replaceMe v} @-}
adjust :: (a -> a) -> ID -> UDict a -> UDict a
adjust f k (UDict n xs) = UDict n (SM.adjust f k xs)


-- Acesso


-- Alguma pré-condição sobre ID estar no Map? 
-- Seria legal fazer k < nextKey x, mas como sem inverter a ordem dos argumentos?
-- Pós-condição de v é 'isJust v == k member x'?
{-@ lookup :: k : ID -> x : UDict a -> {v : Maybe a | replaceMe v} @-}
lookup :: ID -> UDict a -> Maybe a
lookup k (UDict _ xs) = SM.lookup k xs

-- Alguma pré-condição sobre ID estar no Map? 
-- Seria legal fazer k < nextKey x, mas como sem inverter a ordem dos argumentos?
-- Pós-condição de v é 'v <=> k member x'?
-- Provavelmente será promovido a measure (não diretamente pq SM.member teria que ser measure tbm)
{-@ member :: k : ID -> x : UDict a -> {v : Bool | replaceMe v} @-}
member :: ID -> UDict a -> Bool
member k (UDict _ xs) = SM.member k xs


-- Tamanho


-- Pós-condição de v é 'v <=> null x'? 'v <=> size x == 0'?
-- Provavelmente será promovido a measure (não diretamente pq SM.null teria que ser measure tbm)
{-@ null :: x : UDict a -> {v : Bool | replaceMe v} @-}
null :: UDict a -> Bool
null (UDict 0 _) = True
null (UDict _ xs) = SM.null xs

-- Pós-condição de v é 'v == size x'? 'v <= nextKey x'?
-- Provavelmente será promovido a measure (não diretamente pq SM.size teria que ser measure tbm)
{-@ size :: x : UDict a -> {v : Nat | replaceMe v} @-}
size :: UDict a -> Int
size (UDict 0 _) = 0
size (UDict _ xs) = SM.size xs


-- Travessia


-- Nenhuma pré-condição de f nem de x, certo?
-- Pós-condição de v é 'size x == size v'? 'null x = null v'>
-- Mais genericamente 'forall p. p x <=> p v'? Ou melhor:
--    forall x <p :: x -> Prop>. => (a -> b) -> (UDict a)<p> -> (UDict b)<p>
-- Mas não entendi ainda como fazer esse refinamento abstrato funcionar
-- Somente os refinamentos que não envolvem 'a' poderiam ser passados, como 'size'
-- Tem como garantir isso com a sintaxe de refinamento abstrato?
-- Talvez um 'forall a. forall (UDict a)<p :: UDict a -> Prop>.' ?
--https://ucsd-progsys.github.io/liquidhaskell-blog/tags/abstract-refinements.html
{-@ map :: (ID -> a -> b) -> x : UDict a -> {v : UDict b | replaceMe v} @-}
map :: (ID -> a -> b) -> UDict a -> UDict b
map f (UDict n xs) = UDict n (SM.mapWithKey f xs)


-- mapKeys NÃO DEVE SER IMPLEMENTADO
-- Mas mapKeysMonotonic poderia com algum esforço
-- Como garantir que ela é monotônica, i.e. 'x < y => f x < fy'? Ex. '\x -> x*2 + 3'
-- '{f : (ID -> ID) | forall a b. a < b => f a < f b}' ?
-- Quais são as pós-condições de 'v'? Além de 'size v = size x'?
-- {-@ type Monotonic a = M (x :: {f : (a -> a) | replaceMe f }) @-}
-- {-@ mapKeys :: f : Monotonic ID -> x : UDict a -> {v : UDict a | replaceMe v} @-}
{-@ mapKeys :: f : (ID -> ID) -> x : UDict a -> {v : UDict a | replaceMe v} @-}
mapKeys :: (ID -> ID) -> UDict a -> UDict a
mapKeys f (UDict next xs) = UDict (f next) (SM.mapKeysMonotonic f xs)


-- Nenhuma pré-condição de f nem de x, certo?
-- Quais são as pós-condições de 'v'? Além de 'size v <= size x'?
-- Como adicionar um refinamento abstrato? De forma que
    -- filter :: (ID -> a -> {r : Bool | r <=> p a} ) -> x : UDict a -> {v : UDict a<p> | replaceMe v}
{-@ filter :: (ID -> a -> Bool) -> x : UDict a -> {v : UDict a | replaceMe v} @-}
filter :: (ID -> a -> Bool) -> UDict a -> UDict a
filter f (UDict n xs) = UDict n (SM.filterWithKey f xs)

-- Nome mais bonito pra mapMaybeWithKey.
-- Poderia ser 'mapFilter'?
-- Mesma coisa que (map fromJust) . (filter isJust) . (map f)
-- Filter e map podem ser escritos com ele:
--    map f = transform (\x -> Just (f x))
--    filter f = transform (\x -> if f x then Just x else Nothing)
-- Nenhuma pré-condição de f nem de x, certo?
-- Assim como filter não podemos garantir que o número de elementos seja o mesmo
-- Assim como map não podemos garantir que o elemento guardado é o mesmo
-- Tem alguma pós-condição pra v?
-- Assim como o mapSplit não precisa de refinamentos abstratos pois se b for a<p>, o sistema
-- de tipos já tratará corretamente.
transform :: (ID -> a -> Maybe b) -> UDict a -> UDict b
transform f (UDict n xs) = UDict n (SM.mapMaybeWithKey f xs)


-- Nenhuma pré-condição de f nem de x, certo?
-- Quais são as pós-condições de 'v'? Além de 'size v <= size x'?
-- Como adicionar um refinamento abstrato? De forma que
--      split :: (ID -> a -> {r : Bool | r <=> p a} ) 
--                  -> x : UDict a
--                  -> ({v1 : UDict a<p> | replaceMe v1}, {v2 : UDict a<not p> | replaceMe v2})
{-@ split :: (ID -> a -> Bool)
                -> x : UDict a
                -> ({v1 : UDict a | replaceMe v1}, {v2 : UDict a | replaceMe v2}) @-}
split :: (ID -> a -> Bool) -> UDict a -> (UDict a, UDict a)
--split f dict = (filter f dict, filter (not f) dict) -- provavelmente pouco performático
split f = mapSplit (\k v -> if f v then Left v else Right v)

-- Nome mais bonito pra mapEitherWithKey.
-- Mesma coisa que let x' = f x in (map fromLeft _ (filter isLeft x'), map fromRight _ (filter isRight x'))
-- Split pode ser escrito com ele
--    split f = mapSplit (\x -> if (f a) then Left a else Right a)
-- Diferentemente do split não precisa usar os refinamentos abstratos pois se b for a<p>,
-- o sistema de tipos já tratará eles corretamente. Não força necessariamente que c = a<not p>.
{-@ mapSplit :: (ID -> a -> Either b c)
                -> x : UDict a
                -> ({v1 : UDict b | replaceMe v1}, {v2 : UDict c | replaceMe v2}) @-}
mapSplit :: (ID -> a -> Either b c) -> UDict a -> (UDict b, UDict c)
mapSplit f (UDict n xs) = let (x1, x2) = SM.mapEitherWithKey f xs in (UDict n x1, UDict n x2)


-- Conversão


-- Tem alguma pós-condição pra v? 'size v = size x'?
{-@ elems :: x : UDict a -> {v : [a] | replaceMe v} @-}
elems :: UDict a -> [a]
elems (UDict _ xs) = SM.elems xs

-- Tem alguma pós-condição pra v? 'size v = size x'?
{-@ keys :: x : UDict a -> {v : [ID] | replaceMe v} @-}
keys :: UDict a -> [ID]
keys (UDict _ xs) = SM.keys xs

-- Tem alguma pós-condição pra v? 'size v = size x'?
{-@ toList :: x : UDict a -> {v : [(ID,a)] | replaceMe v} @-}
toList :: UDict a -> [(ID,a)]
toList (UDict _ xs) = SM.toList xs

