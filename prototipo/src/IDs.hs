module IDs where


{-@ type ID = {v:Int | v > 0} @-}

{-@ data IDList a = IDList {x:ID} {v:UniqueDict {y:ID | y < x} a} @-}

{-@ type UniqueDict id a = {v:[(id,a)] | uniqueDict v} @-}
{-@ type UniqueList a = {v:[a] | unique v} @-}

{-@ measure uniqueDict @-}
uniqueDict :: Eq a => [(id,a)] -> Bool
uniqueDict x = unique (map fst x)

{-@ measure unique @-}
unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = not (elem x xs) && unique xs

{-@ measure nonEmpty @-}
nonEmpty :: [a] -> Bool
nonEmpty [] = True
nonEmpty (x:xs) = False

-- não lembro se posso fazer essa sobrecarga
{-@ nonEmpty :: IDList a -> Bool @-} --Não sei se posso tirar essa declaração do comentário
nonEmpty (IDList xs ix) = nonEmpty xs

-- CONSTRUCTION

{-@ empty :: {v: IDList a | not (nonEmpty v)} @-}
empty = IDList [] 0

{-@ empty :: {x:[a]} -> {v: IDList a | nonEmpty x ==> nonEmpty v} @-}
fromList = foldr snoc empty --faz diferença aqui usar snoc no lugar de cons??

-- QUERYING

{-@ length :: IDList a -> {v:Int | 0 < v} @-}
length (IDList xs _) = length xs --será que length de listas tem {-@ assume length : [a] -> {v:Int | 0 < v} @-} ??

{-@ null :: IDList a -> Bool @-}
null (IDList xs _) = isNil xs

-- head :: IDList a -> Maybe (Id, a)
{-@ head :: {v:IDList a | nonEmpty v} -> (ID, a) @-}
head (IDList xs _) = head' xs
{-@ safeHead :: {v: IDList a} -> {x: Maybe (ID, a) | nonEmpty v ==> isJust x} @-}
safeHead ids = if nonEmpty ids then Just (head ids) else Nothing

{-@ get :: {x:ID} -> {y: IDList a} -> {v: Maybe a | x elem (ids y) ==> isJust v} @-}
get i (IDList xs _) = lookup i xs

{-@ toList :: IDList a -> UniqueDict ID a @-}
toList (IDList xs _) = xs

{-@ measure ids @-}
{-@ ids :: IDList a -> UniqueList Id @-}
ids = map fst . toList

{-@ elems :: IDList a -> List a @-} -- Não tem como dizer se há elementos repetidos com IDs diferentes.
elems = map snd . toList

{-@ measure nextID @-}
{-@ nextID :: IDList a -> ID @-}
nextID (IDList _ idx) = idx

-- MODIFYING

{-@ set :: ID -> a -> {x: IDList a} -> {v: IDList a | nonEmpty x ==> nonEmpty v} @-}
set i x (IDList ixs n) = IDList (map set' ixs) n
  where
    set' ix = if fst ix == i then (i, x) else ix
-- não basta "set i x xs = modify i (const x) xs" ???

{-@ modify :: Id -> (a -> a) -> {x: IDList a} -> {v: IDList a | nonEmpty x ==> nonEmpty v} @-}
modify i f (IDList ixs n) = IDList (map modify' ixs) n
  where
    modify' ix = if fst ix == i then (i, f x) else ix

{-@ remove :: {x:ID} -> {y: IDList a} -> {v: Maybe (a, IDList a) | x elem (ids y) ==> isJust v} @-}
remove i ixs =
  case get i ixs of
--    Just x  -> Just (x, contents (Prelude.filter (\ix => fst ix /= i')) xs)
    Just x -> Just (x, filterOnFst (\ix => fst ix /= i') xs)
    Nothing -> Nothing

--pop :: IDList a -> Maybe (a, IDList a)
--pop (IDList ((_, x) : ixs) i) = Just (x, IDList ixs i)
--pop _ = Nothing

{-@ cons :: a -> {x: IDList a} -> {v: IDList a | nonEmpty v && nextID v = nextID x + 1} @-}
cons x xs = snd (cons' (const x) xs)

{-@ snoc :: a -> {x: IDList a} -> {v: IDList a | nonEmpty v && nextID v = nextID x + 1} @-}
snoc x xs = snd (snoc' (const x) xs)

-- não entendo pq "(Id -> a)" em vez de só "a".
-- vamos lá, função talvez problemática; talvez tenhamos que limitar o que "f" pode fazer para que as invariantes se mantenham
-- iremos descobrir se o LH rodar e pintar tudo de vermelho
-- cons' :: (Id -> a) -> IDList a -> (Id, IDList a)
{-@ cons' :: (ID -> a) -> {x: IDList a} -> ({w:ID | w = nextID x}, {v: IDList a | nonEmpty v && nextID v = nextID x + 1}) @-}
cons' f (IDList ixs newId) = (newId, IDList ((newId, f newId) : ixs) (newID + 1)))

{-@ snoc' :: (ID -> a) -> {x: IDList a} -> ({w:ID | w = nextID x}, {v: IDList a | nonEmpty v && nextID v = nextID x + 1}) @-}
snoc' f (IDList ixs newId) = (newId, IDList (ixs ++ [(newID, f newId)]) (Id (succ . idToNat  i)))

-- Que nome horrível!!! Essa função deveria se chamar mapInner ou mapList ou algo assim. (Não pode ser fmap pois estaria parametrizado em a e não na lista).
-- Outra função dessa vez certamente problemática. Como garantir que teremos uma IDList válida?
-- contents :: ([(Id, a)] -> [(Id, a)]) -> IDList a -> IDList b
-- E se "f" simplesmente for "\(id, a) -> (id + 1000, a)", como garantir que nextID seja consistente (se "f é passado antes da lista")?
-- Uma maneira é inverter a ordem dos parâmentros pra poder garantir que f seja dependente da lista, mas isso é contra a convenção.
-- {-@ contents :: {x: IDList a} -> (UniqueDict ID a -> UniqueDict {y: ID | y < nextID x} a) -> IDList a @-}
-- Outra é criar um tipo depedente de funções que limitam o ID a um valor...
-- {-@ type UniqueDictFunc n a = UniqueDict ID a -> UniqueDict {y: ID | y < n} a @-}
-- {-@ contents :: UniqueDictFunc n a -> {x: IDList a | nextID x = n} -> {v: IDList a | nextID v = n} @-}
-- Mas como adequar uma função simples como Prelude.filter nessa assinatura. Será que vai ser tranquila? Eu duvido.
-- Vou deixar sem até poder testar e decidir.
contents f (IDList ixs i) = IDList (f ixs) i

-- Vou criar essas funções pra não ter que usar "contents (filter _) _"
{-@ filterOnFst :: (ID -> Bool) -> IDList a -> IDList a @-}
filterOnFst f (IDList ixs i) = IDList (query (f . fst) ixs) i
{-@ filterOnSnd :: (a -> Bool) -> IDList a -> IDList a @-}
filterOnSnd f (IDList ixs i) = IDList (query (f . snd) ixs) i

-- Nome péssimo! Não deixa claro que retorna outra estrutura em vez da mesma. Vou renomear para "query" (ou "select"?).
-- Também vou mudar pra servir pra ambos ID e "a".
-- Também não devia estar aqui em baixo, mover lá pra cima sob "QUERYING"
-- filter :: (a -> Bool) -> IDList a -> [(Id, a)]
{-@ query :: ((ID, a) -> Bool) -> IDList a -> UniqueDict a @-}
query f = Prelude.filter f . toList

-- Vou deixar esses quieto por enquanto
{-
shuffle :: MonadRandom m => IDList a -> m (IDList a)
shuffle (IDList ixs n) = do
  ixs' <- shuffleM ixs
  return (IDList ixs' n)

removeM :: MonadState s m => (s :-> IDList a) -> Id -> m (Maybe a)
removeM label i = do
  list <- gets label
  case remove i list of
    Just (x, list') -> do puts label list'; return (Just x)
    Nothing         -> return Nothing

consM :: MonadState s m => (s :-> IDList a) -> a -> m Id
consM label x = do
  list <- gets label
  let (i, list') = cons' (const x) list
  puts label list'
  return i

snocM :: MonadState s m => (s :-> IDList a) -> a -> m Id
snocM label x = do
  list <- gets label
  let (i, list') = snoc' (const x) list
  puts label list'
  return i
-}

