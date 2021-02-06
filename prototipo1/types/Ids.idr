
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/IdList.hs

--Mudando de Int pra Nat já que pelo visto os IDs são tudo positivos.
data Id = Id Nat

Eq Id where
  Id a == Id b = a == b

Ord Id where
  compare (Id a) (Id b) = compare a b

idToNat : Id -> Nat
idToInt (Id i) = i

-- Porque um IdList carrega um ID no final??? Não entendi ainda. É o ID da lista de IDs?
-- data IdList a = IdList [(Id, a)] Id
-- AAAAAAAHH entendi é o próximo ID a ser guardado (e também o tamanho).
-- Estou considerando reescrever tudo como IdList a = Vect n (Id, a). Mas usando Dpair pra bindar o 'n'.
-- Porque cada membro guarda seu número? não podia ser só Vect n (Id, a)?
-- data IdList a = IdList (n ** Vect n (Id, a))
-- Deixa quieto, tem que ter consistencia com remoções, não tinha pensado nisso, vou voltar pra ideia original
data IdList a = IdList (List (Id, a)) Id

Functor IdList where
  --fmap = contents . fmap . second
  map f = contents $ map (\(id, a) => (id, f a))

-- Show Id where
--   show (Id i) = show i


-- CONSTRUCTION

empty : IdList a
empty = IdList [] (Id Z)

fromList : List a -> IdList a
fromList = foldr (\x, xs => snoc x xs) empty

-- Ainda não entendi a necessidade disso:
{- Haskell:
fromListWithId : (Id -> a -> b) -> [a] -> IdList b
fromListWithId f = foldr (\x xs -> snd (snoc' (\i -> f i x) xs)) empty
-}



-- QUERYING


length : IdList a -> Nat
length (IdList xs _) = length xs

null : IdList a -> Bool
null (IdList xs _) = isNil xs

head : IdList a -> Maybe (Id, a)
head (IdList xs _) = head' xs

get : Id -> IdList a -> Maybe a
get i (IdList xs _) = lookup i xs

toList : IdList a -> List (Id, a)
toList (IdList xs _) = xs

ids : IdList a -> List Id
ids = map fst . toList

elems : IdList a -> List a
elems = map snd . toList


-- MODIFYING

set : Id -> a -> IdList a -> IdList a
set i x (IdList ixs n) = IdList (map set' ixs) n
  where
    set' ix = if fst ix == i then (i, x) else ix
-- não basta "set i x xs = modify i (const x) xs" ???

modify : Id -> (a -> a) -> IdList a -> IdList a
modify i f (IdList ixs n) = IdList (map modify' ixs) n
  where
    modify' ix = if fst ix == i then (i, f x) else ix

remove : Id -> IdList a -> Maybe (a, IdList a)
remove i ixs =
  case get i ixs of
    Just x  -> Just (x, contents (Prelude.filter (\ix => fst ix /= i')) xs)
    Nothing -> Nothing

--pop :: IdList a -> Maybe (a, IdList a)
--pop (IdList ((_, x) : ixs) i) = Just (x, IdList ixs i)
--pop _ = Nothing

cons : a -> IdList a -> IdList a
cons x xs = snd (cons' (const x) xs)

snoc : a -> IdList a -> IdList a
snoc x xs = snd (snoc' (const x) xs)

-- não entendo pq "(Id -> a)" em vez de só "a".
cons' : (Id -> a) -> IdList a -> (Id, IdList a)
cons' f (IdList ixs newId) = (newId, IdList ((newId, f newId) : ixs) (Id (succ . idToNat i)))

snoc' : a -> IdList a -> (Id, IdList a)
snoc' f (IdList ixs newId) = (newId, IdList (ixs ++ [(newID, f newId)]) (Id (succ . idToNat  i)))

contents : (List (Id, a) -> List (Id, a)) -> IdList a -> IdList b
contents f (IdList ixs i) = IdList (f ixs) i

filter : (a -> Bool) -> IdList a -> List (Id, a)
filter f = Prelude.filter (f . snd) . toList

-- Verificar como fazer isso em Idris
{- Haskell:
shuffle :: MonadRandom m => IdList a -> m (IdList a)
shuffle (IdList ixs n) = do
  ixs' <- shuffleM ixs
  return (IdList ixs' n)

removeM :: MonadState s m => (s :-> IdList a) -> Id -> m (Maybe a)
removeM label i = do
  list <- gets label
  case remove i list of
    Just (x, list') -> do puts label list'; return (Just x)
    Nothing         -> return Nothing

consM :: MonadState s m => (s :-> IdList a) -> a -> m Id
consM label x = do
  list <- gets label
  let (i, list') = cons' (const x) list
  puts label list'
  return i

snocM :: MonadState s m => (s :-> IdList a) -> a -> m Id
snocM label x = do
  list <- gets label
  let (i, list') = snoc' (const x) list
  puts label list'
  return i
-}
