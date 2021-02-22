
-- Isso deve facilitar todas as implementações de Ord de tipos que já implementam Enum
compareEnum : Enum a => a -> a -> Ordering
compareEnum x y = compare (toNat x) (toNat y)
