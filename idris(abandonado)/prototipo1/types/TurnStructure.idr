
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

data Step
  = BeginningPhase BeginningStep
  | MainPhase
  | CombatPhase CombatStep
  | EndPhase EndStep

data BeginningStep
  = UntapStep
  | UpkeepStep
  | DrawStep

data CombatStep
  = BeginningOfCombatStep
  | DeclareAttackersStep
  | DeclareBlockersStep
  | CombatDamageStep
  | EndOfCombatStep

data EndStep
  = EndOfTurnStep
  | CleanupStep

-- Eq

Eq Step where
  BeginningPhase a  == BeginningPhase a = a == a
  MainPhase         == MainPhase        = True
  CombatPhase a     == CombatPhase a    = a == a
  EndPhase a        == EndPhase a       = a == a
  _                 == _                = False

Eq BeginningStep where
  UntapStep   == UntapStep  = True
  UpkeepStep  == UpkeepStep = True
  DrawStep    == DrawStep   = True
  _           == _          = False

Eq CombatStep where
  BeginningOfCombatStep == BeginningOfCombatStep  = True
  DeclareAttackersStep  == DeclareAttackersStep   = True
  DeclareBlockersStep   == DeclareBlockersStep    = True
  CombatDamageStep      == CombatDamageStep       = True
  EndOfCombatStep       == EndOfCombatStep        = True
  _                     == _                      = False

Eq EndStep where
  EndOfTurnStep == EndOfTurnStep  = True
  CleanupStep   == CleanupStep    = True
  _             == _              = False

-- Enum

Enum BeginningStep where
  pred n = fromNat (Nat.pred (toNat n))
  succ n = fromNat (S (toNat n))
  toNat UntapStep = Z
  toNat UpkeepStep = S Z
  toNat DrawStep = S (S Z)
  fromNat Z   = UntapStep
  fromNat S Z = UpkeepStep
  fromNat _   = DrawStep

Enum CombatStep where
  pred n = fromNat (Nat.pred (toNat n))
  succ n = fromNat (S (toNat n))
  toNat BeginningOfCombatStep = Z
  toNat DeclareAttackersStep  = S Z
  toNat DeclareBlockersStep   = S (S (S Z))
  toNat CombatDamageStep      = S (S (S (S Z)))
  toNat EndOfCombatStep       = S ((S (S (S Z))))
  fromNat Z               = BeginningOfCombatStep
  fromNat S Z             = DeclareAttackersStep
  fromNat S (S (S Z))     = DeclareBlockersStep
  fromNat S (S (S (S Z))) = CombatDamageStep
  fromNat _               = EndOfCombatStep

Enum EndStep where
  pred n = fromNat (Nat.pred (toNat n))
  succ n = fromNat (S (toNat n))
  toNat EndOfTurnStep = Z
  toNat CleanupStep = S Z
  fromNat Z = EndOfTurnStep
  fromNat _ = CleanupStep

-- Bound

MinBound BeginningStep where
  minBound = UntapStep

MaxBound BeginningStep where
  maxBound = DrawStep

MinBound CombatStep where
  minBound = BeginningOfCombatStep

MaxBound CombatStep where
  maxBound = EndOfCombatStep

MinBound EndStep where
  minBound = EndOfTurnStep

MaxBound EndStep where
  maxBound = CleanupStep

  -- Ord

  -- 'Step' (que representa as fases e não as etapas) não implementa Ord, Enum ou Bounded, pois enquanto a ordem das etapas é sempre
  -- constante dentro de uma fase, além da ordem das fases ser BeginningPhase -> MainPhase 1 -> CombatPhase -> MainPhase 2 -> EndPhase,
  -- ou seja, alternada/repetida, diversos efeitos podem alterar a ordem ou acrescentar/remover fases de um turno

  Ord BeginningStep where
    compare x y = compareEnum x y

    -- compare UntapStep   UntapStep   = EQ
    -- compare UpkeepStep  UpkeepStep  = EQ
    -- compare DrawStep    DrawStep    = EQ
    --
    -- compare UntapStep   UpkeepStep  = LT
    -- compare UntapStep   DrawStep    = LT
    -- compare UpkeepStep  DrawStep    = LT
    --
    -- compare _           _           = GT

  Ord CombatStep where
    compare x y = compareEnum x y

    -- compare BeginningOfCombatStep BeginningOfCombatStep = EQ
    -- compare DeclareAttackersStep  DeclareAttackersStep  = EQ
    -- compare DeclareBlockersStep   DeclareBlockersStep   = EQ
    -- compare CombatDamageStep      CombatDamageStep      = EQ
    -- compare EndOfCombatStep       EndOfCombatStep       = EQ
    --
    -- compare BeginningOfCombatStep DeclareAttackersStep  = LT
    -- compare BeginningOfCombatStep DeclareBlockersStep   = LT
    -- compare BeginningOfCombatStep CombatDamageStep      = LT
    -- compare BeginningOfCombatStep EndOfCombatStep       = LT
    -- compare DeclareAttackersStep  DeclareBlockersStep   = LT
    -- compare DeclareAttackersStep  CombatDamageStep      = LT
    -- compare DeclareAttackersStep  EndOfCombatStep       = LT
    -- compare DeclareBlockersStep   CombatDamageStep      = LT
    -- compare DeclareBlockersStep   EndOfCombatStep       = LT
    -- compare CombatDamageStep      EndOfCombatStep       = LT
    --
    -- compare _                     _                     = GT

  Ord EndStep where
    compare x y = compareEnum x y

    -- compare EndOfTurnStep EndOfTurnStep = EQ
    -- compare CleanupStep   CleanupStep   = EQ
    -- compare EndOfTurnStep CleanupStep   = LT
    -- compare _             _             = GT
