module Magic.Types.Objects.ObjectTypes where


data ObjectTypes = ObjectTypes
  { supertypes           :: Set Supertype
  , artifactSubtypes     :: Maybe (Set ArtifactSubtype)
  , creatureSubtypes     :: Maybe (Set CreatureSubtype)
  , enchantmentSubtypes  :: Maybe (Set EnchantmentSubtype)
  , instantSubtypes      :: Maybe (Set SpellSubtype)
  , landSubtypes         :: Maybe (Set LandSubtype)
  , planeswalkerSubtypes :: Maybe (Set PlaneswalkerSubtype)
  , sorcerySubtypes      :: Maybe (Set SpellSubtype)
  } deriving (Eq, Ord, Show, Read)

instance Semigroup ObjectTypes where
  x <> y = ObjectTypes
    { supertypes           = supertypes x           <> supertypes y
    , artifactSubtypes     = artifactSubtypes x     <> artifactSubtypes y
    , creatureSubtypes     = creatureSubtypes x     <> creatureSubtypes y
    , enchantmentSubtypes  = enchantmentSubtypes x  <> enchantmentSubtypes y
    , instantSubtypes      = instantSubtypes x      <> instantSubtypes y
    , landSubtypes         = landSubtypes x         <> landSubtypes y
    , planeswalkerSubtypes = planeswalkerSubtypes x <> planeswalkerSubtypes y
    , sorcerySubtypes      = sorcerySubtypes x      <> sorcerySubtypes y
    }

instance Monoid ObjectTypes where
  mempty = ObjectTypes mempty mempty mempty mempty mempty mempty mempty mempty

data Supertype = Basic | Legendary
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ArtifactSubtype = Equipment
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CreatureSubtype
  = Advisor | Ally | Angel | Anteater | Antelope | Ape | Archer | Archon
  | Artificer | Assassin | AssemblyWorker | Atog | Aurochs | Avatar | Badger
  | Barbarian | Basilisk | Bat | Bear | Beast | Beeble | Berserker | Bird
  | Blinkmoth | Boar | Bringer | Brushwagg | Camarid | Camel | Caribou
  | Carrier | Cat | Centaur | Cephalid | Chimera | Citizen | Cleric
  | Cockatrice | Construct | Coward | Crab | Crocodile | Cyclops | Dauthi
  | Demon | Deserter | Devil | Djinn | Dragon | Drake | Dreadnought | Drone
  | Druid | Dryad | Dwarf | Efreet | Elder | Eldrazi | Elemental | Elephant
  | Elf | Elk | Eye | Faerie | Ferret | Fish | Flagbearer | Fox | Frog
  | Fungus | Gargoyle | Germ | Giant | Gnome | Goat | Goblin | Golem | Gorgon
  | Graveborn | Gremlin | Griffin | Hag | Harpy | Hellion | Hippo | Hippogriff
  | Homarid | Homunculus | Horror | Horse | Hound | Human | Hydra | Hyena
  | Illusion | Imp | Incarnation | Insect | Jellyfish | Juggernaut | Kavu
  | Kirin | Kithkin | Knight | Kobold | Kor | Kraken | Lammasu | Leech
  | Leviathan | Lhurgoyf | Licid | Lizard | Manticore | Masticore | Mercenary
  | Merfolk | Metathran | Minion | Minotaur | Monger | Mongoose | Monk
  | Moonfolk | Mutant | Myr | Mystic | Nautilus | Nephilim | Nightmare
  | Nightstalker | Ninja | Noggle | Nomad | Octopus | Ogre | Ooze | Orb | Orc
  | Orgg | Ouphe | Ox | Oyster | Pegasus | Pentavite | Pest | Phelddagrif
  | Phoenix | Pincher | Pirate | Plant | Praetor | Prism | Rabbit | Rat
  | Rebel | Reflection | Rhino | Rigger | Rogue | Salamander | Samurai | Sand
  | Saproling | Satyr | Scarecrow | Scorpion | Scout | Serf | Serpent | Shade
  | Shaman | Shapeshifter | Sheep | Siren | Skeleton | Slith | Sliver | Slug
  | Snake | Soldier | Soltari | Spawn | Specter | Spellshaper | Sphinx | Spider
  | Spike | Spirit | Splinter | Sponge | Squid | Squirrel | Starfish | Surrakar
  | Survivor | Tetravite | Thalakos | Thopter | Thrull | Treefolk
  | Triskelavite | Troll | Turtle | Unicorn | Vampire | Vedalken | Viashino
  | Volver | Wall | Warrior | Weird | Werewolf | Whale | Wizard | Wolf
  | Wolverine | Wombat | Worm | Wraith | Wurm | Yeti | Zombie | Zubera
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data EnchantmentSubtype = Aura | Curse
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SpellSubtype = Arcane | Trap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data LandSubtype = Plains | Island | Swamp | Mountain | Forest | Locus
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PlaneswalkerSubtype = Chandra | Elspeth | Garruk | Gideon | Jace
  | Koth | Liliana | Sorin | Tezzeret | Venser | Karn 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- CONVENIENT TYPE SETS

-- | Supertype 'Basic'.
basicType :: ObjectTypes
basicType = mempty { supertypes = Set.singleton Basic }

-- | Supertype 'Legendary'.
legendaryType :: ObjectTypes
legendaryType = mempty { supertypes = Set.singleton Legendary }

-- | Card type @Artifact@.
artifactType :: ObjectTypes
artifactType = mempty { artifactSubtypes = Just mempty }

-- | Card type @Artifact - 'Equipment'@.
equipmentType :: ObjectTypes
equipmentType = mempty { artifactSubtypes = Just (Set.singleton Equipment) }

-- | Card type @Creature@.
creatureType :: ObjectTypes
creatureType = creatureTypes []

-- | Card type @Creature@ with the specified creature types.
creatureTypes :: [CreatureSubtype] -> ObjectTypes
creatureTypes tys = mempty { creatureSubtypes = Just (Set.fromList tys) }

-- | Card type @Enchantment@.
enchantmentType :: ObjectTypes
enchantmentType = mempty { enchantmentSubtypes = Just mempty }

-- | Card type @Enchantment - 'Aura'@.
auraType :: ObjectTypes
auraType = mempty { enchantmentSubtypes = Just (Set.singleton Aura) }

-- | Card type @Enchantment - 'Curse'@.
curseType :: ObjectTypes
curseType = mempty { enchantmentSubtypes = Just (Set.singleton Curse) }

-- | Card type @Instant@.
instantType :: ObjectTypes
instantType = instantTypes []

-- | Card type @Instant@ with the specified spell types.
instantTypes :: [SpellSubtype] -> ObjectTypes
instantTypes tys = mempty { instantSubtypes = Just (Set.fromList tys) }

-- | Card type @Land@.
landType :: ObjectTypes
landType = landTypes []

-- | Card type @Land@ with the specified land types.
landTypes :: [LandSubtype] -> ObjectTypes
landTypes tys = mempty { landSubtypes = Just (Set.fromList tys) }

-- | Card type @Planeswalker@.
planeswalkerType :: ObjectTypes
planeswalkerType = mempty { planeswalkerSubtypes = Just mempty }

-- | Card type @Planeswalker@ with the specified planeswalker type.
planeswalkerWithType :: PlaneswalkerSubtype -> ObjectTypes
planeswalkerWithType tys = mempty { planeswalkerSubtypes = Just (Set.singleton tys) }

-- | Card type @Sorcery@.
sorceryType :: ObjectTypes
sorceryType = mempty { sorcerySubtypes = Just mempty }

-- | Card type @Sorcery@ with the specified spell types.
sorceryTypes :: [SpellSubtype] -> ObjectTypes
sorceryTypes tys = mempty { sorcerySubtypes = Just (Set.fromList tys) }