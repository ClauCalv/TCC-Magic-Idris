module Card

record Card where
    constructor MkCard
    name : String
    cost : Maybe ManaCost
    type : List CardType

playCard :
