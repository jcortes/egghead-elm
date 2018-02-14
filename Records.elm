import Html exposing (text)

type alias Dog =
  { name : String
  , breed : String
  , sterilized : Bool }

type alias Person =
  { name : String
  , age : Int
  , occupation : String
  , salary : Float
  , dog : Dog }

fido : Dog
-- fido =
--   { name = "Fido"
--   , breed = "Husky"
--   , sterilized = False }
fido = Dog "Fido" "Husky" False

joe : Person
joe =
  { name = "Joe"
  , age = 21
  , occupation = "Analyst"
  , dog = fido
  , salary = 10000 }

promote : Person -> Person
promote p =
  { p | occupation = "Manager"
    , salary = p.salary * 1.2 }

sterilizePet : Person -> Person
sterilizePet p =
  let
    dog = p.dog
  in
    { p | dog = { dog | sterilized = True } }

describe : Dog -> String
describe { name, breed } =
  name ++ " is a " ++ breed

main =
  joe |> promote |> sterilizePet |> toString |> text