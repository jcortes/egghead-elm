module Email exposing (Address, validate, toString)

type Address = Address String

validate : String -> Result String Address
validate s =
  if String.contains "@" s
  then Result.Ok (Address s)
  else Result.Err "not a valid email address"

toString : Address -> String
toString (Address s) = s