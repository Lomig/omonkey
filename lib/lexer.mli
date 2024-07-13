type t

val pp : Format.formatter -> t -> unit
val make : string -> t
val next_token : t -> Token.t * t
