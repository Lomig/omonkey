open! Core

module Identifier : sig
  type t [@@deriving show]

  val of_string : string -> t
end = struct
  type t = string [@@deriving show]

  let of_string t = t
end

type expression =
  | False
  | Identifier of Identifier.t
[@@deriving show]

type statement =
  | Error of string
  | Binding of identifier * expression
  | Return of expression
[@@deriving show]

and identifier = Identifier of Identifier.t [@@deriving show]
