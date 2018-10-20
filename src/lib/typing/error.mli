open DLG.Ast
open Utils.Position

(* A variant containing the data  *)
type type_error_reason =
  | ReasonUnbound
  | ReasonIncompatibleDeclaration
  | ReasonNotANumber
  | ReasonNotAValue
  | ReasonNotAFunction
  | ReasonConditionWrongType
  | ReasonBranchesDifferentType
  | ReasonNoProperty
  | ReasonInvalidTypeCast
  | ReasonInvalidTypeOp
  | ReasonInvalidValuePattern
  | ReasonInvalidBindingPattern
  | ReasonAlreadyBound
  | ReasonInvalidCall
  | ReasonNotVoid
  | ReasonNoProduceVoid

(* A payload for a type error exception*)
type type_error = {
  position: position option;  (* the position in the lexbuf of the token *)
  reason: type_error_reason;  (* the reason for the error *)
  expected: type_container list;
  given: type_container option;
}

(* An exception raised whenever there's a typing error *)
exception Type_error of type_error

(* gets the reason of a type error *)
val reason : type_error -> type_error_reason
(* gets the expected types of a type error *)
val expected : type_error -> type_container list
(* gets the given type of a type error *)
val given : type_error -> type_container option

(* makes a type error *)
val make_type_error : type_error_reason -> type_container option -> type_container list -> type_error
(* makes a type error w/ location data *)
val make_located_type_error : type_error_reason -> 'a located -> type_container option -> type_container list -> exn
(* raise a type error *)
val raise_type_error : type_error_reason -> type_container option -> type_container list -> unit
(* raise a type error w/ location data *)
val raise_located_type_error : type_error_reason -> 'a located -> type_container option -> type_container list -> unit
(* get a result, checks if it is okay, else locate the exception and throw  *)
val check_for_type_error : ('a, type_error) result -> 'b located -> 'a

(* prints a type error *)
val print_type_error_at : type_error -> in_channel option -> unit
