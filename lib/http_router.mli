type t

module Context : sig
  type t

  val empty : t
  val create : string array -> t
  val concat : t -> t -> t

  val get_all : t -> string list
  val nth : t -> int -> string option
end

module PathPart : sig
  type t
  val matches : string -> t -> bool * Context.t

  val str : string -> t
  val regex : Re.t -> t
end

module Route : sig
  type t

  val get : t
  val post : t
  val put : t
  val delete : t
  val patch : t

  val meth : t -> Cohttp.Code.meth
  val parts : t -> PathPart.t list

  module Infix : sig
    val (/)  : t -> string -> t
    val (/^) : t -> Re.t -> t
  end
end

module Make (M: Core_kernel.Monad.S) : sig
  type t
  type handler = Cohttp_async.Body.t
                 -> Cohttp.Request.t
                 -> Context.t
                 -> Cohttp_async.Server.response Async_kernel.Deferred.t M.t

  val create : (Route.t * handler) list -> t
  val run : t
        -> Cohttp_async.Body.t
        -> Cohttp.Request.t
        -> Cohttp_async.Server.response Async_kernel.Deferred.t M.t option

  val find_handler : t
        -> Cohttp.Code.meth
        -> Uri.t
        -> (handler * Context.t) option
end

type handler = Cohttp_async.Body.t
               -> Cohttp.Request.t
               -> Context.t
               -> Cohttp_async.Server.response Async_kernel.Deferred.t

val create : (Route.t * handler) list -> t
val run : t 
      -> Cohttp_async.Body.t
      -> Cohttp.Request.t
      -> Cohttp_async.Server.response Async_kernel.Deferred.t option

val find_handler : t
      -> Cohttp.Code.meth
      -> Uri.t
      -> (handler * Context.t) option
