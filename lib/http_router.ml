open Core.Std

module Context = struct
  type t = string list

  let empty = []
  let create vs = Array.to_list vs
  let concat fs snd = List.concat [fs; snd]

  let get_all t = t
  let nth = List.nth
end

module PathPart = struct
  type t =
    | StringPathPart of string
    | RegexPathPart  of Re.re

  let matches p = function
    | StringPathPart s -> (s = p, [])
    | RegexPathPart  r ->
        try
          let ctx = Re.exec r p
                    |> Re.get_all
                    |> Context.create
          in (true, ctx)
        with Not_found -> (false, [])

  let str s = StringPathPart s
  let regex p = RegexPathPart (Re.compile p)
end

module Route = struct
  type t = {
    meth: Cohttp.Code.meth
  ; parts: PathPart.t list
  }

  let get  = { meth = `GET; parts = [] }
  let post = { meth = `POST; parts = [] }
  let put  = { meth = `PUT; parts = [] }
  let delete = { meth = `DELETE; parts = [] }
  let patch = { meth = `PATCH; parts = [] }

  let append_part t p =
     { t with parts = t.parts @ [p] }

  let meth t = t.meth
  let parts t = t.parts

  module Infix = struct
    let (/) r s = append_part r (PathPart.str s)
    let (/^) r re = append_part r (PathPart.regex re)
  end
end

module Monadic(M: Core_kernel.Monad.Basic) = struct
  type handler = Cohttp_async.Body.t
                 -> Cohttp.Request.t
                 -> Context.t
                 -> Cohttp_async.Server.response Async_kernel.Deferred.t M.t

  type t = (Route.t * handler) list

  let create t = t

  let rec matches_all_parts hs ps ctx = match (hs, ps) with
    | ([], _) -> (true, ctx)
    | ((h :: hs), (p :: ps)) -> 
      let (matches, part_context) = PathPart.matches p h in
      if matches
      then matches_all_parts hs ps (Context.concat ctx part_context)
      else (false, [])
    | _ -> (false, [])
 
  let find_handler handlers meth uri = 
    let path = Uri.path uri in
    let parts = Str.split (Str.regexp "/") path in
    let rec go handlers meth parts = 
      match (handlers, parts) with
      | ([], _)                                                          -> None
      | ((r, _) :: hs, ps)  when (Route.meth r) <> meth                  -> 
        go hs meth ps
      | ((r, _) :: hs, ps) when (List.length (Route.parts r)) <> (List.length ps) ->
        go hs meth ps
      | ((r, h) :: hs, ps) ->
        let (matches, ctx) = matches_all_parts (Route.parts r) ps [] in
        if matches
        then Some (h, ctx)
        else go hs meth ps
    in go handlers meth parts
   
  let run t body req =
    let open Option.Monad_infix in
    find_handler t (Cohttp.Request.meth req) (Cohttp.Request.uri req) >>| fun (h, ctx) -> h body req ctx
end

type handler = Cohttp_async.Body.t
               -> Cohttp.Request.t
               -> Context.t
               -> Cohttp_async.Server.response Async_kernel.Deferred.t

type t = (Route.t * handler) list

let create t = t

let rec matches_all_parts hs ps ctx = match (hs, ps) with
  | ([], _) -> (true, ctx)
  | ((h :: hs), (p :: ps)) -> 
    let (matches, part_context) = PathPart.matches p h in
    if matches
    then matches_all_parts hs ps (Context.concat ctx part_context)
    else (false, [])
  | _ -> (false, [])
 
let find_handler handlers meth uri = 
  let path = Uri.path uri in
  let parts = Str.split (Str.regexp "/") path in
  let rec go handlers meth parts = 
    match (handlers, parts) with
    | ([], _)                                                          -> None
    | ((r, _) :: hs, ps)  when (Route.meth r) <> meth                  -> 
      go hs meth ps
    | ((r, _) :: hs, ps) when (List.length (Route.parts r)) <> (List.length ps) ->
      go hs meth ps
    | ((r, h) :: hs, ps) ->
      let (matches, ctx) = matches_all_parts (Route.parts r) ps [] in
      if matches
      then Some (h, ctx)
      else go hs meth ps
  in go handlers meth parts
   
let run t body req =
  let open Option.Monad_infix in
  find_handler t (Cohttp.Request.meth req) (Cohttp.Request.uri req) >>| fun (h, ctx) ->
  h body req ctx
