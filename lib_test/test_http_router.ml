open OUnit
open Http_router
open Http_router.Route
open Http_router.Route.Infix

let hello_world _body _req _ctx = Cohttp_async.Server.respond_with_string ~code:`OK "hello world"

let hello_name _body _req ctx = match Context.get_all ctx with
  | [name] -> Cohttp_async.Server.respond_with_string ~code:`OK ("Hi, " ^ name)
  | _ -> Cohttp_async.Server.respond_with_string ~code:`Internal_server_error "error"

let add_booking _body _req _ctx = Cohttp_async.Server.respond_with_string "ok"

let routes = create [
    get / "hello" / "world", hello_world
  ; get / "hello" /^ (Re_perl.re "^\\w{3,6}$"), hello_name
  ; post / "users" /^ (Re_perl.re "^\\w+$") / "books" /^ (Re_perl.re "^\\d+$"), add_booking
  ]

let routing _ =
  match find_handler routes `GET (Uri.of_string "/hello/world") with
  | None -> assert_failure "Should return a handler"
  | Some (handler, ctx) -> 
    assert_bool "Should return the hello world handler" (handler == hello_world);
    assert_equal (Context.get_all ctx) []

let regex_routing _ =
  match find_handler routes `GET (Uri.of_string "/hello/brecht") with
  | None -> assert_failure "Should return a handler"
  | Some (handler, ctx) ->
    assert_bool "Should return hello_name" (handler == hello_name);
    assert_equal (Context.get_all ctx) ["brecht"]

let not_matching_routing _ =
  let handler = find_handler routes `GET (Uri.of_string "/hello/blaaaaaat") in
  assert_bool "Should not find a handler" (Core_kernel.Option.is_none handler)

let post_routing _ =
  match find_handler routes `POST (Uri.of_string "/users/krusty/books/1") with
  | None -> assert_failure "Should return a handler"
  | Some (handler, ctx) ->
    assert_bool "Should return add_booking" (handler == add_booking);
    assert_equal (Context.get_all ctx) ["krusty"; "1"]

let test_fixtures =
  "Test http_router features" >:::
  [ "test routing with a constants" >:: routing
  ; "test routing with a regex"     >:: regex_routing
  ; "test no handler found"         >:: not_matching_routing
  ; "test routing a post request"   >:: post_routing
  ]

let _ = run_test_tt_main test_fixtures
