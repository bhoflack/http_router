open OUnit
open Http_router.Route
open Http_router.Route.Infix

let hello_world _body _req _ctx = Some (Cohttp_async.Server.respond_with_string ~code:`OK "hello world with context")

module OptionRouter = Http_router.Make (Core_kernel.Option)

let routes = OptionRouter.create [
    get / "hello" / "world", hello_world
  ]

let routing _ =
  match OptionRouter.find_handler routes `GET (Uri.of_string "/hello/world") with
  | Some (handler, ctx) ->
    assert_bool "Should return the hello world handler" (handler == hello_world)
  | None -> assert_failure "Should return a handler"

let test_fixtures =
  "Test monadic http_router features" >:::
  [ "test routing with constants" >:: routing
  ]

let _ = run_test_tt_main test_fixtures
