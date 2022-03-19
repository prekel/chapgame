open Core
open Chapgame
module SC = Scene.Make (Float)
module SF = Solver.MakeSolver (Float)

let%expect_test "" =
  let a = SC.Model.e () in
  let _elt, els = Map.min_elt_exn a in
  let t = SC.Scene.t els in
  print_s [%sexp (t : SC.Solver.Polynomial.t Sequence.t)];
  [%expect
    {|
    (
    ((0 -64) (1 0) (2 0)) 
    ((0 11) (1 16) (2 0.25)) 
    ((0 11) (1 16) (2 0.25))
    ((0 -4) (1 0) (2 0))
    ) |}];
  print_s
    [%sexp
      (Sequence.map t ~f:(SF.PolynomialEquation.roots ~eps:1e-5) : float list Sequence.t)];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Float.sign_exn of NAN" -NAN)
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Chapgame__Solver.MakeSolver.BinarySearch.search.increase_rec in file "common/solver.ml", line 240, characters 42-52
  Called from Base__List.rev_filter_map.loop in file "src/list.ml", line 944, characters 13-17
  Called from Base__List.filter_map in file "src/list.ml" (inlined), line 951, characters 26-47
  Called from Chapgame__Solver.MakeSolver.PolynomialEquation.roots in file "common/solver.ml", line 280, characters 8-170
  Called from Base__Sequence.map.(fun) in file "src/sequence.ml", line 194, characters 33-36
  Called from Base__Sequence.to_list.to_list in file "src/sequence.ml", line 141, characters 12-18
  Called from Base__Sequence.sexp_of_t in file "src/sequence.ml" (inlined), line 149, characters 51-62
  Called from Chapgame_test__Scene_test.(fun) in file "test/scene_test.ml", line 17, characters 67-88
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
;;
