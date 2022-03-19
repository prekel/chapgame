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
    (((0 -64)) ((0 11) (1 16) (2 0.25)) ((0 11) (1 16) (2 0.25)) ((0 -4))) |}];
  print_s
    [%sexp
      (Sequence.map t ~f:(SF.PolynomialEquation.roots ~eps:1e-5) : float list Sequence.t)];
  [%expect{|
    (() (-63.304951667785645 -0.69504833221435547)
     (-63.304951667785645 -0.69504833221435547) ()) |}]
;;
