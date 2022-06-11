open! Core
open Bonsai.Private

(** All performance entries are forwarded to the debugger/profiler, whether or
    not they were generated by Bonsai. Thus, when possible, we extract the
    Bonsai node that the entry corresponds to, but otherwise we just pass along
    the label unmodified. *)
module Entry : sig
  type t =
    { label : [ `Bonsai of Bonsai.Private.Node_path.t | `Other of string ]
    ; entry_type : string
    ; start_time : float
    ; duration : float
    }
  [@@deriving sexp, bin_io]
end

(** The debugger/profiler receives both performance entries as well as
    a description of the Bonsai graph. *)
module Message : sig
  type t =
    | Graph_info of Graph_info.t
    | Performance_measure of Entry.t
  [@@deriving sexp, bin_io]
end