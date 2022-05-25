(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SStrLit of string
  | SNewArr of prim * sexpr
  | SId of string
  | SNot of sexpr
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SArrAssign of string * sexpr * sexpr
  | SArrAccess of string * sexpr
  | SNoteAssign of string * sexpr
  | SPhraseAssign of string
  | SPhraseAdd of string * sexpr * sexpr
  | SSongMeasure of string * sexpr * sexpr
  | SSongAssign of string
  | SCall of string * sexpr list
  | SSongPlay of string
 

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIfElse of sexpr * sstmt * sstmt
  | SIf of sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SFloatLit(f) -> string_of_float f
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStrLit(l) -> "\"" ^ l ^ "\""
  | SNewArr(t,l) -> "new Array<" ^ string_of_prim t ^ "> [" ^ string_of_sexpr l ^ "]"
  | SId(s) -> s
  | SNot(e) -> "not " ^ string_of_sexpr e
  | SBinop(e1, o, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SArrAssign(s, e1, e2) -> s ^ "[" ^ string_of_sexpr e1 ^ "]" ^
                            " = " ^ string_of_sexpr e2
  | SArrAccess(s,e) -> s ^ "[" ^ string_of_sexpr e ^ "]"
  | SNoteAssign(id, pitch) -> id ^ " = " ^ "Note(" ^ string_of_sexpr pitch ^ ")"
  | SPhraseAssign(id) -> id ^ " = Phrase()"
  | SPhraseAdd(id, idx, note) -> id ^ ".add(" ^ string_of_sexpr idx ^ ", " ^ string_of_sexpr note ^ ")"
  | SSongMeasure(id, idx, phrase) -> id ^ ".measure(" ^ string_of_sexpr idx ^ ", " ^ string_of_sexpr phrase ^ ")"
  | SSongAssign(id) -> id ^ " = Song()"
  | SSongPlay(id) -> id ^ ".play()"
  | SCall(f, el) ->
            f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      ) ^ ")"

  and string_of_arr l =
     if List.length l = 0 then "" else
     if List.length l > 1 then string_of_sexpr (List.hd l) ^ "," ^ string_of_arr (List.tl l) else string_of_sexpr (List.hd l)

let rec string_of_sstmt = function
    SBlock(stmts) -> "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIfElse(e, s1, s2) ->  string_of_sif_else(e,s1,s2)
  | SIf(e, s) -> string_of_sif(e,s)
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2 ^ "; " ^ string_of_sexpr e3 ^ "; " ^ string_of_sstmt s
and string_of_sif_else (e, s1, s2)=
    let part1 =
        match s1 with
            SExpr(expr)-> string_of_sstmt s1
            | SBlock(stmts)->string_of_sstmt(s1)
            | _ -> failwith "Error"
    in
    let part2 =
        match s2 with
            SExpr(expr)-> "\n" ^ string_of_sstmt s2
            | SBlock(stmts)->"\n" ^ string_of_sstmt(s2)
            | SIfElse(e2,s3,s4)->string_of_sstmt(s2)
            | SIf(e2,s) ->string_of_sstmt(s2)
            | _ -> failwith "Error"
    in
    "if (" ^ string_of_sexpr e ^ ")\n" ^
        part1 ^  "else " ^ part2
and string_of_sif (e, s)=
    let part1 =
        match s with
            SExpr(expr)-> string_of_sstmt s
            | SBlock(stmts)->string_of_sstmt(s)
            | _ -> failwith "Error"
    in
    "if (" ^ string_of_sexpr e ^ ")\n" ^ part1

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
