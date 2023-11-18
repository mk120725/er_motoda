(*
 * ratio.ml : 有理数
 *
 *            Copyright (C) 2008-2020 Makoto Hiroi
 *)

(* 有理数の定義 *)
type ratio = Rat of int * int

(* 最大公約数 *)
let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

(* データの生成 *)
let make_ratio a b =
  if b = 0 then raise Division_by_zero
  else
    let f = if b < 0 then -1 else 1 and
        z = gcd (abs a) (abs b) in
    Rat ((f * a / z), (f * b / z))

(* 算術演算子の定義*)
let ( +/ ) (Rat (x1, y1)) (Rat (x2, y2)) =
  make_ratio (x1 * y2 + x2 * y1) (y1 * y2)

let ( -/ ) (Rat (x1, y1)) (Rat (x2, y2)) =
  make_ratio (x1 * y2 - x2 * y1) (y1 * y2)

let ( */ ) (Rat (x1, y1)) (Rat (x2, y2)) =
  make_ratio (x1 * x2) (y1 * y2)

let ( // ) (Rat (x1, y1)) (Rat (x2, y2)) =
  make_ratio (x1 * y2) (x2 * y1)

(* 有理数の比較*)
let compare_ratio (Rat (x1, y1)) (Rat (x2, y2)) =
  x1 * y2 - x2 * y1

(* 比較演算子の定義 *)
let ( =/ ) a b  = compare_ratio a b = 0
let ( <>/ ) a b = compare_ratio a b <> 0
let ( </ ) a b  = compare_ratio a b < 0
let ( >/ ) a b  = compare_ratio a b > 0
let ( <=/ ) a b = compare_ratio a b <= 0
let ( >=/ ) a b = compare_ratio a b >= 0

(* 整数の判定 *)
let is_integer (Rat (_, y)) = y = 1

(* 整数に変換 *)
let int_of_ratio (Rat (x, y)) = x / y

(* 浮動小数点数に変換 *)
let float_of_ratio (Rat (x, y)) = (float_of_int x) /. (float_of_int y)

(* 文字列に変換 *)
let string_of_ratio (Rat (x, y)) =
  if y = 1 then string_of_int x
  else (string_of_int x) ^ "/" ^ (string_of_int y)

(* 表示 *)
let print_ratio n = print_string (string_of_ratio n)
