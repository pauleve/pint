
open OUnit2

open PintTypes
open Ph_types

open TestCommon

let exe = "../bin/pint-reach"

let test_exe ?length:(length=OUnitTest.Short)
		?ctx:(ctx="") ?opts:(opts=[]) model reach expected =
	let expected = string_of_ternary expected
	in
	let foutput cout =
		assert_equal expected (cs_next_word cout)
			~printer:(fun a -> a)
	in

	let args = opts @ ["-i"; model]
		@ (if ctx = "" then [] else ["--initial-context"; ctx])
		@ reach
	in

	let test ctxt =
		assert_command ~ctxt ~foutput exe args
	in
	test_case ~length test

let tests =
	"TestPintReach" >::: [
		"Intermediary1" >:
			test_exe "models/interm1.an"
						["d=2"]
						Inconc;
		"CoopPrioMetazoan" >:
			test_exe "models/metazoan-3prio-flattening.an"
						~ctx:"a=1, f=1, c=0"
						["a=0"]
						True;
		"CoopPrioTCR1" >:
			test_exe "models/tcrsig94-TCS.an"
						~ctx:"cd45=1, cd28=1, tcrlig=1"
						["ap1=1"]
						True;
		(*
		"CoopPrioInconc1" >:
			test_exe "models/1-02.an"
						["bp"; "1"; "z"; "1"]
						Inconc; *)
	]


