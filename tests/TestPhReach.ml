
open OUnit2

open PintTypes
open Ph_types

open TestCommon

let phreach_exe = "../bin/ph-reach"

let test_phreach ?length:(length=OUnitTest.Short)
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
		assert_command ~ctxt ~foutput phreach_exe args
	in
	test_case ~length test

let tests =
	"TestPhReach" >::: [
		"Intermediary1" >:
			test_phreach "models/interm1.ph" 
						["d";"2"]
						Inconc;
		"CoopPrioMetazoan" >: 
			test_phreach "models/metazoan-3prio-flattening.ph"
						~ctx:"a 1, f 1, c 0" 
						["a"; "0"]
						~opts:["--coop-priority"]
						True;
		"CoopPrioTCR1" >:
			test_phreach "models/tcrsig94-TCS.ph"
						~ctx:"cd45 1, cd28 1, tcrlig 1"
						["ap1"; "1"]
						~opts:["--coop-priority"]
						True;
		"CoopPrioInconc1" >:
			test_phreach "models/1-02.ph" ["bp"; "1"; "z"; "1"]
						~opts:["--coop-priority"]
						Inconc;
	]


