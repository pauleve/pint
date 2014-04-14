
open OUnit2

open Ph_types

open TestCommon

let test_phreach ?ctx:(ctx="") model reach opts expected =
	let test_output cout =
		assert_equal (cs_next_word cout) (string_of_ternary expected)
	in

	let args = opts @ ["-i"; model]
		@ (if ctx = "" then [] else ["--initial-context"; ctx])
		@ reach
	in

	let test ctxt =
		assert_command ~ctxt:ctxt ~foutput:test_output "../ph-reach" args
	in
	test_case ~length:OUnitTest.Short test

let tests =
	"TestPhReach" >:::
	[
		"CoopPrioMetazoan" >: 
			test_phreach "models/metazoan-3prio-flattening.ph"
						~ctx:"a 1, f 1, c 0" 
						["a"; "0"]
						["--coop-priority"]
						True;
		"CoopPrioTCR1" >:
			test_phreach "models/tcrsig94-TCS.ph"
						~ctx:"cd45 1, cd28 1, tcrlig 1"
						["ap1"; "1"]
						["--coop-priority"]
						True;
		"CoopPrioInconc1" >:
			test_phreach "models/1-02.ph" ["bp"; "1"; "z"; "1"]
						["--coop-priority"]
						Inconc;
	]


