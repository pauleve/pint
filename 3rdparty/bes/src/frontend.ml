(* $Id: promela.mli 2996 2012-03-14 09:58:12Z weissmam $

Copyright (c) 2011 - 2012 Technische Universitaet Muenchen
Copyright (c) 2011 Alexander Ostrovsky <ostrovsk@in.tum.de
Copyright (c) 2011 - 2012 Markus W. Weissmann <markus.weissmann@in.tum.de>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Apple Inc. nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)

let _ = Bes.set_verbose_mode false

let _ = Bes.set_result_verification true

let _ = 
   try
      if Sys.argv.(1) <> "-aAuto" then
         let algorithm = match Sys.argv.(1) with
            | "-aQmc" -> Bes.Qmc 
            | "-aQmcCyclicCore" -> Bes.Qmc_CyclicCore
            | "-aQmcSimpleHeuristicSortOnce" -> Bes.Qmc_SimpleHeuristic_SortOnce
            | "-aQmcSimpleHeuristicSortEachTime" -> Bes.Qmc_SimpleHeuristic_SortEachTime
            | "-aQmcSimpleHeuristicAdvancedHeuristic" -> Bes.Qmc_SimpleHeuristic_AdvancedHeuristic
            | "-aQmcPetricksMethod" -> Bes.Qmc_PetricksMethod
            | "-aInPlaceHeuristic" -> Bes.In_Place_Heuristic
            | _ -> raise (Invalid_argument "Wrong algorithm")
         in
         let file_name = Sys.argv.(2) in
         let expr = Bes.load_from_file file_name in
         let min_expr = Bes.optimize algorithm expr in
         Bes.print_dnf_expression min_expr
      else
         let file_name = Sys.argv.(2) in
         let expr = Bes.load_from_file file_name in
         let (min_expr, exact) = Bes.auto_optimize expr in
         Bes.print_dnf_expression min_expr;
         print_endline ("Exact optimization: " ^ (string_of_bool exact))
   with Invalid_argument x ->
      print_endline "Usage ./optimizer.native -aX [filename] \n X is the algorithm.";
      print_endline ("The following algorithms are supported -aQmc, aQmcCyclicCore, -aQmcSimpleHeuristicSortOnce, "
                 ^ "-aQmcSimpleHeuristicEachTime, -aQmcSimpleHeuristicAdvancedHeuristic, -aAuto "
                 ^ "and -aQmcPetricksMethod. There is also the -aInPlaceHeuristic option. This is fast, " 
                 ^ "but not well tested.");
      print_endline "Example: ./optimizer.native -aQmcPetricksMethod file.data"

