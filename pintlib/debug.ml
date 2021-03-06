(*
Copyright or © or Copr. Loïc Paulevé, Morgan Magnin, Olivier Roux (2010)

loic.pauleve@irccyn.ec-nantes.fr
morgan.magnin@irccyn.ec-nantes.fr
olivier.roux@irccyn.ec-nantes.fr

This software is a computer program whose purpose is to provide Process
Hitting related tools.

This software is governed by the CeCILL license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the
CeCILL license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author, the holder of the
economic rights, and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL license and that you accept its terms.
*)

let dodebug = ref false;;
let debuglevel = ref 1;;

let dbglevel () = if !dodebug then !debuglevel else 0

let dbg_noendl ?level:(level = 5) msg =
	if !dodebug && (level <= !debuglevel) then (prerr_string msg; flush stderr) else ();;
let dbg ?level:(level = 5) msg = dbg_noendl ~level:level (msg^"\n");;

let tic = Unix.gettimeofday;;
let toc ?level:(level=1) ?label:(label = "") t =
	if !dodebug then
	let t = tic () -. t
	in
	dbg ~level ((if label = "" then "execution time" else label)^": "
				^string_of_float t^"s")
;;



