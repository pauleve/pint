(**

T-Cell Receptor Signalling Pathway (94 components)

Bibliography:
* Saez-Rodriguez, J.; Simeoni, L.; Lindquist, J. A.; Hemenway, R.; Bommhardt, U.; Arndt, B.; Haus,
  U.-U.; Weismantel, R.; Gilles, E. D.; Klamt, S. & Schraven, B. A Logical Model Provides Insights
  into T Cell Receptor Signaling PLoS Comput Biol, Public Library of Science, 2007, 3

**)

process cd28 1
process itk 1
process akap79 1
process pkb 1
process jnk 1
process bclxl 1
process gsk3 1
process nfat 1
process pten 1
process creb 1
process ca 1
process dag 1
process cabin1 1
process fyn 1
process mekk1 1
process calcin 1
process calpr1 1
process sh3bp2 1
process ship1 1
process bad 1
process cre 1
process x 1
process rac1p1 1
process rac1p2 1
process plcgb 1
process card11a 1
process mlk3 1
process cyc1 1
process pdk1 1
process rasgrp 1
process fos 1
process pkcth 1
process abl 1
process tcrlig 1
process rlk 1
process mek 1
process ikb 1
process p38 1
process grb2 1
process sre 1
process camk2 1
process lckr 1
process camk4 1
process p70s 1
process gads 1
process lckp2 1
process lckp1 1
process plcga 1
process sos 1
process ccblp2 1
process slp76 1
process ccblp1 1
process ikkg 1
process bcat 1
process shp2 1
process bcl10 1
process rsk 1
process vav1 1
process vav3 1
process ap1 1
process jun 1
process gap 1
process ip3 1
process rac1r 1
process gadd45 1
process nfkb 1
process cam 1
process mkk4 1
process erk 1
process raf 1
process cblb 1
process cd45 1
process tcrb 1
process ras 1
process tcrp 1
process p27k 1
process cdc42 1
process pip3 1
process gab2 1
process pag 1
process card11 1
process ccblr 1
process dgk 1
process zap70 1
process lat 1
process shp1 1
process fkhr 1
process cd4 1
process p21c 1
process csk 1
process pi3k 1
process hpk1 1
process ikkab 1
process malt1 1

COOPERATIVITY([slp76;pip3;zap70] -> itk 0 1, [[1;1;1]])
COOPERATIVITY([slp76;pip3;zap70] -> itk 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1]])
pdk1 1 -> pkb 0 1
COOPERATIVITY([mekk1;mkk4] -> jnk 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([mekk1;mkk4] -> jnk 1 0, [[0;0]])
bad 1 -> bclxl 1 0
pkb 1 -> gsk3 1 0
calcin 1 -> nfat 0 1
rsk 1 -> creb 0 1
ip3 1 -> ca 0 1
COOPERATIVITY([dgk;plcga] -> dag 0 1, [[0;1]])
COOPERATIVITY([dgk;plcga] -> dag 1 0, [[0;0];[1;0];[1;1]])
camk4 1 -> cabin1 1 0
COOPERATIVITY([cd45;lckp1;tcrb;lckr] -> fyn 0 1, [[1;1;0;0];[1;1;1;0];[1;1;0;1];[0;0;1;1];[1;0;1;1];[0;1;1;1];[1;1;1;1]])
COOPERATIVITY([cd45;lckp1;tcrb;lckr] -> fyn 1 0, [[0;0;0;0];[1;0;0;0];[0;1;0;0];[0;0;1;0];[1;0;1;0];[0;1;1;0];[0;0;0;1];[1;0;0;1];[0;1;0;1]])
COOPERATIVITY([rac1p2;hpk1;cdc42] -> mekk1 0 1, [[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([rac1p2;hpk1;cdc42] -> mekk1 1 0, [[0;0;0]])
COOPERATIVITY([cam;akap79;cabin1;calpr1] -> calcin 0 1, [[1;0;0;0]])
COOPERATIVITY([cam;akap79;cabin1;calpr1] -> calcin 1 0, [[0;0;0;0];[0;1;0;0];[1;1;0;0];[0;0;1;0];[1;0;1;0];[0;1;1;0];[1;1;1;0];[0;0;0;1];[1;0;0;1];[0;1;0;1];[1;1;0;1];[0;0;1;1];[1;0;1;1];[0;1;1;1];[1;1;1;1]])
COOPERATIVITY([lat;zap70] -> sh3bp2 0 1, [[1;1]])
COOPERATIVITY([lat;zap70] -> sh3bp2 1 0, [[0;0];[1;0];[0;1]])
pkb 1 -> bad 1 0
creb 1 -> cre 0 1
cd28 1 -> x 0 1
COOPERATIVITY([vav1;rac1r] -> rac1p1 0 1, [[1;1]])
COOPERATIVITY([vav1;rac1r] -> rac1p1 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([vav3;rac1r] -> rac1p2 0 1, [[1;1]])
COOPERATIVITY([vav3;rac1r] -> rac1p2 1 0, [[0;0];[1;0];[0;1]])
lat 1 -> plcgb 0 1
COOPERATIVITY([card11;bcl10;malt1] -> card11a 0 1, [[1;1;1]])
COOPERATIVITY([card11;bcl10;malt1] -> card11a 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1]])
COOPERATIVITY([rac1p1;hpk1] -> mlk3 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([rac1p1;hpk1] -> mlk3 1 0, [[0;0]])
gsk3 1 -> cyc1 1 0
pip3 1 -> pdk1 0 1
dag 1 -> rasgrp 0 1
erk 1 -> fos 0 1
COOPERATIVITY([pdk1;vav1;dag] -> pkcth 0 1, [[1;1;1]])
COOPERATIVITY([pdk1;vav1;dag] -> pkcth 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1]])
COOPERATIVITY([lckp1;fyn] -> abl 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([lckp1;fyn] -> abl 1 0, [[0;0]])
lckp1 1 -> rlk 0 1
raf 1 -> mek 0 1
ikkab 1 -> ikb 1 0
COOPERATIVITY([mekk1;gadd45;zap70] -> p38 0 1, [[1;0;0];[1;1;0];[0;0;1];[1;0;1];[1;1;1]])
COOPERATIVITY([mekk1;gadd45;zap70] -> p38 1 0, [[0;0;0];[0;1;0];[0;1;1]])
lat 1 -> grb2 0 1
COOPERATIVITY([cdc42;rac1p2] -> sre 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([cdc42;rac1p2] -> sre 1 0, [[0;0]])
cam 1 -> camk2 0 1
cam 1 -> camk4 0 1
pdk1 1 -> p70s 0 1
lat 1 -> gads 0 1
COOPERATIVITY([tcrb;lckr] -> lckp2 0 1, [[1;1]])
COOPERATIVITY([tcrb;lckr] -> lckp2 1 0, [[0;0];[1;0];[0;1]])

COOPERATIVITY(
		    [cd45;cd4;lckr] in [[1;1;1]]
		and [shp1;csk] in [[0;0]],
		lckp1, 1, 0)
(*
COOPERATIVITY([cd45;cd4;lckr;shp1;csk] -> lckp1 0 1, [[1;1;1;0;0]])
COOPERATIVITY([cd45;cd4;lckr;shp1;csk] -> lckp1 1 0, [[0;0;0;0;0];[1;0;0;0;0];[0;1;0;0;0];[1;1;0;0;0];[0;0;1;0;0];[1;0;1;0;0];[0;1;1;0;0];[0;0;0;1;0];[1;0;0;1;0];[0;1;0;1;0];[1;1;0;1;0];[0;0;1;1;0];[1;0;1;1;0];[0;1;1;1;0];[1;1;1;1;0];[0;0;0;0;1];[1;0;0;0;1];[0;1;0;0;1];[1;1;0;0;1];[0;0;1;0;1];[1;0;1;0;1];[0;1;1;0;1];[1;1;1;0;1];[0;0;0;1;1];[1;0;0;1;1];[0;1;0;1;1];[1;1;0;1;1];[0;0;1;1;1];[1;0;1;1;1];[0;1;1;1;1];[1;1;1;1;1]])
*)

COOPERATIVITY(
	    [plcgb;itk;rlk] in [[1;0;1];[1;1;0];[1;1;1]]
	and [vav1;zap70;ccblp2;slp76] in [[1;1;0;1]], 
	plcga, 1, 0)
(*
COOPERATIVITY([plcgb;itk;rlk;vav1;zap70;ccblp2;slp76] -> plcga 0 1, [[1;1;0;1;1;0;1];[1;0;1;1;1;0;1];[1;1;1;1;1;0;1]])
COOPERATIVITY([plcgb;itk;rlk;vav1;zap70;ccblp2;slp76] -> plcga 1 0, [[0;0;0;0;0;0;0];[1;0;0;0;0;0;0];[0;1;0;0;0;0;0];[1;1;0;0;0;0;0];[0;0;1;0;0;0;0];[1;0;1;0;0;0;0];[0;1;1;0;0;0;0];[1;1;1;0;0;0;0];[0;0;0;1;0;0;0];[1;0;0;1;0;0;0];[0;1;0;1;0;0;0];[1;1;0;1;0;0;0];[0;0;1;1;0;0;0];[1;0;1;1;0;0;0];[0;1;1;1;0;0;0];[1;1;1;1;0;0;0];[0;0;0;0;1;0;0];[1;0;0;0;1;0;0];[0;1;0;0;1;0;0];[1;1;0;0;1;0;0];[0;0;1;0;1;0;0];[1;0;1;0;1;0;0];[0;1;1;0;1;0;0];[1;1;1;0;1;0;0];[0;0;0;1;1;0;0];[1;0;0;1;1;0;0];[0;1;0;1;1;0;0];[1;1;0;1;1;0;0];[0;0;1;1;1;0;0];[1;0;1;1;1;0;0];[0;1;1;1;1;0;0];[1;1;1;1;1;0;0];[0;0;0;0;0;1;0];[1;0;0;0;0;1;0];[0;1;0;0;0;1;0];[1;1;0;0;0;1;0];[0;0;1;0;0;1;0];[1;0;1;0;0;1;0];[0;1;1;0;0;1;0];[1;1;1;0;0;1;0];[0;0;0;1;0;1;0];[1;0;0;1;0;1;0];[0;1;0;1;0;1;0];[1;1;0;1;0;1;0];[0;0;1;1;0;1;0];[1;0;1;1;0;1;0];[0;1;1;1;0;1;0];[1;1;1;1;0;1;0];[0;0;0;0;1;1;0];[1;0;0;0;1;1;0];[0;1;0;0;1;1;0];[1;1;0;0;1;1;0];[0;0;1;0;1;1;0];[1;0;1;0;1;1;0];[0;1;1;0;1;1;0];[1;1;1;0;1;1;0];[0;0;0;1;1;1;0];[1;0;0;1;1;1;0];[0;1;0;1;1;1;0];[1;1;0;1;1;1;0];[0;0;1;1;1;1;0];[1;0;1;1;1;1;0];[0;1;1;1;1;1;0];[1;1;1;1;1;1;0];[0;0;0;0;0;0;1];[1;0;0;0;0;0;1];[0;1;0;0;0;0;1];[1;1;0;0;0;0;1];[0;0;1;0;0;0;1];[1;0;1;0;0;0;1];[0;1;1;0;0;0;1];[1;1;1;0;0;0;1];[0;0;0;1;0;0;1];[1;0;0;1;0;0;1];[0;1;0;1;0;0;1];[1;1;0;1;0;0;1];[0;0;1;1;0;0;1];[1;0;1;1;0;0;1];[0;1;1;1;0;0;1];[1;1;1;1;0;0;1];[0;0;0;0;1;0;1];[1;0;0;0;1;0;1];[0;1;0;0;1;0;1];[1;1;0;0;1;0;1];[0;0;1;0;1;0;1];[1;0;1;0;1;0;1];[0;1;1;0;1;0;1];[1;1;1;0;1;0;1];[0;0;0;1;1;0;1];[1;0;0;1;1;0;1];[0;1;0;1;1;0;1];[0;0;1;1;1;0;1];[0;1;1;1;1;0;1];[0;0;0;0;0;1;1];[1;0;0;0;0;1;1];[0;1;0;0;0;1;1];[1;1;0;0;0;1;1];[0;0;1;0;0;1;1];[1;0;1;0;0;1;1];[0;1;1;0;0;1;1];[1;1;1;0;0;1;1];[0;0;0;1;0;1;1];[1;0;0;1;0;1;1];[0;1;0;1;0;1;1];[1;1;0;1;0;1;1];[0;0;1;1;0;1;1];[1;0;1;1;0;1;1];[0;1;1;1;0;1;1];[1;1;1;1;0;1;1];[0;0;0;0;1;1;1];[1;0;0;0;1;1;1];[0;1;0;0;1;1;1];[1;1;0;0;1;1;1];[0;0;1;0;1;1;1];[1;0;1;0;1;1;1];[0;1;1;0;1;1;1];[1;1;1;0;1;1;1];[0;0;0;1;1;1;1];[1;0;0;1;1;1;1];[0;1;0;1;1;1;1];[1;1;0;1;1;1;1];[0;0;1;1;1;1;1];[1;0;1;1;1;1;1];[0;1;1;1;1;1;1];[1;1;1;1;1;1;1]]) *)


grb2 1 -> sos 0 1
COOPERATIVITY([fyn;ccblr] -> ccblp2 0 1, [[1;1]])
COOPERATIVITY([fyn;ccblr] -> ccblp2 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([gab2;zap70;gads] -> slp76 0 1, [[0;1;1]])
COOPERATIVITY([gab2;zap70;gads] -> slp76 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[1;1;1]])
COOPERATIVITY([zap70;ccblr] -> ccblp1 0 1, [[1;1]])
COOPERATIVITY([zap70;ccblr] -> ccblp1 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([card11a;pkcth] -> ikkg 0 1, [[1;1]])
COOPERATIVITY([card11a;pkcth] -> ikkg 1 0, [[0;0];[1;0];[0;1]])
gsk3 1 -> bcat 1 0
gab2 1 -> shp2 0 1
erk 1 -> rsk 0 1
COOPERATIVITY([x;zap70;sh3bp2] -> vav1 0 1, [[1;0;0];[1;1;0];[1;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([x;zap70;sh3bp2] -> vav1 1 0, [[0;0;0];[0;1;0];[0;0;1]])
sh3bp2 1 -> vav3 0 1
COOPERATIVITY([jun;fos] -> ap1 0 1, [[1;1]])
COOPERATIVITY([jun;fos] -> ap1 1 0, [[0;0];[1;0];[0;1]])
jnk 1 -> jun 0 1
plcga 1 -> ip3 0 1
ikb 1 -> nfkb 1 0
ca 1 -> cam 0 1
COOPERATIVITY([mekk1;mlk3] -> mkk4 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([mekk1;mlk3] -> mkk4 1 0, [[0;0]])
mek 1 -> erk 0 1
ras 1 -> raf 0 1
cd28 1 -> cblb 1 0
COOPERATIVITY([tcrlig;ccblp1] -> tcrb 0 1, [[1;0]])
COOPERATIVITY([tcrlig;ccblp1] -> tcrb 1 0, [[0;0];[0;1];[1;1]])
COOPERATIVITY([rasgrp;sos;gap] -> ras 0 1, [[1;1;0]])
COOPERATIVITY([rasgrp;sos;gap] -> ras 1 0, [[0;0;0];[1;0;0];[0;1;0];[0;0;1];[1;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([lckp1;tcrb;fyn] -> tcrp 0 1, [[1;1;0];[0;1;1];[1;1;1]])
COOPERATIVITY([lckp1;tcrb;fyn] -> tcrp 1 0, [[0;0;0];[1;0;0];[0;1;0];[0;0;1];[1;0;1]])
pkb 1 -> p27k 1 0
COOPERATIVITY([pten;ship1;pi3k] -> pip3 0 1, [[0;0;1]])
COOPERATIVITY([pten;ship1;pi3k] -> pip3 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[1;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([lat;grb2;zap70;gads] -> gab2 0 1, [[1;1;1;0];[1;0;1;1];[1;1;1;1]])
COOPERATIVITY([lat;grb2;zap70;gads] -> gab2 1 0, [[0;0;0;0];[1;0;0;0];[0;1;0;0];[1;1;0;0];[0;0;1;0];[1;0;1;0];[0;1;1;0];[0;0;0;1];[1;0;0;1];[0;1;0;1];[1;1;0;1];[0;0;1;1];[0;1;1;1]])
COOPERATIVITY([tcrb;fyn] -> pag 0 1, [[0;0];[0;1];[1;1]])
COOPERATIVITY([tcrb;fyn] -> pag 1 0, [[1;0]])
tcrb 1 -> dgk 0 1
COOPERATIVITY([tcrp;abl;ccblp1] -> zap70 0 1, [[1;1;0]])
COOPERATIVITY([tcrp;abl;ccblp1] -> zap70 1 0, [[0;0;0];[1;0;0];[0;1;0];[0;0;1];[1;0;1];[0;1;1];[1;1;1]])
zap70 1 -> lat 0 1
COOPERATIVITY([lckp1;erk] -> shp1 0 1, [[1;0]])
COOPERATIVITY([lckp1;erk] -> shp1 1 0, [[0;0];[0;1];[1;1]])
pkb 1 -> fkhr 1 0
pkb 1 -> p21c 1 0
pag 1 -> csk 0 1
COOPERATIVITY([cblb;lckp2;x] -> pi3k 0 1, [[0;1;0];[0;0;1];[0;1;1]])
COOPERATIVITY([cblb;lckp2;x] -> pi3k 1 0, [[0;0;0];[1;0;0];[1;1;0];[1;0;1];[1;1;1]])
lat 1 -> hpk1 0 1
COOPERATIVITY([ikkg;camk2] -> ikkab 0 1, [[1;1]])
COOPERATIVITY([ikkg;camk2] -> ikkab 1 0, [[0;0];[1;0];[0;1]])

initial_state lckr 1, bcl10 1, rac1r 1, cd45 1, card11 1, ccblr 1, malt1 1
