(**

EGFR/ErbB Signalling (104 components)

* Samaga, R.; Saez-Rodriguez, J.; Alexopoulos, L. G.; Sorger, P. K. & Klamt, S. The Logic of
  EGFR/ErbB Signaling: Theoretical Properties and Analysis of High-Throughput Data PLoS Comput Biol,
  Public Library of Science, 2009, 5

**)

process sos1_eps8_e3b1 1
process pp2b 1
process pkc 1
process pp2a 1
process jnk 1
process gsk3 1
process pi3kr 1
process plcg 1
process p90rskerk12d 1
process sos1r 1
process creb 1
process erbb34 1
process dag 1
process limk1 1
process mekk1 1
process csrc 1
process mekk4 1
process bir 1
process cjun 1
process ship2 1
process cfos 1
process bad 1
process nucerk12 1
process mek12 1
process pro_apoptotic 1
process erbb23 1
process erbb24 1
process mlk3 1
process pdk1 1
process btc 1
process rasgap 1
process erbb14 1
process eps8r 1
process erbb11 1
process erbb12 1
process erbb13 1
process endocyt_degrad 1
process shc 1
process mtorr 1
process p38 1
process grb2 1
process stat5 1
process stat3 1
process stat1 1
process rntre 1
process akt 1
process elk1 1
process cmyc 1
process nrg2a 1
process nrg2b 1
process erbb2 1
process erbb3 1
process erbb1 1
process erbb4 1
process ca 1
process rac_cdc42 1
process mtor_ric 1
process pak1 1
process pten 1
process p90rsk 1
process mkp 1
process erk12 1
process rheb 1
process p70s6_1 1
process p70s6_2 1
process rab5a 1
process sos1 1
process vav2 1
process rin1 1
process hbegf 1
process nck 1
process ship2d 1
process tgfa 1
process pi3k 1
process tsc1_tsc2 1
process mk2 1
process mkk3 1
process raf1 1
process mkk4 1
process mkk7 1
process mkk6 1
process mtor_rap 1
process ar 1
process ras 1
process nrg3 1
process nrg4 1
process aktd 1
process pip3 1
process ccbl 1
process gab1 1
process nrg1a 1
process nrg1b 1
process ptend 1
process ap1 1
process actin_reorg 1
process shp1 1
process shp2 1
process ip3 1
process shp1d 1
process epr 1
process hsp27 1
process pi34p2 1
process erbb44 1
process egf 1

COOPERATIVITY([eps8r;pip3;pi3kr;sos1r] -> sos1_eps8_e3b1 0 1, [[1;1;1;1]])
COOPERATIVITY([eps8r;pip3;pi3kr;sos1r] -> sos1_eps8_e3b1 1 0, [[0;0;0;0];[1;0;0;0];[0;1;0;0];[1;1;0;0];[0;0;1;0];[1;0;1;0];[0;1;1;0];[1;1;1;0];[0;0;0;1];[1;0;0;1];[0;1;0;1];[1;1;0;1];[0;0;1;1];[1;0;1;1];[0;1;1;1]])
COOPERATIVITY([pdk1;ca;dag] -> pkc 0 1, [[1;1;1]])
COOPERATIVITY([pdk1;ca;dag] -> pkc 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1]])
COOPERATIVITY([mkk4;mkk7] -> jnk 0 1, [[1;1]])
COOPERATIVITY([mkk4;mkk7] -> jnk 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([p90rsk;akt] -> gsk3 0 1, [[0;0]])
COOPERATIVITY([p90rsk;akt] -> gsk3 1 0, [[1;0];[0;1];[1;1]])
erbb11 1 -> plcg 0 1
COOPERATIVITY([erk12;p90rsk] -> p90rskerk12d 0 1, [[1;1]])
COOPERATIVITY([erk12;p90rsk] -> p90rskerk12d 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([p90rsk;mk2] -> creb 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([p90rsk;mk2] -> creb 1 0, [[0;0]])

COOPERATIVITY(
	    [erbb2;erbb3;erbb4] in [[0;1;1]]
	and not ([nrg1a;nrg1b] in [[0;0]] and [nrg2a;nrg2b] in [[0;0]]),
	erbb34, 1, 0)

plcg 1 -> dag 0 1
pak1 1 -> limk1 0 1
rac_cdc42 1 -> mekk1 0 1
rac_cdc42 1 -> mekk4 0 1
jnk 1 -> cjun 0 1
COOPERATIVITY([jnk;erk12;p90rsk;pp2a] -> cfos 0 1, [[1;0;0;0];[1;1;0;0];[1;0;1;0];[0;1;1;0];[1;1;1;0]])
COOPERATIVITY([jnk;erk12;p90rsk;pp2a] -> cfos 1 0, [[0;0;0;0];[0;1;0;0];[0;0;1;0];[0;0;0;1];[1;0;0;1];[0;1;0;1];[1;1;0;1];[0;0;1;1];[1;0;1;1];[0;1;1;1];[1;1;1;1]])
COOPERATIVITY([pak1;akt] -> bad 0 1, [[0;0]])
COOPERATIVITY([pak1;akt] -> bad 1 0, [[1;0];[0;1];[1;1]])
COOPERATIVITY([erk12;mkp] -> nucerk12 0 1, [[1;0]])
COOPERATIVITY([erk12;mkp] -> nucerk12 1 0, [[0;0];[0;1];[1;1]])
COOPERATIVITY([mekk1;raf1] -> mek12 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([mekk1;raf1] -> mek12 1 0, [[0;0]])
bad 1 -> pro_apoptotic 0 1

COOPERATIVITY(
	    [erbb2;erbb3] in [[1;1]]
	and not ([epr;nrg1a;nrg1b] in [[0;0;0]] and [btc;nrg2b;bir] in [[0;0;0]]),
	erbb23, 1, 0)

COOPERATIVITY(
	    [erbb2;erbb4] in [[1;1]]
	and not ([epr;nrg1a;nrg1b] in [[0;0;0]]
				and [btc;nrg2b;bir] in [[0;0;0]]
				and [tgfa;nrg3;nrg4] in [[0;0;0]]
				and [hbegf;nrg2a;egf] in [[0;0;0]]),
		erbb24, 1, 0)

rac_cdc42 1 -> mlk3 0 1
COOPERATIVITY([shp2;gab1] -> rasgap 0 1, [[0;1]])
COOPERATIVITY([shp2;gab1] -> rasgap 1 0, [[0;0];[1;0];[1;1]])

COOPERATIVITY(
	    [erbb1;erbb4;erbb2;shp1d] in [[1;1;0;0]]
	and not ([epr;nrg1a;nrg1b] in [[0;0;0]]
				and [nrg2a;nrg2b;nrg4] in [[0;0;0]]
				and [tgfa;egf] in [[0;0]]),
	erbb14, 1, 0)

COOPERATIVITY(
	    [erbb1;shp1d] in [[1;0]]
	and not ([epr;hbegf;ar] in [[0;0;0]]
				and [btc;bir] in [[0;0]]
				and [tgfa;egf] in [[0;0]]),
	erbb11, 1, 0)

COOPERATIVITY(
	    [erbb1;erbb2;shp1d] in [[1;1;0]]
	and not ([epr;hbegf] in [[0;0]]
				and [btc;bir] in [[0;0]]
				and [tgfa;egf] in [[0;0]]),
	erbb12, 1, 0)

COOPERATIVITY(
	    [erbb1;erbb3;shp1d] in [[1;1;0]]
	and [ar;erbb2] in [[1;1];[1;0];[0;0]]
	and not ([epr;nrg1a;nrg1b] in [[0;0;0]]
				and [tgfa;egf;btc;nrg2a] in [[0;0;0;0]]),
	erbb13, 1, 0)

COOPERATIVITY([rab5a;ccbl] -> endocyt_degrad 0 1, [[1;1]])
COOPERATIVITY([rab5a;ccbl] -> endocyt_degrad 1 0, [[0;0];[1;0];[0;1]])

COOPERATIVITY(
	not ([erbb14;erbb11;erbb12;erbb13] in [[0;0;0;0]]
		and [erbb23;erbb44;erbb24;erbb34] in [[0;0;0;0]]),
	shc, 0, 1)

COOPERATIVITY([mkk3;mkk4;mkk6] -> p38 0 1, [[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([mkk3;mkk4;mkk6] -> p38 1 0, [[0;0;0]])

COOPERATIVITY(
	not ([erbb14;erbb11;erbb12;erbb13] in [[0;0;0;0]]
		and [erbb23;erbb44;erbb24;erbb34] in [[0;0;0;0]]
		and [shc] in [[0]]),
	grb2, 0, 1)

COOPERATIVITY([csrc;erbb24;erbb11] -> stat5 0 1, [[1;1;0];[1;0;1];[1;1;1]])
COOPERATIVITY([csrc;erbb24;erbb11] -> stat5 1 0, [[0;0;0];[1;0;0];[0;1;0];[0;0;1];[0;1;1]])
COOPERATIVITY([csrc;erbb11] -> stat3 0 1, [[1;1]])
COOPERATIVITY([csrc;erbb11] -> stat3 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([csrc;erbb11] -> stat1 0 1, [[1;1]])
COOPERATIVITY([csrc;erbb11] -> stat1 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([eps8r;erbb11] -> rntre 0 1, [[1;1]])
COOPERATIVITY([eps8r;erbb11] -> rntre 1 0, [[0;0];[1;0];[0;1]])

COOPERATIVITY(
	    [mtor_ric;pp2a;pdk1] in [[1;0;1]]
	and not [pip3;pi34p2] in [[0;0]],
	akt, 1, 0)

COOPERATIVITY([pp2b;nucerk12] -> elk1 0 1, [[0;1]])
COOPERATIVITY([pp2b;nucerk12] -> elk1 1 0, [[0;0];[1;0];[1;1]])
COOPERATIVITY([nucerk12;gsk3] -> cmyc 0 1, [[1;0]])
COOPERATIVITY([nucerk12;gsk3] -> cmyc 1 0, [[0;0];[0;1];[1;1]])
ip3 1 -> ca 0 1
COOPERATIVITY([sos1_eps8_e3b1;vav2] -> rac_cdc42 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([sos1_eps8_e3b1;vav2] -> rac_cdc42 1 0, [[0;0]])
mtorr 1 -> mtor_ric 0 1
COOPERATIVITY([nck;rac_cdc42;grb2] -> pak1 0 1, [[1;1;0];[0;1;1];[1;1;1]])
COOPERATIVITY([nck;rac_cdc42;grb2] -> pak1 1 0, [[0;0;0];[1;0;0];[0;1;0];[0;0;1];[1;0;1]])
COOPERATIVITY([erk12;pdk1] -> p90rsk 0 1, [[1;1]])
COOPERATIVITY([erk12;pdk1] -> p90rsk 1 0, [[0;0];[1;0];[0;1]])
mek12 1 -> erk12 0 1
tsc1_tsc2 1 -> rheb 1 0
COOPERATIVITY([jnk;erk12] -> p70s6_1 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([jnk;erk12] -> p70s6_1 1 0, [[0;0]])
COOPERATIVITY([pdk1;mtor_rap;p70s6_1] -> p70s6_2 0 1, [[1;1;1]])
COOPERATIVITY([pdk1;mtor_rap;p70s6_1] -> p70s6_2 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1]])
COOPERATIVITY([rntre;rin1] -> rab5a 0 1, [[0;1]])
COOPERATIVITY([rntre;rin1] -> rab5a 1 0, [[0;0];[1;0];[1;1]])
COOPERATIVITY([sos1r;p90rskerk12d;grb2] -> sos1 0 1, [[1;0;1]])
COOPERATIVITY([sos1r;p90rskerk12d;grb2] -> sos1 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([pi34p2;erbb11;pip3] -> vav2 0 1, [[1;1;0];[0;1;1];[1;1;1]])
COOPERATIVITY([pi34p2;erbb11;pip3] -> vav2 1 0, [[0;0;0];[1;0;0];[0;1;0];[0;0;1];[1;0;1]])
ras 1 -> rin1 0 1
COOPERATIVITY([erbb14;erbb44;erbb11] -> nck 0 1, [[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([erbb14;erbb44;erbb11] -> nck 1 0, [[0;0;0]])
ship2 1 -> ship2d 0 1

COOPERATIVITY(
	    [pi3kr] in [[1]]
	and not ([erbb13;erbb23;erbb34] in [[0;0;0]]
				and [pi3kr;gab1;ras] in [[0;0;0]]),
	pi3k, 1, 0)

akt 1 -> tsc1_tsc2 1 0
p38 1 -> mk2 0 1
mlk3 1 -> mkk3 0 1
COOPERATIVITY([csrc;aktd;pak1;ras] -> raf1 0 1, [[1;0;0;1];[0;0;1;1];[1;0;1;1]])
COOPERATIVITY([csrc;aktd;pak1;ras] -> raf1 1 0, [[0;0;0;0];[1;0;0;0];[0;1;0;0];[1;1;0;0];[0;0;1;0];[1;0;1;0];[0;1;1;0];[1;1;1;0];[0;0;0;1];[0;1;0;1];[1;1;0;1];[0;1;1;1];[1;1;1;1]])
COOPERATIVITY([mekk1;mlk3;mekk4] -> mkk4 0 1, [[1;0;0];[0;1;0];[1;1;0];[0;0;1];[1;0;1];[0;1;1];[1;1;1]])
COOPERATIVITY([mekk1;mlk3;mekk4] -> mkk4 1 0, [[0;0;0]])
mekk1 1 -> mkk7 0 1
mlk3 1 -> mkk6 0 1
COOPERATIVITY([rheb;mtorr] -> mtor_rap 0 1, [[1;1]])
COOPERATIVITY([rheb;mtorr] -> mtor_rap 1 0, [[0;0];[1;0];[0;1]])
COOPERATIVITY([rasgap;sos1] -> ras 0 1, [[0;1]])
COOPERATIVITY([rasgap;sos1] -> ras 1 0, [[0;0];[1;0];[1;1]])
akt 1 -> aktd 0 1
COOPERATIVITY([ship2d;ptend;pi3k] -> pip3 0 1, [[0;0;1]])
COOPERATIVITY([ship2d;ptend;pi3k] -> pip3 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[1;0;1];[0;1;1];[1;1;1]])
erbb11 1 -> ccbl 0 1
COOPERATIVITY([grb2;erbb11] -> gab1 0 1, [[1;0];[0;1];[1;1]])
COOPERATIVITY([grb2;erbb11] -> gab1 1 0, [[0;0]])
pten 1 -> ptend 0 1
COOPERATIVITY([cfos;cjun] -> ap1 0 1, [[1;1]])
COOPERATIVITY([cfos;cjun] -> ap1 1 0, [[0;0];[1;0];[0;1]])
limk1 1 -> actin_reorg 0 1
erbb11 1 -> shp1 0 1
gab1 1 -> shp2 0 1
plcg 1 -> ip3 0 1
shp1 1 -> shp1d 0 1
mk2 1 -> hsp27 0 1
COOPERATIVITY([ship2d;ptend;pi3k] -> pi34p2 0 1, [[1;0;1]])
COOPERATIVITY([ship2d;ptend;pi3k] -> pi34p2 1 0, [[0;0;0];[1;0;0];[0;1;0];[1;1;0];[0;0;1];[0;1;1];[1;1;1]])

COOPERATIVITY(
	    [erbb4] in [[1]]
	and not ([btc;nrg2b;bir] in [[0;0;0]]
				and [nrg4;nrg1a;nrg1b;nrg3] in [[0;0;0;0]]),
	erbb44, 1, 0)

