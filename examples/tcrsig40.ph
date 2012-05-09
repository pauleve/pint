
process Itk 1
process TCRbind 1
process Grb2Sos 1
process SEK 1
process PAGCsk 1
process IKK 1
process LCK 1
process PLCg_b 1
process Raf 1
process NFAT 1
process CD45 1
process Fos 1
process TCRphos 1
process IkB 1
process LAT 1
process AP1 1
process Jun 1
process JNK 1
process IP3 1
process cCbl 1
process MEK 1
process Fyn 1
process NFkB 1
process TCRlig 1
process CRE 1
process Ras 1
process ERK 1
process ZAP70 1
process Rsk 1
process Gads 1
process CREB 1
process Calcin 1
process Ca 1
process RasGRP1 1
process PKCth 1
process Slp76 1
process CD8 1
process Rlk 1
process PLCg_a 1
process DAG 1

GRN([
	Itk 1 -> + PLCg_a;
	TCRbind 1 -> + Fyn;
	TCRbind 1 -> + TCRphos;
	TCRbind 1 -> - PAGCsk;
	Grb2Sos 1 -> + Ras;
	SEK 1 -> + JNK;
	PAGCsk 1 -> - LCK;
	IKK 1 -> - IkB;
	LCK 1 -> + Fyn;
	LCK 1 -> + Rlk;
	LCK 1 -> + TCRphos;
	LCK 1 -> + ZAP70;
	PLCg_b 1 -> + PLCg_a;
	Raf 1 -> + MEK;
	CD45 1 -> + Fyn;
	CD45 1 -> + LCK;
	CD45 1 -> + CD45;
	Fos 1 -> + AP1;
	TCRphos 1 -> + ZAP70;
	IkB 1 -> - NFkB;
	LAT 1 -> + Gads;
	LAT 1 -> + Grb2Sos;
	LAT 1 -> + PLCg_b;
	Jun 1 -> + AP1;
	JNK 1 -> + Jun;
	IP3 1 -> + Ca;
	cCbl 1 -> - TCRbind;
	cCbl 1 -> - ZAP70;
	MEK 1 -> + ERK;
	Fyn 1 -> + PAGCsk;
	Fyn 1 -> + TCRphos;
	TCRlig 1 -> + TCRbind;
	TCRlig 1 -> + TCRlig;
	Ras 1 -> + Raf;
	ERK 1 -> + Fos;
	ERK 1 -> + Rsk;
	ZAP70 1 -> + PLCg_a;
	ZAP70 1 -> + Itk;
	ZAP70 1 -> + cCbl;
	ZAP70 1 -> + LAT;
	Rsk 1 -> + CREB;
	Gads 1 -> + Slp76;
	CREB 1 -> + CRE;
	Calcin 1 -> + NFAT;
	Ca 1 -> + Calcin;
	RasGRP1 1 -> + Ras;
	PKCth 1 -> + RasGRP1;
	PKCth 1 -> + IKK;
	PKCth 1 -> + SEK;
	Slp76 1 -> + PLCg_a;
	Slp76 1 -> + Itk;
	CD8 1 -> + LCK;
	CD8 1 -> + CD8;
	Rlk 1 -> + PLCg_a;
	PLCg_a 1 -> + DAG;
	PLCg_a 1 -> + IP3;
	DAG 1 -> + PKCth;
	DAG 1 -> + RasGRP1;
])

COOPERATIVITY([LCK;TCRbind] -> TCRphos 0 1, [[1;1]])
COOPERATIVITY([Fyn;LCK;TCRbind] -> TCRphos 1 0, [[0;0;0];[0;0;1];[0;1;0]])

COOPERATIVITY([CD45;LCK;TCRbind] -> Fyn 0 1, [[1;0;1];[1;1;0];[1;1;1]])
COOPERATIVITY([LCK;TCRbind] -> Fyn 1 0, [[0;0]])



(*
COOPERATIVITY([ZAP70;Slp76;PLCg_b;Itk;Rlk] -> PLCg_a 0 1, [[1;1;1;0;1];[1;1;1;1;0];[1;1;1;1;1]])
COOPERATIVITY([Itk;Rlk] -> PLCg_a 1 0, [[0;0]])
*)
COOPERATIVITY([ZAP70;Slp76;PLCg_b] in [[1;1;1]]
          and [Itk;Rlk] in [[0;1];[1;0];[1;1]],
		  	PLCg_a, 1, 0)


COOPERATIVITY([ZAP70;Slp76] -> Itk 0 1, [[1;1]])
COOPERATIVITY([Fos;Jun] -> AP1 0 1, [[1;1]])
COOPERATIVITY([Grb2Sos;RasGRP1] -> Ras 0 1, [[1;1];[0;1];[1;0]])
COOPERATIVITY([Grb2Sos;RasGRP1] -> Ras 1 0, [[0;0]])
COOPERATIVITY([DAG;PKCth] -> RasGRP1 0 1, [[1;1]])

COOPERATIVITY([cCbl;TCRlig] -> TCRbind 0 1, [[0;1]])
COOPERATIVITY([PAGCsk;CD8;CD45] -> LCK 0 1, [[0;1;1]])
COOPERATIVITY([cCbl;TCRphos;LCK] -> ZAP70 0 1, [[0;1;1]])

(* ?? *)
COOPERATIVITY([TCRbind;Fyn] -> PAGCsk 0 1, [[0;0];[0;1];[1;1]])
COOPERATIVITY([TCRbind;Fyn] -> PAGCsk 1 0, [[1;0]])

CD45 0 -> CD45 0 1
CD8 0 -> CD8 0 1
TCRlig 0 -> TCRlig 0 1

