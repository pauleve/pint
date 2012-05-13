
process AKT1 1
process CDK2 1 process CDK4 1 process CDK6 1
process CycD1 1 process CycE1 1
process EGF 1
process ERalpha 1
process ERBB1 1 process ERBB1_2 1 process ERBB1_3 1
process ERBB2 1 process ERBB2_3 1 process ERBB3 1
process IGF1R 1
process MEK1 1
process MYC 1
process p21 1 process p27 1
process pRB 1

GRN([
	ERBB2_3 1 -> + AKT1; ERBB2_3 1 -> + MEK1; ERBB2_3 1 -> - IGF1R;
	ERBB2 1 -> + ERBB2_3; ERBB2 1 -> + ERBB1_2; ERBB3 1 -> + ERBB2_3;
	ERBB3 1 -> + ERBB1_3;
	CycE1 1 -> + CDK2;
	MEK1 1 -> + CycD1; MEK1 1 -> + ERalpha; MEK1 1 -> + MYC;
	CDK4 1 -> + pRB; CDK4 1 -> - p21; CDK4 1 -> - p27;
	ERalpha 1 -> + CycD1; ERalpha 1 -> + IGF1R; ERalpha 1 -> + p21; ERalpha 1 -> + MYC; ERalpha 1 -> + p27;
	MYC 1 -> + CycE1; MYC 1 -> - p21; MYC 1 -> + CycD1; MYC 1 -> - p27;
	CDK6 1 -> + pRB;
	ERBB1 1 -> + ERBB1_2; ERBB1 1 -> + ERBB1_3; ERBB1 1 -> + AKT1; ERBB1 1 -> + MEK1;
	IGF1R 1 -> + AKT1; IGF1R 1 -> + MEK1;
	ERBB1_3 1 -> + AKT1; ERBB1_3 1 -> + MEK1;
	p27 1 -> - CDK2; p27 1 -> - CDK4;
	CDK2 1 -> - p27; CDK2 1 -> + pRB;
	p21 1 -> - CDK2; p21 1 -> - CDK4;
	CycD1 1 -> + CDK4; CycD1 1 -> + CDK6;
	EGF 1 -> + ERBB1; EGF 1 -> + ERBB2; EGF 1 -> + ERBB3;
	AKT1 1 -> + CycD1; AKT1 1 -> + MYC; AKT1 1 -> - p27; AKT1 1 -> + ERalpha; AKT1 1 -> + IGF1R; AKT1 1 -> - p21;
	ERBB1_2 1 -> + AKT1; ERBB1_2 1 -> + MEK1;
])

COOPERATIVITY([ERBB1;ERBB2] -> ERBB1_2 0 1, [[1;1]])
COOPERATIVITY([ERBB1;ERBB3] -> ERBB1_3 0 1, [[1;1]])
COOPERATIVITY([ERBB2;ERBB3] -> ERBB2_3 0 1, [[1;1]])

COOPERATIVITY([ERBB2_3;AKT1] -> IGF1R 0 1, [[0;1]])
COOPERATIVITY([ERBB2_3;ERalpha] -> IGF1R 0 1, [[0;1]])
COOPERATIVITY([AKT1;ERalpha] -> IGF1R 1 0, [[0;0]])

COOPERATIVITY([AKT1;MEK1] -> ERalpha 1 0, [[0;0]])
COOPERATIVITY([AKT1;MEK1;ERalpha] -> MYC 1 0, [[0;0;0]])

(*COOPERATIVITY([ERBB1;ERBB1_2;ERBB1_3;ERBB2_3;IGF1R] -> AKT1 1 0, [[0;0;0;0;0]])
COOPERATIVITY([ERBB1;ERBB1_2;ERBB1_3;ERBB2_3;IGF1R] -> MEK1 1 0, [[0;0;0;0;0]])*)
COOPERATIVITY([ERBB1;ERBB1_2;ERBB1_3] in [[0;0;0]]
					and [ERBB2_3;IGF1R] in [[0;0]], AKT1, 0, 1)
COOPERATIVITY([ERBB1;ERBB1_2;ERBB1_3] in [[0;0;0]]
					and [ERBB2_3;IGF1R] in [[0;0]], MEK1, 0, 1)

COOPERATIVITY([CycE1;p21;p27] -> CDK2 0 1, [[1;0;0]])
COOPERATIVITY([CycD1;p21;p27] -> CDK4 0 1, [[1;0;0]])

(*COOPERATIVITY([ERalpha;MYC;AKT1;MEK1] -> CycD1 0 1, [[1;1;1;0];[1;1;0;1];[1;1;1;1]])*)
COOPERATIVITY([ERalpha;MYC] in [[1;1]]
					and [AKT1;MEK1] in [[1;0];[0;1];[1;1]], CycD1, 1, 0)

COOPERATIVITY([AKT1;MEK1] -> CycD1 1 0, [[0;0]])
(*COOPERATIVITY([ERalpha;AKT1;MYC;CDK4] -> p21 0 1, [[1;0;0;0]])*)
COOPERATIVITY([ERalpha;AKT1] in [[1;0]]
					and [MYC;CDK4] in [[0;0]], p21, 1, 0)

(*COOPERATIVITY([ERalpha;CDK4;CDK2;AKT1;MYC] -> p27 0 1, [[1;0;0;0;0]])*)
COOPERATIVITY([AKT1;ERalpha] in [[0;1]]
			and [CDK2;CDK4;MYC] in [[0;0;0]], p27, 1, 0)

COOPERATIVITY([CDK2;CDK4;CDK6] -> pRB 0 1, [[0;1;1];[1;1;1]])
RM({CDK2 0 -> pRB 1 0})
RM({EGF 1 -> EGF 1 0}) (* prevent self-degradation (input) *)

initial_state EGF 1

