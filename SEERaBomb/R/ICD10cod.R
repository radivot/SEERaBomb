ICD10cod<-function(){
  
  COD=list(oral=1:10,esophagus=11,stomach=12,intestine=13,colon=14,rectal=c(24,27),liver=29:30,gallBladder=31:32,pancreas=33,
           GI=34:36,sinus=37,larynx=38,lung=39,chest=c(40,41,43),bone=42,
           melanoma=44,skin=45,breast=46,cervix=47,uterus=48:49,ovary=50,femGen=c(51,53),prostate=54,testes=55,maleGen=56:57,
           bladder=58,renal=59:61,eye=62,       brain=90, #  brain jumps out only in SEER code, not in ICD10 below
           thyroid=65,thymus=66,HL=67,NHL=70,MM=73,LL=c(74,75),OL=c(76,83,89),ML=c(77,78,80),subleu=85,otherMalig=86, IS=130,
           TBsyph=c(133,136),sepsis=142,otherInfect=145,diabetes=148,alzheimers=151,heart=154,hypertension=157,cerebroVasc=160,athero=163,aneurysm=166,otherVasc=169,
           pneumonia=172,COPD=175,ulcer=178,livFail=181,renFail=184,birthing=c(187,193),congenDef=190,illDef=196,accidents=199,suicide=202,homocide=205,other=208,unknown=252)
  
  C=list(oral=c(paste0("C0",0:9),paste0("C",10:14)),
         esophagus="C15",stomach="C16",intestine="C17",colon=c("C18","C26"),rectal=c("C19","C20","C21"),
         liver="C22",gallBladder=c("C23","C24"),pancreas="C25",
         GI=c("C45","C48"),sinus=c("C30","C31"),larynx="C32",
         lung="C34",
         chest=c("C33","C38","C39","C45","C47","C49"),#chest= pleura + trachea +soft+meso  #C45 from otherMalig will make this high, so don't trust
         bone=c("C40","C41"),
         melanoma="C43",skin=c("C44","C46"), # skin includes KS
         breast="C50",cervix="C53",uterus=c("C54","C55"),
         ovary="C56",femGen=c("C51","C52","C57","C58"),
         prostate="C61",testes="C62",maleGen=c("C60","C63"), 
         bladder="C67",renal=c("C64","C65","C68"), eye="C69", brain=c("C70","C71","C72"),
         thyroid="C73",thymus=c("C74","C75"),
         HL="C81",
         NHL=c(paste0("C",82:86),"C96"),  # C96 from other malig will make this high, so don't trust
         MM="C90", #takes from subleu so don't trust
         LL="C91", #takes from subleu so don't trust
         ML=c("C92","C93"), #myeloid cancers
         OL="C94", #takes from subleu  so don't trust)
         subleu="C95",  # short on 90, 91, and 94 given above
         otherMalig=paste0("C",c(76:80,88,97))) #leaving out 26 (colon) 45 (chest),  94 (OL) and 96 (NHL) since already donated above  
  D=list(IS=c(paste0("D0",0:9),paste0("D",10:48)))
  AB=list(TBsyph=paste0("A",c(15:19,50:43)),
          sepsis=paste0("A",40:41),
          otherInfect=c(paste0("A0",0:8),paste0("A",c(20:33,35:39,42:49,54:99)),paste0("B0",0:9),paste0("B",10:99)))
  E=list(diabetes=paste0("E",10:14))
  G=list(alzheimers="G30")
  I=list(heart=c(paste0("I0",0:9),paste0("I",c(11,13,20:51))),
         hypertension=c("I10","I12"),
         cerebroVasc=paste0("I",60:69),
         athero="I70", aneurysm="I71",
         otherVasc=paste0("I",72:78))
  J=list(pneumonia=c("J09",paste0("J",10:18)),
         COPD=paste0("J",40:47))
  K=list(ulcer=paste0("K",25:28),
         livFail=paste0("K",c(70,73:74)))
  N=list(renFail=c(paste0("N0",0:7),paste0("N",c(17:19,25:27))))
  OP=list(birthing=c(paste0("O0",0:9),paste0("O",c(10:95,98:99)),
                     paste0("P0",0:9),paste0("P",c(10:96))))
  Q=list(congenDef=c(paste0("Q0",0:9),paste0("Q",10:99)))
  R=list(illDef=c(paste0("R0",0:9),paste0("R",10:99)))
  VWX=list(accidents=c(paste0("V0",0:9),paste0("V",10:99),
                       paste0("W0",0:9),paste0("W",10:99),
                       paste0("X0",0:9),paste0("X",10:59),
                       "Y85","Y86"))
  U=list(suicide=c("U03", paste0("X",60:84),"Y87"),
         homocide=c("U01","U02", paste0("X",84:99),paste0("Y0",0:9),"Y35","Y89")) # skip rest of "Y87" since got all of it above
  
  ICD10=c(C,D,AB,E,G,I,J,K,N,OP,Q,R,VWX,U)
  seer=sort(unlist(ICD10,use.names = FALSE))
  # uni=c(paste0("A0",0:9),paste0("A",10:99),
  #       paste0("B0",0:9),paste0("B",10:99),
  #       paste0("C0",0:9),paste0("C",10:99),
  #       paste0("D0",0:9),paste0("D",10:99),
  #       paste0("E0",0:9),paste0("E",10:99),
  #       paste0("F0",0:9),paste0("F",10:99),
  #       paste0("G0",0:9),paste0("G",10:99),
  #       paste0("I0",0:9),paste0("I",10:99),
  #       paste0("J0",0:9),paste0("J",10:99),
  #       paste0("K0",0:9),paste0("K",10:99),
  #       paste0("N0",0:9),paste0("N",10:99),
  #       paste0("O0",0:9),paste0("O",10:99),
  #       paste0("P0",0:9),paste0("P",10:99),
  #       paste0("Q0",0:9),paste0("Q",10:99),
  #       paste0("R0",0:9),paste0("R",10:99),
  #       paste0("K0",0:9),paste0("W",10:99),
  #       paste0("U0",0:9),paste0("U",10:99),
  #       paste0("V0",0:9),paste0("V",10:99),
  #       paste0("W0",0:9),paste0("W",10:99),
  #       paste0("X0",0:9),paste0("X",10:99),
  #       paste0("Y0",0:9),paste0("Y",10:99))
  
  hcd=c("A00", "A01", "A02", "A03", "A04", "A05", "A06", "A07",  # this is from the hcd full listing file
    "A08", "A09", "A16", "A17", "A18", "A19", "A20", "A21", "A23", 
    "A24", "A25", "A26", "A27", "A28", "A30", "A31", "A32", "A35", 
    "A36", "A37", "A38", "A39", "A40", "A41", "A42", "A43", "A44", 
    "A46", "A48", "A49", "A50", "A51", "A52", "A53", "A54", "A55", 
    "A58", "A59", "A60", "A63", "A64", "A68", "A69", "A70", "A71", 
    "A74", "A75", "A77", "A78", "A79", "A80", "A81", "A82", "A83", 
    "A84", "A85", "A86", "A87", "A88", "A89", "A92", "A93", "A94", 
    "A95", "A96", "A97", "A98", "A99", "B00", "B01", "B02", "B05", 
    "B06", "B07", "B08", "B09", "B15", "B16", "B17", "B18", "B19", 
    "B20", "B21", "B22", "B23", "B24", "B25", "B26", "B27", "B30", 
    "B33", "B34", "B35", "B36", "B37", "B38", "B39", "B40", "B41", 
    "B42", "B43", "B44", "B45", "B46", "B47", "B48", "B49", "B50", 
    "B51", "B54", "B55", "B57", "B58", "B59", "B60", "B64", "B65", 
    "B66", "B67", "B68", "B69", "B70", "B74", "B75", "B76", "B77", 
    "B78", "B81", "B82", "B83", "B85", "B86", "B87", "B88", "B89", 
    "B90", "B91", "B92", "B94", "B99", "C00", "C01", "C02", "C03", 
    "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", 
    "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", 
    "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32", "C33", 
    "C34", "C37", "C38", "C39", "C40", "C41", "C43", "C44", "C45", 
    "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54", 
    "C55", "C56", "C57", "C58", "C60", "C61", "C62", "C63", "C64", 
    "C65", "C66", "C67", "C68", "C69", "C70", "C71", "C72", "C73", 
    "C74", "C75", "C76", "C77", "C78", "C79", "C80", "C81", "C82", 
    "C83", "C84", "C85", "C88", "C90", "C91", "C92", "C93", "C94", 
    "C95", "C96", "D00", "D01", "D02", "D03", "D04", "D05", "D06", 
    "D07", "D09", "D10", "D11", "D12", "D13", "D14", "D15", "D16", 
    "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", 
    "D26", "D27", "D28", "D29", "D30", "D31", "D32", "D33", "D34", 
    "D35", "D36", "D37", "D38", "D39", "D40", "D41", "D42", "D43", 
    "D44", "D45", "D46", "D47", "D48", "D50", "D51", "D52", "D53", 
    "D55", "D56", "D57", "D58", "D59", "D60", "D61", "D62", "D64", 
    "D65", "D66", "D67", "D68", "D69", "D70", "D71", "D72", "D73", 
    "D74", "D75", "D76", "D80", "D81", "D82", "D83", "D84", "D86", 
    "D89", "E00", "E01", "E02", "E03", "E04", "E05", "E06", "E07", 
    "E10", "E11", "E12", "E13", "E14", "E15", "E16", "E20", "E21", 
    "E22", "E23", "E24", "E25", "E26", "E27", "E28", "E29", "E30", 
    "E31", "E32", "E34", "E40", "E41", "E42", "E43", "E44", "E45", 
    "E46", "E50", "E51", "E52", "E53", "E54", "E55", "E56", "E58", 
    "E59", "E61", "E63", "E64", "E65", "E66", "E67", "E68", "E70", 
    "E71", "E72", "E73", "E74", "E75", "E76", "E77", "E78", "E79", 
    "E80", "E83", "E84", "E85", "E86", "E87", "E88", "F01", "F03", 
    "F04", "F05", "F06", "F07", "F09", "F10", "F11", "F12", "F13", 
    "F14", "F15", "F16", "F17", "F18", "F19", "F20", "F21", "F22", 
    "F23", "F25", "F28", "F29", "F30", "F31", "F32", "F33", "F34", 
    "F38", "F39", "F40", "F41", "F42", "F43", "F44", "F45", "F48", 
    "F50", "F51", "F52", "F54", "F55", "F59", "F60", "F62", "F63", 
    "F64", "F65", "F66", "F68", "F69", "F70", "F71", "F72", "F73", 
    "F78", "F79", "F80", "F81", "F82", "F84", "F88", "F89", "F90", 
    "F91", "F93", "F95", "F98", "F99", "G00", "G03", "G04", "G06", 
    "G08", "G09", "G10", "G11", "G12", "G14", "G20", "G21", "G23", 
    "G24", "G25", "G30", "G31", "G35", "G36", "G37", "G40", "G41", 
    "G43", "G44", "G45", "G47", "G50", "G51", "G52", "G54", "G56", 
    "G57", "G58", "G60", "G61", "G62", "G64", "G70", "G71", "G72", 
    "G80", "G81", "G82", "G83", "G90", "G91", "G92", "G93", "G95", 
    "G96", "G98", "H01", "H02", "H04", "H05", "H10", "H11", "H16", 
    "H17", "H18", "H20", "H21", "H26", "H30", "H31", "H33", "H34", 
    "H35", "H40", "H43", "H44", "H46", "H47", "H49", "H50", "H51", 
    "H52", "H53", "H54", "H57", "H60", "H61", "H65", "H66", "H69", 
    "H70", "H71", "H72", "H73", "H74", "H80", "H81", "H83", "H90", 
    "H91", "H92", "H93", "I00", "I01", "I02", "I05", "I06", "I07", 
    "I08", "I09", "I10", "I11", "I12", "I13", "I15", "I20", "I21", 
    "I22", "I24", "I25", "I26", "I27", "I28", "I30", "I31", "I33", 
    "I34", "I35", "I36", "I37", "I38", "I40", "I42", "I44", "I45", 
    "I46", "I47", "I48", "I49", "I50", "I51", "I60", "I61", "I62", 
    "I63", "I64", "I67", "I69", "I70", "I71", "I72", "I73", "I74", 
    "I77", "I78", "I80", "I81", "I82", "I83", "I85", "I86", "I87", 
    "I88", "I89", "I95", "I99", "J00", "J01", "J02", "J03", "J04", 
    "J05", "J06", "J09", "J10", "J11", "J12", "J13", "J14", "J15", 
    "J16", "J18", "J20", "J21", "J22", "J30", "J31", "J32", "J33", 
    "J34", "J35", "J36", "J37", "J38", "J39", "J40", "J41", "J42", 
    "J43", "J44", "J45", "J46", "J47", "J60", "J61", "J62", "J63", 
    "J64", "J65", "J66", "J67", "J68", "J69", "J70", "J80", "J81", 
    "J82", "J84", "J85", "J86", "J90", "J92", "J93", "J94", "J96", 
    "J98", "K00", "K02", "K03", "K04", "K05", "K06", "K07", "K08", 
    "K09", "K10", "K11", "K12", "K13", "K14", "K20", "K21", "K22", 
    "K25", "K26", "K27", "K28", "K29", "K30", "K31", "K35", "K36", 
    "K37", "K38", "K40", "K41", "K42", "K43", "K44", "K45", "K46", 
    "K50", "K51", "K52", "K55", "K56", "K57", "K58", "K59", "K60", 
    "K61", "K62", "K63", "K64", "K65", "K66", "K70", "K71", "K72", 
    "K73", "K74", "K75", "K76", "K80", "K81", "K82", "K83", "K85", 
    "K86", "K90", "K92", "L00", "L01", "L02", "L03", "L04", "L05", 
    "L08", "L10", "L11", "L12", "L13", "L20", "L21", "L22", "L23", 
    "L24", "L25", "L26", "L27", "L28", "L29", "L30", "L40", "L41", 
    "L43", "L44", "L50", "L51", "L52", "L53", "L55", "L57", "L59", 
    "L60", "L63", "L67", "L70", "L72", "L73", "L74", "L75", "L80", 
    "L81", "L83", "L84", "L85", "L87", "L88", "L89", "L90", "L91", 
    "L92", "L93", "L94", "L95", "L97", "L98", "M00", "M02", "M05", 
    "M06", "M08", "M10", "M11", "M12", "M13", "M15", "M16", "M17", 
    "M18", "M19", "M20", "M21", "M23", "M24", "M25", "M30", "M31", 
    "M32", "M33", "M34", "M35", "M40", "M41", "M42", "M43", "M45", 
    "M46", "M47", "M48", "M50", "M51", "M53", "M54", "M60", "M61", 
    "M62", "M65", "M66", "M67", "M70", "M71", "M72", "M75", "M76", 
    "M77", "M79", "M80", "M81", "M83", "M84", "M85", "M86", "M87", 
    "M88", "M89", "M91", "M92", "M93", "M94", "M95", "M99", "N00", 
    "N01", "N02", "N03", "N04", "N05", "N07", "N10", "N11", "N12", 
    "N13", "N14", "N15", "N17", "N18", "N19", "N20", "N21", "N23", 
    "N25", "N26", "N27", "N28", "N30", "N31", "N32", "N34", "N35", 
    "N36", "N39", "N40", "N41", "N42", "N43", "N44", "N45", "N46", 
    "N47", "N48", "N49", "N50", "N60", "N61", "N62", "N63", "N64", 
    "N70", "N71", "N72", "N73", "N75", "N76", "N80", "N81", "N82", 
    "N83", "N84", "N85", "N86", "N87", "N88", "N89", "N90", "N91", 
    "N92", "N93", "N94", "N95", "N97", "N98", "O00", "O01", "O02", 
    "O03", "O04", "O05", "O06", "O07", "O10", "O11", "O12", "O13", 
    "O14", "O15", "O16", "O20", "O21", "O22", "O23", "O24", "O25", 
    "O26", "O29", "O30", "O32", "O33", "O34", "O35", "O36", "O40", 
    "O41", "O42", "O43", "O44", "O45", "O46", "O60", "O61", "O62", 
    "O63", "O64", "O66", "O67", "O68", "O69", "O70", "O71", "O72", 
    "O73", "O74", "O75", "O80", "O85", "O86", "O87", "O88", "O89", 
    "O90", "O91", "O95", "O96", "O97", "O98", "O99", "P00", "P01", 
    "P02", "P03", "P04", "P05", "P07", "P08", "P10", "P11", "P12", 
    "P13", "P14", "P15", "P20", "P21", "P22", "P23", "P24", "P25", 
    "P26", "P27", "P28", "P29", "P35", "P36", "P37", "P38", "P39", 
    "P50", "P51", "P52", "P53", "P54", "P55", "P56", "P57", "P58", 
    "P59", "P60", "P61", "P70", "P71", "P72", "P74", "P76", "P77", 
    "P78", "P80", "P81", "P83", "P90", "P91", "P92", "P94", "P96", 
    "Q00", "Q01", "Q02", "Q03", "Q04", "Q05", "Q06", "Q07", "Q10", 
    "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q18", "Q20", "Q21", 
    "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q30", "Q31", 
    "Q32", "Q33", "Q34", "Q35", "Q36", "Q37", "Q38", "Q39", "Q40", 
    "Q41", "Q42", "Q43", "Q44", "Q45", "Q50", "Q51", "Q52", "Q53", 
    "Q54", "Q55", "Q56", "Q60", "Q61", "Q62", "Q63", "Q64", "Q65", 
    "Q66", "Q67", "Q68", "Q69", "Q70", "Q71", "Q72", "Q73", "Q74", 
    "Q75", "Q76", "Q77", "Q78", "Q79", "Q80", "Q81", "Q82", "Q83", 
    "Q84", "Q85", "Q86", "Q87", "Q89", "Q90", "Q91", "Q92", "Q93", 
    "Q95", "Q96", "Q97", "Q98", "Q99", "R95", "V01", "V02", "V03", 
    "V04", "V05", "V06", "V09", "V10", "V11", "V12", "V13", "V14", 
    "V15", "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", 
    "V24", "V25", "V26", "V27", "V28", "V29", "V30", "V31", "V32", 
    "V33", "V34", "V35", "V36", "V37", "V38", "V39", "V40", "V41", 
    "V42", "V43", "V44", "V45", "V46", "V47", "V48", "V49", "V50", 
    "V51", "V52", "V53", "V54", "V55", "V56", "V57", "V58", "V59", 
    "V60", "V61", "V62", "V63", "V64", "V65", "V66", "V67", "V68", 
    "V69", "V70", "V72", "V73", "V74", "V75", "V76", "V77", "V78", 
    "V79", "V80", "V81", "V82", "V83", "V84", "V85", "V86", "V87", 
    "V88", "V89", "V90", "V91", "V92", "V93", "V94", "V95", "V96", 
    "V97", "V98", "V99", "W00", "W01", "W02", "W03", "W04", "W05", 
    "W06", "W07", "W08", "W09", "W10", "W11", "W12", "W13", "W14", 
    "W15", "W16", "W17", "W18", "W19", "W20", "W21", "W22", "W23", 
    "W24", "W25", "W26", "W27", "W28", "W29", "W30", "W31", "W32", 
    "W33", "W34", "W35", "W36", "W37", "W38", "W39", "W40", "W41", 
    "W42", "W44", "W45", "W46", "W49", "W50", "W51", "W52", "W53", 
    "W54", "W55", "W56", "W57", "W58", "W59", "W60", "W64", "W65", 
    "W66", "W67", "W68", "W69", "W70", "W73", "W74", "W75", "W76", 
    "W77", "W78", "W79", "W80", "W81", "W83", "W84", "W85", "W86", 
    "W87", "W88", "W89", "W90", "W91", "W92", "W93", "W94", "W99", 
    "X00", "X01", "X02", "X03", "X04", "X05", "X06", "X08", "X09", 
    "X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", 
    "X19", "X20", "X21", "X22", "X23", "X24", "X25", "X26", "X27", 
    "X28", "X29", "X30", "X31", "X32", "X33", "X34", "X35", "X36", 
    "X37", "X38", "X39", "X40", "X41", "X42", "X43", "X44", "X45", 
    "X46", "X47", "X48", "X49", "X50", "X51", "X52", "X53", "X54", 
    "X57", "X58", "X59", "X60", "X61", "X62", "X63", "X64", "X65", 
    "X66", "X67", "X68", "X69", "X70", "X71", "X72", "X73", "X74", 
    "X75", "X76", "X77", "X78", "X79", "X80", "X81", "X82", "X83", 
    "X84", "X85", "X86", "X87", "X88", "X89", "X90", "X91", "X92", 
    "X93", "X94", "X95", "X96", "X97", "X98", "X99", "Y00", "Y01", 
    "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08", "Y09", "Y10", 
    "Y11", "Y12", "Y13", "Y14", "Y15", "Y16", "Y17", "Y18", "Y19", 
    "Y20", "Y21", "Y22", "Y23", "Y24", "Y25", "Y26", "Y27", "Y28", 
    "Y29", "Y30", "Y31", "Y32", "Y33", "Y34", "Y35", "Y36", "Y40", 
    "Y41", "Y42", "Y43", "Y44", "Y45", "Y46", "Y47", "Y48", "Y49", 
    "Y50", "Y51", "Y52", "Y53", "Y54", "Y55", "Y56", "Y57", "Y58", 
    "Y59", "Y60", "Y61", "Y62", "Y63", "Y64", "Y65", "Y66", "Y69", 
    "Y70", "Y71", "Y72", "Y73", "Y74", "Y75", "Y78", "Y80", "Y81", 
    "Y82", "Y83", "Y84", "Y85", "Y86", "Y87", "Y88", "Y89")
  empty=setdiff(seer,hcd) # SEER asks for 277 that hcd has nothing for. Only 2 are in C, C86 and C97, neither of which detangle anything in the hemes
  # [1] "A15" "A22" "A29" "A33" "A45" "A47" "A56" "A57" "A61" "A62" "A65" "A66" "A67" "A72" "A73" "A76" "A90" "A91" "B03" "B04" "B10" "B11" "B12" "B13" "B14" "B28" "B29" "B31" "B32"
  # [30] "B52" "B53" "B56" "B61" "B62" "B63" "B71" "B72" "B73" "B79" "B80" "B84" "B93" "B95" "B96" "B97" "B98" "C86" "C97" "D08" "I03" "I04" "I23" "I29" "I32" "I39" "I41" "I43" "I65"
  # [59] "I66" "I68" "I75" "I76" "J17" "N06" "O08" "O09" "O17" "O18" "O19" "O27" "O28" "O31" "O37" "O38" "O39" "O47" "O48" "O49" "O50" "O51" "O52" "O53" "O54" "O55" "O56" "O57" "O58"
  # [88] "O59" "O65" "O76" "O77" "O78" "O79" "O81" "O82" "O83" "O84" "O92" "O93" "O94" "P06" "P09" "P16" "P17" "P18" "P19" "P30" "P31" "P32" "P33" "P34" "P40" "P41" "P42" "P43" "P44"
  # [117] "P45" "P46" "P47" "P48" "P49" "P62" "P63" "P64" "P65" "P66" "P67" "P68" "P69" "P73" "P75" "P79" "P82" "P84" "P85" "P86" "P87" "P88" "P89" "P93" "P95" "Q08" "Q09" "Q17" "Q19"
  # [146] "Q29" "Q46" "Q47" "Q48" "Q49" "Q57" "Q58" "Q59" "Q88" "Q94" "R00" "R01" "R02" "R03" "R04" "R05" "R06" "R07" "R08" "R09" "R10" "R11" "R12" "R13" "R14" "R15" "R16" "R17" "R18"
  # [175] "R19" "R20" "R21" "R22" "R23" "R24" "R25" "R26" "R27" "R28" "R29" "R30" "R31" "R32" "R33" "R34" "R35" "R36" "R37" "R38" "R39" "R40" "R41" "R42" "R43" "R44" "R45" "R46" "R47"
  # [204] "R48" "R49" "R50" "R51" "R52" "R53" "R54" "R55" "R56" "R57" "R58" "R59" "R60" "R61" "R62" "R63" "R64" "R65" "R66" "R67" "R68" "R69" "R70" "R71" "R72" "R73" "R74" "R75" "R76"
  # [233] "R77" "R78" "R79" "R80" "R81" "R82" "R83" "R84" "R85" "R86" "R87" "R88" "R89" "R90" "R91" "R92" "R93" "R94" "R96" "R97" "R98" "R99" "U01" "U02" "U03" "V00" "V07" "V08" "V71"
  # [262] "W43" "W47" "W48" "W61" "W62" "W63" "W71" "W72" "W82" "W95" "W96" "W97" "W98" "X07" "X55" "X56"
  # x=ICD10$TBsyph
  ICD10=lapply(ICD10,function(x) setdiff(x,empty)) #remove empty asks
  seer=sort(unlist(ICD10,use.names = FALSE))
  setdiff(seer,hcd) # check, now empty
  ICD10$other=setdiff(hcd,seer) # hcd offers info on 629 that SEER never asks for, call all of these other deaths 
  ICD10$unknown="17"
  dplyr::tibble(type=names(ICD10),seerCode=COD,ICD10=ICD10)
}  

# Neoplasm Causes of Death
# Neoplasm Causes of Death;ICD-8;ICD-9 (1979-1998) #;ICD-10 (1999+) #;Recode
# All Malignant Cancers;;140-208, 238.6;C00-C97;--;
# Oral Cavity and Pharynx;
# Lip;;140;C00;20010;
# Tongue;;141;C01-C02;20020;
# Salivary Gland;;142;C07-C08;20030;
# Floor of Mouth;;144;C04;20040;
# Gum and Other Mouth;;143, 145;C03, C05-C06;20050;
# Nasopharynx;;147;C11;20060;
# Tonsil;;146.0-146.2;C09;20070;
# Oropharynx;;146.3-146.9;C10;20080;
# Hypopharynx;;148;C12-C13;20090;
# Other Oral Cavity and Pharynx;;149;C14;20100;
# Digestive System;
# Esophagus;;150;C15;21010;
# Stomach;;151;C16;21020;
# Small Intestine;;152;C17;21030;
# Colon and Rectum;
# Colon excluding Rectum;;153, 159.0;C18, C26.0;21040;
# Rectum and Rectosigmoid Junction;;154.0-154.1;C19-C20;21050;
# Anus, Anal Canal and Anorectum;;154.2-154.3, 154.8;C21;21060;
# Liver and Intrahepatic Bile Duct;
# Liver;;155.0, 155.2;C22.0, C22.2-C22.4, C22.7, C22.9;21071;
# Intrahepatic Bile Duct;;155.1;C22.1;21072;
# Gallbladder;;156.0;C23;21080;
# Other Biliary;;156.1-156.2, 156.8-156.9;C24;21090;
# Pancreas;;157;C25;21100;
# Retroperitoneum;;158.0;C48.0;21110;
# Peritoneum, Omentum and Mesentery;;158.8-158.9;C45.1+, C48.1-C48.2;21120;
# Other Digestive Organs;;159.8-159.9;C26.8-C26.9, C48.8;21130;
# Respiratory System;
# Nose, Nasal Cavity and Middle Ear;;160;C30-C31;22010;
# Larynx;;161;C32;22020;
# Lung and Bronchus;;162.2-162.5, 162.8-162.9;C34;22030;
# Pleura;;163;C38.4, C45.0+;22050;
# Trachea, Mediastinum and Other Respiratory Organs;;162.0, 164.2-164.3, 164.8-164.9, 165;C33, C38.1-C38.3, C38.8, C39;22060;
# Bones and Joints;;170;C40-C41;23000;
# Soft Tissue including Heart$;;164.1, 171;C47, C49, C38.0, C45.2+;24000;
# Skin&;
# Melanoma of the Skin;;172;C43;25010;
# Non-Melanoma Skin&;;173;C44, C46+;25020;
# Breast;;174-175;C50;26000;
# Female Genital System;
# Cervix Uteri;;180;C53;27010;
# Corpus and Uterus, NOS;
# Corpus Uteri;;182;C54;27020;
# Uterus, NOS;;179;C55;27030;
# Ovary;;183.0;C56;27040;
# Vagina;;184.0;C52;27050;
# Vulva;;184.1-184.4;C51;27060;
# Other Female Genital Organs;;181, 183.2-183.5, 183.8-183.9, 184.8-184.9;C57-C58;27070;
# Male Genital System;
# Prostate;;185;C61;28010;
# Testis;;186;C62;28020;
# Penis;;187.1-187.4;C60;28030;
# Other Male Genital Organs;;187.5-187.9;C63;28040;
# Urinary System;
# Urinary Bladder;;188;C67;29010;
# Kidney and Renal Pelvis;;189.0-189.1;C64-C65;29020;
# Ureter;;189.2;C66;29030;
# Other Urinary Organs;;189.3-189.4, 189.8-189.9;C68;29040;
# Eye and Orbit;;190;C69;30000;
# Brain and Other Nervous System;;191, 192;C70, C71, C72;31010;
# Endocrine System;
# Thyroid;;193;C73;32010;
# Other Endocrine including Thymus$;;164.0, 194;C37, C74-C75;32020;
# Lymphoma;
# Hodgkin Lymphoma;;201;C81;33010;
# Non-Hodgkin Lymphoma;;200, 202.0-202.2, 202.8-202.9;C82-C86, C96.3;33040;
# Myeloma;;203.0, 238.6;C90.0, C90.2, C90.3;34000;
# Leukemia;
# Lymphocytic Leukemia;
# Acute Lymphocytic Leukemia;;204.0;C91.0;35011;
# Chronic Lymphocytic Leukemia;;204.1;C91.1;35012;
# Other Lymphocytic Leukemia;;202.4, 204.2, 204.8-204.9;C91.2-C91.4, C91.6-C91.9;35013;
# Myeloid and Monocytic Leukemia;
# Acute myeloid;;205.0, 207.0, 207.2;C92.0, C92.4-C92.6, C92.8, C94.0, C94.2;35021;
# Acute Monocytic Leukemia;;206.0;C93.0;35031;
# Chronic Myeloid Leukemia;;205.1;C92.1;35022;
# Other Myeloid/Monocytic Leukemia;;205.2-205.3, 205.8-205.9, 206.1-206.2, 206.8-206.9;C92.2-C92.3, C92.7, C92.9, C93.1-C93.3, C93.7, C93.9;35023;
# Other Leukemia;
# Other Acute Leukemia;;208.0;C94.4, C94.5, C95.0;35041;
# Aleukemic, subleukemic and NOS;;203.1, 207.1, 207.8, 208.1-208.2, 208.8-208.9;C90.1, C91.5, C94.1, C94.3, C94.7, C95.1, C95.2, C95.7, C95.9;35043;
# Mesothelioma (ICD-10 only)+;;N/A;C45+;36010;
# Kaposi Sarcoma (ICD-10 only)+;;N/A;C46+;36020;
# Miscellaneous Malignant Cancer;;159.1, 195-199, 202.3, 202.5-202.6, 203.8;C26.1, C45.7+, C45.9+, C76-C80, C88, C94.6, C96.0-C96.2, C96.4-C96.9, C97;37000;
# In situ, benign or unknown behavior neoplasm;;210-237, 238.0-238.5, 238.7-238.9, 239;D00-D48 ;38000;
# 
# 
# Non-Neoplasm Causes of Death
# Non-Neoplasm Causes of Death;ICD-8;ICD-9 (1979-1998) #;ICD-10 (1999+) #;Recode
# Tuberculosis;010-018;010-018;A15-A19 ;50000;
# Syphilis;090-097;090-097;A50-A53 ;50010;
# Human Immunodeficiency Virus (HIV) (1987+);N/A;042-044;B20-B24 ;50040%;
# Septicemia;038;038;A40-A41 ;50030;
# Other Infectious and Parasitic Diseases;001-009, 020-037, 039-043, 045-065, 067-076, 078-089, 098-130.1, 130.3-136;001-009, 020-037, 039-041, 045-088, 098-139;A00-A08, A20-A33, A35-A39, A42-A49, A54-B19, B25-B99;50040%;
# Diabetes Mellitus;250;250;E10-E14 ;50050;
# Alzheimers (ICD-9 and 10 only);N/A;331.0;G30 ;50051;
# Diseases of Heart;390-398, 402, 404, 410-429;390-398, 402, 404, 410-429;I00-I09, I11, I13, I20-I51 ;50060;
# Hypertension without Heart Disease;400-401, 403;401, 403;I10, I12 ;50070;
# Cerebrovascular Diseases;430-438;430-438;I60-I69 ;50080;
# Atherosclerosis;440;440;I70 ;50090;
# Aortic Aneurysm and Dissection;441;441;I71 ;50100;
# Other Diseases of Arteries, Arterioles, Capillaries;442-448;442-448;I72-I78;50110;
# Pneumonia and Influenza;470-474, 480-486;480-487;J09-J18 ;50120;
# Chronic Obstructive Pulmonary Disease and Allied Cond;490-493, 519.3;490-496;J40-J47 ;50130;
# Stomach and Duodenal Ulcers;531-533;531-533;K25-K28 ;50140;
# Chronic Liver Disease and Cirrhosis;571;571;K70, K73-K74 ;50150;
# Nephritis, Nephrotic Syndrome and Nephrosis;580-584, 593.0-593.3, 593.5;580-589;N00-N07, N17-N19, N25-N27 ;50160;
# Complications of Pregnancy, Childbirth, Puerperium;630-678;630-676;A34, O00-O95, O98-O99 ;50170;
# Congenital Anomalies;740-759;740-759;Q00-Q99 ;50180;
# Certain Conditions Originating in Perinatal Period;760-779;760-779;P00-P96 ;50190;
# Symptoms, Signs and Ill-Defined Conditions;780-796;780-799;R00-R99 ;50200;
# Accidents and Adverse Effects;800-949*;800-949*;V01-X59, Y85-Y86 ;50210;
# Suicide and Self-Inflicted Injury;950-959*;950-959*;U03, X60-X84, Y87.0 ;50220;
# Homicide and Legal Intervention;960-978*;960-978*;U01-U02, X85-Y09, Y35, Y87.1, Y89.0 ;50230;
# Other Cause of Death;--;--;--;50300;
# 
# 
# 
# Codes Not Indicating Causes of Death
# Codes Not Indicating Causes of Death^;Recode
# Alive;00000^;
# State DC not available or state DC available but no COD;41000^;
# Unknown/missing/invalid COD;99999^;
