(* ::Package:: *)

BeginPackage["Amplitudes`"]

RetLVecHeads::usage="Returns the contents of LVecHeads, the Heads of all Lorentz vectors.";
LVecQ::usage="LVecQ[vec] returns true if vec is in LVecHeads and false otherwise.";
AddLVecHeads::usage="AddLVecHeads[p] and AddLVecHeads[p, e...] adds p (and e) to LVecHeads, the list of all Heads of Lorentz vectors.  p[1], p[2]... will all be vaild LVecs.";
DelLVecHeads::usage="DelLVecHeads[p] deletes p from LVecHeads, the list of Lorentz vector Heads.";
d::usage="d[p1,p2] is the Lorentz dot product of p1 and p2.  If the input is numerical vectors, then d returns the product with (-+++...) with a mostly positive metric signature unless MetricSignature is set to -1.";
d2::usage="d2[p] is the Lorentz dot product p^2 and is equivalent to d[p,p].";
PBasis::usage="PBasis[{p1,p2,...}] returns a minimal set of Mandelstams d[pi,pj].";
PAndEBasis::usage="PAndEBasis[{p1,p2,...},{e1,e2,...}] returns a minimal set of Mandelstams d[pi,pj], d[pi,ej], and d[ei,ej].";
PAndEAndEBarBasis::usage="PAndEAndEBarBasis[{p1,p2...},{e1,e2...},{eB1,eB2...}] returns a minimal set of Mandelstams d[pi,pj], d[pi,ej], d[pi,eBj], d[ei,ej], d[eBi,eBj], and d[ei,eBj].";
RepPAnalytical::usage="RepPAnalytical[{p1,p2,...},m:0] returns the set of replacement rules that will reduce any Mandelstams d[pi,pj] to the basis Mandelstams given by PBasis.";
RepPAndEAnalytical::usage="RepPAndEAnalytical[{p1,p2,...},{e1,e2,...},m:0] returns the set of replacement rules that will reduce any Mandelstams to the basis Mandelstams given by PAndEBasis.";
RepPAndEAndEBarAnalytical::usage="RepPAndEAndEBarAnalytical[{p1,p2...},{e1,e2...},{eB1,eB2...},m:0] returns the set of replacement rules that will reduce any Mandelstams to the basis Mandelstams given by PAndEAndEBarBasis.";
RepPNumerical::usage="RepPNumerical[{p1,p2,...},m:0] returns a set of numerical replacement rules for the Mandelstams d[pi,pj].  The characteristic numerical value of the Mandelstams is set by the global variable RndMax.  The numerical values tend to be positive and non-zero to avoid accidentally hitting a pole.";
RepPAndENumerical::usage="RepPAndENumerical[{p1,p2,...},{e1,e2,...},m:0] returns a set of numerical replacement rules for Mandelstams.  The characteristic numerical value of the Mandelstams is set by the global variable RndMax.  The numerical values tend to be positive and non-zero to avoid accidentally hitting a pole.";
RepPAndEAndEBarNumerical::usage="RepPAndEAndEBarNumerical[{p1,p2...},{e1,e2...},{eB1,eB2...},m:0] returns a set of numerical replacement rules for Mandelstams.  The characteristic numerical value of the Mandelstams is set by the global variable RndMax.  The numerical values tend to be positive and non-zero to avoid accidentally hitting a pole.";
Spaa::usage="Spaa[x,y] is the 4D double angle spinor product <xy>.";
Spbb::usage="Spbb[x,y] is the 4D double brack spinor product [xy].";
SpBasis::usage="SpBasis[SpinorList] returns an independent set of Spaa's and Spbb's.";
RepSpAnalytical::usage="RepSpAnalytical[SpinorList] returns a list of replacement rules that will analytically reduce the Spaa's and Spbb's to the basis given by SpBasis.  These replacement rules solve momentum conservation and the Schouten identity.";
RepSpNumerical::usage="RepSpNumerical[SpinorList] is a list of replacement rules that replaces all Spaa's and Spbb's with rational numbers of order RndMax where RndMax is a global variable.";
DDMTofabc::usage="DDMTofabc[expr,cHead] converts an expression from DDM half ladder color factors DDM[a1,a2,a3,a4,a5,...] to fabc[a1,a2,cHead[1]]fabc[cHead[1],a3,cHead[2]]...";
DDMBasis::usage="DDMBasis[VarList,LeftAnchor,RightAnchor] generates a list of DDM half ladder (multiperipheral form) color factors {DDM[LeftAnchor, sigma, RightAnchor]} where sigma runs over the permutations of the remaining variables in VarList";
fabcToDDMAnalytical::usage="fabcToDDMAnalytical[expr,LeftAnchor,RightAnchor] takes an expression of fabc[x1,y1,z1]fabc[x2,y2,z2]... and converts it into a linear combination of DDM half ladder (multiperipheral form) basis elements of the form DDM[LeftAnchor,...,RightAnchor]";
fabcToDDMNumerical::usage="fabcToDDMNumerical[expr,LeftAnchor,RightAnchor] takes an expression of fabc[x1,y1,z1]fabc[x2,y2,z2]... and converts it into a linear combination of DDM half ladder (multiperipheral form) basis elements of the form DDM[LeftAnchor,...,RightAnchor] and then replaces the DDM basis elements with random numbers in the range {1,RndMax} where RndMax is a global variable";
RepDDMNumerical::usage="RepDDMNumerical[VarList,LeftAnchor,RightAnchor] returns a list of replacement rules to replace the DDM basis given by DDMBasis[VarList,LeftAnchor,RightAnchor] with numbers in the range {1,RndMax} where RndMax is a global variable";
DDMToTrSUN::usage="DDMToTrSUN[expr] takes an expression of DDM[a1,a2,a3...] and converts it into a linear combination of tr[...] using the normalizations of Elvang and Huang BUT DROPPING ALL FACTORS OF i as in the appendix to Carrasco's TASI lectures.";
DDMToTrUN::usage="DDMToTrUN[expr] takes an expression of DDM[a1,a2,a3...] and converts it into a linear combination of tr[...] using the normalizations of Elvang and Huang BUT DROPPING ALL FACTORS OF i as in the appendix to Carrasco's TASI lectures.";
DDM::usage = "DDM[a1,a2,...] is the Head used to represent the DDM half ladder/multiperipheral form color factors.";
fabc::usage="fabc[a,b,c] is the Head used to represent the structure constant fabc.";
dabc::usage="dabc[a,b,c] is the Head used to represent the structure constant dabc.";
fabcToTr::usage="fabcToTr[expr] converts fabc[...] to tr[...].";
dabcToTr::usage="dabcToTr[expr] converts dabc[...] to tr[...].";
tr::usage="tr[a1,a2,a3...] is the Head used to represent the U(N) or SU(N) trace over the generators Tr[T^a1 T^a2...]";
Nc::usage="Nc is the Symbol used to represent the number of colors in U(Nc) or SU(Nc).";
TrCanonicalizeSUN::usage="TrCanonicalizeSUN[expr] expands expr, converts objects like tr[...]tr[...] to tr[...] where possible, and canonicalizes all traces.  The traces are assumed to act on generators of SU(N).";
TrCanonicalizeUN::usage="TrCanonicalizeUN[expr] expands expr, converts objects like tr[...]tr[...] to tr[...] where possible, and canonicalizes all traces.  The traces are assumed to act on generators of U(N).";
ListMult::usage="ListMult[list1,list2] returns the pairwise multiplication of the lists and deletes the duplicates";
ListPower::usage="ListPower[list,n] returns the pairwise multiplication of list with itself n times";
RepList::usage="RepList[ListOld,ListNew] generates a set of replacement rules to replace the entries of ListOld with the entries of ListNew";
RepListWithRndNums::usage="RepListWithRndNums[list] maps every element of 'list' to a separate random number in the range {1,RndMax}.";
CyclicPermP::usage="CyclicPermP[expr,pList,cycle] cycliclally permutes the momenta in the expression and returns the result.  Cycle=0 returns the original expression, cycle=1 permutes the momenta once 1->2, 2->3, etc.";
CyclicPermPAndE::usage="CyclicPermPAndE[expr,pList,eList,cycle] cycliclally permutes the momenta and polarization vectors in the expression and returns the result.  Cycle=0 returns the original expression, cycle=1 permutes the momenta once 1->2, 2->3, etc.";
CyclicSumP::usage="CyclicSumP[expr,pList,MaxCycle] computes the cyclic sum of the expression over the momenta.  MaxCycle=1 just returns the original expression, MaxCycle=2 returns the original expression plus the expression with the momenta cycled one position to the left, etc.";
CyclicSumPAndE::usage="CyclicSumPAndE[expr,pList,eList,MaxCycle] computes the cyclic sum of the expression over the momenta and polarization vectors.  MaxCycle=1 just returns the original expression, MaxCycle=2 returns the original expression plus the expression with the kinematics cycled one position to the left, etc.";
PermSumP::usage="PermSumP[expr,pList] computes the permutation sum of the expression over all permutations of the momenta in pList";
BCJp::usage="BCJp[amp,pList] returns the fundamental BCJ relation of the amplitude (amp) that depends on momenta (pList).";
BCJpe::usage="BCJpe[amp,pList,eList] returns the fundamental BCJ relation of the amplitude (amp) that depends on momenta (pList) and polarization vectors (eList).";
KLT::usage="KLT[LAmpHead, RAmpHead, pHead, n] returns the n-pt KLT product.  pHead is the Head of the momentum label, e.g., if momenta look like p[1], p[17]... then pHead is p.  LAmpHead is the Head such that LAmpHead[{p[1],p[7]...}] returns the desired color-ordered amplitude.  So for example LAmpHead could be NLSM or YM or BS, etc.  Of course YM carries polarization labels so in the end you have to apply some replacement rule like YM[pList_]:>YM[pList,e/@pList].  RAmpHead behaves just like LAmpHead.";
ContractMu::usage="ContractMu[mu,dim][expr] will expand the expression and contract mu indices.  For example ContractMu[mu,dim][d[mu[1],x]d[mu[1],y]] returns d[x,y] and ContractMu[mu,dim][d[mu[1],mu[1]] returns dim (the spacetime dimension).";

RndMax=10;
RndMax::usage="RndMax controls the size of the random numbers generated in this package.  Its default value is 10 but you can change that if you want.  All of the numerical functions in this package depend implicitly/internally on RndMax.";

MetricSignature=1;
MetricSignature::usage="MetricSignature controls the sign of the signature of d[x,y].  MetricSignature defaults to +1 for a mostly plus metric but you can set it to -1 for a mostly minus metric."


Begin["`Private`"]


(*---Define LVecList---*)
LVecHeads={};
RetLVecHeads:=LVecHeads;
LVecQ[vec_]:=MemberQ[LVecHeads,Head[vec]]; 
AddLVecHeads[p__]:=(LVecHeads=Union[LVecHeads,{p}];);
DelLVecHeads[p_]:=(LVecHeads=DeleteCases[LVecHeads,p];);



(*---Define the Lorentz product---*)
SetAttributes[d,Orderless];
d[p1_Plus,p2_]:=Map[d[#,p2]&,p1];
d[p1_,num__ p2_?LVecQ]:=num d[p1,p2];
d[p1_,num__?(FreeQ[#,_?LVecQ]&) p2_Plus]:=num d[p1,p2];
d[p1:{__},p2:{__}]:=MetricSignature*(p1 . p2-2p1[[1]]p2[[1]]);
d2[p_]:=d[p,p];
d2[p:{__}]:=MetricSignature*(p . p-2p[[1]]^2);



(*---Mandelstam basis generation---*)
PBasis[pList_]:=Module[{n,pp,ret},n=Length[pList];
pp[i_,j_]:=d[pList[[i]],pList[[j]]];
ret=Table[pp[1,i],{i,3,n-1}];(*Fix p1.p2 using -m^2=pn^2=(p1+p2+...)^2*)
Join[ret,Flatten[Table[pp[i,j],{i,2,n-2},{j,i+1,n-1}]]](*Use momentum convservation 0=sum_{j=1...n} (pj.pi) to fix pn.pi=-(p1.pi+p2.pi...)*)
(*The diagonal is fixed via pi^2=-m^2.*)];

PAndEBasis[pList_,eList_]:=Module[{n,pe,ee,ret,peInclude},n=Length[pList];
pe[i_,j_]:=d[pList[[i]],eList[[j]]];
ee[i_,j_]:=d[eList[[i]],eList[[j]]];
ret=PBasis[pList];
ret=Flatten[Join[ret,Table[ee[i,j],{i,1,n},{j,i+1,n}]]];(*Don't include ei^2 since (by multilinearity) it never apears in an amplitude and furthermore it is either 0 or 1 depending on the normalization convention.*)
peInclude=ConstantArray[1,{n,n}];
peInclude=peInclude-IdentityMatrix[n];(*Take care of pi.ei=0*)
peInclude[[1]][[n]]=0;(*From pn.en=0 and momentum conservation we get p1.en=-(p2.en+p3.en...p_{n-1}.en)*)
Table[peInclude[[n]][[i]]=0,{i,1,n}];(*From momentum conservation sum_{j} pj.ei=0 so pn.ei=-sum_{j\[NotEqual]n} pj.ei*)
DeleteCases[Flatten[Join[ret,Table[peInclude[[i]][[j]]pe[i,j],{i,1,n},{j,1,n}]]],0]
];

PAndEAndEBarBasis[pList_,eList_,eBarList_]:=Join[PAndEBasis[pList,eList],PAndEBasis[pList,eBarList],Outer[d,eList,eBarList]//Flatten//Complement[#,MapThread[d,{eList,eBarList}]]&]//DeleteDuplicates;

RepPAnalytical[pList_,m_:0]:=Module[{n,pp,ret},n=Length[pList];
pp[i_,j_]:=d[pList[[i]],pList[[j]]];
ret=Table[pp[i,i]->-m^2,{i,1,n}];(*The diagonal is fixed via pi^2=-m^2.*)
AppendTo[ret,pp[1,2]->m^2 (n-2)/2-Sum[pp[1,j],{j,3,n-1}]-Sum[pp[i,j],{i,2,n-1},{j,i+1,n-1}]];(*Fix p1.p2 using -m^2=pn^2=(p1+p2+...)^2*)
ret=Flatten[Join[ret,Table[pp[n,i]->-Sum[pp[j,i],{j,1,n-1}]/.ret,{i,1,n-1}]]];(*Use momentum convservation 0=sum(pi.pj) to fix pn.pi=-(p1.pi+p2.pi...)*)
DeleteCases[ret,0->0](*If m=0 and pi is a NullLVec then d[p1,p1] is 0 automatically so you don't want to add the rule 0\[Rule]0*)];

RepPAndEAnalytical[pList_,eList_,m_:0]:=Module[{n,pe,ret},n=Length[pList];
pe[i_,j_]:=d[pList[[i]],eList[[j]]];
ret=RepPAnalytical[pList,m];
(*Don't include ei^2 since (by multilinearity) it never apears in an amplitude and furthermore it is either 0 or 1 depending on the normalization convention.*)
ret=Flatten[Join[ret,Table[pe[i,i]->0,{i,1,n}]]];(*Take care of pi.ei=0*)
AppendTo[ret,pe[1,n]->-Sum[pe[i,n],{i,2,n-1}]];
(*From pn.en=0 and momentum conservation we get p1.en=-(p2.en+p3.en...p_{n-1}.en)*)
Flatten[Join[ret,Table[pe[n,i]->-Sum[pe[j,i],{j,1,n-1}]/.pe[i,i]->0,{i,1,n-1}]]](*From momentum conservation sum_{j} pj.ei=0 so pn.ei=-sum_{j\[NotEqual]n} pj.ei*)];

RepPAndEAndEBarAnalytical[pList_,eList_,eBarList_,m_:0]:=Join[RepPAndEAnalytical[pList,eList,m],RepPAndEAnalytical[pList,eBarList,m]]//DeleteDuplicates;

(*The reason you take random ints from 1 to RndMax is that almost all of the Mandelstams are positive.  This means that it is very unlikely to have a propagator that is identically zero.  Most of the negative Mandelstams will come from conserving momentum so those negative Mandelstams will never make a propagator negative since you'd never have a propagator involving all the momenta since that'd be identically zero no matter what.*)
RepListWithRndNums[list_]:=MapThread[Rule,{list,RandomInteger[{1,RndMax},Length[list]]}];

RepPNumerical[pList_,m_:0]:=Module[{basis,ret},
basis=PBasis[pList];
ret=RepListWithRndNums[basis];
Join[ret,RepPAnalytical[pList,m]/.ret]
];

RepPAndENumerical[pList_,eList_,m_:0]:=Module[{basis,ret},
basis=PAndEBasis[pList,eList];
ret=RepListWithRndNums[basis];
Join[ret,RepPAndEAnalytical[pList,eList,m]/.ret]
];

RepPAndEAndEBarNumerical[pList_,eList_,eBarList_,m_:0]:=Module[{basis,ret},
basis=PAndEAndEBarBasis[pList,eList,eBarList];
ret=RepListWithRndNums[basis];
Join[ret,RepPAndEAndEBarAnalytical[pList,eList,eBarList,m]/.ret]
];



(*---4D spinors---*)
Spaa[x_,y_]:=-Spaa[y,x]/;!OrderedQ[{x,y}];(*Implement antisymmetry*)
Spbb[x_,y_]:=-Spbb[y,x]/;!OrderedQ[{x,y}];
Spaa[x__,x__]:=0;
Spbb[x__,x__]:=0;

SpBasis[s_]:=
Module[{SpAA,SpBB},
SpAA[x_,y_]:=Spaa[s[[x]],s[[y]]];
SpBB[x_,y_]:=Spbb[s[[x]],s[[y]]];Join[Flatten[Join[{SpAA[1,2]},Table[{SpAA[1,i],SpAA[2,i]},{i,3,Length[s]}]]],Flatten[Join[{SpBB[1,2]},Table[{SpBB[1,i],SpBB[2,i]},{i,5,Length[s]}]]]]];(*This is the list of spinors I'll use as a basis*)

SpSchouten[s_]:=Flatten[Map[Table[Rule[#[s[[i]],s[[j]]],(#[s[[1]],s[[i]]]#[s[[2]],s[[j]]]-#[s[[1]],s[[j]]]#[s[[2]],s[[i]]])/#[s[[1]],s[[2]]]],{i,3,Length[s]-1},{j,i+1,Length[s]}]&,{Spaa,Spbb}]];

SpConserveP[s_]:=Module[{SpAA,SpBB,expr},
SpAA[x_,y_]:=Spaa[s[[x]],s[[y]]];
SpBB[x_,y_]:=Spbb[s[[x]],s[[y]]];expr[a_,b_,x_,y_]:=(SpAA[y,b]SpAA[x,y]SpBB[y,x]+SpAA[y,b]Sum[SpAA[x,i]SpBB[i,x],{i,5,Length[s]}]-SpAA[x,b]Sum[SpAA[y,i]SpBB[i,x],{i,5,Length[s]}])/(SpAA[x,a]SpAA[y,b]-SpAA[x,b]SpAA[y,a]);
{SpBB[1,3]->expr[3,4,1,2],SpBB[1,4]->expr[4,3,1,2],SpBB[2,3]->expr[3,4,2,1],SpBB[2,4]->expr[4,3,2,1]}];

RepSpAnalytical[s_]:=Flatten[Join[SpSchouten[s]/.SpConserveP[s],SpConserveP[s]]];

RepSpNumerical[s_]:=Module[{NumRepBasis,basis},
basis=DeleteCases[SpBasis[s],Spaa[s[[1]],s[[3]]]];(*If <13><24> = <23><14> you'll divide by zero when conserving momentum so we'll avoid that by taking <13> \[NotEqual] <23><14>/<24> and the rest to be integers greather than 1.*)
NumRepBasis=RepListWithRndNums[basis];
NumRepBasis=Flatten[Join[NumRepBasis,{Spaa[s[[1]],s[[3]]]->RandomChoice[DeleteCases[Range[RndMax],Spaa[s[[2]],s[[3]]]Spaa[s[[1]],s[[4]]]/Spaa[s[[2]],s[[4]]]/.NumRepBasis]]}]];
Flatten[Join[RepSpAnalytical[s]/.NumRepBasis,NumRepBasis]]];

(*I still need to implement the rule d[a,b]\[Rule]1/2Spaa[a,b]Spbb[a,b].*)



(*---Define the DDM half ladder basis/multiperipheral form for color structues---*)

Clear[CanonicalizeListRotation];
CanonicalizeListRotation[list_]:=
Module[{i},Table[RotateLeft[list,i],{i,1,Length[list]}]//Sort//First
];

tr[x__]:=tr@@CanonicalizeListRotation[{x}]/;{x}=!=CanonicalizeListRotation[{x}];

fabcToTr[expr_]:=expr/.fabc[a_,b_,c_]:>tr[a,b,c]-tr[b,a,c];
dabcToTr[expr_]:=expr/.dabc[a_,b_,c_]:>tr[a,b,c]+tr[b,a,c];

DDMTofabc[expr_,cHead_]:=
expr/.DDM[x__]:>Module[{cList},
cList=Array[cHead,Length[{x}]-3];
Apply[Times,Map[Apply[fabc,#]&,Partition[Flatten[Join[{{x}[[1]]},Riffle[{x}[[2;;-3]],Map[{#,#}&,cList]],{x}[[-2;;]]]],3]]]
];

DDMBasis[VarList_,LeftAnchor_,RightAnchor_]:=
Map[Apply[DDM,Join[{LeftAnchor},#,{RightAnchor}]]&,Permutations[DeleteCases[DeleteCases[VarList,LeftAnchor],RightAnchor]]];

fabcToDDMAnalytical[expr_,LeftAnchor_,RightAnchor_]:=Module[{ret,fProd,NewMiddle,DealWithMiddle,SignFlipMiddle},
ret=expr;
ret=ret//.x_*y_:>Map[Times[#,x]&,y]/;(Head[y]===Plus&&!FreeQ[x,fabc]&&!FreeQ[y,fabc]);(*This expands all necessary products invovling fabc's*)
ret=ret/.fabc[x__]:>fProd[{x}]//.fProd[x__]fProd[y__]:>fProd[x,y];(*Convert fabc's to fProds's*)
ret=ret/.fProd[x__]:>fProd[{},{},{x}];

SignFlipMiddle=fProd[{left___},{middle__},{right___}]:>Module[{a,b,c,ConnectedTob,sign,triple},
a=middle[[1]];
b=middle[[2]];
c=middle[[3]];
ConnectedTob=FixedPoint[Sort[DeleteDuplicates[Flatten[Table[Select[{right},MemberQ[#,elm]&],{elm,#}]]]]&,{b}];
If[MemberQ[Join[ConnectedTob,{b}],RightAnchor],
sign=-1;triple={a,c,b};,
sign=+1;triple={a,b,c};];
sign fProd[{left},{triple},{right}]
];

NewMiddle=fProd[{left___},{},{right__}]:>Module[{triple,NewRight,anchor},
anchor=If[left===BlankSequence[],LeftAnchor,Last[Last[{left}]]];
triple={right}[[(Select[Position[{right},anchor],Length[#]==2&][[1,1]])]];(*You have to use all of these Select statements for a hackish reason.  Asking for the position of 1 in {1,c[1]} returns two results and you use Select to get the 1 (the desired answer) and drop the c[1] where the 1 is nested.*)
NewRight=Complement[{right},{triple}];
triple=RotateLeft[triple,Select[Position[triple,anchor],Length[#]==1&][[1]]-1];
fProd[{left},{triple},NewRight]/.SignFlipMiddle];

DealWithMiddle=fProd[{left___},{middle__},{right___}]:>Module[{a,b,c,ExternalLegs,RightTriple,RemainingRight,x,y},
a=middle[[1]];
b=middle[[2]];
c=middle[[3]];
ExternalLegs=Map[First[#]&,Select[Tally[Flatten[Join[{left},{middle},{right}]]],Last[#]==1&]];
If[MemberQ[ExternalLegs,b],
fProd[Join[{left},{middle}],{},{right}],
RightTriple={right}[[Select[Position[{right},b],Length[#]==2&][[1,1]]]];
RemainingRight=Complement[{right},{RightTriple}];
RightTriple=RotateLeft[RightTriple,Select[Position[RightTriple,b],Length[#]==1&][[1]]-1];
x=RightTriple[[2]];
y=RightTriple[[3]];
(fProd[{left},{{a,y,b}},Join[{{b,c,x}},RemainingRight]]+fProd[{left},{{a,x,b}},Join[{{b,y,c}},RemainingRight]])/.SignFlipMiddle
]
];
ret=ret//.{NewMiddle,DealWithMiddle};
ret/.fProd[x__,{},{}]:>Apply[DDM,Map[First[#]&,Select[Tally[Flatten[x]],Last[#]==1&]]]
];

fabcToDDMNumerical[expr_,LeftAnchor_,RightAnchor_]:=Module[{ret,DDMVars},
ret=fabcToDDMAnalytical[expr,LeftAnchor,RightAnchor];
DDMVars=Apply[FullForm[ret][[##]]&,Drop[FirstPosition[FullForm[ret],DDM],-1]]/.DDM[x__]:>DeleteCases[DeleteCases[{x},LeftAnchor],RightAnchor];
ret/.RepListWithRndNums[DDMBasis[DDMVars,LeftAnchor,RightAnchor]]
];

RepDDMNumerical[VarList_,LeftAnchor_,RightAnchor_]:=RepListWithRndNums[DDMBasis[VarList,LeftAnchor,RightAnchor]];


SimplificationRulesTrSUN={
tr[w___,a_]^2:>tr[w,w]-1/Nc tr[w]^2,

tr[a_,w___]^2:>tr[w,w]-1/Nc tr[w]^2,

Times[f1___,HoldPattern[tr[w___,a_,x___]],f2___,HoldPattern[tr[y___,a_,z___]],f3___]:>Times[f1,f2,f3,tr[x,w,z,y]-1/Nc tr[x,w]tr[z,y]],

HoldPattern[tr[x___,a_,y___,a_,z___]]:>tr[x,z]tr[y]-1/Nc tr[x,y,z],

tr[a_,a_]:>Nc^2-1,

tr[]:>Nc,

tr[a_]:>0
};
(*See the end of JJ's TASI notes https://arxiv.org/pdf/1506.00974*)


SimplificationRulesTrUN={
tr[w___,a_]^2:>tr[w,w],

tr[a_,w___]^2:>tr[w,w],

Times[f1___,HoldPattern[tr[w___,a_,x___]],f2___,HoldPattern[tr[y___,a_,z___]],f3___]:>Times[f1,f2,f3,tr[x,w,z,y]],

HoldPattern[tr[x___,a_,y___,a_,z___]]:>tr[x,z]tr[y],

tr[a_,a_]:>Nc^2,

tr[]:>Nc
};
(*See https://scipp.ucsc.edu/~haber/ph218/sunid17.pdf and then remember that SU(N) has N^2-1 traceless hermitian generators and U(N) has N^2 hermitian (not necessarily traceless) generators.*)


TrCanonicalizeHelper[expr_,rules_]:=Module[{repRules,SmartExpand},
repRules[x_]:=Fold[#1/.#2&,x,rules];
(*FixedPoint[(#//Expand//repRules)&,expr]*)
(*There's a smarter way to do this Expand in fabcToDDMAnalytical*)
SmartExpand[expression_]:=expression//.x_*y_:>Map[Times[#,x]&,y]/;(Head[y]===Plus&&!FreeQ[x,tr]&&!FreeQ[y,tr]);
FixedPoint[(#//SmartExpand//repRules)&,expr]
];


TrCanonicalizeSUN[expr_]:=TrCanonicalizeHelper[expr,SimplificationRulesTrSUN];
TrCanonicalizeUN[expr_]:=TrCanonicalizeHelper[expr,SimplificationRulesTrUN];


DDMToTrSUN[expr_]:=Module[{c},
expr/.DDM[x__]:>(DDM[x]//DDMTofabc[#,c]&//fabcToTr//TrCanonicalizeSUN)
];

DDMToTrUN[expr_]:=Module[{c},
expr/.DDM[x__]:>(DDM[x]//DDMTofabc[#,c]&//fabcToTr//TrCanonicalizeUN)
];


(*---List product and power functions---*)

ListMult[list1_,list2_]:=Times[list1,#]&/@list2//Flatten//DeleteDuplicates;
ListMult[list_,{}]:=list;
ListMult[{},list_]:=list;
ListPower[list_,0]:={};
ListPower[list_,k_]:=ListMult[ListPower[list,k-1],list];(*This recursive definition with the ugly DeleteDuplicates is somehow 100 to 1000 times faster than anything else I could make especially for small powers*)
(*Cliff also suggested a different fast way of generating a mandelstam basis which is to sum up all of the PBasis elements, raise the sum to a power, and then call Expand.*)


(*---Replacement functions, cyclic sums, and fundamental BCJ ---*)

RepList[ListOld_,ListNew_]:=Dispatch[MapThread[Rule,{ListOld,ListNew}]];

PermSumP[expr_,pList_]:=Sum[expr/.RepList[pList,perm],{perm,Permutations[pList]}];

CyclicPermP[x_,pList_,Cycle_]:=Module[{pNew},
pNew=RotateLeft[pList,Cycle];
x/.RepList[pList,pNew]];

CyclicPermPAndE[x_,pList_,eList_,Cycle_]:=Module[{pNew,eNew},
pNew=RotateLeft[pList,Cycle];
eNew=RotateLeft[eList,Cycle];
x/.RepList[Join[pList,eList],Join[pNew,eNew]]];

CyclicSumP[x_,pList_,MaxCycle_]:=
Sum[CyclicPermP[x,pList,i],{i,0,MaxCycle-1}];

CyclicSumPAndE[x_,pList_,eList_,MaxCycle_]:=
Sum[CyclicPermPAndE[x,pList,eList,i],{i,0,MaxCycle-1}];

BCJp[amp_,pList_]:=Sum[Sum[d[pList[[2]],pList[[j]]],{j,3,i}](amp/.RepList[pList,Insert[Delete[pList,2],pList[[2]],i]]),{i,3,Length[pList]}];

BCJpe[amp_,pList_,eList_]:=Sum[Sum[d[pList[[2]],pList[[j]]],{j,3,i}](amp/.RepList[Join[pList,eList],Join[Insert[Delete[pList,2],pList[[2]],i],Insert[Delete[eList,2],eList[[2]],i]]]),{i,3,Length[pList]}];


KLT[LAmpHead_, RAmpHead_, pHead_, n_] := Module[{j, jBar, s, g, f, fBar, AmpL, AmpR, nOver2Floor, nOver2Ceil},
   nOver2Floor = Floor[n/2];
   nOver2Ceil = Ceiling[n/2];
   g[i_, j_] := If[i > j, s[i, j], 0];
   j = nOver2Floor - 1;
   jBar = nOver2Ceil - 2;
   
   f[i_List] :=
    If[Length[i] > 0, s[1, i[[j]]], 1]*Product[s[1, i[[m]]] + Sum[g[i[[m]], i[[k]]], {k, m + 1, j}], {m, 1, j - 1}];
   
   fBar[l_List] :=
    If[Length[l] > 0, s[l[[1]], n - 1], 1]*Product[s[l[[m]], n - 1] + Sum[g[l[[k]], l[[m]]], {k, 1, m - 1}], {m, 2, jBar}];
   
   (-1)^(n + 1)*AmpL[Range[n]]*Sum[f[i] fBar[l] AmpR[Join[i, {1, n - 1}, l, {n}]], {i, Permutations[Range[2, nOver2Floor]]}, {l, Permutations[Range[nOver2Floor + 1, n - 2]]}] /. AmpL[list_] :> AmpL[pHead /@ list] /. AmpR[list_] :> AmpR[pHead /@ list] /. s[x_, y_] :> s[pHead[x], pHead[y]] // PermSumP[#, pHead /@ Range[2, n - 2]] & // # /. s[x_, y_] :> 2 d[x, y] & // # /. AmpL -> LAmpHead & // # /. AmpR -> RAmpHead &
   ];
(*From hep-th/9811140*)


(*--- Contract mu indices ---*)

ContractMu[mu_,dim_][expr_]:=Expand[expr]//.{d[mu[z_],x_]d[mu[z_],y_]:>d[x,y],d[mu[z_],x_]^2:>d[x,x],d[mu[x_],mu[x_]]:>dim};


End[]
EndPackage[]

Print["\n --- AMPLITUDES --- \n\n This package implements several things for working with scattering amplitudes:  1) The Minkowski dot product in any spacetime dimension that defaults to the mostly plus signature (-+++...).  2) Some 4D spinor things.  3)  Some color (fabc and dabc) stuff including how to reduce to DDM/half ladder basis and simplify trace expressions.  I use the conventions of Elvang and Huang or Srednicki."];
