(* ::Package:: *)

(* ::Input::Initialization:: *)
Clear["Global`*"];
ClearSystemCache[];
SetSystemOptions["CacheOptions"->{"Numeric"->{"Cache"->True,"CacheTableLength"->1021,"CacheTableWidth"->28000000,"CacheKeyMaxBytes"->2000000000,"CacheResultMaxBytes"->2000000000,"KeyComparison"->None,"ResultComparison"->GreaterEqual}}];
SetDirectory[NotebookDirectory[]];
pnr=pnc=1;
nc=68; (*number of conductors in a slot*)
(************************************************************************)

(******************** Slot Region Inductance ********************************)
LFLUX=First@Import["LRS-100Khz-wo-return-solid-con.xls"];

Ls={LFLUX[[12;;12+67,4]]};
Do[Ls=Join[Ls,{LFLUX[[12+294i;;12+67+294 i,4]]}],{i,67}];
Ls=Table[ToExpression@StringReplace[Ls[[i,j]],"E"-> "*10^"],{i,pnr,pnr+nc-1},{j,pnc,pnc+nc-1}];
Do[Ls[[i,1;;i-1]]=Ls[[1;;i-1,i]],{i,nc}];(*removing calculation error of FEM flux software and hence symetrize the matrix*)
Subscript[L, us]=Subscript[L, ls]=Ls;

Luls={LFLUX[[12+68;;12+68+67,4]]};
Do[Luls=Join[Luls,{LFLUX[[12+68+294i;;12+68+67+294 i,4]]}],{i,67}];
Luls=Table[ToExpression@StringReplace[Luls[[i,j]],"E"-> "*10^"],{i,pnr,pnr+nc-1},{j,pnc,pnc+nc-1}];
Do[Luls[[i,1;;i-1]]=Luls[[1;;i-1,i]],{i,nc}];
Subscript[L, uls]=Luls;

Subscript[L, mu]={LFLUX[[12+294*68;;12+294*68+67,4]]};
Subscript[L, mu]=Table[ToExpression@StringReplace[Subscript[L, mu][[i,j]],"E"-> "*10^"],{i,1},{j,pnc,pnc+nc-1}];

Subscript[L, ml]={LFLUX[[12+68+294*68;;12+68+294*68+67,4]]};
Subscript[L, ml]=Table[ToExpression@StringReplace[Subscript[L, ml][[i,j]],"E"-> "*10^"],{i,1},{j,pnc,pnc+nc-1}];

Subscript[L, r]=LFLUX[[12+294*68+2*68,4]];
Subscript[L, r]=ToExpression@StringReplace[Subscript[L, r],"E"-> "*10^"];

Subscript[L, slot]=ArrayFlatten@({
 {Subscript[L, us], Subscript[L, uls], Transpose[Subscript[L, mu]]},
 {Transpose[Subscript[L, uls]], Subscript[L, ls], Transpose[Subscript[L, ml]]},
 {Subscript[L, mu], Subscript[L, ml], Subscript[L, r]}
});
(**************************************************************************************************)

(****************************** Rear and Front OverHang (RO, FO) Zone Inductance **************************)
LOFLUX=First@Import["LROH-100Khz-wo-return-solid-con.xls"];

Lo={LOFLUX[[15;;15+67,4]]};
Do[Lo=Join[Lo,{LOFLUX[[15+162i;;15+162i+67,4]]}],{i,67}];
Lo=Table[ToExpression@StringReplace[Lo[[i,j]],"E"-> "*10^"],{i,pnr,pnr+nc-1},{j,pnc,pnc+nc-1}];
Do[Lo[[i,1;;i-1]]=Lo[[1;;i-1,i]],{i,nc}];

Subscript[L, 0]=SparseArray[{{_ , _}-> 0.},{nc,nc}];

Subscript[L, oh]=ArrayFlatten@({
 {Lo, Subscript[L, 0]},
 {Subscript[L, 0], Lo}
});
(***********************************************************************************************)

(******************************** Slot Region Capacitance ****************************************)
CFLUX=First@Import["CFLUX.xls"];

Cs={CFLUX[[12;;12+67,4]]};
Do[Cs=Join[Cs,{CFLUX[[12+156i;;12+67+156 i,4]]}],{i,67}];
Cs=Table[ToExpression@StringReplace[StringDrop[Cs[[i,j]],-2],"E"-> "*10^"],{i,pnr,pnr+nc-1},{j,pnc,pnc+nc-1}];
Do[Cs[[i,1;;i-1]]=Cs[[1;;i-1,i]],{i,nc}];
Subscript[c, us]=Subscript[c, ls]=Cs;

Csr={CFLUX[[12+156*68;;12+156*68+67,4]]};
Csr=Table[ToExpression@StringReplace[StringDrop[Csr[[i,j]],-2],"E"-> "*10^"],{i,1},{j,pnr,pnr+nc-1}];
Subscript[c, mu]=Subscript[c, ml]=Csr;

Subscript[c, r]=CFLUX[[12+156*68+68,4]];
Subscript[c, r]=ToExpression@StringReplace[StringDrop[Subscript[c, r],-2],"E"-> "*10^"];

Subscript[c, 0]=SparseArray[{{_ , _}-> 0.},{nc,nc}];

Subscript[c, slot]=ArrayFlatten@({
 {Subscript[c, us], Subscript[c, 0], Subscript[c, mu]\[Transpose]},
 {Subscript[c, 0], Subscript[c, ls], Subscript[c, ml]\[Transpose]},
 {Subscript[c, mu], Subscript[c, ml], Subscript[c, r]}
});
(***********************************************************************************************)

(******************************** Rear and Front OverHang (RO, FO) Zone Capacitance *******************)
COFLUX=First@Import["COFLUX.xls"];

Co={COFLUX[[12;;12+67,4]]};
Do[Co=Join[Co,{COFLUX[[12+154i;;12+67+154i,4]]}],{i,67}];
Co=Table[ToExpression@StringReplace[StringDrop[Co[[i,j]],-2],"E"-> "*10^"],{i,pnr,pnr+nc-1},{j,pnc,pnc+nc-1}];
Do[Co[[i,1;;i-1]]=Co[[1;;i-1,i]],{i,nc}];
Subscript[c, oh]=ArrayFlatten@({
 {Co, Subscript[c, 0]},
 {Subscript[c, 0], Co}
});
(***********************************************************************************************)

(**********************  Bearing parameters  ********************)
Subscript[R, b1]=0.24;
Subscript[R, b2]=0.24;
(*Subscript[R, ins]=5;*)
Subscript[c, b1]=271.369*10^-12; 
Subscript[c, b2]=271.369*10^-12;
Subscript[G, b1]=0.1*10^-2; 
Subscript[G, b2]=0.1*10^-2;
\[CapitalDelta]t=1;
(**********************      Skin effect      ******************)
Subscript[d, c]=1.0*10^-3;                         (** Conductor radius **)
\[Rho]=0.0168*10^-6;                           (** Resistivity of stator bar conductors **)
f=10.*10^6;                                (** ferequency **)
Subscript[\[Mu], 0]=4*\[Pi]*10^-7;                        (** Permeability of free space **)
Subscript[\[Epsilon], 0]=8.854*10^-12;
Subscript[\[Mu], r]=0.99999;                            (** Relative Permeability **)
Subscript[d, l]=4.0*10^-3;                       (** Cable radius **)
\[Delta]=Sqrt[(2*\[Rho])/(2*\[Pi]*f*Subscript[\[Mu], 0]*Subscript[\[Mu], r])];       (** Skin depth **)
Subscript[k, px]=(1+2/3 (68^2-1))*0.01;         (** Proximity Factor **)
(************************   Cable Parameters   **************************)
Subscript[l, c]=1*^-6;                                     (** Cable Length **)
Subscript[R, cable]=2 \[Rho]/(\[Pi]*Subscript[d, l]*\[Delta]);
Subscript[R, c]=Subscript[R, cable]/2*Subscript[l, c];                       (** Cable Resistance **)
Subscript[L, cable]=0.3*10^-6;                (** Cable Inductance **)
Subscript[c, cable]=600*10^-12;              (** Cable Capacitance **)
Subscript[G, cable]=0.1*10^-2;
Subscript[G, c]=Subscript[G, cable]/2*Subscript[l, c];                     (** Cable Conductance **)
(**   Characteristic Impedance and Propagation Speed and Time Constant of Cable   **) 
Subscript[Z, c]=Sqrt[Subscript[L, cable]/Subscript[c, cable]];Subscript[\[Nu], c]=1/Sqrt[Subscript[L, cable]*Subscript[c, cable]];Subscript[\[Tau], c]=N[Subscript[l, c]/Subscript[\[Nu], c]];
(***********************   Coil Parameters   *************************)
Subscript[l, s]=0.08;                                 (** Slot length **)
Subscript[l, oh]=0.045*(165*\[Pi])/180;                               (** Overhang length **)
Subscript[R, coil]=2 \[Rho]/(\[Pi]*Subscript[d, c]*\[Delta]) Subscript[k, px];         (** Coil Resistance **)
Subscript[R, s]=Subscript[R, coil]/2*Subscript[l, s];
Subscript[R, oh]=Subscript[R, coil]/2*Subscript[l, oh];
Subscript[A, slot]=Subscript[L, slot] . Subscript[c, slot];
Subscript[A, oh]=Subscript[L, oh] . Subscript[c, oh];
(********************* Voltage Transmission Matrices *********************)
Subscript[T, vslot]=Transpose[Eigenvectors[Subscript[A, slot]]];
Subscript[T, voh]=Transpose[Eigenvectors[Subscript[A, oh]]];
(********************* Current Transmission Matrices *********************)
Subscript[T, islot]=Inverse[Transpose[Subscript[T, vslot]]];
Subscript[T, ioh]=Inverse[Transpose[Subscript[T, voh]]];
(************************** Modal Matrices ***************************)
Subscript[L, Mslot]=Inverse[Subscript[T, vslot]] . Subscript[L, slot] . Subscript[T, islot];
Subscript[L, Moh]=Inverse[Subscript[T, voh]] . Subscript[L, oh] . Subscript[T, ioh];
Subscript[c, Mslot]=Inverse[Subscript[T, islot]] . Subscript[c, slot] . Subscript[T, vslot];(*Transpose[Subscript[T, vslot]].Subscript[c, slot].Subscript[T, vslot]*)
Subscript[c, Moh]=Inverse[Subscript[T, ioh]] . Subscript[c, oh] . Subscript[T, voh];(*Transpose[Subscript[T, voh]].Subscript[c, oh].Subscript[T, voh]*)
Subscript[V, kslot]=Join[Array[\!\(\*SubscriptBox[\(V\), \("\<k\>" <> ToString[#] <> "\<1\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(V\), \("\<k\>" <> ToString[#] <> "\<3\>"\)]\)[n]&,{nc,1}],{{Subscript[V, kr][n]}}];
Subscript[V, mslot]=Join[Array[\!\(\*SubscriptBox[\(V\), \("\<m\>" <> ToString[#] <> "\<1\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(V\), \("\<m\>" <> ToString[#] <> "\<3\>"\)]\)[n]&,{nc,1}],{{Subscript[V, mr][n]}}];
Subscript[V, koh]=Join[Array[\!\(\*SubscriptBox[\(V\), \("\<k\>" <> ToString[#] <> "\<2\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(V\), \("\<k\>" <> ToString[#] <> "\<4\>"\)]\)[n]&,{nc,1}]];
Subscript[V, moh]=Join[Array[\!\(\*SubscriptBox[\(V\), \("\<m\>" <> ToString[#] <> "\<2\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(V\), \("\<m\>" <> ToString[#] <> "\<4\>"\)]\)[n]&,{nc,1}]];

Subscript[i, kslot]=Join[Array[\!\(\*SubscriptBox[\(i\), \("\<k\>" <> ToString[#] <> "\<1\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(i\), \("\<k\>" <> ToString[#] <> "\<3\>"\)]\)[n]&,{nc,1}],{{Subscript[i, kr][n]}}];
Subscript[i, mslot]=Join[Array[\!\(\*SubscriptBox[\(i\), \("\<m\>" <> ToString[#] <> "\<1\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(i\), \("\<m\>" <> ToString[#] <> "\<3\>"\)]\)[n]&,{nc,1}],{{Subscript[i, mr][n]}}];
Subscript[i, koh]=Join[Array[\!\(\*SubscriptBox[\(i\), \("\<k\>" <> ToString[#] <> "\<2\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(i\), \("\<k\>" <> ToString[#] <> "\<4\>"\)]\)[n]&,{nc,1}]];
Subscript[i, moh]=Join[Array[\!\(\*SubscriptBox[\(i\), \("\<m\>" <> ToString[#] <> "\<2\>"\)]\)[n]&,{nc,1}],Array[\!\(\*SubscriptBox[\(i\), \("\<m\>" <> ToString[#] <> "\<4\>"\)]\)[n]&,{nc,1}]];
(** Subscript[Z, ij] = Characteristic impedance of ith turn in jth region **)
(** Subscript[\[Nu], ij] = Propagation speed of ith turn in jth region **)
(** Subscript[\[Tau], ij] = Constant time of ith turn in jth region **)
(** i : 1,2,...,10,...,nc **)
(** j : 1=up slot , 2=right overhang , 3=down slot , 4=left overhang **)
Subscript[L, Mslot]=DiagonalMatrix[Diagonal[Subscript[L, Mslot]]];
Subscript[c, Mslot]=DiagonalMatrix[Diagonal[Subscript[c, Mslot]]];
Subscript[L, Moh]=DiagonalMatrix[Diagonal[Subscript[L, Moh]]];
Subscript[c, Moh]=DiagonalMatrix[Diagonal[Subscript[c, Moh]]];
Subscript[Z, Mslot]=Diagonal[\[Sqrt](Subscript[L, Mslot] . DiagonalMatrix[Diagonal[Inverse[Subscript[c, Mslot]]]])];
Subscript[Z, Moh]=Diagonal[Sqrt[Subscript[L, Moh] . DiagonalMatrix[Diagonal[Inverse[Subscript[c, Moh]]]]]];
Subscript[\[Nu], Mslot]=Sqrt[Diagonal[Inverse[Subscript[L, Mslot] . Subscript[c, Mslot]]]];
Subscript[\[Nu], Moh]=Sqrt[Diagonal[Inverse[Subscript[L, Moh] . Subscript[c, Moh]]]];
Subscript[\[Tau], Mslot]=N[Subscript[l, s]/Subscript[\[Nu], Mslot]]
Subscript[\[Tau], Moh]=N[Subscript[l, oh]/Subscript[\[Nu], Moh]]
dt=0.5*^-9;
Subscript[\[Tau], Mslot]=Round[N[Subscript[\[Tau], Mslot]/dt]]
Subscript[\[Tau], Moh]=Round[N[Subscript[\[Tau], Moh]/dt]]
Subscript[\[Tau], c]=Floor[N[Subscript[\[Tau], c]/dt]];
Subscript[Z, r]=Subscript[Z, Mslot][[2 nc+1]];
Subscript[\[Tau], r]=Subscript[\[Tau], Mslot][[2 nc+1]];
(**************   Load   *****************************)
Subscript[R, load]=10*10^6;
Subscript[R, G]=0;
(********************* Applied Voltage ***********************)
Subscript[t, max]=Floor[1.*^-9/dt]*10000;
Tv0inv=Import["ALL0001-NP-260595.CSV"][[147+18;;3644+18,2]];(**** One period of data*****)
st=20*^-6*10/5000.;(*oscope sample time*)
tv0inv=Table[Interpolation[Table[{(i-1) st,Tv0inv[[i]]},{i,Length@Tv0inv}]][(i-1) dt],{i,Subscript[t, max]}];
u[t_]:=tv0inv[[t]]
(**********************************************************************)



(* ::Input::Initialization:: *)
td=0;
Do[td=Subscript[\[Tau], Mslot][[i]]+Subscript[\[Tau], Moh][[i]]+Subscript[\[Tau], Mslot][[i+nc]]+Subscript[\[Tau], Moh][[i+nc]]+td;If[td>=Floor[100.*^-9/dt],Print[i,"--",td];Break[]],{i,nc}]


(* ::Input::Initialization:: *)
Subscript[\[Tau], in]=Max[Subscript[\[Tau], Mslot],Subscript[\[Tau], Moh],Subscript[\[Tau], c]];
Do[u[n]=0;Subscript[i, kc][n]=0;Subscript[V, kc][n]=0;Subscript[i, mc][n]=0;Subscript[V, mc][n]=0,{n,-Subscript[\[Tau], in],0,1}];
MapThread[Set,{
Flatten@Table[{Subscript[i, kslot][[p,1]],Subscript[V, kslot][[p,1]],Subscript[i, mslot][[p,1]],Subscript[V, mslot][[p,1]]}
,{n,-Subscript[\[Tau], in],0,1},{p,2 nc+1}]
,Flatten@Table[{0,0,0,0},{n,-Subscript[\[Tau], in],0,1},{p,2 nc+1}]
}];
MapThread[Set,{
Flatten@Table[{Subscript[i, koh][[p,1]],Subscript[V, koh][[p,1]],Subscript[i, moh][[p,1]],Subscript[V, moh][[p,1]]}
,{n,-Subscript[\[Tau], in],0,1},{p,2nc}]
,Flatten@Table[{0,0,0,0},{n,-Subscript[\[Tau], in],0,1},{p,2 nc}]
}];
Do[Subscript[V, b1][n]=0;Subscript[V, b2][n]=0;Subscript[i, b1][n]=0;Subscript[i, b2][n]=0;Subscript[i, Gb1][n]=0;Subscript[i, Gb2][n]=0,{n,-Subscript[\[Tau], in],0,1}];


(* ::Input::Initialization:: *)
AbsoluteTiming[Do[
Bergeron=First@Solve[Flatten@{
Subscript[V, kc][n]==(u[n]-(Subscript[R, G]+Subscript[R, c])*Subscript[i, kc][n])/((Subscript[R, G]+Subscript[R, c])*Subscript[G, c]+1),
Subscript[i, mc][n]==Subscript[G, c]*Subscript[V, mc][n]+1/(Subscript[R, c]+Subscript[R, s]) (Subscript[V, mc][n]-(Subscript[T, vslot][[1]] . Subscript[V, kslot])),
Subscript[T, islot][[1]] . Subscript[i, kslot]==1/(Subscript[R, c]+Subscript[R, s]) (Subscript[V, mc][n]-(Subscript[T, vslot][[1]] . Subscript[V, kslot])),
Flatten@Table[{
Subscript[T, islot][[p]] . Subscript[i, mslot]==1/(Subscript[R, s]+Subscript[R, oh]) (Subscript[T, vslot][[p]] . Subscript[V, mslot]-(Subscript[T, voh][[p]] . Subscript[V, koh])),
Subscript[T, ioh][[p]] . Subscript[i, koh]==1/(Subscript[R, s]+Subscript[R, oh]) (Subscript[T, vslot][[p]] . Subscript[V, mslot]-(Subscript[T, voh][[p]] . Subscript[V, koh])),
Subscript[T, ioh][[p]] . Subscript[i, moh]==1/(Subscript[R, oh]+Subscript[R, s]) (Subscript[T, voh][[p]] . Subscript[V, moh]-(Subscript[T, vslot][[p+nc]] . Subscript[V, kslot])),
Subscript[T, islot][[p+nc]] . Subscript[i, kslot]==1/(Subscript[R, oh]+Subscript[R, s]) (Subscript[T, voh][[p]] . Subscript[V, moh]-(Subscript[T, vslot][[p+nc]] . Subscript[V, kslot])),
Subscript[T, islot][[p+nc]] . Subscript[i, mslot]==1/(Subscript[R, s]+Subscript[R, oh]) (Subscript[T, vslot][[p+nc]] . Subscript[V, mslot]-(Subscript[T, voh][[p+nc]] . Subscript[V, koh])),
Subscript[T, ioh][[p+nc]] . Subscript[i, koh]==1/(Subscript[R, s]+Subscript[R, oh]) (Subscript[T, vslot][[p+nc]] . Subscript[V, mslot]-(Subscript[T, voh][[p+nc]] . Subscript[V, koh]))
},{p,nc}],
Flatten@Table[{
Subscript[T, ioh][[p+nc]] . Subscript[i, moh]==1/(Subscript[R, oh]+Subscript[R, s]) (Subscript[T, voh][[p+nc]] . Subscript[V, moh]-(Subscript[T, vslot][[p+1]] . Subscript[V, kslot])),
Subscript[T, islot][[p+1]] . Subscript[i, kslot]==1/(Subscript[R, oh]+Subscript[R, s]) (Subscript[T, voh][[p+nc]] . Subscript[V, moh]-(Subscript[T, vslot][[p+1]] . Subscript[V, kslot]))
},{p,nc-1}],
Subscript[T, ioh][[2 nc]] . Subscript[i, moh]==(1/(Subscript[R, oh]+Subscript[R, load]))*(Subscript[T, voh][[2 nc]] . Subscript[V, moh]),
Subscript[V, kc][n-Subscript[\[Tau], c]]+Subscript[Z, c]*Subscript[i, kc][n-Subscript[\[Tau], c]]==Subscript[V, mc][n]+Subscript[Z, c]*Subscript[i, mc][n],
Subscript[V, mc][n-Subscript[\[Tau], c]]-Subscript[Z, c]*Subscript[i, mc][n-Subscript[\[Tau], c]]==Subscript[V, kc][n]-Subscript[Z, c]*Subscript[i, kc][n],
Flatten@Table[{
(Subscript[V, kslot][[p,1]]/.n-> (n-Subscript[\[Tau], Mslot][[p]]))+Subscript[Z, Mslot][[p]](Subscript[i, kslot][[p,1]]/.n-> (n-Subscript[\[Tau], Mslot][[p]]))==Subscript[V, mslot][[p,1]]+Subscript[Z, Mslot][[p]]Subscript[i, mslot][[p,1]],
(Subscript[V, mslot][[p,1]]/.n-> (n-Subscript[\[Tau], Mslot][[p]]))-Subscript[Z, Mslot][[p]](Subscript[i, mslot][[p,1]]/.n-> (n-Subscript[\[Tau], Mslot][[p]]))==Subscript[V, kslot][[p,1]]-Subscript[Z, Mslot][[p]]Subscript[i, kslot][[p,1]],
(Subscript[V, koh][[p,1]]/.n-> (n-Subscript[\[Tau], Moh][[p]]))+Subscript[Z, Moh][[p]](Subscript[i, koh][[p,1]]/.n-> (n-Subscript[\[Tau], Moh][[p]]))==Subscript[V, moh][[p,1]]+Subscript[Z, Moh][[p]]Subscript[i, moh][[p,1]],
(Subscript[V, moh][[p,1]]/.n-> (n-Subscript[\[Tau], Moh][[p]]))-Subscript[Z, Moh][[p]](Subscript[i, moh][[p,1]]/.n-> (n-Subscript[\[Tau], Moh][[p]]))==Subscript[V, koh][[p,1]]-Subscript[Z, Moh][[p]]Subscript[i, koh][[p,1]]
},{p,2nc}],
Subscript[V, kr][n-Subscript[\[Tau], r]]+Subscript[Z, r]*(Subscript[i, kr][n-Subscript[\[Tau], r]])==Subscript[V, mr][n]+Subscript[Z, r]*(Subscript[i, mr][n]),
Subscript[V, mr][n-Subscript[\[Tau], r]]-Subscript[Z, r]*(Subscript[i, mr][n-Subscript[\[Tau], r]])==Subscript[V, kr][n]-Subscript[Z, r]*(Subscript[i, kr][n]),
Subscript[T, islot][[2 nc+1]] . Subscript[i, kslot]==-Subscript[i, b1][n]-Subscript[i, Gb1][n],
(Subscript[i, b1][n]+Subscript[i, b1][n-\[CapitalDelta]t])==(2*Subscript[c, b1])/dt*(Subscript[V, b1][n]-Subscript[V, b1][n-\[CapitalDelta]t]),
Subscript[i, Gb1][n]==Subscript[G, b1]*(Subscript[T, vslot][[2 nc+1]] . Subscript[V, kslot]),
Subscript[V, b1][n]==(Subscript[T, vslot][[2 nc+1]] . Subscript[V, kslot])-(-Subscript[R, b1]*(Subscript[T, islot][[2 nc+1]] . Subscript[i, kslot])),
Subscript[T, islot][[2 nc+1]] . Subscript[i, mslot]==Subscript[i, b2][n]+Subscript[i, Gb2][n],
(Subscript[i, b2][n]+Subscript[i, b2][n-\[CapitalDelta]t])==(2*Subscript[c, b2])/dt*(Subscript[V, b2][n]-Subscript[V, b2][n-\[CapitalDelta]t]),
Subscript[i, Gb2][n]==Subscript[G, b2]*(Subscript[T, vslot][[2 nc+1]] . Subscript[V, mslot]),
Subscript[V, b2][n]==(Subscript[T, vslot][[2 nc+1]] . Subscript[V, mslot])-(Subscript[R, b2]*(Subscript[T, islot][[2 nc+1]] . Subscript[i, mslot]))},Flatten@{Flatten@Table[{Subscript[V, kslot][[p,1]],Subscript[V, mslot][[p,1]],Subscript[i, kslot][[p,1]],Subscript[i, mslot][[p,1]],
Subscript[V, koh][[p,1]],Subscript[V, moh][[p,1]],Subscript[i, koh][[p,1]],Subscript[i, moh][[p,1]]},{p,2nc}],Subscript[i, kc][n],Subscript[V, kc][n],Subscript[i, mc][n],Subscript[V, mc][n],Subscript[i, kr][n],Subscript[V, kr][n],Subscript[i, mr][n],Subscript[V, mr][n],Subscript[V, b1][n],Subscript[V, b2][n],Subscript[i, b1][n],Subscript[i, b2][n],Subscript[i, Gb1][n],Subscript[i, Gb2][n]}];
Subscript[i, kc][n]=Subscript[i, kc][n]/.Bergeron;Subscript[V, kc][n]=Subscript[V, kc][n]/.Bergeron;Subscript[i, mc][n]=Subscript[i, mc][n]/.Bergeron;Subscript[V, mc][n]=Subscript[V, mc][n]/.Bergeron;
MapThread[Set,{
Flatten@Table[{Subscript[V, kslot][[p,1]],Subscript[V, mslot][[p,1]],Subscript[i, kslot][[p,1]],Subscript[i, mslot][[p,1]],Subscript[V, koh][[p,1]],Subscript[V, moh][[p,1]],Subscript[i, koh][[p,1]],Subscript[i, moh][[p,1]]},{p,2nc}],
Flatten@Table[{Subscript[V, kslot][[p,1]],Subscript[V, mslot][[p,1]],Subscript[i, kslot][[p,1]],Subscript[i, mslot][[p,1]],Subscript[V, koh][[p,1]],Subscript[V, moh][[p,1]],Subscript[i, koh][[p,1]],Subscript[i, moh][[p,1]]}/.Bergeron,{p,2nc}]}];
Subscript[i, kr][n]=Subscript[i, kr][n]/.Bergeron;Subscript[V, kr][n]=Subscript[V, kr][n]/.Bergeron;Subscript[i, mr][n]=Subscript[i, mr][n]/.Bergeron;Subscript[V, mr][n]=Subscript[V, mr][n]/.Bergeron;Subscript[V, b1][n]=Subscript[V, b1][n]/.Bergeron;Subscript[V, b2][n]=Subscript[V, b2][n]/.Bergeron;Subscript[i, b1][n]=Subscript[i, b1][n]/.Bergeron;Subscript[i, b2][n]=Subscript[i, b2][n]/.Bergeron;Subscript[i, Gb1][n]=Subscript[i, Gb1][n]/.Bergeron;Subscript[i, Gb2][n]=Subscript[i, Gb2][n]/.Bergeron,
{n,1,Subscript[t, max]}];]


(* ::Input::Initialization:: *)
ListPlot[tv0inv,Joined->True,PlotRange->All,AxesLabel->{ns,v0inv}]


(* ::Input::Initialization:: *)
ListPlot[Table[Subscript[V, mc][n],{n,1,Subscript[t, max]}],Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[V, B]}]


(* ::Input::Initialization:: *)
Tvbof=Import["ALL0001-NP-260595.CSV"][[147+18;;3644+18,6]];
Tvbor=Import["ALL0001-NP-260595.CSV"][[147+18;;3644+18,8]];
tvbof=Table[Interpolation[Table[{(i-1) st,Tvbof[[i]]},{i,Length@Tvbof}]][(i-1) dt],{i,Subscript[t, max]}];
tvbor=Table[Interpolation[Table[{(i-1) st,Tvbor[[i]]},{i,Length@Tvbor}]][(i-1) dt],{i,Subscript[t, max]}];


(* ::Input::Initialization:: *)
{ListPlot[{Table[Subscript[V, b1][n],{n,1,Subscript[t, max]}],0.1tvbof},Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[V, b1]},ImageSize->300],ListPlot[Table[First[Subscript[T, islot][[2 nc+1]] . Subscript[i, kslot]],{n,1,Subscript[t, max]}],Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[i, kr]},ImageSize->300],
ListPlot[Table[Subscript[i, b1][n],{n,1,Subscript[t, max]}],Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[i, br1]},ImageSize->300]}


(* ::Input::Initialization:: *)
{ListPlot[{Table[Subscript[V, b2][n],{n,1,Subscript[t, max]}],0.1tvbor},Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[V, b2]},ImageSize->300],ListPlot[Table[First[Subscript[T, islot][[2 nc+1]] . Subscript[i, mslot]],{n,1,Subscript[t, max]}],Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[i, mr]},ImageSize->300],
ListPlot[Table[Subscript[i, b2][n],{n,1,Subscript[t, max]}],Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[i, br2]},ImageSize->300]}


(* ::Input::Initialization:: *)
ListPlot[{Table[Subscript[V, b1][n]-Subscript[V, b2][n],{n,1,Subscript[t, max]}],0.01(tvbof-tvbor)},Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[V, sh]},ImageSize->300]


(* ::Input::Initialization:: *)
ListPlot[Table[First[Subscript[T, islot][[2 nc+1]] . (Subscript[i, kslot]-Subscript[i, mslot])],{n,1,Subscript[t, max]}],Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[i, sh]},ImageSize->300]


(* ::Input::Initialization:: *)
Tvno=Import["ALL0001-NP-260595.CSV"][[147+18;;3644+18,4]];
tvno=Table[Interpolation[Table[{(i-1) st,Tvno[[i]]},{i,Length@Tvno}]][(i-1) dt],{i,Subscript[t, max]}];


(* ::Input::Initialization:: *)
ListPlot[{Table[First[Subscript[T, voh][[2 nc]] . Subscript[V, moh]],{n,1,Subscript[t, max]}],2tvno},Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[V, no]},ImageSize->300]


(* ::Input::Initialization:: *)
ListPlot[Table[Table[First[Subscript[T, voh][[p]] . Subscript[V, moh]],{n,1,Subscript[t, max]}],{p,nc, 2nc}],Joined->True,PlotRange->All,AxesLabel->{ns,end turns},ImageSize->300]


(* ::Input::Initialization:: *)
stHM=20*^-6*10/2000.;(*HAMEG OSCOPE SAMPLE TIME*)
Tino=Import["WAV00007-NP-260595.CSV"][[1492-1399;;1492,2]];
tino=Table[Interpolation[Table[{(i-1) stHM,Tino[[i]]},{i,Length@Tino}]][(i-1) dt],{i,Subscript[t, max]}];


(* ::Input::Initialization:: *)
ListPlot[{Table[First[Subscript[T, ioh][[2 nc]] . Subscript[i, moh]],{n,1,Subscript[t, max]}],0.1tino},Joined->True,PlotRange->All,AxesLabel->{ns,Subscript[i, no]},ImageSize->300]
