(* ::Package:: *)

BeginPackage["SequenceScanner`"];


isMinor::usage = "Checks if chord is minor or not.";
generatePostfix::usage = "generates chords postfix";


Begin["`Private`"];


isMinor[sequence_, minorNote_, majorNote_]:=(For[i = 1, i <= Length[sequence], i++, If[sequence[[i]] == majorNote, Return["Major"],If[sequence[[i]] ==  minorNote, Return["Minor"]]]]; Return["Nothing"]);
generatePostfix[noteMap_, diffMap_, tone_, sequence_]:=(postfix = "";For[i = 1, i <= Length[sequence], i++, 
														If[sequence[[i]]!= tone, 
															If[noteMap[[sequence[[i]]]] > noteMap[[tone]], 
																(*TrueCase*)
																postfix = postfix <> Key[Key[sequence[[i]]][noteMap]- Key[tone][noteMap]][diffMap],
																postfix = postfix <> Key[-(Key[sequence[[i]]][noteMap]- Key[tone][noteMap])][diffMap]]]];
														Return[postfix]);


End[];


EndPackage[];


 
