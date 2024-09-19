(* ::Package:: *)

BeginPackage["SequenceScanner`"];


isMinor::usage = "Checks if chord is minor or not.";
generatePostfix::usage = "generates chords postfix";
formatSequence::usage = "Changes sequence by given map";


Begin["`Private`"];


isMinor[sequence_, minorNote_, majorNote_]:=(For[i = 1, i <= Length[sequence], i++, If[sequence[[i]] == majorNote, Return["Major"],If[sequence[[i]] ==  minorNote, Return["Minor"]]]]; Return["Nothing"]);
generatePostfix[noteMap_, diffMap_, tone_, sequence_]:=Module[{postfix}, postfix = "";For[i = 1, i <= Length[sequence], i++, If[sequence[[i]]!= tone, If[noteMap[[sequence[[i]]]] > noteMap[[tone]],postfix = postfix <> Key[Key[sequence[[i]]][noteMap]- Key[tone][noteMap]][diffMap],postfix = postfix <> Key[(Key[sequence[[i]]][noteMap] + 12)- Key[tone][noteMap]][diffMap]]]];Return[postfix]];
isSept[sequence_, septNote_]:=(For[i = 1, i <= Length[sequence], i++, If[sequence[[i]] == septNote,Return[True]]],)
formatSequence[formMap_, sequence_]:=(formated = {}; For[i = 1, i <= Length[sequence], i++, AppendTo[formated, Key[Part[sequence, i]][formMap]]; Print[formated]]; Return[formated]) 


End[];


EndPackage[];


 
