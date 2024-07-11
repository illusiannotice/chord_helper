(* ::Package:: *)

BeginPackage["SequenceScanner`"];
Begin["`Private`"];
(*functions docs*)
isMinor::usage = "Checks if chord is minor or not.";
(*functions*)
isMinor[sequence_, minorNote_, majorNote_]:= (For[i = 1, i <= Length[sequence], i++, If[sequence[[i]] == majorNote, Return["Major"],If[sequence[[i]] == minorNote, Return["Minor"]]]], Return[False])
End[];
EndPackage;
