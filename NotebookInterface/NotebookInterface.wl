(* ::Package:: *)

(*
	By David Creech, Mike Grigola 2018-2022 (CB&I)
	Based on concept by John Fultz, Lou D'Andria (Wolfram Research)
*)

BeginPackage["NotebookInterface`"]


evaporativeInputField::usage = "evaporativeInputField[] represents a blank editable evaporative input field that is replaced by its setting when evaluated.\n \
evaporativeInputField[\!\(\*StyleBox[\"x\", \"TI\"]\)] represents an editable evaporative input field that currently contains the expression \!\(\*StyleBox[\"x\", \"TI\"]\).\n \
evaporativeInputField[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"type\", \"TI\"]\)] represents an editable evaporative input field whose contents are taken to be of the specified \!\(\*StyleBox[\"type\", \"TI\"]\).";

evaporativePopupMenu::usage = "evaporativePopupMenu[{\!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"],StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), ...}] represents an evaporative popup menu with an initially-blank setting of Null and possible values \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"i\", \"TI\"]]\) that is replaced by its setting when evaluated.\n \
evaporativePopupMenu[\!\(\*StyleBox[\"x\", \"TI\"]\), {\!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), ...}] represents an evaporative popup menu with setting \!\(\*StyleBox[\"x\", \"TI\"]\) and possible values \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"i\", \"TI\"]]\).\n \
evaporativePopupMenu[\!\(\*StyleBox[\"x\", \"TI\"]\), {\!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"],StyleBox[\"1\", \"TR\"]]\)\[Rule]\!\(\*SubscriptBox[StyleBox[\"lbl\", \"TI\"],StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"],StyleBox[\"2\", \"TR\"]]\)\[Rule]\!\(\*SubscriptBox[StyleBox[\"lbl\", \"TI\"],StyleBox[\"2\", \"TR\"]]\), ...}] represents an evaporative popup menu in which possible value \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"],StyleBox[\"i\", \"TI\"]]\) is indicated by \!\(\*SubscriptBox[StyleBox[\"lbl\", \"TI\"],StyleBox[\"i\", \"TI\"]]\).";

evaporativeUnitsMenu::usage = "evaporativeUnitsMenu[{\!\(\*StyleBox[\"unittype\", \"TI\"]\)] represents an evaporative popup menu of units with an initially-blank setting of Null and possible values determined by \!\(\*StyleBox[\"unittype\", \"TI\"]\) that is replaced by its setting when evaluated.\n \
evaporativeUnitsMenu[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"unittype\", \"TI\"]\)] represents an evaporative popup menu of units with setting \!\(\*StyleBox[\"x\", \"TI\"]\) and possible values determined by \!\(\*StyleBox[\"unittype\", \"TI\"]\).";

unitsMenuRule::usage = "unitsMenuRule[unit] creates a single rule from an expression unit for use in a list accepted by the unittype argument of evaporativeUnitsMenu, where unit can be any known unit accepted by the second argument to Quantity.";
unitsMenuList::usage = "unitsMenuList[unittype] creates a list of rules accepted by the unittype argument of evaporativeUnitsMenu, where unittype can be from a list of supported types or the result of UnitDimensions.";

inputFieldTemplate::usage = "inputFieldTemplate[\!\(\*StyleBox[\"x\", \"TI\"]\)] represents a template to create an evaporative input field containing the expression \!\(\*StyleBox[\"x\", \"TI\"]\).\n \
inputFieldTemplate[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"type\", \"TI\"]\)] represents a template to create an evaporative input field whose contents are taken to be of the specified \!\(\*StyleBox[\"type\", \"TI\"]\).\n \
inputFieldTemplate[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"type\", \"TI\"]\), \!\(\*StyleBox[\"f\", \"TI\"]\), \!\(\*StyleBox[\"message\", \"TI\"]\), \!\(\*StyleBox[\"hint\", \"TI\"]\)] represents a template to create an evaporative input field that validates its contents with function \!\(\*StyleBox[\"f\", \"TI\"]\), displays \!\(\*StyleBox[\"message\", \"TI\"]\) when validation fails and displays \!\(\*StyleBox[\"hint\", \"TI\"]\) in a blank field.";

popupMenuTemplate::usage = "popupMenuTemplate[\!\(\*StyleBox[\"x\", \"TI\"]\), {\!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), ...}] represents a template to create an evaporative popup menu with setting \!\(\*StyleBox[\"x\", \"TI\"]\) and possible values \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"i\", \"TI\"]]\).";

unitsMenuTemplate::usage = "unitsMenuTemplate[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"unittype\", \"TI\"]\)] represents a template to create an evaporative popup menu of units with setting \!\(\*StyleBox[\"x\", \"TI\"]\) and possible values determined by \!\(\*StyleBox[\"unittype\", \"TI\"]\).";

setRowTemplate::usage="setRowTemplate[\!\(\*StyleBox[\"var\", \"TI\"]\), \!\(\*StyleBox[\"controls\", \"TI\"]\)] represents a template to create a row that sets \!\(\*StyleBox[\"var\", \"TI\"]\) to a list of one or more \!\(\*StyleBox[\"controls\", \"TI\"]\)";

setRowTemplateBoxes::usage="setRowTemplateBoxes[\!\(\*StyleBox[\"varBoxes\", \"TI\"]\), \!\(\*StyleBox[\"controls\", \"TI\"]\)] represents a template to create a row that sets \!\(\*StyleBox[\"varBoxes\", \"TI\"]\) to a list of one or more \!\(\*StyleBox[\"controls\", \"TI\"]\) where  \!\(\*StyleBox[\"varBoxes\", \"TI\"]\) is the low-level representation of the variable";

verbatimSetRowTemplate::usage="verbatimSetRowTemplate[{\!\(\*StyleBox[\"var\", \"TI\"]\), \!\(\*StyleBox[\"expr\", \"TI\"]\)}] represents a template to create a row that sets \!\(\*StyleBox[\"var\", \"TI\"]\) to \!\(\*StyleBox[\"expr\", \"TI\"]\).\n \
verbatimSetRowTemplate[{\!\(\*StyleBox[\"var\", \"TI\"]\), \!\(\*StyleBox[\"expr\", \"TI\"]\)}, \!\(\*StyleBox[\"controls\", \"TI\"]\)] represents a template to create a row that sets \!\(\*StyleBox[\"var\", \"TI\"]\) to \!\(\*StyleBox[\"expr\", \"TI\"]\)\!\(\*StyleBox[\" \", \"TI\"]\)substituting the nth item from a list of one or more \!\(\*StyleBox[\"controls\", \"TI\"]\) where RawBoxes[TagBox[n, \"control\"]] appears in \!\(\*StyleBox[\"expr\", \"TI\"]\).";

verbatimSetRowTemplateBoxes::usage="verbatimSetRowTemplateBoxes[{\!\(\*StyleBox[\"varBoxes\", \"TI\"]\), \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\)}] represents a template to create a row that sets \!\(\*StyleBox[\"varBoxes\", \"TI\"]\) to \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\) where \!\(\*StyleBox[\"varBoxes\", \"TI\"]\) and \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\) are the low-level representation of the var and expr.\n \
verbatimSetRowTemplate[{\!\(\*StyleBox[\"varBoxes\", \"TI\"]\), \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\)}, \!\(\*StyleBox[\"controls\", \"TI\"]\)] represents a template to create a row that sets \!\(\*StyleBox[\"varBoxes\", \"TI\"]\) to \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\)\!\(\*StyleBox[\" \", \"TI\"]\)substituting the nth item from a list of one or more \!\(\*StyleBox[\"controls\", \"TI\"]\) where TagBox[n, \"control\"] appears in \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\).";

verbatimRowTemplate::usage="verbatimRowTemplate[\!\(\*StyleBox[\"expr\", \"TI\"]\)] represents a template to create a row containing \!\(\*StyleBox[\"expr\", \"TI\"]\).";

verbatimRowTemplateBoxes::usage ="verbatimRowTemplateBoxes[\!\(\*StyleBox[\"exprBoxes\", \"TI\"]\)] represents a template to create a row containing \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\) where \!\(\*StyleBox[\"exprBoxes\", \"TI\"]\) is the low-level representation of expr.";

rowTemplateToBoxes::usage = "rowTemplateToBoxes[\!\(\*StyleBox[\"template\", \"TI\"]\)] generates boxes for a Row from \!\(\*StyleBox[\"template\", \"TI\"]\).";

cellTemplateToBoxes::usage = "cellTemplateToBoxes[\!\(\*StyleBox[\"templates\", \"TI\"]\)] generates boxes for an Input Cell from a list of \!\(\*StyleBox[\"templates\", \"TI\"]\).";

replaceEvaporativeControlsInCellByTemplate::usage = "replaceEvaporativeControlsInCellByTemplate[\!\(\*StyleBox[\"cell\", \"TI\"]\), \!\(\*StyleBox[\"templates\", \"TI\"]\)] replaces evaporative controls in \!\(\*StyleBox[\"cell\", \"TI\"]\) by those defined in a list of \!\(\*StyleBox[\"templates\", \"TI\"]\).";

readCellAndValidateAgainstTemplate::usage = "readCellAndValidateAgainstTemplate[\!\(\*StyleBox[\"cellobj\", \"TI\"]\), \!\(\*StyleBox[\"templates\", \"TI\"]\)] reads the Cell in \!\(\*StyleBox[\"cellobj\", \"TI\"]\) and yields True if it matches the form of \!\(\*StyleBox[\"templates\", \"TI\"]\) or False with messages if it does not.";

inputAidTemplateToBoxes::usage = "inputAidTemplateToBoxes[\!\(\*StyleBox[\"templates\", \"TI\"]\), \!\(\*StyleBox[\"name\", \"TI\"]\)] generates boxes for an Input Cell from a list of \!\(\*StyleBox[\"templates\", \"TI\"]\) with a button that calls the input aid named \!\(\*StyleBox[\"name\", \"TI\"]\).";

initializeInputAidCell::usage = "initializeInputAidCell[\!\(\*StyleBox[\"templateValues\", \"TI\"]\), \!\(\*StyleBox[\"aidValues\", \"TI\"]\), \!\(\*StyleBox[\"obj\", \"TI\"]\), \!\(\*StyleBox[\"templateName\", \"TI\"]\), \!\(\*StyleBox[\"symbolStrings\", \"TI\"]\), \!\(\*StyleBox[\"inputAidName\", \"TI\"]\)] \n \
creates and initializes the Input cell to a CellObject or NotebookObject, \!\(\*StyleBox[\"obj\", \"TI\"]\), by writing input fields from the template named \!\(\*StyleBox[\"templateName\", \"TI\"]\) initialized to \!\(\*StyleBox[\"templateValues\", \"TI\"]\), attached to the input aid named \!\(\*StyleBox[\"inputAidName\", \"TI\"]\), \
and setting TaggingRules for each symbol name in the list of \!\(\*StyleBox[\"symbolStrings\", \"TI\"]\) to the list of \!\(\*StyleBox[\"aidValues\", \"TI\"]\) to save the input aid state, and finally moves the notebook selection after the cell to prepare for writing additional cells.";

inputAidCellButtonAction::usage = "inputAidCellButtonAction[\!\(\*StyleBox[\"cellobj\", \"TI\"]\), \!\(\*StyleBox[\"newcell\", \"TI\"]\), \!\(\*StyleBox[\"name\", \"TI\"]\), \!\(\*StyleBox[\"taggingrules\", \"TI\"]\)] handles the replace or update button of an input aid named \!\(\*StyleBox[\"name\", \"TI\"]\) attached to \!\(\*StyleBox[\"cellobj\", \"TI\"]\) by writing \!\(\*StyleBox[\"newcell\", \"TI\"]\) and setting \!\(\*StyleBox[\"taggingrules\", \"TI\"]\) to save the input aid state.";

inputAidCellFormatWarning::usage = "inputAidCellFormatWarning[\!\(\*StyleBox[\"isvalid\", \"TI\"]\), \!\(\*StyleBox[\"validmsg\", \"TI\"]\)] generates a Grid of \!\(\*StyleBox[\"validmsg\", \"TI\"]\) if \!\(\*StyleBox[\"isvalid\", \"TI\"]\) is False or Nothing if it is True.";

inputAidInterface::usage = "inputAidInterface[cellobj, inputAidContent, replaceButtonAction, updateButtonAction, isvalid, validmsg] generates an interface for an input aid acting on cellobj";

inputAidInputField::usage = "inputAidInputField[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"type\", \"TI\"]\)] represents an input field that takes the contents of the input field to be the dynamically updated current value of \!\(\*StyleBox[\"x\", \"TI\"]\) whose contents are taken to be of the specified \!\(\*StyleBox[\"type\", \"TI\"]\) with formatting and options identical to evaporativeInputField.";

inputAidPopupMenu::usage = "inputAidPopupMenu[\!\(\*StyleBox[\"x\", \"TI\"]\), {\!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), ...}] represents a popup menu that takes the setting to be the dynamically updated current value of  \!\(\*StyleBox[\"x\", \"TI\"]\) and possible values \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"i\", \"TI\"]]\) with formatting and options identical to evaporativePopupMenu.\n \
inputAidPopupMenu[\!\(\*StyleBox[\"x\", \"TI\"]\), {\!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"1\", \"TR\"]]\)\[Rule]\!\(\*SubscriptBox[StyleBox[\"lbl\", \"TI\"], StyleBox[\"1\", \"TR\"]]\), \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"2\", \"TR\"]]\)\[Rule]\!\(\*SubscriptBox[StyleBox[\"lbl\", \"TI\"], StyleBox[\"2\", \"TR\"]]\), ...}] represents a popup menu in which possible value \!\(\*SubscriptBox[StyleBox[\"val\", \"TI\"], StyleBox[\"i\", \"TI\"]]\) is indicated by \!\(\*SubscriptBox[StyleBox[\"lbl\", \"TI\"], StyleBox[\"i\", \"TI\"]]\).";

inputAidUnitsMenu::usage = "inputAidUnitsMenu[\!\(\*StyleBox[\"x\", \"TI\"]\), \!\(\*StyleBox[\"unittype\", \"TI\"]\)] represents a popup menu that takes the setting to be the dynamically updated current value of \!\(\*StyleBox[\"x\", \"TI\"]\) and possible values determined by \!\(\*StyleBox[\"unittype\", \"TI\"]\) with formatting and options identical to evaporativeUnitsMenu.";

readInputAidTaggingRules::usage = "readInputAidTaggingRules[vars, tags, cellobj, aid] initializes the list of Symbols var to the list tags of TaggingRules subselectors stored in cellobj under selector aid.";

writeInputAidTaggingRules::usage = "writeInputAidTaggingRules[tags, vars] creates a list of Rules setting the list tags of TaggingRules subselectors to the values of the list vars suitable for use in inputAidCellButtonAction.";

inputAidFromTemplate::usage = "inputAidFromTemplate[cellobj, templateName, symbolStrings, aid] generates a standardized input aid acting on cellobj from a cell template function named templateName (as a string) that takes a list of inputs named symbolStrings (each as a string) and called from a top-level input aid named aid (as a string).";

initializeInputAidCellFromTemplate::usage = "initializeInputAidCellFromTemplate[values, obj, templateName, symbolStrings, inputAidName] creates an Input cell (at the current selection if obj is a NotebookObject or replacing obj if it is a CellObject) from a cell template function named templateName (as a string) that takes a list of inputs named symbolStrings (each as a string) initialized to a list of values and attached to an input aid named aid (as a string).";


Begin["`Private`"]

$$useVersion11Compatibility = True;



(* InputField: ImageSize->x gives same size inputField in v11 - v13 *)
(* FieldSize->x in v11 is ~same size as FieldSize->(1.3*x) in v13 (v13 is smaller). So this block adjusts so size is consistent across versions *)
(* Note: default (Automatic) FieldSize is 20. But we set/scale explicitly, else fields in older content may be too small *)
(* Note: Magnification also affects the relative size of input fields *)
If[!$$useVersion11Compatibility || $VersionNumber < 12,
	scaleInputFieldOptions[opts___] := FilterRules[{opts}, Options[InputField]];
	,
	scaleInputFieldOptions[opts___] :=
		Module[{fldSize, imgSize, fieldOpts = FilterRules[{opts}, Options[InputField]]},
			fldSize = Lookup[fieldOpts, FieldSize, Automatic];
			imgSize = Lookup[fieldOpts, ImageSize, Automatic] /. {Small -> 132, Large -> 396};
			fieldOpts = Normal@KeyDrop[fieldOpts, {FieldSize, ImageSize}];
			
			Which[
				NumericQ@imgSize, Append[fieldOpts, ImageSize -> imgSize]
				, NumericQ@fldSize, Append[fieldOpts, ImageSize -> 13.2*fldSize] (* this assumes default font size, but the scaling is wacky with font/magnification anyway *)
				, True, Append[fieldOpts, ImageSize -> 264]
			]
		];
];


(* PopupMenu: v12+ style changed so there is no longer a white, sunken section with the value and a gray arrow button (on Windows). It's just one gray box and doesn't respect the Background option we use to indicate changed values (all v12+ Appearances are ~identical) *)
If[!$$useVersion11Compatibility || $VersionNumber < 12,
	popupMenu[p___] := PopupMenu[p];
	,
	(* This recreates the value + arrow sections using the 4th param of PopupMenu, which allows manually styling the menu. popupMenu's style is mostly fixed and only respects the Background option *)
	(* todo: SuperscriptBox text is same size as regular text in v13. Can be hard to read. "Input" style makes the supersript a bit smaller. Possibly this is fixed in v13.1 - Bug #2033 - then can remove BaseStyle->"Input" *)
	expandPopupList[key_ -> val_] := key -> val;
	expandPopupList[val_] := val -> val;
	popupMenu[Dynamic[v_], list_, opts:OptionsPattern[PopupMenu]] :=
		With[{ruleList = expandPopupList/@list}, (* this 4th parameter doesn't do the key->val display mapping that PopupMenu usually does, so we do it manually *)
		PopupMenu[Dynamic[v], ruleList, "",
			Row[{
				 Button[Dynamic[v/.ruleList], ImageSize->{Automatic, 24}, BaseStyle->{"Input", FontWeight->"Plain", FontFamily->"Segoe UI"}, Alignment->{Center, Center}, Background->OptionValue[Background], FrameMargins->{{2,2},{0,-4}}, BaselinePosition->Baseline] (* BaseStyle affects the scale of superscript relative to base. GenericButton (dflt) has superscript same font size as base *)
				,Button["\[DownPointer]", ImageSize->{16, 24}, BaseStyle->{20}, Background->GrayLevel[.85], ContentPadding->False, Alignment->{Center, Center}, BaselinePosition->Baseline, FrameMargins->{{-1, 1}, {-2, -6}}]
			}]
			, opts
		]];
];


$$helpIcon = Image[CompressedData["
1:eJxTTMoPSmNiYGAo5gASQYnljkVFiZXBAkBOaF5xZnpeaopnXklqemqRRRIz
UFAAilkYaAAcekQEAyd3i4VOucPm2febya3nv3DwlMd8/hMnMXh2yuDTyuzW
HSEYMPldXPf2n0evv/j/7MP3/y8///x/8tbL/8l9u34JBEz+yO09MR6XXuGg
Kd9B+n78/Q/Wp1687r9sziowGyR29u7r/xJhU79gmAF0s0jQlNcwvSD89tvv
/9pl68FmgNgwcZAZIHcweHVJwLSD/AtyM0wNMj5x9w2KfhAG+YXPb9I0mH5Q
WCHbDcILDt/9b1y96T9f0pL/t19+QZEDhQcoTGH6WTx6/3759RfDbs2iNf95
4hYA9X/GkAPFCyH9Grkr/nNFzP5/68UnvPqxuR+EVdMX/2cPnPb/1vNPeN2P
K/yUk+b/Z/We+P8emvvRww+c5oDpBtkNUzZf+M/lMwHszrxp+zHjz6NfAV/6
wYZxph8kM/gDJn9I6N3xC2QOLExB+mDpl8OzPwlfHgD5RSBgUr9w8NT7oHgB
uV8ocPJTsH/R3EwNAADbagRS
"], "Byte", ColorSpace -> "RGB", Interleaving -> True, ImageSize->{16,16}];

(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Evaporative Controls*)


(* ::Subsubsection:: *)
(*evaporativeInputField*)


(*
	evaporativeInputField creates a Boxes-style InputField for editing the box
	representation of expr, and which evaluates as the parsed boxes would.
	It also turns off the Appearance of the InputField, and instead specifies 
	aspects of the framing via Framed because InputField doesn't support changing 
	Background color. The input default specifies the value of expr originally 
	supplied to the input. If the user changes the input field then expr will 
	not match default, and the field color will be highlighted to indicate the change.
	Based on the prototype by Lou D'Andria, Wolfram Research.
*)

SetAttributes[evaporativeInputField, HoldFirst]

Options[evaporativeInputField]= Join[Options[InputField], Options[evaporativeInputTooltip]];

(*
	Handle the case of an initially-blank value.
*)
evaporativeInputField[opts:OptionsPattern[]] := evaporativeInputField[Null, Boxes, opts]

evaporativeInputField[expr_, opts:OptionsPattern[]] := evaporativeInputField[expr, Boxes, opts]

(*
	Handle the Expression, Number, and String types
*)
evaporativeInputField[expr_, type:(Expression|Number|String), opts:OptionsPattern[]] :=
	DynamicSetting[
		DynamicModule[{
			evaporativeInputFieldVariable = expr, 
			orig = expr
		},
			Interpretation[
				evaporativeInputTooltip[evaporativeInputFieldVariable, FilterRules[{opts},Options[evaporativeInputTooltip]]][
					Framed[
						InputField[Dynamic[evaporativeInputFieldVariable], type, scaleInputFieldOptions[opts], Appearance -> None],
						FrameStyle -> evaporativeInputFieldFrameStyle[evaporativeInputFieldVariable, FilterRules[{opts},Options[evaporativeInputFieldFrameStyle]]],
						FrameMargins -> 1,
						Background -> evaporativeInputFieldBackground[evaporativeInputFieldVariable, orig],
						BaselinePosition -> Baseline
					]
				],
				evaporativeInputFieldVariable
			]
		]
	]

(*
	Handle the Boxes type separately: expects expr to be in box form, and we need 
	to convert from box form for intrepretation, tooltip, background and frame.
	We Quiet the ToExpression calls in the tooltip, frame and background to 
	prevent improperly-formatted expressions from generating errors while
	the user is still editing the field. The final ToExpression which is run
	when the field is evaluated is not Quieted because the user should see
	any remaining errors before proceeding. 
*)
evaporativeInputField[expr_, Boxes, opts:OptionsPattern[]] :=
	DynamicSetting[
		DynamicModule[{
			(* handle special case of Null - MakeBoxes returns "Null" but InputField gives "" *)
			evaporativeInputFieldVariable = Replace[expr, {Null -> "", e_ :> e}],
			orig = Replace[expr, {Null -> "", e_ :> e}]
		},
			Interpretation[
				evaporativeInputTooltip[Quiet@ToExpression[evaporativeInputFieldVariable, StandardForm], FilterRules[{opts},Options[evaporativeInputTooltip]]][
					Framed[
						InputField[Dynamic[evaporativeInputFieldVariable], Boxes, scaleInputFieldOptions[opts], Appearance -> None, BaselinePosition->Baseline],
						FrameStyle -> evaporativeInputFieldFrameStyle[Quiet@ToExpression[evaporativeInputFieldVariable, StandardForm], FilterRules[{opts},Options[evaporativeInputFieldFrameStyle]]],
						FrameMargins -> 1,
						Background -> evaporativeInputFieldBackground[Quiet@ToExpression[evaporativeInputFieldVariable, StandardForm], Quiet@ToExpression[orig, StandardForm]],
						BaselinePosition -> Baseline
					]
				],
				ToExpression[evaporativeInputFieldVariable, StandardForm]
			]
		]
	]


(*
	evaporativeInputField Background styling to indicate changes from original value
*)

SetAttributes[evaporativeInputFieldBackground, HoldFirst]

evaporativeInputFieldBackground[var_, Null, opts:OptionsPattern[]] := Automatic

evaporativeInputFieldBackground[var_, orig_, opts:OptionsPattern[]] := Dynamic[If[var===orig,Automatic,LightYellow]]



(*
	evaporativeInputField Frame styling for validation success / failure
*)

SetAttributes[evaporativeInputFieldFrameStyle, HoldFirst]

Options[evaporativeInputFieldFrameStyle]={"InputFieldValidationFunction" -> None};

evaporativeInputFieldFrameStyle[expr_, opts:OptionsPattern[]] :=
	Module[{test = OptionValue["InputFieldValidationFunction"]},
		evaporativeInputFieldFrameStyleHandler[expr, test]
	]

SetAttributes[evaporativeInputFieldFrameStyleHandler, HoldFirst]

evaporativeInputFieldFrameStyleHandler[expr_, test_Function] :=
	Dynamic[If[test[expr]===True,GrayLevel[.7], Directive[Red,Thick]]]

evaporativeInputFieldFrameStyleHandler[expr_, ___] := GrayLevel[.7]


(*
	evaporativeInputTooltip Tooltip handling for validation failure messages
	Creates a function that is applied to an expression containing the InputField
*)

SetAttributes[evaporativeInputTooltip, HoldFirst]

Options[evaporativeInputTooltip]={"InputFieldValidationFunction" -> None, "InputFieldValidationMessage" -> None};

evaporativeInputTooltip[expr_, opts:OptionsPattern[]] :=
	Module[{
			test = OptionValue["InputFieldValidationFunction"],
			failmessage = OptionValue["InputFieldValidationMessage"]
		},
		evaporativeInputTooltipHandler[expr, test, failmessage]
	]

SetAttributes[evaporativeInputTooltipHandler, HoldFirst]

(* no message specified - don't output Tooltip *)
evaporativeInputTooltipHandler[expr_, test_, None] := Function[#]

evaporativeInputTooltipHandler[expr_, test_Function, failmessage_] :=
	Function[
		Tooltip[
			#,
			Dynamic[If[test[expr]===True,"",failmessage]],
			TooltipStyle->Dynamic[If[test[expr]===True,
				{Background -> Directive[{White, Opacity[0]}], CellFrame -> None}, 
				Automatic
			]]
		]
	]

(* no test function or other issue - don't output Tooltip *)
evaporativeInputTooltipHandler[expr_, ___] := Function[#]


(* ::Subsubsection:: *)
(*evaporativePopupMenu*)


(*
	evaporativePopupMenu creates a Boxes-style PopupMenu for editing the box
	representation of expr, and which evaluates as the parsed boxes would.
	Works the same as evaporativeInputField, but uses the Background option
	to set the color, which unfortunately produces an ugly control. To
	improve this we would need to build up the graphics manually, which isn't
	as easy for PopupMenu as it is for InputField.
*)

Options[evaporativePopupMenu] = Options[PopupMenu];
SetAttributes[evaporativePopupMenu, HoldFirst];

(* Handle the case of an initially-blank value. No highlighting of changes is performed. *)
evaporativePopupMenu[list_List, opts:OptionsPattern[]] := evaporativePopupMenu[Null, list, opts]


evaporativePopupMenu[Null, list_List, opts:OptionsPattern[]] :=
	DynamicSetting[
		DynamicModule[{evaporativePopupMenuVariable = ""},
			Interpretation[
				popupMenu[Dynamic[evaporativePopupMenuVariable], list, opts, Background -> White],
				evaporativePopupMenuVariable
			]
		]
	];

(* Handle the case with a provided value *)
evaporativePopupMenu[val_, list_List, opts:OptionsPattern[]] :=
	DynamicSetting[
		DynamicModule[{
			evaporativePopupMenuVariable = val, 
			orig = val
		},
			Interpretation[
				popupMenu[Dynamic[evaporativePopupMenuVariable], list, opts, Background -> Dynamic[If[evaporativePopupMenuVariable===orig,White,LightYellow]]],
				evaporativePopupMenuVariable
			]
		]
	];


(* ::Subsubsection:: *)
(*evaporativeUnitsMenu*)


(*
	Create a PopupMenu with units for an input UnitDimension. The value must be a Quantity with magnitude 1.
*)

Options[evaporativeUnitsMenu] = Options[evaporativePopupMenu];

(*
	Handle the case of an initially-blank value.
*)
evaporativeUnitsMenu[unittype_, opts:OptionsPattern[]] := evaporativeUnitsMenu[Null, unittype, opts]

evaporativeUnitsMenu[Null, unittype_, opts:OptionsPattern[]] := evaporativePopupMenu[Null, unitsMenuList[unittype], opts]

(*
	Handle the case with a provided value
*)
evaporativeUnitsMenu[val_, unittype_, opts:OptionsPattern[]] := evaporativePopupMenu[val, unitsMenuList[unittype], opts]


(*
	Lists of frequently used engineering units for various unittypes for use in evaporativeUnitsMenu
*)

(* handle input of a manually created list of units *)
(* list should be of the format {Quantity[1, "Meters"]\[Rule]"m",...}. Units created with the Units Palette work too. *)
unitsMenuList[unittype_List] := unittype

(* helper functions *)
unitAbbreviation[unitstring_] := QuantityUnits`Private`UnitAbbreviation[unitstring]
unitTooltip[unitstring_] := "unit: "<>(QuantityUnits`Private`qlsafe[unitstring,"Format"->"UnitString"]/.$Failed->"")
unitRule[unitstring_] := Quantity[1,unitstring]->Tooltip[RawBoxes[unitAbbreviation[unitstring]],unitTooltip[unitstring]]
(* public function *)
unitsMenuRule[unitstring_] := unitRule[unitstring]

(* strings form of unitsMenuList. Strings are not canonical. List of units is not complete - these are frequently used units for engineering *)
unitsMenuList["Length"] := unitRule/@{"Meters","Millimeters","Centimeters","Kilometers","Inches","Feet","Yards","Miles"}
unitsMenuList["Mass"] := unitRule/@{"Kilograms","MetricTons","Pounds","LongTons","ShortTons","SlugsMass"}
unitsMenuList["Time"] := unitRule/@{"Milliseconds","Seconds","Minutes","Hours","Days","Years"}
unitsMenuList["Angle"] := unitRule/@{"Radians","AngularDegrees"}
unitsMenuList["Temperature"] := unitRule/@{"DegreesCelsius","DegreesFahrenheit","Kelvins","DegreesRankine"}
unitsMenuList["Velocity"] := unitRule/@{"Meters"/"Seconds","Kilometers"/"Hours","Inches"/"Seconds","Feet"/"Seconds","Miles"/"Hours","Knots"}
unitsMenuList["AngularVelocity"] := unitRule/@{"Revolutions"/"Minutes","Radians"/"Seconds"}
unitsMenuList["Acceleration"] := unitRule/@{"Meters"/"Seconds"^2,"StandardAccelerationOfGravity","Feet"/"Seconds"^2,"Inches"/"Seconds"^2}
unitsMenuList["Area"] := unitRule/@{"Meters"^2,"Centimeters"^2,"Millimeters"^2,"Hectares","Inches"^2,"Feet"^2,"Acres","Miles"^2}
unitsMenuList["Volume"] := unitRule/@{"Centimeters"^3,"Meters"^3,"Liters","Inches"^3,"Feet"^3,"Gallons","BarrelsOfOil"}
unitsMenuList["Density"] := unitRule/@{"Kilograms"/"Meters"^3,"Grams"/"Centimeters"^3,"Pounds"/"Inches"^3,"Pounds"/"Feet"^3,"SlugsMass"/"Feet"^3,"Pounds"/"Gallons"}
unitsMenuList["Force"] := unitRule/@{"Newtons","Kilonewtons","PoundsForce","KipsForce"}
unitsMenuList["Pressure"] := unitRule/@{"Pascals","Kilopascals","Megapascals","Bars","Atmospheres","PoundsForce"/ "Feet"^2,"PoundsForce"/"Inches"^2,"FeetOfWaterColumn","InchesOfWaterColumn"}
unitsMenuList["Stress"] := unitRule/@{"Pascals","Megapascals","Gigapascals","PoundsForce"/"Inches"^2,"KipsForce"/"Inches"^2}
unitsMenuList["LinearLoad"] := unitRule/@{"Newtons"/"Meters","Kilonewtons"/"Meters","Newtons"/"Millimeters","PoundsForce"/"Inches","PoundsForce"/"Feet"}
unitsMenuList["Moment"] := unitRule/@{"Newtons"*"Meters","Kilonewtons"*"Meters","PoundsForce"*"Inches","PoundsForce"*"Feet", "KipsForce"*"Feet"}
unitsMenuList["Energy"] := unitRule/@{"Joules","Kilojoules","Kilowatts"*"Hours","Newtons"* "Meters","BritishThermalUnitsIT","CaloriesThermochemical","HorsepowerMechanical"*"Hours","Feet"*"PoundsForce"}
unitsMenuList["Power"] := unitRule/@{"Milliwatts","Watts","Kilowatts","Megawatts","HorsepowerMechanical","BritishThermalUnitsIT"/"Hours","Feet"*"PoundsForce"/"Seconds"}
unitsMenuList["MassFlowrate"] := unitRule/@{"Kilograms"/"Seconds","Kilograms"/"Hours","Pounds"/"Seconds","Pounds"/"Hours"}
unitsMenuList["VolumetricFlowrate"] := unitRule/@{"Meters"^3/"Seconds","Liters"/"Seconds","Feet"^3/"Seconds","Gallons"/"Minutes"}
unitsMenuList["DynamicViscosity"] := unitRule/@{"Centipoise","Pounds"/("Feet"*"Seconds")}
unitsMenuList["KinematicViscosity"] := unitRule/@{"Centistokes","Feet"^2/"Seconds"}
unitsMenuList["TemperatureDifference"] := unitRule/@{"DegreesCelsiusDifference","KelvinsDifference","DegreesFahrenheitDifference","DegreesRankineDifference"}
unitsMenuList["SpecificHeat"] := unitRule/@{"Joules"/("KelvinsDifference"*"Kilograms"),"BritishThermalUnitsIT"/("DegreesFahrenheitDifference"*"Pounds")}
unitsMenuList["ThermalConductivity"] := unitRule/@{"Watts"/("KelvinsDifference"*"Meters"),"BritishThermalUnitsIT"/("Feet"*"Hours"*"DegreesFahrenheitDifference")}
unitsMenuList["ThermalDiffusivity"] := unitRule/@{"Meters"^2/"Seconds","Feet"^2/"Seconds"}
unitsMenuList["HeatTransferCoefficient"] := unitRule/@{"Watts"/("KelvinsDifference"*"Meters"^2),"BritishThermalUnitsIT"/("Feet"^2*"Hours"*"DegreesFahrenheitDifference")}
unitsMenuList["HeatFlux"] := unitRule/@{"Watts"/"Meters"^2,"BritishThermalUnitsIT"/("Feet"^2*"Hours")}
unitsMenuList["ElectricalResistance"] := unitRule/@{"Ohms","Kiloohms","Megaohms","Gigaohms"}
unitsMenuList["Current"] := unitRule/@{"Microamperes","Milliamperes","Amperes","Biots"}
unitsMenuList["ElectricalCharge"] := unitRule/@{"Nanocoulombs","Microcoulombs","Millicoulombs","Coulombs"}
unitsMenuList["Voltage"] := unitRule/@{"Microvolts","Millivolts","Volts","Kilovolts"}
unitsMenuList["Capacitance"] := unitRule/@{"Picofarads","Nanofarads","Microfarads","Farads"}
unitsMenuList["Inductance"] := unitRule/@{"Nanohenries","Microhenries","Millihenries","Henries"}
unitsMenuList["MagneticFlux"] := unitRule/@{"Nanowebers","Microwebers","Milliwebers","Webers"}
unitsMenuList["MagneticFluxDensity"] := unitRule/@{"Nanoteslas","Microteslas","Milliteslas","Teslas"}

(* UnitDimensions form of unitsMenuList *)
unitsMenuList[{{"LengthUnit",1}}] := unitsMenuList["Length"]
unitsMenuList[{{"MassUnit",1}}] := unitsMenuList["Mass"]
unitsMenuList[{{"TimeUnit",1}}] := unitsMenuList["Time"]
unitsMenuList[{{"AngleUnit",1}}] := unitsMenuList["Angle"]
unitsMenuList[{{"TemperatureUnit",1}}] := unitsMenuList["Temperature"]
unitsMenuList[{{"LengthUnit",1},{"TimeUnit",-1}}] := unitsMenuList["Velocity"]
unitsMenuList[{{"AngleUnit",1},{"TimeUnit",-1}}] := unitsMenuList["AngularVelocity"]
unitsMenuList[{{"LengthUnit",1},{"TimeUnit",-2}}] := unitsMenuList["Acceleration"]
unitsMenuList[{{"LengthUnit",2}}] := unitsMenuList["Area"]
unitsMenuList[{{"LengthUnit",3}}] := unitsMenuList["Volume"]
unitsMenuList[{{"LengthUnit",-3},{"MassUnit",1}}] := unitsMenuList["Density"]
unitsMenuList[{{"LengthUnit",1},{"MassUnit",1},{"TimeUnit",-2}}] := unitsMenuList["Force"]
unitsMenuList[{{"LengthUnit",-1},{"MassUnit",1},{"TimeUnit",-2}}] := unitsMenuList["Pressure"]
(* Moment and Energy are same units. Combine them. To get only Moment or Energy, specify unittype string instead of UnitDimensions *)
unitsMenuList[{{"LengthUnit",2},{"MassUnit",1},{"TimeUnit",-2}}] := Join[unitsMenuList["Moment"], unitsMenuList["Energy"]]
unitsMenuList[{{"LengthUnit",2},{"MassUnit",1},{"TimeUnit",-3}}] := unitsMenuList["Power"]
unitsMenuList[{{"MassUnit",1},{"TimeUnit",-1}}] := unitsMenuList["MassFlowrate"]
unitsMenuList[{{"LengthUnit",3},{"TimeUnit",-1}}] := unitsMenuList["VolumetricFlowrate"]
unitsMenuList[{{"LengthUnit",-1},{"MassUnit",1},{"TimeUnit",-1}}] := unitsMenuList["DynamicViscosity"]
unitsMenuList[{{"LengthUnit",2},{"TimeUnit",-1}}] := unitsMenuList["KinematicViscosity"]
unitsMenuList[{{"TemperatureDifferenceUnit",1}}] := unitsMenuList["TemperatureDifference"]
unitsMenuList[{{"LengthUnit",2},{"TemperatureDifferenceUnit",-1},{"TimeUnit",-2}}] := unitsMenuList["SpecificHeat"]
unitsMenuList[{{"LengthUnit",1},{"MassUnit",1},{"TemperatureDifferenceUnit",-1},{"TimeUnit",-3}}] := unitsMenuList["ThermalConductivity"]
unitsMenuList[{{"LengthUnit",2},{"TimeUnit",-1}}] := unitsMenuList["ThermalDiffusivity"]
unitsMenuList[{{"MassUnit",1},{"TemperatureDifferenceUnit",-1},{"TimeUnit",-3}}] := unitsMenuList["HeatTransferCoefficient"]
unitsMenuList[{{"MassUnit",1},{"TimeUnit",-3}}] := unitsMenuList["HeatFlux"]
unitsMenuList[{{"ElectricCurrentUnit",-2},{"LengthUnit",2},{"MassUnit",1},{"TimeUnit",-3}}] := unitsMenuList["ElectricalResistance"]
unitsMenuList[{{"ElectricCurrentUnit",1}}] := unitsMenuList["Current"]
unitsMenuList[{{"ElectricCurrentUnit",1},{"TimeUnit",1}}] := unitsMenuList["ElectricalCharge"]
unitsMenuList[{{"ElectricCurrentUnit",-1},{"LengthUnit",2},{"MassUnit",1},{"TimeUnit",-3}}] := unitsMenuList["Voltage"]
unitsMenuList[{{"ElectricCurrentUnit",2},{"LengthUnit",-2},{"MassUnit",-1},{"TimeUnit",4}}] := unitsMenuList["Capacitance"]
unitsMenuList[{{"ElectricCurrentUnit",-2},{"LengthUnit",2},{"MassUnit",1},{"TimeUnit",-2}}] := unitsMenuList["Inductance"]
unitsMenuList[{{"ElectricCurrentUnit",-1},{"LengthUnit",2},{"MassUnit",1},{"TimeUnit",-2}}] := unitsMenuList["MagneticFlux"]
unitsMenuList[{{"ElectricCurrentUnit",-1},{"MassUnit",1},{"TimeUnit",-2}}] := unitsMenuList["MagneticFluxDensity"]

(* if a Quantity is passed in, get the UnitDimensions and use that form of unitsMenuList *)
unitsMenuList[quantity_Quantity] := unitsMenuList[UnitDimensions[quantity]]


(* ::Subsection:: *)
(*Parsing input cells, templates, replacement of values and controls*)


(* ::Subsubsection:: *)
(*Parse Cell for variables and controls*)


(*
	Return the position of each Set row from boxes, typically the BoxData portion of a Cell expression
*)
setRowPositions[boxes_] := Position[boxes, RowBox[{__,  ("="|":="), __}], Infinity]


(*
	Get the variable name from a Set row. The input boxes should be extracted using
	setRowPositions. Assumes that there could be a space (or some other character that 
	isn't part of the variable's RowBox) after the variable. A space before the variable 
	name seems to be put into a RowBox that wraps the Set row.
*)
variableNameFromSetRow[boxes_] := boxes /. {RowBox[{var_, ___, ("="|":="), __}] :> var}


(*
	Find the position of the evaporative controls in boxes. The position is relative to boxes
	Using SymbolName[Unevaluated[var]] so that the context of the variable is not considered - usually
	$CellContext` is appended but not when calling function from an attached cell. Unevaluated is just in 
	case someone has defined a value for a global variable of the same name so we don't get the value.
*)
evaporativeControlPositions[boxes_] :=
	Position[
		boxes,
		(* assume that there can be other DynamicModule variables before and after the evaporative control variable *)
		(* need one pattern for each type of evaporative control supported *)
		TagBox[HoldPattern[DynamicModuleBox][{___,_[var_/;MemberQ[{"evaporativeInputFieldVariable$$","evaporativePopupMenuVariable$$"},SymbolName[Unevaluated[var]]], _],___}, __], Setting]
	]

(* if boxes is part of a larger expression, give the base position of boxes and return absolute positions within the larger expression *)
evaporativeControlPositions[boxes_, baseposition_] :=
	Map[
		Join[baseposition,#]&,
		evaporativeControlPositions[boxes]
	]


(*(*
	Get the control type and value from the boxes for an evaporative control and return as an association
*)
getEvaporativeControlTypeAndValue[controlboxes_] := 
	controlboxes /. {
		TagBox[HoldPattern[DynamicModuleBox][{___,_[HoldPattern[$CellContext`evaporativeInputFieldVariable$$], val_],___}, __], Setting]\[RuleDelayed]<|"control type"\[Rule]"evaporativeInputField", "control value"\[Rule]val|>,
		TagBox[HoldPattern[DynamicModuleBox][{___,_[HoldPattern[$CellContext`evaporativePopupMenuVariable$$], val_],___}, __], Setting]\[RuleDelayed]<|"control type"\[Rule]"evaporativePopupMenu", "control value"\[Rule]val|>
	}
*)


(*
	Get the control type and value from the boxes for an evaporative control and return as an association
	Using SymbolName[Unevaluated[var]] so that the context of the variable is not considered - usually
	$CellContext` is appended but not when calling function from an attached cell. Unevaluated is just in 
	case someone has defined a value for a global variable of the same name so we don't get the value.
*)
getEvaporativeControlTypeAndValue[controlboxes_] :=
	controlboxes /. {
		TagBox[HoldPattern[DynamicModuleBox][{___,_[var_/;(SymbolName[Unevaluated[var]]==="evaporativeInputFieldVariable$$"), val_],___}, __], Setting] :> <|"control type"->"evaporativeInputField", "control value"->val|>,
		TagBox[HoldPattern[DynamicModuleBox][{___,_[var_/;(SymbolName[Unevaluated[var]]==="evaporativePopupMenuVariable$$"), val_],___}, __], Setting] :> <|"control type"->"evaporativePopupMenu", "control value"->val|>
	}



(*
	Parse a single Set row to extract control(s) position, type and value
*)
parseEvaporativeControls[rowboxes_,rowpos_] :=
	Module[{relativepos, absolutepos, controls},
		(* absolute position of evaporative controls within BoxData *)
		absolutepos = evaporativeControlPositions[rowboxes, rowpos];
		(* relative position of evaporative controls within BoxData *)
		relativepos = evaporativeControlPositions[rowboxes];
		controls = Extract[rowboxes, relativepos];
		(* return list of evaporative control position, type and value *)
		MapThread[
			Join[<|"control position"->#1|>,getEvaporativeControlTypeAndValue[#2]]&,
			{absolutepos, controls}
		]
	]


(*
	Parse all Set rows and controls in a Cell expression, returning an association with positions (relative to the BoxData expression) and values.
*)
parseSetRows[Cell[boxdata_BoxData, rest___]] :=
	Module[{pos, setRows},
		(* position of Set rows in BoxData portion of Cell *)
		pos = setRowPositions[boxdata];
		(* extract Set rows *)
		setRows = Extract[boxdata,pos];
		(* parse Set rows to give variable and list of control types, *)
		(* position of evaporative controls in Set rows *)
		Dataset[Association@@MapThread[
			#3-><|
				"index"->#3,
				"set row position" -> #1,
				"variable name" -> variableNameFromSetRow[#2],
				"controls" -> parseEvaporativeControls[#2, #1]
			|>&,
			{pos, setRows, Range[Length[pos]]}
		]]
	]


(* ::Subsubsection:: *)
(*Validate parsed set rows*)


(*
	Check if the variable name is present in parsedSetRows and if so check 
	that controls matches the list of control types.
*)

SetAttributes[selectSetRowByVariableName, HoldFirst]

selectSetRowByVariableName[variable_, parsedSetRows_Dataset] :=
	parsedSetRows[SelectFirst[(#["variable name"]==MakeBoxes[variable])&]]


(*
	Check if the variable name is present in parsedSetRows and if so check 
	that controls matches the list of control types.
*)

SetAttributes[setRowMatchQ, HoldFirst]

setRowMatchQ[variable_, controls_, parsedSetRows_Dataset] := 
	If[
		!MissingQ[parsedSetRows[selectSetRowByVariableName[variable,data]]],
		If[
			controls===Normal@parsedSetRows[SelectFirst[#["variable name"]==MakeBoxes[variable]&],"controls",All,"control type"],
			True,
			False
		],
		False
	]
	
setRowMatchQ[variable_, parsedSetRows_Dataset] := !MissingQ[parsedSetRows[SelectFirst[(#["variable name"]==MakeBoxes[variable])&],"index"]]


(* ::Subsubsection:: *)
(*Templates to define and manipulate input cells*)


(* ::Text:: *)
(*To create a new cell from an Input Aid we need:*)
(*	Row template with variable names, controls (with value, field hint, lists, etc.). Semicolon (CompoundExpression) cannot be included in template, as it will evaluate before Set and the variable will be assigned Null. Use compoundExpression input to turn off ;  It will be on by default.*)
(*	Support non-Set rows  - for example, comments, or commands that validate input*)


(*
	create an association template with fields necessary to make an evaporativeInputField
	Enter Null for value for an initially empty field
	inputType may be Expression, Number, String, or Boxes
	Enter None to disable InputFieldValidationFunction or InputFieldValidationMessage
*)

Options[inputFieldTemplate] = Options[InputField];

inputFieldTemplate[value_, opts:OptionsPattern[]] := inputFieldTemplate[value, Boxes, None, None, Null, opts]

inputFieldTemplate[value_, inputType:(Expression|Number|String|Boxes), opts:OptionsPattern[]] := inputFieldTemplate[value, inputType, None, None, Null, opts]

inputFieldTemplate[value_, inputType:(Expression|Number|String|Boxes), validationFunction:(_Function|None), validationMessage_, fieldHint_, opts:OptionsPattern[]] := 
	<|
		"control type" -> "evaporativeInputField",
		"control value" -> value,
		"input type" -> inputType,
		"validation function" -> validationFunction,
		"validation message" -> validationMessage,
		"field hint" -> fieldHint,
		"InputField options" -> FilterRules[{opts},Options[inputFieldTemplate]]
	|>


(*
	create an association template with fields necessary to make an evaporativePopupMenu
	Enter Null for value for an initially unselected menu
*)

Options[popupMenuTemplate] = Options[PopupMenu];

popupMenuTemplate[value_, list_List, opts:OptionsPattern[]] := 
	<|
		"control type" -> "evaporativePopupMenu",
		"control value" -> value,
		"menu list" -> list,
		"PopupMenu options" -> FilterRules[{opts},Options[PopupMenu]]
	|>


(*
	create an association template with fields necessary to make an evaporativeUnitsMenu
	Enter Null for value for an initially unselected menu
*)

Options[unitsMenuTemplate] = Options[PopupMenu];

unitsMenuTemplate[value_, unittype_, opts:OptionsPattern[]] := 
	<|
		"control type" -> "evaporativeUnitsMenu",
		"control value" -> value,
		"unit type" -> unittype,
		"PopupMenu options" -> FilterRules[{opts},Options[PopupMenu]]
	|>


(*
	create evaporative control from a template definition
	If we ever needed to support passing boxes into the evaporativeInputField 
	expr we will need to wrap them in RawBoxes because evaporativeInputField
	runs MakeBoxes on expr.
*)


evaporativeControlFromTemplate[template_Association] :=
	With[{
			type=template[["control type"]]
		},
		Switch[type,
			"evaporativeInputField",
			With[{
					value=template[["control value"]],
					inptype=template[["input type"]],
					fcn=template[["validation function"]],
					msg=template[["validation message"]],
					hint=template[["field hint"]],
					opts=template[["InputField options"]]
				}, 
				If[MissingQ[value],
					$Failed,
					evaporativeInputField[
						value, 
						Replace[inptype, Except[Expression|Number|String|Boxes] -> Boxes],
						"InputFieldValidationFunction" -> Replace[fcn, Except[_Function|None] -> None],
						"InputFieldValidationMessage" -> Replace[msg, _Missing -> None],
						FieldHint -> Replace[hint, _Missing -> Null],
						Replace[opts,{_Missing :> Sequence[], o_ :> Sequence@@o}]
					]
				]
			],
			"evaporativePopupMenu",
			With[{
					value=template[["control value"]],
					list=template[["menu list"]],
					opts=template[["PopupMenu options"]]
				},
				If[MissingQ[value]||MissingQ[list],
					$Failed,
					evaporativePopupMenu[value, list, Replace[opts,{_Missing :> Sequence[], o_ :> Sequence@@o}]]
				]
			],
			"evaporativeUnitsMenu",
			With[{
					value=template[["control value"]],
					unittype=template[["unit type"]],
					opts=template[["PopupMenu options"]]
				},
				If[MissingQ[value]||MissingQ[unittype],
					$Failed,
					evaporativeUnitsMenu[value, unittype, Replace[opts,{_Missing :> Sequence[], o_ :> Sequence@@o}]]
				]
			],
			(*  Missing or not supported *)
			_,
			$Failed
		]
	]




(*
	create an association template for a row that Sets a variable to a list of one or more controls
*)

SetAttributes[setRowTemplate, HoldFirst]

setRowTemplate[variable_, controls_List, compoundExpression_:True, tooltip_:None] := setRowTemplateBoxes[MakeBoxes[variable], controls, compoundExpression, tooltip]


(*
	This form is necessary because in packages the context of symbols in var can be evaluated in 
	SetRowTemplate even though it has the HoldFirst attribute. This context is
	written in the notebook when a cell is generated from the templates, and would cause
	problems since the package expects the variable to be in the Global` context. Therefore
	this function allows the package developer to convert the variable with
	MakeBoxes before passing to setRowTemplateBoxes, which will use them without conversion.
*)

setRowTemplateBoxes[variableBoxes_, controls_List, compoundExpression_:True, tooltip_:None] :=
	<|
		"row type" -> "setRow",
		"variable name" -> variableBoxes,
		"variable tooltip" -> tooltip,
		"controls" -> controls,
		"compoundExpression" -> compoundExpression
	|>


(*
	create boxes for a Set row. Does not support "variable tooltip", which is used for input aids. 
	To do so in input cells we would need to wrap the variable in Intrepretation, which would 
	conflict with the philosophy of keeping the input cells understandable by users.
*)

setRowTemplateToBoxes[template_Association] := 
	Module[{
			var=template[["variable name"]],
			controls=template[["controls"]],
			expr,
			compoundExpression=template[["compoundExpression"]]
		},
		expr=RowBox[
					Map[
						With[{c=evaporativeControlFromTemplate[#]},
							MakeBoxes[c]
						]&,
						controls
					]
				];
		If[compoundExpression===False,
			RowBox[{var, "=", expr}],
			RowBox[{RowBox[{var, "=", expr}],";"}]
		]
	]


(*
	Create an association template for a Set row with an expression that is repeated verbatim.
	Place a RawBoxes[TagBox[n, "control"]] into expr to specify where the control #n should be
	inserted. verbatimRowTemplateToBoxes will search and replace them with the controls.
	Default is to place a semicolon at the end of the expression to suppress output, set 
	compoundExpression to False to eliminate the semicolon. 
*)

SetAttributes[verbatimSetRowTemplate, HoldFirst]

verbatimSetRowTemplate[{var_,expr_}] :=
	verbatimSetRowTemplate[{var,expr}, {}, True]

verbatimSetRowTemplate[{var_,expr_}, controls_List, compoundExpression_:True, tooltip_:None] :=
	verbatimSetRowTemplateBoxes[{MakeBoxes[var],MakeBoxes[expr]}, controls, compoundExpression, tooltip]


(*
	This form is necessary because in packages the context of symbols in var and expr can be evaluated in 
	verbatimSetRowTemplate even though it has the HoldFirst attribute. This context is
	written in the notebook when a cell is generated from the templates, and would cause
	problems since the package expects the symbol to be in the Global` context. Therefore
	this function allows the package developer to convert the variable and expression with
	MakeBoxes before passing to verbatimSetRowTemplateBoxes, which will use them without 
	conversion.	
*)
verbatimSetRowTemplateBoxes[{varBoxes_,exprBoxes_}] := 
	verbatimSetRowTemplateBoxes[{varBoxes,exprBoxes}, {}, True]

verbatimSetRowTemplateBoxes[{varBoxes_,exprBoxes_}, controls_List, compoundExpression_:True, tooltip_:None] :=
	<|
		"row type" -> "verbatimSetRow",
		"variable name" -> varBoxes,
		"variable tooltip" -> tooltip,
		"expression" -> exprBoxes,
		"controls" -> controls,
		"compoundExpression" -> compoundExpression
	|>


(*
	create boxes for a verbatim row. Replace TagBox[n, "control"] with control #n in the controls list.
	If no controls are present, enter {} for "controls".
	"compoundExpression" controls whether a semicolon is placed at the end of the row.
	Does not support "variable tooltip", which is used for input aids. To do so in input cells we would 
	need to wrap the variable in Intrepretation, which would conflict with the philosophy of keeping 
	the input cells understandable by users.
*)

verbatimSetRowTemplateToBoxes[template_Association] :=
	With[{
		var=template[["variable name"]],
		expr=template[["expression"]],
		controls=template[["controls"]],
		compoundExpression=template[["compoundExpression"]]
	},
	Module[{n=Length[template[["controls"]]], newexpr},
		newexpr= If[n>0,
			expr /. MapIndexed[With[{c=evaporativeControlFromTemplate[#1]},TagBox[First@#2, "control"]->MakeBoxes[c]]&,controls]
			,
			expr
		];
		If[compoundExpression===False,
			RowBox[{var, "=", newexpr}]
			,
			RowBox[{RowBox[{var, "=", newexpr}],";"}]
		]
	]]


(*
	create an association template for a row that is repeated verbatim
*)

SetAttributes[verbatimRowTemplate, HoldFirst]

verbatimRowTemplate[expr_] := verbatimRowTemplateBoxes[MakeBoxes[expr]]


(*
	This form is necessary because in packages the context of symbols in expr can be 
	evaluated in verbatimRowTemplate even though it has the HoldFirst attribute. This context is
	written in the notebook when a cell is generated from the templates, and would cause
	problems since the package expects the symbol to be in the Global` context. Therefore
	this function allows the package developer to convert the expression with MakeBoxes before 
	passing to verbatimRowTemplateBoxes, which will use them without conversion.
*)
verbatimRowTemplateBoxes[expr_] :=
	<|
		"row type" -> "verbatimRow",
		"expression" -> expr
	|>


(*
	create boxes for a verbatim row
*)

verbatimRowTemplateToBoxes[template_Association] := template[["expression"]]


(*
	create boxes for a row
*)

rowTemplateToBoxes[template_Association] :=
	With[{rowtype=template[["row type"]]},
		Switch[rowtype,
			"setRow",
			setRowTemplateToBoxes[template],
			"verbatimSetRow",
			verbatimSetRowTemplateToBoxes[template],
			"verbatimRow",
			verbatimRowTemplateToBoxes[template],
			_,
			$Failed
		]
	]


(*
	create boxes for an input cell from a list of row templates.
	FromCharacterCode prevents the IndentingNewLine from evaluating before boxes are generated.
*)

cellTemplateToBoxes[template_List, opts:OptionsPattern[Cell]] :=
	With[{boxdata=Riffle[Map[rowTemplateToBoxes,template],FromCharacterCode[62371]]},
		Cell[BoxData[boxdata], "Input", opts]
	]


(* ::Subsubsection:: *)
(*Replace a control from a template*)


(*
	Extract a part of an expression and apply replacement rules on only that part.
	Returns the entire expression with replacements made on the part specified by
	position.
*)

replaceWithinPart[expr_, position_, rule_] :=
	ReplacePart[expr, position->(Extract[expr,position] /. rule)]


(* ::Text:: *)
(*We could leave the structure of the control alone and just change the value of the DynamicModule variable.*)
(*Particularly for a PopupMenu, this could cause an issue if the new value isn't on the list of values. We can instead replace the entire control from a control template.*)


(*
	Rules for replacing evaporative controls from a control template.
*)

evaporativeControlReplaceControlRule[template_Association] :=
	(* evaporativeInputField uses type Boxes, so need to apply MakeBoxes to value. Inject into held rule using With *)
	With[{c=evaporativeControlFromTemplate[template]},
	With[{cboxes=MakeBoxes[c]},
		(* assume that there can be other DynamicModule variables before and after the evaporative control variable *)
		(* need one pattern for each type of evaporative control supported *)
		TagBox[HoldPattern[DynamicModuleBox][{___,_[var_/;MemberQ[{"evaporativeInputFieldVariable$$","evaporativePopupMenuVariable$$"},SymbolName[Unevaluated[var]]], _],___}, __], Setting] :> 
		cboxes
	]]


(*
	Replace a single evaporative control from a control template by a position generated using evaporativeControlPositions and higher-level functions such as parseSetRows.
*)

replaceEvaporativeControlByPosition[Cell[boxdata_BoxData, rest___], {position_, template_Association}] := 
	Cell[replaceWithinPart[boxdata, position, evaporativeControlReplaceControlRule[template]], rest]


(*Application can present a choice to the user if there are errors to replace the entire cell from a template or proceed with making the limited replacements that worked.*)

(*
	First check if row type is a Set type - return the cell unchanged if not.
	Then check if the variable name is present in parsedSetRows and if so check 
	that controls matches the list of control types.
*)

replaceEvaporativeControlsInRowByTemplate[cell_Cell, rowtemplate_Association, parsedSetRows_Dataset] :=
	Module[{row},
		row=Replace[rowtemplate["row type"],
			{
				(* only rows of row type input are supported for controls replacement *)
				type_/;MemberQ[{"setRow","verbatimSetRow"},type] :> Normal@parsedSetRows[SelectFirst[(#["variable name"]==rowtemplate[["variable name"]])&]],
				(* verbatimRow or anything other row type will return row=$Failed *)
				else_ :> $Failed
			}
		];
		Replace[row,
			{
				(* verbatimRow or anything other row type will return row=$Failed - return the cell unchanged with no errors *)
				$Failed :> cell,
				(* mising variable - error condition *)
				r_Missing :> (
					Sow[StringForm["No matching row in Cell for variable named ``.", RawBoxes[rowtemplate[["variable name"]]]]]; 
					cell
				),
				(* this shouldn't ever happen - it represents a problem in the parsedSetRows - error condition *)
				r_/;MissingQ[r["controls"]] :> (
					Sow[StringForm["The parsed cell data in row for variable named `` is missing the controls key. Check programming.", RawBoxes[row[["variable name"]]]]]; 
					cell
				),
				(* normal operation *)
				r_/;(r[["controls",All,"control type"]]===(rowtemplate[["controls",All,"control type"]]/.{"evaporativeUnitsMenu"->"evaporativePopupMenu"})) :> (
					Fold[
						replaceEvaporativeControlByPosition[#1,#2]&,
						cell,
						Transpose[{row[["controls",All,"control position"]],rowtemplate[["controls"]]}]
					]
				),
				(* controls don't match template - error condition *)
				else_ :> (
					Sow[StringForm["Controls in row for the variable named `` do not match template controls.", RawBoxes[rowtemplate[["variable name"]]]]];
					cell
				)
			}
			,{0}
		]
	]


(*
	Take an input cell and replace all controls defined in the celltemplate list.
*)

replaceEvaporativeControlsInCellByTemplate[cell_Cell, celltemplate_List] :=
	Module[{data},
		data=parseSetRows[cell];
		Reap[Fold[
			replaceEvaporativeControlsInRowByTemplate[#1,#2,data]&,
			cell,
			celltemplate
		]]
	]


(* ::Subsection:: *)
(*Input Aids*)


(* ::Subsubsection:: *)
(*Input Aid interface*)


(*
	Create Cell expression for a named input aid from a template
*)
inputAidTemplateToBoxes[template_List, inputAidName_String, opts:OptionsPattern[Cell]] :=
	cellTemplateToBoxes[template, CellFrameLabels->{{None,ToBoxes[editButton[inputAidName]]},{None,None}}, opts]

(* if no aid is specified output cell without button *)
inputAidTemplateToBoxes[template_List, opts:OptionsPattern[Cell]] := 
	cellTemplateToBoxes[template, opts]


(*
	read a cell, check it against a template, and return cell contents, messages and T/F indication of whether the cell is valid
*)
readCellAndValidateAgainstTemplate[cellobj_CellObject, template_List] :=
	Module[{cell, validmsg},
		cell = NotebookRead[cellobj];
		(* check the cell against the template (with Null values). If they don't match, then handle the choice to replace the cell or cancel *)
		validmsg=Flatten@Last@replaceEvaporativeControlsInCellByTemplate[cell, template];
		{cell,validmsg,MatchQ[validmsg,{}]}
	]


(*
	check for errors validating input cells and output a Grid to notify the user or Nothing.
*)

(*
	"CellFormatWarningMessage" controls the message the user receives when the input 
	cell doesn't match the cell template. Automatic (default) issues the default warning
	text and a list of errors. None removes the message and list. Entering a string
	allows for a custom message and the standard list of errors. One example where a custom 
	message is advisable is when the cell template is based on other inputs, and the cell 
	needs to be replaced each time the other inputs change. The developer may want to 
	explain in the warning message that this is expected behavior.
*)
Options[inputAidCellFormatWarning]={"CellFormatWarningMessage"->Automatic};

inputAidCellFormatWarning[isvalid_?BooleanQ, validmsg_List, opts:OptionsPattern[]] :=
	Module[{warnmsg},
		warnmsg = Replace[OptionValue["CellFormatWarningMessage"], {
			m_String :> TextCell[m],
			None -> None,
			(* Automatic or any other type of input will give the default message *)
			_ -> TextCell["Warning: the rows in the cell do not have the expected format. Errors are shown below. Please cancel and fix the errors or choose \"Update Cell\" to replace the entire cell, including any user modifications, by a new input cell."]
		}];
		If[isvalid||warnmsg===None,
			Nothing,
			Grid[Transpose@{{
				warnmsg,
				OpenerView[{"Cell Format Errors",Column@validmsg}],
				Spacer[{1,10}]
			}},Alignment->Left, Dividers-> {False,{-2->GrayLevel[.7]}}]
		]
	]

inputAidCellFormatWarning[___] := $Failed


(*
	write an Input cell with an attached input aid and tagging rules that save the aid's state
	This can be called to initialize a new cell or to replace or update a cell from the aid
*)
writeInputAidCell[obj:(_CellObject|_NotebookObject), newcell_Cell, inputAidName_String, taggingrules_List] :=
	Module[{newcellObj,newNotebook},
		(* need the notebook that we are writing newcell to if obj is a CellObject, else newNotebook=obj *)
		newNotebook = Replace[obj, {o_CellObject:>ParentNotebook[o]} ];
		(* if obj is a CellObject then write replace obj with the new cell *)
		(* if obj is a NotebookObject then write the new cell at the current selection *)
		NotebookWrite[obj, newcell, All];
		(* get the new cell object from the selected cell (based on NotebookWrite selection=All) *)
		newcellObj = First[SelectedCells[newNotebook]];
		(* Set TaggingRules to save input aid state *)
		CurrentValue[newcellObj, {TaggingRules, inputAidName}] = taggingrules;
		(* return the new CellObject *)
		newcellObj
	]


(* ::Text:: *)
(*initializeInputAidCell creates a new Input cell with attached input aid initialized to values, and can be called by the code that generates a new notebook interface with custom input aids.*)
(*This allows the input aid controls to be initialized in addition to the evaporative controls. *)


(* ::Text:: *)
(*Because there is not a one-to-one correspondence between the controls in the cell and the input aid, values for both the template and the input aid need to be specified.*)
(*symbolStrings is a list of strings used to store the input aid variables in TaggingRules.*)


(*
	initialize an Input cell to templateValues and an attached input aid with tagging rules to aidValues
*)
initializeInputAidCell[templateValues_List, aidValues_List, obj:(_CellObject|_NotebookObject), templateName_String, symbolStrings_List, inputAidName_String] :=
	Module[{newcell},
		newcell=writeInputAidCell[
			obj,
			inputAidTemplateToBoxes[Symbol[templateName]@@templateValues, inputAidName],
			inputAidName,
			writeInputAidTaggingRules[symbolStrings, aidValues]
		];
		(* move the selection to after the created cell so notebook is ready to write another cell, else this cell will be overwritten *)
		SelectionMove[ParentNotebook[newcell],After,Cell]
	]


(*
	handle a Replace or Update Cell button click in an input aid. Returns new CellObject
	When updating a cell only pass the cell, not the messages.
	taggingrules should be a list of rules. It will be applied to the Cell's options as TaggingRules -> {inputAidName -> taggingrules}
*)
inputAidCellButtonAction[cellobj_CellObject, cell_Cell, inputAidName_String, taggingrules_List] :=
	Module[{newcellObj},
		Replace[
			cell, 
			{
				(* action if the function returns a Cell *)
				newcell_Cell :> (
					(* write the new cell to the cellobj the input aid was called from and set TaggingRules to save input aid state *)
					newcellObj = writeInputAidCell[cellobj, newcell, inputAidName, taggingrules];
					(* delete the attached cell *)
					NotebookDelete[EvaluationCell[]];
					newcellObj
				),
				(* Action if the function returns anything else, like $Failed *)
				(* This should never happen, so if it does it is a programming error. *)
				else_ :> (
					Beep[];
					MessageDialog[TextCell["An unexpected error occured"]];
					cellobj
				)
			}
		]
	]


(*
	inputAidAttachedCell[] is intended to be used in the ButtonFunction of a ButtonBox in a CellFrameLabel of a cell.
	When activated, it shows the interface (specified with a string of the function name) in an attached cell, overlaying the existing cell content.
	FrontEnd`AttachCell[
		parentObject,             (*Box or Cell Object*)
		attachedCellBoxes,
		{offset, alignment anchor in parentObject},
		alignment anchor in attachedCell,
		"ClosingActions" -> {...}
	]
	offset can be Automatic, a single number that applies to x and y (typically 0) or Offset[{dx,dy}, position] for greater control.

	We set the alignment anchors so that the input aid appears on top of the input cell.
	This ensures that the window can always be resized and scrolled to view the input aid.
	
	ClosingActions is set to only EvaluatorQuit and ParentChanged. It was found that the use of OutsideMouseClick and SelectionDeparture
	prevented the user from copying data from another notebook and that the cell would often close unintentionally.
*)
inputAidAttachedCell[inputAidName_String] :=
	MathLink`CallFrontEnd[FrontEnd`AttachCell[
		ParentCell @ EvaluationCell[],
		Cell[
			BoxData@ToBoxes[Symbol[inputAidName][ParentCell @ EvaluationCell[]]]
			, "Input"
			(* Note: this is to maintain look consistent with v11 without redesigning things. *)
			,Sequence@@If[!$$useVersion11Compatibility || $VersionNumber < 12, {}, {
				 Magnification -> 0.75
				,GraphicsBoxOptions -> {BaseStyle -> {Magnification -> .75} }
				,RasterBoxOptions -> {"SmoothingQuality" -> "High"}
				,InputFieldBoxOptions -> {BaseStyle  -> {FontSize->13} }
				,GridBoxOptions -> {BaseStyle->{FontSize->13}}
				}
			]
		],
		{0, {Left, Top}},
		{Left, Top},
		"ClosingActions" -> {"EvaluatorQuit", "ParentChanged"}
	]]


(*
	editButton is a simple utility for building the button intended to be placed in the CellFrameLabel of a cell.
*)
editButton[inputAidName_String] := Button["Input Aid", inputAidAttachedCell[inputAidName], BaseStyle -> {"DialogStyle", FontSize->12}, Method -> "Queued"]


(*
	inputAidInterface creates a standard input aid interface that handles validation messages, content and buttons
*)

SetAttributes[inputAidInterface, HoldAll]

Options[inputAidInterface]=Append[Options[inputAidCellFormatWarning], "HelpMessage"->Null];

inputAidInterface[cellobj_, inputAidContent_, replaceButtonAction_, updateButtonAction_, isvalid_, validmsg_, opts:OptionsPattern[]] :=
	(* inject value of options using With so they evaluated, otherwise the OptionValue expressions are Held inside the Dynamic *)
	With[{
			cellFormatWarningMessage=OptionValue["CellFormatWarningMessage"],
			helpMessage=OptionValue["HelpMessage"]
		},
		DynamicModule[{showHelp=False, replace=!isvalid},
			Framed[
				Column[{
					(* If the parsed cell doesn't match the template we are writing, then present user a dialog asking to replace the entire cell or cancel *)
					inputAidCellFormatWarning[isvalid, validmsg, "CellFormatWarningMessage" -> cellFormatWarningMessage],
					inputAidContent,
					Spacer[{1,20}],
					Item[
						Row[{
							If[helpMessage=!=Null, Button[$$helpIcon, showHelp = !showHelp, Appearance->None], Nothing],
							Spacer[10],
							Tooltip[Row[{"Replace? ", Checkbox[Dynamic@replace, Enabled->isvalid]}], "If checked, the cell will be rebuilt to its original state,\nclearing any user modifications other than the values entered"],
							Spacer[20],
							DefaultButton["Update Cell", If[replace, replaceButtonAction, updateButtonAction], Tooltip->"Update or replace cell, depending on if the checkbox is checked"],
							Spacer[20],
							(* if user cancels, delete the attached cell *)
							CancelButton[NotebookDelete[EvaluationCell[]]]
						}],
						Alignment -> Right
					],
					Item[Dynamic[If[showHelp, Column[{Spacer[5],ToExpression@helpMessage}], Style["",FontSize->0]], TrackedSymbols:>{showHelp}], ItemSize->{Automatic,0}]
				}, ItemSize->{Automatic, 0}, Spacings->0],
				Background -> GrayLevel[0.95],
				FrameStyle -> LightGray,
				RoundingRadius -> 5,
				FrameMargins -> 15*{{1,1},{1,1}},
				(* Add a 100 margin outside the right edge to prevent the right side from being cut off by the notebook window *)
				(* this happens because the AttachedCell is anchored right of the cell label area but uses the full window width *)
				(* when ItemSize is Automatic and the content is set to Full *)
				ImageMargins -> {{Automatic,100},{Automatic,Automatic}},
				BaseStyle -> {"DialogStyle", "ControlStyle"}
			]
		]
	];

(* ::Subsubsection:: *)
(*Standard Input Aids from Templates*)


(* ::Text:: *)
(*If an Input Aid simply needs to present the same controls as the cell does, we can generate the Input Aid programmatically.*)


(* ::Text:: *)
(*Using templates to generate fields for use in an input aid requires some changes from evaporative inputs used in a notebook.*)
(*First, the field will never be evaluated, instead the value of the Dynamic variable will be used directly in the input aid.*)
(*Second, we need unique variable names because the fields will be contained in a single DynamicModule instead of having one around each field.*)
(*Lastly, the expression can be initialized in the input aid's DynamicModule instead of the DynamicModule wrapping the evaporative fields.*)
(*Therefore we define fields with the same look, including validation and tooltip, but with different structure.*)


(* ::Text:: *)
(*If special behavior is needed, such as using the  second argument of Dynamic to run evaluations during use,  custom controls must be defined.*)


Options[evaporativeInputField]= Join[Options[InputField], Options[evaporativeInputTooltip]];

SetAttributes[inputAidInputField, HoldFirst]

(*
	Handle the Expression, Number and String types
*)
inputAidInputField[var_, type:(Expression|Number|String), opts:OptionsPattern[]] :=
	evaporativeInputTooltip[var, FilterRules[{opts},Options[evaporativeInputTooltip]]][
		Framed[
			InputField[Dynamic[var], type, scaleInputFieldOptions[opts], Appearance -> None],
			FrameStyle -> evaporativeInputFieldFrameStyle[var, FilterRules[{opts},Options[evaporativeInputFieldFrameStyle]]],
			FrameMargins -> 1,
			BaselinePosition->Baseline
		]
	]

(*
	Handle the Boxes type separately: need to convert from box form
*)
inputAidInputField[var_, Boxes, opts:OptionsPattern[]] :=
	evaporativeInputTooltip[ToExpression[var, StandardForm], FilterRules[{opts},Options[evaporativeInputTooltip]]][
		Framed[
			InputField[Dynamic[var], Boxes, scaleInputFieldOptions[opts], Appearance -> None],
			FrameStyle -> evaporativeInputFieldFrameStyle[ToExpression[var, StandardForm], FilterRules[{opts},Options[evaporativeInputFieldFrameStyle]]],
			FrameMargins -> 1,
			BaselinePosition->Baseline
		]
	]


Options[inputAidPopupMenu] = Options[PopupMenu];

SetAttributes[inputAidPopupMenu, HoldFirst]

inputAidPopupMenu[var_, list_List, opts:OptionsPattern[]] :=
	popupMenu[Dynamic[var], list, opts, Background -> White]


Options[inputAidUnitsMenu] = Options[inputAidPopupMenu];

SetAttributes[inputAidUnitsMenu, HoldFirst]

inputAidUnitsMenu[var_, unittype_, opts:OptionsPattern[]] := inputAidPopupMenu[var, unitsMenuList[unittype], opts]


(* ::Text:: *)
(*Create an input Aid control from a template definition.*)


SetAttributes[inputAidControlFromTemplate, HoldFirst]

inputAidControlFromTemplate[var_, template_Association] :=
	With[{
		type=template[["control type"]]
	},
		Switch[type,
			"evaporativeInputField",
			With[{
					inptype=template[["input type"]],
					fcn=template[["validation function"]],
					msg=template[["validation message"]],
					hint=template[["field hint"]],
					opts=template[["InputField options"]]
				},
				inputAidInputField[
					var,
					Replace[inptype, Except[Expression|Number|String|Boxes] -> Boxes],
					"InputFieldValidationFunction" -> Replace[fcn, Except[_Function|None] -> None],
					"InputFieldValidationMessage" -> Replace[msg, _Missing -> None],
					FieldHint -> Replace[hint, _Missing -> Null],
					Replace[opts,{_Missing :> Sequence[], o_ :> Sequence@@o}]
				]
			],
			"evaporativePopupMenu",
			With[{
					list=template[["menu list"]],
					opts=template[["PopupMenu options"]]
				},
				If[MissingQ[list],
					$Failed,
					inputAidPopupMenu[var, list, Replace[opts,{_Missing :> Sequence[], o_ :> Sequence@@o}]]
				]
			],
			"evaporativeUnitsMenu",
			With[{
					unittype=template[["unit type"]],
					opts=template[["PopupMenu options"]]
				},
				If[MissingQ[unittype],
					$Failed,
					inputAidUnitsMenu[var, unittype, Replace[opts,{_Missing :> Sequence[], o_ :> Sequence@@o}]]
				]
			],
			(*  Missing or not supported *)
			_,
			$Failed
		]
	]


(* ::Text:: *)
(*Create a string from the input variable boxes for use in input aids. *)


(* ::Text:: *)
(*We must use MakeExpression instead of ToExpression, which evaluates var and will cause unexpected results if a symbol with the same name is defined. MakeExpression wraps in HoldComplete, so we remove this from the final string.*)


variableBoxesToString[var_] := StringReplace[ToString[MakeExpression[var, StandardForm]],"HoldComplete["~~v__~~"]":>v]


(* ::Text:: *)
(*The output of preprocessInputAidTemplate is a list of Associations so that we can later add more information about rows, such as using the expression in a verbatimSetRow to format the display of controls in the input aid. *)


preprocessInputAidTemplate[templates_List, symbolStrings_List] :=
	Module[{counter=0},
		Map[
			preprocessInputAidRowTemplate[counter, #, symbolStrings]&, 
			templates
		]
	]


(* ::Text:: *)
(*Preprocess one row of the template. The "variable string" is used in the Input Aid interface. Expression is set for verbatimSetRows in order to duplicate the look of the controls in the input cell.  We don't process verbatimRows, since we have no way of knowing what is in them and how to interpret them. *)


SetAttributes[preprocessInputAidRowTemplate, HoldFirst]
preprocessInputAidRowTemplate[counter_, template_Association, symbolStrings_List] :=
	With[{
			rowtype=template[["row type"]],
			n=Length[template[["controls"]]]
		},
		Replace[rowtype,{
			(* only process setRow if it has more than 0 controls *)
			"setRow"/;n>0 :>
			<|
				"variable string"->variableBoxesToString[template["variable name"]],
				"variable tooltip"->template["variable tooltip"],
				"expression"->None,
				"controls"->preprocessInputAidSetRowTemplate[counter, template, symbolStrings]
			|>,
			(* if n!>0 *)
			"setRow" :> Nothing,
			(* only process verbatimSetRow if it has more than 0 controls *)
			"verbatimSetRow"/;n>0 :>
			<|
				"variable string"->variableBoxesToString[template["variable name"]],
				"variable tooltip"->template[["variable tooltip"]],
				"expression"->template[["expression"]],
				(* the same function is used to preprocess verbatimSetRow as setRow *)
				"controls"->preprocessInputAidSetRowTemplate[counter, template, symbolStrings]
			|>,
			(* if n!>0 *)
			"verbatimSetRow" :> Nothing,
			(* don't process verbatimRow *)
			"verbatimRow" :> Nothing,
			_ :> $Failed
		}]
	]


(* ::Text:: *)
(*Reorganize the SetRow template into a list of associations with the string for use the Input Aid interface, the symbolString associated with the control and used in the DynamicModule and controls, and the unmodified control template. *)


SetAttributes[preprocessInputAidSetRowTemplate, HoldFirst]
preprocessInputAidSetRowTemplate[counter_, template_Association, symbolStrings_List] :=
	Module[{
			controls=template[["controls"]]
		},
		Map[
			(counter++;
			<|
				"symbolString"->symbolStrings[[counter]],
				"control"->#
			|>)&,
			controls
		]
	]


(* ::Text:: *)
(*Generate a grid of variable names and controls from the preprocessed templates to use as the main content of the Input Aid:*)


inputAidTemplateToControlGrid[templates_List] :=
	With[{},
		Grid[Map[
			{
				(* If a tooltip is defined wrap the variable string in it *)
				With[{var=#[["variable string"]]},
					Replace[#[["variable tooltip"]],{(_Missing|None) :> var, t_ :> Tooltip[var, t]}]
				],
				"=",
				inputAidTemplateToControlRow[#[["expression"]], #[["controls"]]]
			}&, 
			templates
		],
		Alignment->{{Right,Center,Left}}
		]
	]


(* ::Text:: *)
(*Process each row of controls. If expr is None the controls will be placed in a RowBox, otherwise expr will be replaced with the list of controls as in verbatimSetRowTemplateToBoxes:*)


inputAidTemplateToControlRow[None, controls_List] :=
	With[{},
		RawBoxes[RowBox[
			Map[
				With[{var=Symbol[#[["symbolString"]]]},
					With[{c=inputAidControlFromTemplate[var, #[["control"]]]},
						MakeBoxes[c]
					]
				]&,
				controls
			]
		]]
	]

inputAidTemplateToControlRow[expr_, controls_List] :=
	With[{},
		RawBoxes[
			expr /. 
				MapIndexed[
					With[{var=Symbol[#[["symbolString"]]]},
						With[{c=inputAidControlFromTemplate[var, #1[["control"]]]},
							TagBox[First@#2, "control"]->MakeBoxes[c]
						]
					]&,
					controls
				]
		]
	]


(* ::Text:: *)
(*Read TaggingRules from cellobj (under the selector provided by the "aid" parameter) and set vars to their values or Null if rule isn't stored in cellobj.*)


SetAttributes[readInputAidTaggingRules, HoldFirst]
readInputAidTaggingRules[vars_, tags_List, cellobj_CellObject, aid_String]:=
	MapThread[
		Set,
		{
			vars, 
			(* the 3rd undocumented argument of CurrentValue prevents the button that calls the input aid *)
			(* from working on the first click. Use Replace to replace uninitialized TaggingRules with Null *)
			Replace[CurrentValue[cellobj,{TaggingRules, aid, #}],{Inherited:>Null,val_:>val}]&/@tags
		}
	]


(* ::Text:: *)
(*To write the TaggingRules we just generate a list of rules that inputAidCellButtonAction will handle. *)


writeInputAidTaggingRules[tags_,vars_]:=
	MapThread[
		Rule,
		{tags, vars}
	]


(* ::Text:: *)
(*inputAidFromTemplate is intended to be called from a top-level input aid function in the application.  This top-level input aid must pass the CellObject of the cell that it is called from, the name of the template function for the cell as a string, a list of unique variable names for each control in the template, and the name of the top-level input aid as a string. *)


(* ::Text:: *)
(*It is required that the template have one input for each control in the same order as the controls appear in the template. Without this condition there would be no way for  inputAidFromTemplate  to match inputs to controls. If this condition can not be met then a custom input aid is required.*)


Options[inputAidFromTemplate] = {"AfterContent"->Null, "HelpMessage"->Null};
inputAidFromTemplate[cellobj_CellObject, templateName_String, symbolStrings_List, aid_String, OptionsPattern[]]:=
	Module[{
			nullTemplate=Symbol[templateName]@@ConstantArray[Null,Length[symbolStrings]],
			uVars, uSymbolStrings, aidtemplate,
			cell, validmsg, isvalid
		},
		uVars = Unique[symbolStrings];
		uSymbolStrings = ToString/@uVars;
		aidtemplate=preprocessInputAidTemplate[nullTemplate,uSymbolStrings];
		{cell, validmsg, isvalid} = readCellAndValidateAgainstTemplate[cellobj, nullTemplate];
		With[{
				(* inject evaluated expressions into DynamicModule *)
				vars=uVars,
				grid=inputAidTemplateToControlGrid[aidtemplate],
				tags=symbolStrings,
				cell=cell, validmsg=validmsg, isvalid=isvalid,
				afterContent = OptionValue["AfterContent"],
				helpMessage = OptionValue["HelpMessage"]
			},
			DynamicModule[
				vars,
				(* initialize variables to TaggingRules stored in cellobj *)
				readInputAidTaggingRules[vars, tags, cellobj, aid];
				(* call the interface *)
				inputAidInterface[
					cellobj,
					(* the interface content *)
					Column[{
						grid,
						If[afterContent=!=Null, Dynamic[afterContent@@Unevaluated@vars, TrackedSymbols:>vars], Nothing]
					}],
					(* the Replace button action *)
					inputAidCellButtonAction[
						cellobj,
						inputAidTemplateToBoxes[Symbol[templateName]@@vars, aid],
						aid,
						writeInputAidTaggingRules[tags, vars]
					],
					(* the Update button action *)
					inputAidCellButtonAction[
						cellobj,
						First@replaceEvaporativeControlsInCellByTemplate[cell, Symbol[templateName]@@vars],
						aid,
						writeInputAidTaggingRules[tags, vars]
					],
					(* validation *)
					isvalid, 
					validmsg, 
					(* options *)
					"CellFormatWarningMessage" -> Automatic,
					"HelpMessage" -> helpMessage
				]
			]
		]
	]


(* ::Text:: *)
(*initializeInputAidCellFromTemplate creates a new Input cell with attached input aid initialized to values, and can be called by the code that generates a new notebook interface with input aids that use inputAidFromTemplate. This allows the input aid controls to be initialized in addition to the evaporative controls. *)


(* ::Text:: *)
(*When there is not a one-to-one correspondence between the controls in the cell and the input aid, which is often the case for custom input aids, the cell will need to be initialized manually by calling writeInputAidCell. This allows the initialization values for the cell template to differ from those in the TaggingRules for the input aid.*)


(*
	initialize an Input cell with an attached input aid and tagging rules to the list of values
*)
initializeInputAidCellFromTemplate[values_List, obj:(_CellObject|_NotebookObject), templateName_String, symbolStrings_List, inputAidName_String] :=
	Module[{newcell},
		newcell=writeInputAidCell[
			obj,
			inputAidTemplateToBoxes[Symbol[templateName]@@values, inputAidName],
			inputAidName,
			writeInputAidTaggingRules[symbolStrings, values]
		];
		(* move the selection to after the created cell so notebook is ready to write another cell, else this cell will be overwritten *)
		SelectionMove[ParentNotebook[newcell],After,Cell]
	]


(*
	Explictly specify the TaggingRules for custom InputAids
*)
initializeInputAidCellFromTemplate[values_List, obj:(_CellObject|_NotebookObject), templateName_String, symbolStrings_List, inputAidName_String, taggingRules_List] :=
	Module[{newcell},
		newcell=writeInputAidCell[
			obj,
			inputAidTemplateToBoxes[Symbol[templateName]@@values, inputAidName],
			inputAidName,
			taggingRules
		];
		(* move the selection to after the created cell so notebook is ready to write another cell, else this cell will be overwritten *)
		SelectionMove[ParentNotebook[newcell],After,Cell]
	]


(* ::Section:: *)
(*End Package*)


End[]

EndPackage[]
