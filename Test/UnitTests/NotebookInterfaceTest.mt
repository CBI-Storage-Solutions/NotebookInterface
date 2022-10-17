(* Wolfram Language Test file *)

Begin["NotebookInterface`Test`"]

BeginTestSection["notebookInterface-Controls"];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = RandomInteger[1000]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[MakeBoxes@uniqueSymbolValue]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-IntegerData"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = RandomReal[1000]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[MakeBoxes@uniqueSymbolValue]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-RealData"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = RandomReal[1000]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[uniqueSymbolValue, Number]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-RealDataNumberType"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = RandomReal[1000, 3]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[MakeBoxes@uniqueSymbolValue, Boxes]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-RowRealDataBoxType"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = StringRiffle[RandomChoice[{"indefinitely", "meretriciously", "resentfully", "afterward", "votive", "pink-slipped", "ungodly", "perceptual", "strong-minded", "comate"}, 2], " "]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[MakeBoxes@uniqueSymbolValue]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-StringData"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = StringRiffle[RandomChoice[{"indefinitely", "meretriciously", "resentfully", "afterward", "votive", "pink-slipped", "ungodly", "perceptual", "strong-minded", "comate"}, 2], " "]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[uniqueSymbolValue, String]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-StringDataStringType"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = RandomInteger[1000]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[MakeBoxes@uniqueSymbolValue, FieldHint -> "Input value"]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-IntegerFieldHint"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue = RandomReal[]
	}
	,
	actual = ToExpression[ToBoxes@evaporativeInputField[MakeBoxes@uniqueSymbolValue, "InputFieldValidationFunction" -> Function[NumericQ[#] && # >= 0 && # <= 1], "InputFieldValidationMessage" -> "0 <= x <= 1"]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeInputField-20160118-Q6P6F4-RealDataFieldHintValidationPass"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue,
		list = Table[CreateUUID[], 10]
	}
	,
	uniqueSymbolValue = RandomChoice[list];
	actual = ToExpression[ToBoxes@evaporativePopupMenu[uniqueSymbolValue, list]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativePopupMenu-20160118-Q6P6F4-Strings"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue,
		list = RandomInteger[1*^16, 10]
	}
	,
	uniqueSymbolValue = RandomChoice[list];
	actual = ToExpression[ToBoxes@evaporativePopupMenu[uniqueSymbolValue, list]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativePopupMenu-20160118-Q6P6F4-Integers"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue,
		unittype = "Energy"
	}
	,
	uniqueSymbolValue = First@RandomChoice[unitsMenuList[unittype]];
	actual = ToExpression[ToBoxes@evaporativeUnitsMenu[uniqueSymbolValue, unittype]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeUnitsMenu-20160118-Q6P6F4-StringUnitType"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue,
		unittype = {{"LengthUnit",2},{"MassUnit",1},{"TimeUnit",-2}}
	}
	,
	uniqueSymbolValue = First@RandomChoice[unitsMenuList[unittype]];
	actual = ToExpression[ToBoxes@evaporativeUnitsMenu[uniqueSymbolValue, unittype]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeUnitsMenu-20160118-Q6P6F4-UnitDimensionsUnitType"
	]
];

Module[
	{
		actual,
		expected,
		uniqueSymbolValue,
		unittype = {
			Quantity[1, "Inches"] -> Tooltip[RawBoxes[StyleBox["\"in\"", ShowStringCharacters -> False]], "unit: inches"],
			Quantity[1, "Feet"] -> Tooltip[RawBoxes[StyleBox["\"ft\"", ShowStringCharacters -> False]], "unit: feet"],
			Quantity[1, "Yards"] -> Tooltip[RawBoxes[StyleBox["\"yd\"", ShowStringCharacters -> False]], "unit: yards"],
			Quantity[1, "Miles"] -> Tooltip[RawBoxes[StyleBox["\"mi\"", ShowStringCharacters -> False]], "unit: miles"]
		}
	}
	,
	uniqueSymbolValue = First@RandomChoice[unitsMenuList[unittype]];
	actual = ToExpression[ToBoxes@evaporativeUnitsMenu[uniqueSymbolValue, unittype]];
	expected = uniqueSymbolValue;

	Test[
		actual,
		expected,
		TestID -> "evaporativeUnitsMenu-20160118-Q6P6F4-ListUnitType"
	]
];

EndSection;

BeginTestSection["notebookInterface-Templates"];

(* The input row template tests work by converting the template to boxes, evaluating the boxes with ToExpression,
	and then setting actual to the symbol in the input row, which should have a value after evaluation. *)

With[{uniqueSymbol = Unique[][RandomInteger[100]]},
	Module[
	{
		actual,
		expected,
		template,
		inputFieldValue = RandomInteger[1000],
		popupMenuValue,
		unittype = "Energy"
	}
	,
	popupMenuValue = First@RandomChoice[unitsMenuList[unittype]];
	template = setRowTemplate[uniqueSymbol, {inputFieldTemplate[inputFieldValue], unitsMenuTemplate[popupMenuValue, unittype]}];
	ToExpression[rowTemplateToBoxes@template];
	actual = uniqueSymbol;
	expected = inputFieldValue*popupMenuValue;

	Test[
		actual,
		expected,
		TestID -> "setRowTemplate-20160118-Q6P6F4-ExpressionUnit"
	]
]];

With[{uniqueSymbol = Unique[][RandomInteger[100]]},
	Module[
	{
		actual,
		expected,
		template,
		inputFieldValue1 = RandomInteger[1000],
		inputFieldValue2 = RandomInteger[1000]
	}
	,
	template = verbatimSetRowTemplate[
		{
			uniqueSymbol,
			{inputFieldValue1, inputFieldValue2}
		}
	];
	ToExpression[rowTemplateToBoxes@template];
	actual = uniqueSymbol;
	expected = {inputFieldValue1, inputFieldValue2};

	Test[
		actual,
		expected,
		TestID -> "verbatimSetRowTemplate-20160118-Q6P6F4-NoControls"
	]
]];

With[{uniqueSymbol = Unique[][RandomInteger[100]]},
	Module[
	{
		actual,
		expected,
		template,
		inputFieldValue1 = RandomInteger[1000],
		inputFieldValue2 = RandomInteger[1000],
		popupMenuValue,
		unittype = "Energy"
	}
	,
	popupMenuValue = First@RandomChoice[unitsMenuList[unittype]];
	template = verbatimSetRowTemplate[
		{
			uniqueSymbol,
			{RawBoxes[TagBox[1, "control"]], RawBoxes[TagBox[2, "control"]]} RawBoxes[TagBox[3, "control"]]
		},
		{inputFieldTemplate[inputFieldValue1], inputFieldTemplate[inputFieldValue2], unitsMenuTemplate[popupMenuValue, unittype]},
		False
	];
	ToExpression[rowTemplateToBoxes@template];
	actual = uniqueSymbol;
	expected = {inputFieldValue1, inputFieldValue2}*popupMenuValue;

	Test[
		actual,
		expected,
		TestID -> "verbatimSetRowTemplate-20160118-Q6P6F4-ExpressionListUnitNoCompoundExpr"
	]
]];

With[{},
	Module[
	{
		actual,
		expected,
		template,
		inputFieldValue1 = RandomReal[{1,50}]
	}
	,
	template = verbatimRowTemplate[BesselJ[0, inputFieldValue1]];
	actual = ToExpression[rowTemplateToBoxes@template];
	expected = BesselJ[0, inputFieldValue1];

	Test[
		actual,
		expected,
		TestID -> "verbatimRowTemplate-20160118-Q6P6F4-Expression"
	]
]];


EndSection;


BeginTestSection["notebookInterface-Cells"];

(* Define cell templates to be used in all tests. *)
With[{
	uniqueSymbol = Unique[],
	inputFieldInteger1 = RandomInteger[1000],
	inputFieldInteger2 = RandomInteger[1000],
	inputFieldInteger3 = RandomInteger[1000],
	inputFieldInteger4 = RandomInteger[1000],
	inputFieldReal1 = RandomReal[1],
	inputFieldReal2 = RandomReal[1000],
	inputFieldReal3 = RandomReal[1],
	inputFieldReal4 = RandomReal[1000],
	string1 = "str",
	string2 = "another string",
	string3 = "new string",
	string4 = "another new string"
},
With[{
	(* template checks various ways users might enter inputs or modify rows *)
	testCellTemplate = {
		(* test null value *)
		setRowTemplate[uniqueSymbol[0], {
		inputFieldTemplate[Null, Expression, None, None, "Input Value"]
		}],
		(* test rational number with Boxes (shouldn't make any difference except for how it is stored in the field *)
		setRowTemplate[uniqueSymbol[1], {
		inputFieldTemplate[inputFieldInteger1*Pi/inputFieldInteger2, Expression, None, None, "Input Value"]
		}],
		(* test Number with faling validation - should display differently but still evalute to the input number*)
		setRowTemplate[uniqueSymbol[2], {
		inputFieldTemplate[inputFieldReal1, Number, Function[NumericQ[#] && # > 3], "The input must be numeric and greater than 3", "Input Value"]
		}],
		(* test a field multiplied by a Quantity *)
		verbatimSetRowTemplate[{
			uniqueSymbol[3],
			RawBoxes[TagBox[1, "control"]] Quantity[1, "Meters"]
		}, {
		inputFieldTemplate[inputFieldReal2, Expression, None, None, "Input Value"]
		}],
		(* Input times Units *)
		setRowTemplate[uniqueSymbol[4], {
		inputFieldTemplate[inputFieldReal1, Number, None, None, "Input Value"],
		unitsMenuTemplate[Quantity[1, "Yards"], "Length"]
		}],
		(* Null popup *)
		setRowTemplate[uniqueSymbol[5], {
		popupMenuTemplate[Null, {True -> "True", False -> "False"}]
		}],
		(* a comment, shouldn't affect anything *)
		verbatimRowTemplate[
		 RawBoxes[RowBox[{"(*", " ", "comment", " ", "*)"}]]],
		(* A list of string input fields with one failing validation and one passing *)
		verbatimSetRowTemplate[{
			uniqueSymbol[6],
			{RawBoxes[TagBox[1, "control"]], RawBoxes[TagBox[2, "control"]]}
		}, {
		inputFieldTemplate[string1, String, Function[StringLength[#] > 3], "The string length must be greater than 3", "Enter String"],
		inputFieldTemplate[string2, String, Function[StringLength[#] > 3], "The string length must be greater than 3", "Enter String"]
		}]
	},

	replacementTemplate = {
		(* value *)
		setRowTemplate[uniqueSymbol[0], {
		inputFieldTemplate[inputFieldReal3, Expression, None, None, "Input Value"]
		}],
		(* test rational number with Boxes (shouldn't make any difference except for how it is stored in the field *)
		setRowTemplate[uniqueSymbol[1], {
		inputFieldTemplate[inputFieldInteger3*Pi/inputFieldInteger4, Expression, None, None, "Input Value"]
		}],
		(* test Number with faling validation - should display differently but still evalute to the input number*)
		setRowTemplate[uniqueSymbol[2], {
		inputFieldTemplate[inputFieldReal3, Number, Function[NumericQ[#] && # > 3], "The input must be numeric and greater than 3", "Input Value"]
		}],
		(* test a field multiplied by a Quantity *)
		verbatimSetRowTemplate[{
			uniqueSymbol[3],
			RawBoxes[TagBox[1, "control"]] Quantity[1, "Meters"]
		}, {
		inputFieldTemplate[inputFieldReal4, Expression, None, None, "Input Value"]
		}],
		(* Input times Units *)
		setRowTemplate[uniqueSymbol[4], {
		inputFieldTemplate[inputFieldReal3, Number, None, None, "Input Value"],
		unitsMenuTemplate[Quantity[1, "Yards"], "Length"]
		}],
		(* popup *)
		setRowTemplate[uniqueSymbol[5], {
		popupMenuTemplate[False, {True -> "True", False -> "False"}]
		}],
		(* a comment, shouldn't affect anything *)
		verbatimRowTemplate[
		 RawBoxes[RowBox[{"(*", " ", "comment", " ", "*)"}]]],
		(* A list of string input fields with one failing validation and one passing *)
		verbatimSetRowTemplate[{
			uniqueSymbol[6],
			{RawBoxes[TagBox[1, "control"]], RawBoxes[TagBox[2, "control"]]}
		}, {
		inputFieldTemplate[string3, String, Function[StringLength[#] > 3], "The string length must be greater than 3", "Enter String"],
		inputFieldTemplate[string4, String, Function[StringLength[#] > 3], "The string length must be greater than 3", "Enter String"]
		}]
	},

(* invalid template with:
	missing uniqueSymbol[0],
	swap PopupMenu for InputField for uniqueSymbol[1],
	too many controls for uniqueSymbol[2],
	wrong order for uniqueSymbol[4],
	Missing Rawboxes for second control for uniqueSymbol[6]
	 *)
	invalidTemplate = {
		(* test rational number with Boxes (shouldn't make any difference except for how it is stored in the field *)
		setRowTemplate[uniqueSymbol[1], {
		popupMenuTemplate[False, {True -> "True", False -> "False"}]
		}],
		(* test Number with faling validation - should display differently but still evalute to the input number*)
		setRowTemplate[uniqueSymbol[2], {
		inputFieldTemplate[inputFieldReal3, Number, Function[NumericQ[#] && # > 3], "The input must be numeric and greater than 3", "Input Value"],
		inputFieldTemplate[inputFieldReal3, Number, Function[NumericQ[#] && # > 3], "The input must be numeric and greater than 3", "Input Value"]
		}],
		(* test a field multiplied by a Quantity *)
		verbatimSetRowTemplate[{
			uniqueSymbol[3],
			RawBoxes[TagBox[1, "control"]] Quantity[1, "Meters"]
		}, {
		inputFieldTemplate[inputFieldReal4, Expression, None, None, "Input Value"],
		inputFieldTemplate[inputFieldReal2, Expression, None, None, "Input Value"]
		}],
		(* Input times Units *)
		setRowTemplate[uniqueSymbol[4], {
		unitsMenuTemplate[Quantity[1, "Yards"], "Length"],
		inputFieldTemplate[inputFieldReal3, Number, None, None, "Input Value"]
		}],
		(* popup *)
		setRowTemplate[uniqueSymbol[5], {
		popupMenuTemplate[False, {True -> "True", False -> "False"}]
		}],
		(* a comment, shouldn't affect anything *)
		verbatimRowTemplate[
		 RawBoxes[RowBox[{"(*", " ", "comment", " ", "*)"}]]],
		(* extra comment, shouldn't affect anything *)
		verbatimRowTemplate[
		 RawBoxes[RowBox[{"(*", " ", "comment", " ", "*)"}]]],
		(* A list of string input fields with one failing validation and one passing *)
		verbatimSetRowTemplate[{
			uniqueSymbol[6],
			{RawBoxes[TagBox[1, "control"]]}
		}, {
		inputFieldTemplate[string3, String, Function[StringLength[#] > 3], "The string length must be greater than 3", "Enter String"],
		inputFieldTemplate[string4, String, Function[StringLength[#] > 3], "The string length must be greater than 3", "Enter String"]
		}]
	}
},

Module[
	{
		actual,
		expected
	}
	,
	ToExpression[First@First@cellTemplateToBoxes@testCellTemplate];
	actual = {uniqueSymbol[0], uniqueSymbol[1], uniqueSymbol[2], uniqueSymbol[3], uniqueSymbol[4], uniqueSymbol[5], uniqueSymbol[6]};
	expected = {Null, inputFieldInteger1*Pi/inputFieldInteger2, inputFieldReal1, Quantity[inputFieldReal2, "Meters"], Quantity[inputFieldReal1, "Yards"], "", {string1, string2}};

	Test[
		actual,
		expected,
		TestID -> "cellTemplateToBoxes-20160118-Q6P6F4"
	]
];

Module[
	{
		cell,
		newcell,
		actual,
		expected
	}
	,
	cell=cellTemplateToBoxes@testCellTemplate;
	newcell=First@replaceEvaporativeControlsInCellByTemplate[cell, replacementTemplate];
	ToExpression[First@First@newcell];
	actual = {uniqueSymbol[0], uniqueSymbol[1], uniqueSymbol[2], uniqueSymbol[3], uniqueSymbol[4], uniqueSymbol[5], uniqueSymbol[6]};
	expected = {inputFieldReal3, inputFieldInteger3*Pi/inputFieldInteger4, inputFieldReal3, Quantity[inputFieldReal4, "Meters"], Quantity[inputFieldReal3, "Yards"], False, {string3, string4}};

	Test[
		actual,
		expected,
		TestID -> "replaceEvaporativeControlsInCellByTemplate-20160118-Q6P6F4-Valid"
	]
];

Module[
	{
		cell,
		actual,
		expected
	}
	,
	cell=cellTemplateToBoxes@invalidTemplate;
	(* check number of error messages *)
	actual = Length@Flatten@Last@replaceEvaporativeControlsInCellByTemplate[cell, replacementTemplate];
	expected = 5;

	Test[
		actual,
		expected,
		TestID -> "replaceEvaporativeControlsInCellByTemplate-20160118-Q6P6F4-Invalid"
	]
];

(* end With *)
]]

EndSection;

End[]