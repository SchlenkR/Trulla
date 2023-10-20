import { cons, tail, head, isEmpty, ofArray, toArray, find } from "../fable_modules/fable-library.4.3.0/List.js";
import { disposeSafe, getEnumerator, equals } from "../fable_modules/fable-library.4.3.0/Util.js";
import { join, printf, toFail } from "../fable_modules/fable-library.4.3.0/String.js";
import { containsKey } from "../fable_modules/fable-library.4.3.0/Map.js";
import { String_assertLetterDigitUnderscore } from "../Common/CoreFiles/Utils.js";
import { StringBuilder_$ctor } from "../fable_modules/fable-library.4.3.0/System.Text.js";
import { lni, br, ln0 } from "../Common/CoreFiles/Text.js";
import { PrintfModule_PrintFormatToStringBuilder } from "../fable_modules/fable-library.4.3.0/FSharp.Core.js";
import { TVar } from "../Common/CoreFiles/Ast.js";
import { toString } from "../fable_modules/fable-library.4.3.0/Types.js";
import { defaultArg } from "../fable_modules/fable-library.4.3.0/Option.js";
import { map, delay, toList } from "../fable_modules/fable-library.4.3.0/Seq.js";

export function MemberExp_getLastSegment(_arg) {
    if (_arg.tag === 1) {
        return _arg.fields[0];
    }
    else {
        return _arg.fields[0].memberName;
    }
}

export function makeTypeName(potentialRecordNames, tvar) {
    let matchValue;
    const x_1 = find((x) => equals(x.id, tvar), potentialRecordNames);
    const arg = toArray((matchValue = ofArray(x_1.name.split("")), isEmpty(matchValue) ? (() => {
        throw new Error("Empty possible record name is not supported.");
    })() : cons(head(matchValue).toUpperCase(), tail(matchValue))));
    return arg.join('');
}

export function toTypeName(potentialRecordNames, typ) {
    let matchResult, pt, tvar;
    switch (typ.tag) {
        case 0: {
            switch (typ.fields[0]) {
                case "string": {
                    matchResult = 0;
                    break;
                }
                case "bool": {
                    matchResult = 1;
                    break;
                }
                default:
                    matchResult = 4;
            }
            break;
        }
        case 1: {
            if (typ.fields[0] === "sequence") {
                matchResult = 2;
                pt = typ.fields[1];
            }
            else {
                matchResult = 4;
            }
            break;
        }
        case 3: {
            matchResult = 3;
            tvar = typ.fields[0];
            break;
        }
        default:
            matchResult = 4;
    }
    switch (matchResult) {
        case 0:
            return "string";
        case 1:
            return "bool";
        case 2:
            return `List<${toTypeName(potentialRecordNames, pt)}>`;
        case 3:
            return makeTypeName(potentialRecordNames, tvar);
        default:
            return toFail(printf("Unsupported reference for type \'%A\'."))(typ);
    }
}

export function memberExpToIdent(exp) {
    const matchValue = exp.value;
    if (matchValue.tag === 0) {
        const acc = matchValue.fields[0];
        return (memberExpToIdent(acc.instanceExp) + ".") + acc.memberName;
    }
    else {
        const ident = matchValue.fields[0];
        return (containsKey(ident, exp.bindingContext) ? "" : "model.") + ident;
    }
}

export function renderTemplate(solution, namespaceName) {
    let txt_2, txt_4, txt_5, txt_6, txt_8, txt_10, txt_12, txt_13, txt_16, txt_32, txt_34, txt_35;
    String_assertLetterDigitUnderscore("namespace", namespaceName);
    const toStringLiteral = (txt) => (("\"" + txt) + "\"");
    const b_93 = StringBuilder_$ctor();
    const b_91 = b_93;
    ((txt_2 = ln0(`namespace ${namespaceName};`), (b) => {
        PrintfModule_PrintFormatToStringBuilder(b, printf("%s"))(txt_2);
    }))(b_91);
    const b_89 = b_91;
    PrintfModule_PrintFormatToStringBuilder(b_89, printf("%s"))(br);
    const b_87 = b_89;
    ((txt_4 = ln0("using System;"), (b_2) => {
        PrintfModule_PrintFormatToStringBuilder(b_2, printf("%s"))(txt_4);
    }))(b_87);
    const b_85 = b_87;
    ((txt_5 = ln0("using System.Collections.Generic;"), (b_3) => {
        PrintfModule_PrintFormatToStringBuilder(b_3, printf("%s"))(txt_5);
    }))(b_85);
    const b_83 = b_85;
    ((txt_6 = ln0("using System.Linq;"), (b_4) => {
        PrintfModule_PrintFormatToStringBuilder(b_4, printf("%s"))(txt_6);
    }))(b_83);
    const b_81 = b_83;
    PrintfModule_PrintFormatToStringBuilder(b_81, printf("%s"))(br);
    const b_79 = b_81;
    const records = solution.records;
    const e_1 = getEnumerator(records);
    try {
        while (e_1["System.Collections.IEnumerator.MoveNext"]()) {
            const b_16 = b_79;
            const r = e_1["System.Collections.Generic.IEnumerator`1.get_Current"]();
            ((txt_8 = lni(0, `public record ${makeTypeName(records, r.id)} {`), (b_6) => {
                PrintfModule_PrintFormatToStringBuilder(b_6, printf("%s"))(txt_8);
            }))(b_16);
            const b_14 = b_16;
            const e = getEnumerator(r.fields);
            try {
                while (e["System.Collections.IEnumerator.MoveNext"]()) {
                    const field = e["System.Collections.Generic.IEnumerator`1.get_Current"]();
                    const txt_9 = lni(1, `public required ${toTypeName(records, field.typ)} ${field.name} { get; init; }`);
                    PrintfModule_PrintFormatToStringBuilder(b_14, printf("%s"))(txt_9);
                }
            }
            finally {
                disposeSafe(e);
            }
            const b_12 = b_14;
            ((txt_10 = lni(0, "}"), (b_9) => {
                PrintfModule_PrintFormatToStringBuilder(b_9, printf("%s"))(txt_10);
            }))(b_12);
            PrintfModule_PrintFormatToStringBuilder(b_12, printf("%s"))(br);
        }
    }
    finally {
        disposeSafe(e_1);
    }
    const b_77 = b_79;
    ((txt_12 = lni(0, "public static class Rendering {"), (b_18) => {
        PrintfModule_PrintFormatToStringBuilder(b_18, printf("%s"))(txt_12);
    }))(b_77);
    const b_75 = b_77;
    ((txt_13 = lni(1, `public static string Render(this ${makeTypeName(solution.records, new TVar(0, []))} ${"model"}) {`), (b_19) => {
        PrintfModule_PrintFormatToStringBuilder(b_19, printf("%s"))(txt_13);
    }))(b_75);
    const b_73 = b_75;
    const sbAppend = (indent_1, txt_14) => {
        const b_22 = StringBuilder_$ctor();
        const txt_15 = lni(indent_1, `__sb.Append(${txt_14});`);
        PrintfModule_PrintFormatToStringBuilder(b_22, printf("%s"))(txt_15);
        return toString(b_22);
    };
    ((txt_16 = lni(2, "var __sb = new System.Text.StringBuilder();"), (b_23) => {
        PrintfModule_PrintFormatToStringBuilder(b_23, printf("%s"))(txt_16);
    }))(b_73);
    const b_71 = b_73;
    const render = (indent_2, tree) => {
        let texp, txt_19, elems, xIdent, idxIdent, f_19, txt_20, f_23, txt_26, f_27, txt_29, txt_18;
        const b_58 = StringBuilder_$ctor();
        const e_2 = getEnumerator(tree);
        try {
            while (e_2["System.Collections.IEnumerator.MoveNext"]()) {
                ((texp = e_2["System.Collections.Generic.IEnumerator`1.get_Current"](), (texp.tag === 1) ? ((txt_19 = sbAppend(indent_2, memberExpToIdent(texp.fields[0])), (b_25) => {
                    PrintfModule_PrintFormatToStringBuilder(b_25, printf("%s"))(txt_19);
                })) : ((texp.tag === 2) ? ((elems = memberExpToIdent(texp.fields[1]), (xIdent = (`x_${indent_2}`), (idxIdent = (`idx_${indent_2}`), (f_19 = ((txt_20 = lni(indent_2, `foreach (var (${idxIdent},${texp.fields[0].value}) in ${elems}.Select((${xIdent},${idxIdent}) => (${idxIdent},${xIdent}))) {`), (b_26) => {
                    PrintfModule_PrintFormatToStringBuilder(b_26, printf("%s"))(txt_20);
                })), (b_41) => {
                    let txt_21, txt_22, txt_23, txt_24;
                    f_19(b_41);
                    const b_39 = b_41;
                    ((txt_21 = render(indent_2 + 1, texp.fields[3]), (b_27) => {
                        PrintfModule_PrintFormatToStringBuilder(b_27, printf("%s"))(txt_21);
                    }))(b_39);
                    const b_37 = b_39;
                    const sep_1 = defaultArg(texp.fields[2].result, "");
                    ((txt_22 = lni(indent_2 + 1, `if (${idxIdent} < ${elems}.Count - 1) {`), (b_28) => {
                        PrintfModule_PrintFormatToStringBuilder(b_28, printf("%s"))(txt_22);
                    }))(b_37);
                    const b_35 = b_37;
                    ((txt_23 = sbAppend(indent_2 + 2, toStringLiteral(sep_1)), (b_29) => {
                        PrintfModule_PrintFormatToStringBuilder(b_29, printf("%s"))(txt_23);
                    }))(b_35);
                    const b_33 = b_35;
                    ((txt_24 = lni(indent_2 + 1, "}"), (b_30) => {
                        PrintfModule_PrintFormatToStringBuilder(b_30, printf("%s"))(txt_24);
                    }))(b_33);
                    const txt_25 = lni(indent_2, "}");
                    PrintfModule_PrintFormatToStringBuilder(b_33, printf("%s"))(txt_25);
                }))))) : ((texp.tag === 3) ? ((f_23 = ((txt_26 = lni(indent_2, `if (${memberExpToIdent(texp.fields[0])}) {`), (b_42) => {
                    PrintfModule_PrintFormatToStringBuilder(b_42, printf("%s"))(txt_26);
                })), (b_48) => {
                    let txt_27;
                    f_23(b_48);
                    const b_46 = b_48;
                    ((txt_27 = render(indent_2 + 1, texp.fields[1]), (b_43) => {
                        PrintfModule_PrintFormatToStringBuilder(b_43, printf("%s"))(txt_27);
                    }))(b_46);
                    const txt_28 = lni(indent_2, "}");
                    PrintfModule_PrintFormatToStringBuilder(b_46, printf("%s"))(txt_28);
                })) : ((texp.tag === 4) ? ((f_27 = ((txt_29 = lni(indent_2, "else {"), (b_49) => {
                    PrintfModule_PrintFormatToStringBuilder(b_49, printf("%s"))(txt_29);
                })), (b_55) => {
                    let txt_30;
                    f_27(b_55);
                    const b_53 = b_55;
                    ((txt_30 = render(indent_2 + 1, texp.fields[1]), (b_50) => {
                        PrintfModule_PrintFormatToStringBuilder(b_50, printf("%s"))(txt_30);
                    }))(b_53);
                    const txt_31 = lni(indent_2, "}");
                    PrintfModule_PrintFormatToStringBuilder(b_53, printf("%s"))(txt_31);
                })) : ((txt_18 = sbAppend(indent_2, toStringLiteral(texp.fields[0])), (b_24) => {
                    PrintfModule_PrintFormatToStringBuilder(b_24, printf("%s"))(txt_18);
                })))))))(b_58);
            }
        }
        finally {
            disposeSafe(e_2);
        }
        return toString(b_58);
    };
    ((txt_32 = render(2, solution.tree), (b_59) => {
        PrintfModule_PrintFormatToStringBuilder(b_59, printf("%s"))(txt_32);
    }))(b_71);
    const b_69 = b_71;
    PrintfModule_PrintFormatToStringBuilder(b_69, printf("%s"))(br);
    const b_67 = b_69;
    ((txt_34 = lni(2, "return __sb.ToString();"), (b_61) => {
        PrintfModule_PrintFormatToStringBuilder(b_61, printf("%s"))(txt_34);
    }))(b_67);
    const b_65 = b_67;
    ((txt_35 = lni(1, "}"), (b_62) => {
        PrintfModule_PrintFormatToStringBuilder(b_62, printf("%s"))(txt_35);
    }))(b_65);
    const txt_36 = lni(0, "}");
    PrintfModule_PrintFormatToStringBuilder(b_65, printf("%s"))(txt_36);
    return toString(b_93);
}

export function renderErrors(errors) {
    return `
Errors in Trulla template:

${join("\n\n", toList(delay(() => map(toString, errors))))};
`;
}

