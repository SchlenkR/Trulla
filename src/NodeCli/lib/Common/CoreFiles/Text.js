import { class_type } from "../../fable_modules/fable-library.4.3.0/Reflection.js";
import { printf, replicate } from "../../fable_modules/fable-library.4.3.0/String.js";
import { StringBuilder_$ctor } from "../../fable_modules/fable-library.4.3.0/System.Text.js";
import { PrintfModule_PrintFormatToStringBuilder } from "../../fable_modules/fable-library.4.3.0/FSharp.Core.js";
import { toString } from "../../fable_modules/fable-library.4.3.0/Types.js";

export class TextBuilder {
    constructor() {
    }
}

export function TextBuilder_$reflection() {
    return class_type("Trulla.Core.Text.TextBuilder", void 0, TextBuilder);
}

export function TextBuilder_$ctor() {
    return new TextBuilder();
}

export const text = TextBuilder_$ctor();

export const quot = "\"";

export const plus = " + ";

export const pareno = "(";

export const parenc = ")";

export const br = "\n";

export function getIndentation(indent) {
    return replicate(indent, "    ");
}

export function ind(indent, txt) {
    let txt_1;
    const b_5 = StringBuilder_$ctor();
    const b_3 = b_5;
    ((txt_1 = getIndentation(indent), (b) => {
        PrintfModule_PrintFormatToStringBuilder(b, printf("%s"))(txt_1);
    }))(b_3);
    PrintfModule_PrintFormatToStringBuilder(b_3, printf("%s"))(txt);
    return toString(b_5);
}

export function lni(indent, txt) {
    let b_5, b_3;
    return ind(indent, (b_5 = StringBuilder_$ctor(), ((b_3 = b_5, (PrintfModule_PrintFormatToStringBuilder(b_3, printf("%s"))(txt), PrintfModule_PrintFormatToStringBuilder(b_3, printf("%s"))(br))), toString(b_5))));
}

export const ln0 = (txt) => lni(0, txt);

export function stri(indent, txt) {
    let b_8, b_6, b_4;
    return ind(indent, (b_8 = StringBuilder_$ctor(), ((b_6 = b_8, (PrintfModule_PrintFormatToStringBuilder(b_6, printf("%s"))(quot), (b_4 = b_6, (PrintfModule_PrintFormatToStringBuilder(b_4, printf("%s"))(txt), PrintfModule_PrintFormatToStringBuilder(b_4, printf("%s"))(quot))))), toString(b_8))));
}

export function str(txt) {
    return stri(0, txt);
}

export function strlni(indent, txt) {
    return ln0(stri(indent, txt));
}

export function strln(txt) {
    return ln0(str(txt));
}

