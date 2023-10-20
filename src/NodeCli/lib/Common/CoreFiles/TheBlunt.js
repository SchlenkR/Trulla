import { union_type, record_type, int32_type, string_type, class_type } from "../../fable_modules/fable-library.4.3.0/Reflection.js";
import { join, toText, printf, toFail, substring } from "../../fable_modules/fable-library.4.3.0/String.js";
import { Union, Record } from "../../fable_modules/fable-library.4.3.0/Types.js";
import { tail, head, isEmpty, map, reduce } from "../../fable_modules/fable-library.4.3.0/List.js";
import { getEnumerator } from "../../fable_modules/fable-library.4.3.0/Util.js";
import { singleton, append, empty, enumerateWhile, delay, toList } from "../../fable_modules/fable-library.4.3.0/Seq.js";
import { isDigit, isLetter } from "../../fable_modules/fable-library.4.3.0/Char.js";

export class StringExtensions {
    constructor() {
    }
}

export function StringExtensions_$reflection() {
    return class_type("TheBlunt.StringExtensions", void 0, StringExtensions);
}

export function StringExtensions_StringStartsWithAt_107DD5FC(this$, other, idx) {
    return substring(this$, idx).indexOf(other) === 0;
}

export function StringExtensions_Slice_Z18115A39(this$, start) {
    return substring(this$, start);
}

export class Cursor extends Record {
    constructor(original, idx) {
        super();
        this.original = original;
        this.idx = (idx | 0);
    }
}

export function Cursor_$reflection() {
    return record_type("TheBlunt.Cursor", [], Cursor, () => [["original", string_type], ["idx", int32_type]]);
}

export class ParserResult$1 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["POk", "PError"];
    }
}

export function ParserResult$1_$reflection(gen0) {
    return union_type("TheBlunt.ParserResult`1", [gen0], ParserResult$1, () => [[["ok", PVal$1_$reflection(gen0)]], [["error", ParseError_$reflection()]]]);
}

export class PVal$1 extends Record {
    constructor(range, result) {
        super();
        this.range = range;
        this.result = result;
    }
}

export function PVal$1_$reflection(gen0) {
    return record_type("TheBlunt.PVal`1", [gen0], PVal$1, () => [["range", Range$_$reflection()], ["result", gen0]]);
}

export class ParseError extends Record {
    constructor(idx, message) {
        super();
        this.idx = (idx | 0);
        this.message = message;
    }
}

export function ParseError_$reflection() {
    return record_type("TheBlunt.ParseError", [], ParseError, () => [["idx", int32_type], ["message", string_type]]);
}

export class Range$ extends Record {
    constructor(startIdx, endIdx) {
        super();
        this.startIdx = (startIdx | 0);
        this.endIdx = (endIdx | 0);
    }
}

export function Range$_$reflection() {
    return record_type("TheBlunt.Range", [], Range$, () => [["startIdx", int32_type], ["endIdx", int32_type]]);
}

export class DocPos extends Record {
    constructor(idx, ln, col) {
        super();
        this.idx = (idx | 0);
        this.ln = (ln | 0);
        this.col = (col | 0);
    }
}

export function DocPos_$reflection() {
    return record_type("TheBlunt.DocPos", [], DocPos, () => [["idx", int32_type], ["ln", int32_type], ["col", int32_type]]);
}

export function Cursor__CanGoto_Z524259A4(c, idx) {
    if (idx >= c.idx) {
        return idx <= c.original.length;
    }
    else {
        return false;
    }
}

export function Cursor__CanWalkFwd_Z524259A4(c, steps) {
    return Cursor__CanGoto_Z524259A4(c, c.idx + steps);
}

export function Cursor__get_IsAtEnd(c) {
    return c.idx === c.original.length;
}

export function Cursor__get_HasRest(c) {
    return c.idx < c.original.length;
}

export function Cursor__get_Rest(c) {
    return StringExtensions_Slice_Z18115A39(c.original, c.idx);
}

export function Cursor__StartsWith_Z721C83C5(c, s) {
    return StringExtensions_StringStartsWithAt_107DD5FC(Cursor__get_Rest(c), s, 0);
}

export function Cursor__Goto_Z524259A4(c, idx) {
    if (!Cursor__CanGoto_Z524259A4(c, idx)) {
        const arg_1 = c.original.length | 0;
        toFail(printf("Index %d is out of range of string of length %d."))(idx)(arg_1);
    }
    return new Cursor(c.original, idx);
}

export function Cursor__WalkFwd_Z524259A4(c, steps) {
    return Cursor__Goto_Z524259A4(c, c.idx + steps);
}

export function Cursor__MoveNext(c) {
    return Cursor__WalkFwd_Z524259A4(c, 1);
}

export const RangeModule_zero = new Range$(0, 0);

export function PVal_ranges(pvals) {
    return reduce((r, r_1) => (new Range$(r.startIdx, r_1.endIdx)), map((x) => x.range, pvals));
}

export function DocPosModule_create(index, input) {
    if ((index < 0) ? true : (index > input.length)) {
        const arg_1 = input.length | 0;
        toFail(printf("Index %d is out of range of input string of length %d."))(index)(arg_1);
    }
    let currIdx = 0;
    let line = 1;
    let column = 1;
    while (currIdx !== index) {
        if (StringExtensions_StringStartsWithAt_107DD5FC(input, "\n", currIdx)) {
            line = ((line + 1) | 0);
            column = 1;
        }
        else {
            column = ((column + 1) | 0);
        }
        currIdx = ((currIdx + 1) | 0);
    }
    return new DocPos(index, line, column);
}

export function DocPosModule_ofInput(pi) {
    return DocPosModule_create(pi.idx, pi.original);
}

export function CursorModule_hasRemainingChars(n, inp) {
    if (!Cursor__CanWalkFwd_Z524259A4(inp, n)) {
        return new ParserResult$1(1, [new ParseError(inp.idx, toText(printf("Expected %d more characters."))(n))]);
    }
    else {
        return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, inp.idx), void 0)]);
    }
}

export class ParserBuilder {
    constructor() {
    }
}

export function ParserBuilder_$reflection() {
    return class_type("TheBlunt.ParserBuilder", void 0, ParserBuilder);
}

export function ParserBuilder_$ctor() {
    return new ParserBuilder();
}

export function ParserBuilder__Return_Z3CB29237(_, pval) {
    return (inp) => (new ParserResult$1(0, [pval]));
}

export function ParserBuilder__Return_306E893E(_, err) {
    return (inp) => (new ParserResult$1(1, [err]));
}

export const parse = ParserBuilder_$ctor();

export function pseq(s) {
    const enum$ = getEnumerator(s);
    return (inp) => {
        if (enum$["System.Collections.IEnumerator.MoveNext"]()) {
            return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, inp.idx), enum$["System.Collections.Generic.IEnumerator`1.get_Current"]())]);
        }
        else {
            return new ParserResult$1(1, [new ParseError(inp.idx, "No more elements in sequence.")]);
        }
    };
}

export const punit = (inp) => (new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, inp.idx), void 0)]));

export function pstr(s) {
    return (inp) => {
        if (Cursor__StartsWith_Z721C83C5(inp, s)) {
            return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, inp.idx + s.length), s)]);
        }
        else {
            return new ParserResult$1(1, [new ParseError(inp.idx, toText(printf("Expected: \'%s\'"))(s))]);
        }
    };
}

export const op_Splice = pstr;

export function pgoto(idx) {
    return (inp) => {
        let arg_1;
        if (Cursor__CanGoto_Z524259A4(inp, idx)) {
            return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, idx), void 0)]);
        }
        else {
            return new ParserResult$1(1, [new ParseError(idx, (arg_1 = (inp.original.length | 0), toText(printf("Index %d is out of range of string of length %d."))(idx)(arg_1)))]);
        }
    };
}

export function op_LessBarGreater() {
    return (pa) => ((pb) => ((inp) => {
        const matchValue = pa(inp);
        if (matchValue.tag === 1) {
            return pb(inp);
        }
        else {
            return new ParserResult$1(0, [matchValue.fields[0]]);
        }
    }));
}

export function firstOf(parsers) {
    return reduce((pa, pb) => ((inp) => {
        const matchValue = pa(inp);
        if (matchValue.tag === 1) {
            return pb(inp);
        }
        else {
            return new ParserResult$1(0, [matchValue.fields[0]]);
        }
    }), parsers);
}

export const anyChar = (inp_1) => {
    const matchValue = CursorModule_hasRemainingChars(1, inp_1);
    if (matchValue.tag === 0) {
        return ((inp) => {
            let copyOfStruct;
            return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, inp.idx + 1), (copyOfStruct = Cursor__get_Rest(inp)[0], copyOfStruct))]);
        })(inp_1);
    }
    else {
        return new ParserResult$1(1, [new ParseError(inp_1.idx, matchValue.fields[0].message)]);
    }
};

export const eoi = (inp) => {
    if (Cursor__get_IsAtEnd(inp)) {
        return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, inp.idx), void 0)]);
    }
    else {
        return new ParserResult$1(1, [new ParseError(inp.idx, "Expected end of input.")]);
    }
};

export const blank = pstr(" ");

export const blanks = (inp_1) => {
    const matchValue_1 = ((inp) => {
        let currIdx = inp.idx;
        let run = true;
        let iterations = 0;
        const res_1 = toList(delay(() => enumerateWhile(() => run, delay(() => {
            const matchValue = blank(Cursor__Goto_Z524259A4(inp, currIdx));
            if (matchValue.tag === 1) {
                run = false;
                return empty();
            }
            else {
                const res = matchValue.fields[0];
                return append(singleton(res), delay(() => {
                    currIdx = (res.range.endIdx | 0);
                    if (inp.idx === currIdx) {
                        run = false;
                    }
                    else {
                        iterations = ((iterations + 1) | 0);
                    }
                    return empty();
                }));
            }
        }))));
        if (iterations < 0) {
            return new ParserResult$1(1, [new ParseError(currIdx, `Expected ${0} occurances, but got ${iterations}.`)]);
        }
        else {
            return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, currIdx), res_1)]);
        }
    })(inp_1);
    if (matchValue_1.tag === 0) {
        const pres = matchValue_1.fields[0];
        return new ParserResult$1(0, [new PVal$1(pres.range, join("", map((x) => x.result, pres.result)))]);
    }
    else {
        return new ParserResult$1(1, [matchValue_1.fields[0]]);
    }
};

export const blanks1 = (() => {
    let p_4;
    const builder$0040 = parse;
    p_4 = ((inp_1) => {
        let res_2;
        const matchValue_2 = ((inp) => {
            let currIdx = inp.idx;
            let run = true;
            let iterations = 0;
            const res_1 = toList(delay(() => enumerateWhile(() => run, delay(() => {
                const matchValue = blank(Cursor__Goto_Z524259A4(inp, currIdx));
                if (matchValue.tag === 1) {
                    run = false;
                    return empty();
                }
                else {
                    const res = matchValue.fields[0];
                    return append(singleton(res), delay(() => {
                        currIdx = (res.range.endIdx | 0);
                        if (inp.idx === currIdx) {
                            run = false;
                        }
                        else {
                            iterations = ((iterations + 1) | 0);
                        }
                        return empty();
                    }));
                }
            }))));
            if (iterations < 0) {
                return new ParserResult$1(1, [new ParseError(currIdx, `Expected ${0} occurances, but got ${iterations}.`)]);
            }
            else {
                return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, currIdx), res_1)]);
            }
        })(inp_1);
        if (matchValue_2.tag === 0) {
            const pRes = matchValue_2.fields[0];
            return ((res_2 = pRes, isEmpty(res_2.result) ? ParserBuilder__Return_306E893E(builder$0040, new ParseError(res_2.range.startIdx, "Expected at least one element.")) : ParserBuilder__Return_Z3CB29237(builder$0040, res_2)))(Cursor__Goto_Z524259A4(inp_1, pRes.range.endIdx));
        }
        else {
            return new ParserResult$1(1, [matchValue_2.fields[0]]);
        }
    });
    return (inp_2) => {
        const matchValue_3 = p_4(inp_2);
        if (matchValue_3.tag === 0) {
            const pres = matchValue_3.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres.range, join("", map((x) => x.result, pres.result)))]);
        }
        else {
            return new ParserResult$1(1, [matchValue_3.fields[0]]);
        }
    };
})();

export function many1Str2(p1, p2) {
    return (inp_3) => {
        let r1;
        const matchValue_3 = p1(inp_3);
        if (matchValue_3.tag === 0) {
            const pRes_1 = matchValue_3.fields[0];
            return ((r1 = pRes_1, (inp_2) => {
                let r2;
                const matchValue_2 = ((inp_1) => {
                    const matchValue_1 = ((inp) => {
                        let currIdx = inp.idx;
                        let run = true;
                        let iterations = 0;
                        const res_1 = toList(delay(() => enumerateWhile(() => run, delay(() => {
                            const matchValue = p2(Cursor__Goto_Z524259A4(inp, currIdx));
                            if (matchValue.tag === 1) {
                                run = false;
                                return empty();
                            }
                            else {
                                const res = matchValue.fields[0];
                                return append(singleton(res), delay(() => {
                                    currIdx = (res.range.endIdx | 0);
                                    if (inp.idx === currIdx) {
                                        run = false;
                                    }
                                    else {
                                        iterations = ((iterations + 1) | 0);
                                    }
                                    return empty();
                                }));
                            }
                        }))));
                        if (iterations < 0) {
                            return new ParserResult$1(1, [new ParseError(currIdx, `Expected ${0} occurances, but got ${iterations}.`)]);
                        }
                        else {
                            return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, currIdx), res_1)]);
                        }
                    })(inp_1);
                    if (matchValue_1.tag === 0) {
                        const pres = matchValue_1.fields[0];
                        return new ParserResult$1(0, [new PVal$1(pres.range, join("", map((x) => x.result, pres.result)))]);
                    }
                    else {
                        return new ParserResult$1(1, [matchValue_1.fields[0]]);
                    }
                })(inp_2);
                if (matchValue_2.tag === 0) {
                    const pRes = matchValue_2.fields[0];
                    return ((r2 = pRes, ParserBuilder__Return_Z3CB29237(parse, new PVal$1(new Range$(r1.range.startIdx, r2.range.endIdx), r1.result + r2.result))))(Cursor__Goto_Z524259A4(inp_2, pRes.range.endIdx));
                }
                else {
                    return new ParserResult$1(1, [matchValue_2.fields[0]]);
                }
            }))(Cursor__Goto_Z524259A4(inp_3, pRes_1.range.endIdx));
        }
        else {
            return new ParserResult$1(1, [matchValue_3.fields[0]]);
        }
    };
}

export function pchar(predicate, errMsg) {
    return (inp) => {
        const matchValue = CursorModule_hasRemainingChars(1, inp);
        if (matchValue.tag === 0) {
            return ((inp_1) => {
                const c = Cursor__get_Rest(inp_1)[0];
                if (predicate(c)) {
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_1.idx, inp_1.idx + 1), c)]);
                }
                else {
                    return new ParserResult$1(1, [new ParseError(inp_1.idx, errMsg(c))]);
                }
            })(inp);
        }
        else {
            return new ParserResult$1(1, [new ParseError(inp.idx, matchValue.fields[0].message)]);
        }
    };
}

export const letter = pchar(isLetter, (() => {
    const clo = toText(printf("Expected letter, but got \'%c\'."));
    return clo;
})());

export const digit = pchar(isDigit, (() => {
    const clo = toText(printf("Expected letter, but got \'%c\'."));
    return clo;
})());

export function pchoice(parsers) {
    return (inp) => {
        const iter = (parsers_1_mut) => {
            iter:
            while (true) {
                const parsers_1 = parsers_1_mut;
                if (!isEmpty(parsers_1)) {
                    const matchValue = head(parsers_1)(inp);
                    if (matchValue.tag === 1) {
                        parsers_1_mut = tail(parsers_1);
                        continue iter;
                    }
                    else {
                        return matchValue;
                    }
                }
                else {
                    return new ParserResult$1(1, [new ParseError(inp.idx, "No more parsers to try.")]);
                }
                break;
            }
        };
        return iter(parsers);
    };
}

