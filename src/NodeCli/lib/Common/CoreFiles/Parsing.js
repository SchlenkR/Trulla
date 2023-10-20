import { Union } from "../../fable_modules/fable-library.4.3.0/Types.js";
import { anonRecord_type, union_type, option_type, string_type } from "../../fable_modules/fable-library.4.3.0/Reflection.js";
import { Cursor, eoi, anyChar, Cursor__CanGoto_Z524259A4, blanks, blanks1, pchoice, parse, ParserBuilder__Return_Z3CB29237, op_Splice, digit, op_LessBarGreater, letter, many1Str2, Cursor__Goto_Z524259A4, ParseError, Range$, ParserResult$1, pstr, PVal$1, PVal$1_$reflection } from "./TheBlunt.js";
import { ofArray, reduce, cons, tail, head, isEmpty } from "../../fable_modules/fable-library.4.3.0/List.js";
import { singleton, append, empty, enumerateWhile, delay, toList } from "../../fable_modules/fable-library.4.3.0/Seq.js";
import { substring } from "../../fable_modules/fable-library.4.3.0/String.js";

export class Token extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Text", "Hole", "For", "If", "Else", "End"];
    }
}

export function Token_$reflection() {
    return union_type("Trulla.Core.Token", [], Token, () => [[["Item", string_type]], [["Item", PVal$1_$reflection(MemberToken_$reflection())]], [["ident", PVal$1_$reflection(string_type)], ["exp", PVal$1_$reflection(MemberToken_$reflection())], ["sep", PVal$1_$reflection(option_type(string_type))]], [["Item", PVal$1_$reflection(MemberToken_$reflection())]], [], []]);
}

export class MemberToken extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["AccessToken", "IdentToken"];
    }
}

export function MemberToken_$reflection() {
    return union_type("Trulla.Core.MemberToken", [], MemberToken, () => [[["Item", anonRecord_type(["instanceExp", PVal$1_$reflection(MemberToken_$reflection())], ["memberName", string_type])]], [["Item", string_type]]]);
}

export const Parsing_Consts_beginExp = "{{";

export const Parsing_Consts_endExp = "}}";

export const Parsing_Keywords_for$0027 = "for";

export const Parsing_Keywords_in$0027 = "in";

export const Parsing_Keywords_sep = "|";

export const Parsing_Keywords_if$0027 = "if";

export const Parsing_Keywords_elseIf$0027 = "else if";

export const Parsing_Keywords_else$0027 = "else";

export const Parsing_Keywords_end$0027 = "end";

export function Parsing_MemberToken_createFromSegments(segments) {
    let pval_1;
    if (isEmpty(segments)) {
        throw new Error("Should never happen: Information loss in sepBy1 parser.");
    }
    else {
        const mkAccToks = (curr_mut, segs_1_mut) => {
            mkAccToks:
            while (true) {
                const curr = curr_mut, segs_1 = segs_1_mut;
                if (isEmpty(segs_1)) {
                    return curr;
                }
                else {
                    const seg = head(segs_1);
                    curr_mut = (new PVal$1(seg.range, new MemberToken(0, [{
                        instanceExp: curr,
                        memberName: seg.result,
                    }])));
                    segs_1_mut = tail(segs_1);
                    continue mkAccToks;
                }
                break;
            }
        };
        return mkAccToks((pval_1 = head(segments), new PVal$1(pval_1.range, new MemberToken(1, [pval_1.result]))), tail(segments));
    }
}

export const Parsing_Internal_begin$0027 = (() => {
    const pa = pstr(Parsing_Consts_beginExp);
    let pb;
    const p = pstr("{");
    pb = ((inp) => {
        const matchValue = ((inp_1) => {
            const matchValue_1 = p(inp_1);
            if (matchValue_1.tag === 1) {
                return new ParserResult$1(1, [matchValue_1.fields[0]]);
            }
            else {
                const res = matchValue_1.fields[0];
                return new ParserResult$1(0, [new PVal$1(res.range, res)]);
            }
        })(inp);
        if (matchValue.tag === 1) {
            return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, inp.idx), void 0)]);
        }
        else {
            return new ParserResult$1(1, [new ParseError(inp.idx, "Unexpected.")]);
        }
    });
    return (inp_3) => {
        const matchValue_3 = ((inp_2) => {
            const matchValue_2 = pa(inp_2);
            if (matchValue_2.tag === 1) {
                return new ParserResult$1(1, [matchValue_2.fields[0]]);
            }
            else {
                const ares = matchValue_2.fields[0];
                const matchValue_1_1 = pb(Cursor__Goto_Z524259A4(inp_2, ares.range.endIdx));
                if (matchValue_1_1.tag === 1) {
                    return new ParserResult$1(1, [matchValue_1_1.fields[0]]);
                }
                else {
                    const bres = matchValue_1_1.fields[0];
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_2.idx, bres.range.endIdx), [ares.result, bres.result])]);
                }
            }
        })(inp_3);
        if (matchValue_3.tag === 0) {
            const pres = matchValue_3.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres.range, pres.result[0])]);
        }
        else {
            return new ParserResult$1(1, [matchValue_3.fields[0]]);
        }
    };
})();

export const Parsing_Internal_templateExp = (() => {
    let p_28, pa_6, pa_4, pa_2, pb_4, p_33, pa_18, pa_16, p_35, p_37;
    const endExp = pstr(Parsing_Consts_endExp);
    let propAccess;
    let p_6;
    const pelem = many1Str2(letter, op_LessBarGreater()(letter)(digit));
    const psep = op_Splice(".");
    const pelem_1 = pelem;
    p_6 = ((inp_4) => {
        let x;
        const matchValue_5 = pelem_1(inp_4);
        if (matchValue_5.tag === 0) {
            const pRes_1 = matchValue_5.fields[0];
            return ((x = pRes_1, (inp_3) => {
                let xs;
                const matchValue_4 = ((inp_2) => {
                    let currIdx = inp_2.idx;
                    let run = true;
                    let iterations = 0;
                    const res_1 = toList(delay(() => enumerateWhile(() => run, delay(() => {
                        const matchValue_3 = ((inp_1) => {
                            const matchValue_2 = ((inp) => {
                                const matchValue = psep(inp);
                                if (matchValue.tag === 1) {
                                    return new ParserResult$1(1, [matchValue.fields[0]]);
                                }
                                else {
                                    const ares = matchValue.fields[0];
                                    const matchValue_1 = pelem_1(Cursor__Goto_Z524259A4(inp, ares.range.endIdx));
                                    if (matchValue_1.tag === 1) {
                                        return new ParserResult$1(1, [matchValue_1.fields[0]]);
                                    }
                                    else {
                                        const bres = matchValue_1.fields[0];
                                        return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, bres.range.endIdx), [ares.result, bres.result])]);
                                    }
                                }
                            })(inp_1);
                            if (matchValue_2.tag === 0) {
                                const pres = matchValue_2.fields[0];
                                return new ParserResult$1(0, [new PVal$1(pres.range, pres.result[1])]);
                            }
                            else {
                                return new ParserResult$1(1, [matchValue_2.fields[0]]);
                            }
                        })(Cursor__Goto_Z524259A4(inp_2, currIdx));
                        if (matchValue_3.tag === 1) {
                            run = false;
                            return empty();
                        }
                        else {
                            const res = matchValue_3.fields[0];
                            return append(singleton(res), delay(() => {
                                currIdx = (res.range.endIdx | 0);
                                if (inp_2.idx === currIdx) {
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
                        return new ParserResult$1(0, [new PVal$1(new Range$(inp_2.idx, currIdx), res_1)]);
                    }
                })(inp_3);
                if (matchValue_4.tag === 0) {
                    const pRes = matchValue_4.fields[0];
                    return ((xs = pRes, ParserBuilder__Return_Z3CB29237(parse, new PVal$1(new Range$(x.range.startIdx, xs.range.endIdx), cons(x, xs.result)))))(Cursor__Goto_Z524259A4(inp_3, pRes.range.endIdx));
                }
                else {
                    return new ParserResult$1(1, [matchValue_4.fields[0]]);
                }
            }))(Cursor__Goto_Z524259A4(inp_4, pRes_1.range.endIdx));
        }
        else {
            return new ParserResult$1(1, [matchValue_5.fields[0]]);
        }
    });
    propAccess = ((inp_5) => {
        const matchValue_6 = p_6(inp_5);
        if (matchValue_6.tag === 0) {
            const pRes_2 = matchValue_6.fields[0];
            return ParserBuilder__Return_Z3CB29237(parse, Parsing_MemberToken_createFromSegments(pRes_2.result))(Cursor__Goto_Z524259A4(inp_5, pRes_2.range.endIdx));
        }
        else {
            return new ParserResult$1(1, [matchValue_6.fields[0]]);
        }
    });
    const body = pchoice(ofArray([(p_28 = ((pa_6 = ((pa_4 = ((pa_2 = pstr(Parsing_Keywords_for$0027), (inp_7) => {
        const matchValue_8 = ((inp_6) => {
            const matchValue_7 = pa_2(inp_6);
            if (matchValue_7.tag === 1) {
                return new ParserResult$1(1, [matchValue_7.fields[0]]);
            }
            else {
                const ares_1 = matchValue_7.fields[0];
                const matchValue_1_1 = blanks1(Cursor__Goto_Z524259A4(inp_6, ares_1.range.endIdx));
                if (matchValue_1_1.tag === 1) {
                    return new ParserResult$1(1, [matchValue_1_1.fields[0]]);
                }
                else {
                    const bres_1 = matchValue_1_1.fields[0];
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_6.idx, bres_1.range.endIdx), [ares_1.result, bres_1.result])]);
                }
            }
        })(inp_7);
        if (matchValue_8.tag === 0) {
            const pres_1 = matchValue_8.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_1.range, pres_1.result[1])]);
        }
        else {
            return new ParserResult$1(1, [matchValue_8.fields[0]]);
        }
    })), (pb_4 = many1Str2(letter, op_LessBarGreater()(letter)(digit)), (inp_9) => {
        const matchValue_10 = ((inp_8) => {
            const matchValue_9 = pa_4(inp_8);
            if (matchValue_9.tag === 1) {
                return new ParserResult$1(1, [matchValue_9.fields[0]]);
            }
            else {
                const ares_2 = matchValue_9.fields[0];
                const matchValue_1_2 = pb_4(Cursor__Goto_Z524259A4(inp_8, ares_2.range.endIdx));
                if (matchValue_1_2.tag === 1) {
                    return new ParserResult$1(1, [matchValue_1_2.fields[0]]);
                }
                else {
                    const bres_2 = matchValue_1_2.fields[0];
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_8.idx, bres_2.range.endIdx), [ares_2.result, bres_2.result])]);
                }
            }
        })(inp_9);
        if (matchValue_10.tag === 0) {
            const pres_2 = matchValue_10.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_2.range, pres_2.result[1])]);
        }
        else {
            return new ParserResult$1(1, [matchValue_10.fields[0]]);
        }
    }))), (inp_11) => {
        const matchValue_12 = ((inp_10) => {
            const matchValue_11 = pa_6(inp_10);
            if (matchValue_11.tag === 1) {
                return new ParserResult$1(1, [matchValue_11.fields[0]]);
            }
            else {
                const ares_3 = matchValue_11.fields[0];
                const matchValue_1_3 = blanks1(Cursor__Goto_Z524259A4(inp_10, ares_3.range.endIdx));
                if (matchValue_1_3.tag === 1) {
                    return new ParserResult$1(1, [matchValue_1_3.fields[0]]);
                }
                else {
                    const bres_3 = matchValue_1_3.fields[0];
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_10.idx, bres_3.range.endIdx), [ares_3.result, bres_3.result])]);
                }
            }
        })(inp_11);
        if (matchValue_12.tag === 0) {
            const pres_3 = matchValue_12.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_3.range, pres_3.result[0])]);
        }
        else {
            return new ParserResult$1(1, [matchValue_12.fields[0]]);
        }
    })), (inp_26) => {
        let identExp, p_27, pa_10, pa_8;
        const matchValue_27 = p_28(inp_26);
        if (matchValue_27.tag === 0) {
            const pRes_5 = matchValue_27.fields[0];
            return ((identExp = pRes_5, (p_27 = ((pa_10 = ((pa_8 = pstr(Parsing_Keywords_in$0027), (inp_13) => {
                const matchValue_14 = ((inp_12) => {
                    const matchValue_13 = pa_8(inp_12);
                    if (matchValue_13.tag === 1) {
                        return new ParserResult$1(1, [matchValue_13.fields[0]]);
                    }
                    else {
                        const ares_4 = matchValue_13.fields[0];
                        const matchValue_1_4 = blanks1(Cursor__Goto_Z524259A4(inp_12, ares_4.range.endIdx));
                        if (matchValue_1_4.tag === 1) {
                            return new ParserResult$1(1, [matchValue_1_4.fields[0]]);
                        }
                        else {
                            const bres_4 = matchValue_1_4.fields[0];
                            return new ParserResult$1(0, [new PVal$1(new Range$(inp_12.idx, bres_4.range.endIdx), [ares_4.result, bres_4.result])]);
                        }
                    }
                })(inp_13);
                if (matchValue_14.tag === 0) {
                    const pres_4 = matchValue_14.fields[0];
                    return new ParserResult$1(0, [new PVal$1(pres_4.range, pres_4.result[1])]);
                }
                else {
                    return new ParserResult$1(1, [matchValue_14.fields[0]]);
                }
            })), (inp_15) => {
                const matchValue_16 = ((inp_14) => {
                    const matchValue_15 = pa_10(inp_14);
                    if (matchValue_15.tag === 1) {
                        return new ParserResult$1(1, [matchValue_15.fields[0]]);
                    }
                    else {
                        const ares_5 = matchValue_15.fields[0];
                        const matchValue_1_5 = propAccess(Cursor__Goto_Z524259A4(inp_14, ares_5.range.endIdx));
                        if (matchValue_1_5.tag === 1) {
                            return new ParserResult$1(1, [matchValue_1_5.fields[0]]);
                        }
                        else {
                            const bres_5 = matchValue_1_5.fields[0];
                            return new ParserResult$1(0, [new PVal$1(new Range$(inp_14.idx, bres_5.range.endIdx), [ares_5.result, bres_5.result])]);
                        }
                    }
                })(inp_15);
                if (matchValue_16.tag === 0) {
                    const pres_5 = matchValue_16.fields[0];
                    return new ParserResult$1(0, [new PVal$1(pres_5.range, pres_5.result[1])]);
                }
                else {
                    return new ParserResult$1(1, [matchValue_16.fields[0]]);
                }
            })), (inp_25) => {
                let memberExp, p_26, p_22, pa_14, pb_12;
                const matchValue_26 = p_27(inp_25);
                if (matchValue_26.tag === 0) {
                    const pRes_4 = matchValue_26.fields[0];
                    return ((memberExp = pRes_4, (p_26 = op_LessBarGreater()((p_22 = ((pa_14 = ((pb_12 = pstr(Parsing_Keywords_sep), (inp_17) => {
                        const matchValue_18 = ((inp_16) => {
                            const matchValue_17 = blanks(inp_16);
                            if (matchValue_17.tag === 1) {
                                return new ParserResult$1(1, [matchValue_17.fields[0]]);
                            }
                            else {
                                const ares_6 = matchValue_17.fields[0];
                                const matchValue_1_6 = pb_12(Cursor__Goto_Z524259A4(inp_16, ares_6.range.endIdx));
                                if (matchValue_1_6.tag === 1) {
                                    return new ParserResult$1(1, [matchValue_1_6.fields[0]]);
                                }
                                else {
                                    const bres_6 = matchValue_1_6.fields[0];
                                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_16.idx, bres_6.range.endIdx), [ares_6.result, bres_6.result])]);
                                }
                            }
                        })(inp_17);
                        if (matchValue_18.tag === 0) {
                            const pres_6 = matchValue_18.fields[0];
                            return new ParserResult$1(0, [new PVal$1(pres_6.range, pres_6.result[1])]);
                        }
                        else {
                            return new ParserResult$1(1, [matchValue_18.fields[0]]);
                        }
                    })), (inp_21) => {
                        const matchValue_22 = ((inp_20) => {
                            const matchValue_21 = pa_14(inp_20);
                            if (matchValue_21.tag === 1) {
                                return new ParserResult$1(1, [matchValue_21.fields[0]]);
                            }
                            else {
                                const ares_7 = matchValue_21.fields[0];
                                const matchValue_1_7 = ((inp_18) => {
                                    const iter = (currIdx_1_mut_1) => {
                                        iter:
                                        while (true) {
                                            const currIdx_1 = currIdx_1_mut_1;
                                            const matchValue_19 = ((inp_19) => {
                                                const matchValue_20 = endExp(inp_19);
                                                if (matchValue_20.tag === 1) {
                                                    return new ParserResult$1(1, [matchValue_20.fields[0]]);
                                                }
                                                else {
                                                    const res_2 = matchValue_20.fields[0];
                                                    return new ParserResult$1(0, [new PVal$1(res_2.range, res_2)]);
                                                }
                                            })(Cursor__Goto_Z524259A4(inp_18, currIdx_1));
                                            if (matchValue_19.tag === 1) {
                                                if (!Cursor__CanGoto_Z524259A4(inp_18, currIdx_1 + 1)) {
                                                    return new ParserResult$1(1, [new ParseError(currIdx_1, "End of input.")]);
                                                }
                                                else {
                                                    currIdx_1_mut_1 = (currIdx_1 + 1);
                                                    continue iter;
                                                }
                                            }
                                            else {
                                                return new ParserResult$1(0, [new PVal$1(new Range$(inp_18.idx, currIdx_1), substring(inp_18.original, inp_18.idx, currIdx_1 - inp_18.idx))]);
                                            }
                                            break;
                                        }
                                    };
                                    return iter(inp_18.idx);
                                })(Cursor__Goto_Z524259A4(inp_20, ares_7.range.endIdx));
                                if (matchValue_1_7.tag === 1) {
                                    return new ParserResult$1(1, [matchValue_1_7.fields[0]]);
                                }
                                else {
                                    const bres_7 = matchValue_1_7.fields[0];
                                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_20.idx, bres_7.range.endIdx), [ares_7.result, bres_7.result])]);
                                }
                            }
                        })(inp_21);
                        if (matchValue_22.tag === 0) {
                            const pres_7 = matchValue_22.fields[0];
                            return new ParserResult$1(0, [new PVal$1(pres_7.range, pres_7.result[1])]);
                        }
                        else {
                            return new ParserResult$1(1, [matchValue_22.fields[0]]);
                        }
                    })), (inp_22) => {
                        const matchValue_23 = p_22(inp_22);
                        if (matchValue_23.tag === 0) {
                            const pres_8 = matchValue_23.fields[0];
                            return new ParserResult$1(0, [new PVal$1(pres_8.range, pres_8.result)]);
                        }
                        else {
                            return new ParserResult$1(1, [matchValue_23.fields[0]]);
                        }
                    }))((inp_23) => {
                        const matchValue_24 = blanks(inp_23);
                        if (matchValue_24.tag === 0) {
                            return new ParserResult$1(0, [new PVal$1(matchValue_24.fields[0].range, void 0)]);
                        }
                        else {
                            return new ParserResult$1(1, [matchValue_24.fields[0]]);
                        }
                    }), (inp_24) => {
                        let sepExp;
                        const matchValue_25 = p_26(inp_24);
                        if (matchValue_25.tag === 0) {
                            const pRes_3 = matchValue_25.fields[0];
                            return ((sepExp = pRes_3, ParserBuilder__Return_Z3CB29237(parse, new PVal$1(reduce((r, r_1) => (new Range$(r.startIdx, r_1.endIdx)), ofArray([identExp.range, memberExp.range, sepExp.range])), new Token(2, [identExp, memberExp, sepExp])))))(Cursor__Goto_Z524259A4(inp_24, pRes_3.range.endIdx));
                        }
                        else {
                            return new ParserResult$1(1, [matchValue_25.fields[0]]);
                        }
                    })))(Cursor__Goto_Z524259A4(inp_25, pRes_4.range.endIdx));
                }
                else {
                    return new ParserResult$1(1, [matchValue_26.fields[0]]);
                }
            })))(Cursor__Goto_Z524259A4(inp_26, pRes_5.range.endIdx));
        }
        else {
            return new ParserResult$1(1, [matchValue_27.fields[0]]);
        }
    }), (p_33 = ((pa_18 = ((pa_16 = pstr(Parsing_Keywords_if$0027), (inp_28) => {
        const matchValue_29 = ((inp_27) => {
            const matchValue_28 = pa_16(inp_27);
            if (matchValue_28.tag === 1) {
                return new ParserResult$1(1, [matchValue_28.fields[0]]);
            }
            else {
                const ares_8 = matchValue_28.fields[0];
                const matchValue_1_8 = blanks1(Cursor__Goto_Z524259A4(inp_27, ares_8.range.endIdx));
                if (matchValue_1_8.tag === 1) {
                    return new ParserResult$1(1, [matchValue_1_8.fields[0]]);
                }
                else {
                    const bres_8 = matchValue_1_8.fields[0];
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_27.idx, bres_8.range.endIdx), [ares_8.result, bres_8.result])]);
                }
            }
        })(inp_28);
        if (matchValue_29.tag === 0) {
            const pres_10 = matchValue_29.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_10.range, pres_10.result[1])]);
        }
        else {
            return new ParserResult$1(1, [matchValue_29.fields[0]]);
        }
    })), (inp_30) => {
        const matchValue_31 = ((inp_29) => {
            const matchValue_30 = pa_18(inp_29);
            if (matchValue_30.tag === 1) {
                return new ParserResult$1(1, [matchValue_30.fields[0]]);
            }
            else {
                const ares_9 = matchValue_30.fields[0];
                const matchValue_1_9 = propAccess(Cursor__Goto_Z524259A4(inp_29, ares_9.range.endIdx));
                if (matchValue_1_9.tag === 1) {
                    return new ParserResult$1(1, [matchValue_1_9.fields[0]]);
                }
                else {
                    const bres_9 = matchValue_1_9.fields[0];
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_29.idx, bres_9.range.endIdx), [ares_9.result, bres_9.result])]);
                }
            }
        })(inp_30);
        if (matchValue_31.tag === 0) {
            const pres_11 = matchValue_31.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_11.range, pres_11.result[1])]);
        }
        else {
            return new ParserResult$1(1, [matchValue_31.fields[0]]);
        }
    })), (inp_31) => {
        const matchValue_32 = p_33(inp_31);
        if (matchValue_32.tag === 0) {
            const pres_12 = matchValue_32.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_12.range, new Token(3, [pres_12]))]);
        }
        else {
            return new ParserResult$1(1, [matchValue_32.fields[0]]);
        }
    }), (p_35 = pstr(Parsing_Keywords_else$0027), (inp_32) => {
        const matchValue_33 = p_35(inp_32);
        if (matchValue_33.tag === 0) {
            return new ParserResult$1(0, [new PVal$1(matchValue_33.fields[0].range, new Token(4, []))]);
        }
        else {
            return new ParserResult$1(1, [matchValue_33.fields[0]]);
        }
    }), (p_37 = pstr(Parsing_Keywords_end$0027), (inp_33) => {
        const matchValue_34 = p_37(inp_33);
        if (matchValue_34.tag === 0) {
            return new ParserResult$1(0, [new PVal$1(matchValue_34.fields[0].range, new Token(5, []))]);
        }
        else {
            return new ParserResult$1(1, [matchValue_34.fields[0]]);
        }
    }), (inp_34) => {
        const matchValue_35 = propAccess(inp_34);
        if (matchValue_35.tag === 0) {
            const pres_15 = matchValue_35.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_15.range, new Token(1, [pres_15]))]);
        }
        else {
            return new ParserResult$1(1, [matchValue_35.fields[0]]);
        }
    }]));
    return (inp_42) => {
        const matchValue_43 = ((inp_41) => {
            const matchValue_42 = ((inp_40) => {
                const matchValue_41 = ((inp_39) => {
                    const matchValue_40 = ((inp_38) => {
                        const matchValue_39 = ((inp_37) => {
                            const matchValue_38 = ((inp_36) => {
                                const matchValue_37 = ((inp_35) => {
                                    const matchValue_36 = Parsing_Internal_begin$0027(inp_35);
                                    if (matchValue_36.tag === 1) {
                                        return new ParserResult$1(1, [matchValue_36.fields[0]]);
                                    }
                                    else {
                                        const ares_10 = matchValue_36.fields[0];
                                        const matchValue_1_10 = blanks(Cursor__Goto_Z524259A4(inp_35, ares_10.range.endIdx));
                                        if (matchValue_1_10.tag === 1) {
                                            return new ParserResult$1(1, [matchValue_1_10.fields[0]]);
                                        }
                                        else {
                                            const bres_10 = matchValue_1_10.fields[0];
                                            return new ParserResult$1(0, [new PVal$1(new Range$(inp_35.idx, bres_10.range.endIdx), [ares_10.result, bres_10.result])]);
                                        }
                                    }
                                })(inp_36);
                                if (matchValue_37.tag === 0) {
                                    const pres_16 = matchValue_37.fields[0];
                                    return new ParserResult$1(0, [new PVal$1(pres_16.range, pres_16.result[0])]);
                                }
                                else {
                                    return new ParserResult$1(1, [matchValue_37.fields[0]]);
                                }
                            })(inp_37);
                            if (matchValue_38.tag === 1) {
                                return new ParserResult$1(1, [matchValue_38.fields[0]]);
                            }
                            else {
                                const ares_11 = matchValue_38.fields[0];
                                const matchValue_1_11 = body(Cursor__Goto_Z524259A4(inp_37, ares_11.range.endIdx));
                                if (matchValue_1_11.tag === 1) {
                                    return new ParserResult$1(1, [matchValue_1_11.fields[0]]);
                                }
                                else {
                                    const bres_11 = matchValue_1_11.fields[0];
                                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_37.idx, bres_11.range.endIdx), [ares_11.result, bres_11.result])]);
                                }
                            }
                        })(inp_38);
                        if (matchValue_39.tag === 0) {
                            const pres_17 = matchValue_39.fields[0];
                            return new ParserResult$1(0, [new PVal$1(pres_17.range, pres_17.result[1])]);
                        }
                        else {
                            return new ParserResult$1(1, [matchValue_39.fields[0]]);
                        }
                    })(inp_39);
                    if (matchValue_40.tag === 1) {
                        return new ParserResult$1(1, [matchValue_40.fields[0]]);
                    }
                    else {
                        const ares_12 = matchValue_40.fields[0];
                        const matchValue_1_12 = blanks(Cursor__Goto_Z524259A4(inp_39, ares_12.range.endIdx));
                        if (matchValue_1_12.tag === 1) {
                            return new ParserResult$1(1, [matchValue_1_12.fields[0]]);
                        }
                        else {
                            const bres_12 = matchValue_1_12.fields[0];
                            return new ParserResult$1(0, [new PVal$1(new Range$(inp_39.idx, bres_12.range.endIdx), [ares_12.result, bres_12.result])]);
                        }
                    }
                })(inp_40);
                if (matchValue_41.tag === 0) {
                    const pres_18 = matchValue_41.fields[0];
                    return new ParserResult$1(0, [new PVal$1(pres_18.range, pres_18.result[0])]);
                }
                else {
                    return new ParserResult$1(1, [matchValue_41.fields[0]]);
                }
            })(inp_41);
            if (matchValue_42.tag === 1) {
                return new ParserResult$1(1, [matchValue_42.fields[0]]);
            }
            else {
                const ares_13 = matchValue_42.fields[0];
                const matchValue_1_13 = endExp(Cursor__Goto_Z524259A4(inp_41, ares_13.range.endIdx));
                if (matchValue_1_13.tag === 1) {
                    return new ParserResult$1(1, [matchValue_1_13.fields[0]]);
                }
                else {
                    const bres_13 = matchValue_1_13.fields[0];
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp_41.idx, bres_13.range.endIdx), [ares_13.result, bres_13.result])]);
                }
            }
        })(inp_42);
        if (matchValue_43.tag === 0) {
            const pres_19 = matchValue_43.fields[0];
            return new ParserResult$1(0, [new PVal$1(pres_19.range, pres_19.result[0])]);
        }
        else {
            return new ParserResult$1(1, [matchValue_43.fields[0]]);
        }
    };
})();

export const Parsing_Internal_expOrText = pchoice(ofArray([Parsing_Internal_templateExp, (inp_2) => {
    const matchValue_2 = ((inp) => {
        const iter = (currIdx_mut) => {
            iter:
            while (true) {
                const currIdx = currIdx_mut;
                const matchValue = ((inp_1) => {
                    const matchValue_1 = Parsing_Internal_begin$0027(inp_1);
                    if (matchValue_1.tag === 1) {
                        return new ParserResult$1(1, [matchValue_1.fields[0]]);
                    }
                    else {
                        const res = matchValue_1.fields[0];
                        return new ParserResult$1(0, [new PVal$1(res.range, res)]);
                    }
                })(Cursor__Goto_Z524259A4(inp, currIdx));
                if (matchValue.tag === 1) {
                    if (!Cursor__CanGoto_Z524259A4(inp, currIdx + 1)) {
                        return new ParserResult$1(1, [new ParseError(currIdx, "End of input.")]);
                    }
                    else {
                        currIdx_mut = (currIdx + 1);
                        continue iter;
                    }
                }
                else {
                    return new ParserResult$1(0, [new PVal$1(new Range$(inp.idx, currIdx), substring(inp.original, inp.idx, currIdx - inp.idx))]);
                }
                break;
            }
        };
        return iter(inp.idx);
    })(inp_2);
    if (matchValue_2.tag === 0) {
        const pres = matchValue_2.fields[0];
        return new ParserResult$1(0, [new PVal$1(pres.range, new Token(0, [pres.result]))]);
    }
    else {
        return new ParserResult$1(1, [matchValue_2.fields[0]]);
    }
}, (inp_3) => {
    const matchValue_3 = anyChar(inp_3);
    if (matchValue_3.tag === 0) {
        const pres_1 = matchValue_3.fields[0];
        return new ParserResult$1(0, [new PVal$1(pres_1.range, new Token(0, [pres_1.result]))]);
    }
    else {
        return new ParserResult$1(1, [matchValue_3.fields[0]]);
    }
}]));

export const Parsing_Internal_ptemplate = (inp_2) => {
    const matchValue_2 = ((inp_1) => {
        const matchValue_1 = ((inp) => {
            let currIdx = inp.idx;
            let run = true;
            let iterations = 0;
            const res_1 = toList(delay(() => enumerateWhile(() => run, delay(() => {
                const matchValue = Parsing_Internal_expOrText(Cursor__Goto_Z524259A4(inp, currIdx));
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
        if (matchValue_1.tag === 1) {
            return new ParserResult$1(1, [matchValue_1.fields[0]]);
        }
        else {
            const ares = matchValue_1.fields[0];
            const matchValue_1_1 = eoi(Cursor__Goto_Z524259A4(inp_1, ares.range.endIdx));
            if (matchValue_1_1.tag === 1) {
                return new ParserResult$1(1, [matchValue_1_1.fields[0]]);
            }
            else {
                const bres = matchValue_1_1.fields[0];
                return new ParserResult$1(0, [new PVal$1(new Range$(inp_1.idx, bres.range.endIdx), [ares.result, bres.result])]);
            }
        }
    })(inp_2);
    if (matchValue_2.tag === 0) {
        const pres = matchValue_2.fields[0];
        return new ParserResult$1(0, [new PVal$1(pres.range, pres.result[0])]);
    }
    else {
        return new ParserResult$1(1, [matchValue_2.fields[0]]);
    }
};

export function Parsing_parseTemplate(templateString) {
    return Parsing_Internal_ptemplate(new Cursor(templateString, 0));
}

