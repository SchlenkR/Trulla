import { Record, toString, Union } from "../../fable_modules/fable-library.4.3.0/Types.js";
import { anonRecord_type, list_type, option_type, record_type, class_type, string_type, union_type, int32_type } from "../../fable_modules/fable-library.4.3.0/Reflection.js";
import { printf, toText } from "../../fable_modules/fable-library.4.3.0/String.js";
import { RangeModule_zero, PVal$1_$reflection, Range$_$reflection } from "./TheBlunt.js";
import { add, empty } from "../../fable_modules/fable-library.4.3.0/Map.js";
import { comparePrimitives, compare } from "../../fable_modules/fable-library.4.3.0/Util.js";
import { singleton as singleton_1, reverse, tail, head, isEmpty, item, length, cons, empty as empty_1 } from "../../fable_modules/fable-library.4.3.0/List.js";
import { singleton, append, delay, toList } from "../../fable_modules/fable-library.4.3.0/Seq.js";
import { TrullaError } from "./Core.js";
import { FSharpResult$2 } from "../../fable_modules/fable-library.4.3.0/Choice.js";

export class TVar extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Root", "TVar"];
    }
}

export function TVar_$reflection() {
    return union_type("Trulla.Core.TVar", [], TVar, () => [[], [["Item", int32_type]]]);
}

export class TVal$1 extends Record {
    constructor(range, tvar, bindingContext, value) {
        super();
        this.range = range;
        this.tvar = tvar;
        this.bindingContext = bindingContext;
        this.value = value;
    }
    toString() {
        const this$ = this;
        const arg = toString(this$.range);
        let arg_1;
        let copyOfStruct = this$.value;
        arg_1 = toString(copyOfStruct);
        return toText(printf("(%s)%s"))(arg)(arg_1);
    }
}

export function TVal$1_$reflection(gen0) {
    return record_type("Trulla.Core.TVal`1", [gen0], TVal$1, () => [["range", Range$_$reflection()], ["tvar", TVar_$reflection()], ["bindingContext", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, TVar_$reflection()])], ["value", gen0]]);
}

export class TExp extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Text", "Hole", "For", "If", "Else"];
    }
}

export function TExp_$reflection() {
    return union_type("Trulla.Core.TExp", [], TExp, () => [[["Item", string_type]], [["Item", TVal$1_$reflection(MemberExp_$reflection())]], [["ident", TVal$1_$reflection(string_type)], ["exp", TVal$1_$reflection(MemberExp_$reflection())], ["sep", PVal$1_$reflection(option_type(string_type))], ["body", list_type(TExp_$reflection())]], [["cond", TVal$1_$reflection(MemberExp_$reflection())], ["body", list_type(TExp_$reflection())]], [["cond", TVal$1_$reflection(MemberExp_$reflection())], ["body", list_type(TExp_$reflection())]]]);
}

export class MemberExp extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["AccessExp", "IdentExp"];
    }
}

export function MemberExp_$reflection() {
    return union_type("Trulla.Core.MemberExp", [], MemberExp, () => [[["Item", anonRecord_type(["instanceExp", TVal$1_$reflection(MemberExp_$reflection())], ["memberName", string_type])]], [["Item", string_type]]]);
}

export class Ast extends Record {
    constructor(tree, tvarToMemberExp) {
        super();
        this.tree = tree;
        this.tvarToMemberExp = tvarToMemberExp;
    }
}

export function Ast_$reflection() {
    return record_type("Trulla.Core.Ast", [], Ast, () => [["tree", list_type(TExp_$reflection())], ["tvarToMemberExp", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [TVar_$reflection(), MemberExp_$reflection()])]]);
}

class AstModule_Scope extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["IfOrElseScope", "Other"];
    }
}

function AstModule_Scope_$reflection() {
    return union_type("Trulla.Core.AstModule.Scope", [], AstModule_Scope, () => [[["cond", TVal$1_$reflection(MemberExp_$reflection())]], []]);
}

export function AstModule_createTVal(range, tvar, bindingContext, value) {
    return new TVal$1(range, tvar, bindingContext, value);
}

export function AstModule_buildTree(tokens) {
    let tvarToMemberExp = empty({
        Compare: compare,
    });
    let newTVar;
    let x_1 = -1;
    newTVar = (() => {
        x_1 = ((x_1 + 1) | 0);
        return new TVar(1, [x_1]);
    });
    const buildMemberExp = (bindingContext, pexp) => {
        const ofPExpZero = (pexp_1) => {
            const newTValAndAdd = (exp) => {
                const tvar_1 = newTVar();
                tvarToMemberExp = add(tvar_1, exp, tvarToMemberExp);
                return AstModule_createTVal(pexp_1.range, tvar_1, bindingContext, exp);
            };
            const matchValue = pexp_1.result;
            if (matchValue.tag === 0) {
                const accExp = matchValue.fields[0];
                return newTValAndAdd(new MemberExp(0, [{
                    instanceExp: ofPExpZero(accExp.instanceExp),
                    memberName: accExp.memberName,
                }]));
            }
            else {
                return newTValAndAdd(new MemberExp(1, [matchValue.fields[0]]));
            }
        };
        return ofPExpZero(pexp);
    };
    let currTokIdx = 0;
    let openScopeStack = empty_1();
    let elseBlockOpen = false;
    const toTree = (bindingContext_1) => {
        let tupledArg;
        let scopeClosed = false;
        let revTree = empty_1();
        let errors = empty_1();
        const addToken = (x_2) => {
            revTree = cons(x_2, revTree);
        };
        while (!scopeClosed && (currTokIdx < length(tokens))) {
            const token = item(currTokIdx, tokens);
            currTokIdx = ((currTokIdx + 1) | 0);
            const addError = (message) => {
                errors = toList(delay(() => append(errors, delay(() => singleton(new TrullaError(token.range, message))))));
            };
            const addErrors = (newErrors) => {
                errors = toList(delay(() => append(errors, delay(() => newErrors))));
            };
            const matchValue_1 = token.result;
            switch (matchValue_1.tag) {
                case 1: {
                    addToken(new TExp(1, [buildMemberExp(bindingContext_1, matchValue_1.fields[0])]));
                    break;
                }
                case 2: {
                    const ident_1 = matchValue_1.fields[0];
                    const accExp_2 = buildMemberExp(bindingContext_1, matchValue_1.fields[1]);
                    const tvarIdent = newTVar();
                    openScopeStack = cons(new AstModule_Scope(1, []), openScopeStack);
                    const matchValue_2 = toTree(add(ident_1.result, tvarIdent, bindingContext_1));
                    if (matchValue_2.tag === 1) {
                        addErrors(matchValue_2.fields[0]);
                    }
                    else {
                        addToken((tupledArg = [AstModule_createTVal(ident_1.range, tvarIdent, bindingContext_1, ident_1.result), accExp_2, matchValue_1.fields[2], matchValue_2.fields[0]], new TExp(2, [tupledArg[0], tupledArg[1], tupledArg[2], tupledArg[3]])));
                    }
                    break;
                }
                case 3: {
                    const cond = buildMemberExp(bindingContext_1, matchValue_1.fields[0]);
                    openScopeStack = cons(new AstModule_Scope(0, [cond]), openScopeStack);
                    const matchValue_3 = toTree(bindingContext_1);
                    if (matchValue_3.tag === 1) {
                        addErrors(matchValue_3.fields[0]);
                    }
                    else {
                        addToken(new TExp(3, [cond, matchValue_3.fields[0]]));
                    }
                    break;
                }
                case 4: {
                    if (elseBlockOpen) {
                        elseBlockOpen = false;
                        let matchResult, cond_1;
                        if (!isEmpty(openScopeStack)) {
                            if (head(openScopeStack).tag === 0) {
                                matchResult = 0;
                                cond_1 = head(openScopeStack).fields[0];
                            }
                            else {
                                matchResult = 1;
                            }
                        }
                        else {
                            matchResult = 1;
                        }
                        switch (matchResult) {
                            case 0: {
                                const matchValue_4 = toTree(bindingContext_1);
                                if (matchValue_4.tag === 1) {
                                    errors = toList(delay(() => append(errors, delay(() => matchValue_4.fields[0]))));
                                }
                                else {
                                    addToken(new TExp(4, [cond_1, matchValue_4.fields[0]]));
                                }
                                break;
                            }
                            case 1: {
                                addError("An else needs an if.");
                                break;
                            }
                        }
                    }
                    else {
                        currTokIdx = ((currTokIdx - 1) | 0);
                        elseBlockOpen = true;
                        scopeClosed = true;
                    }
                    break;
                }
                case 5: {
                    if (!isEmpty(openScopeStack)) {
                        const xs = tail(openScopeStack);
                        openScopeStack = xs;
                        scopeClosed = true;
                    }
                    else {
                        addError("Closing a scope is not possible without having a scope open.");
                    }
                    break;
                }
                default:
                    addToken(new TExp(0, [matchValue_1.fields[0]]));
            }
        }
        if (isEmpty(errors)) {
            return new FSharpResult$2(0, [reverse(revTree)]);
        }
        else {
            return new FSharpResult$2(1, [errors]);
        }
    };
    const matchValue_5 = toTree(empty({
        Compare: comparePrimitives,
    }));
    if (matchValue_5.tag === 1) {
        return new FSharpResult$2(1, [matchValue_5.fields[0]]);
    }
    else if (length(openScopeStack) > 0) {
        return new FSharpResult$2(1, [singleton_1(new TrullaError(RangeModule_zero, "TODO: Unclosed scope detected."))]);
    }
    else {
        return new FSharpResult$2(0, [new Ast(matchValue_5.fields[0], tvarToMemberExp)]);
    }
}

