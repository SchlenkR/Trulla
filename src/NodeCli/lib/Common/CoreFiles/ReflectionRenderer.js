import { Solver_solve } from "./Solver.js";
import { printf, toFail } from "../../fable_modules/fable-library.4.3.0/String.js";
import { StringBuilder__Append_Z721C83C5, StringBuilder_$ctor } from "../../fable_modules/fable-library.4.3.0/System.Text.js";
import { name, getValue } from "../../fable_modules/fable-library.4.3.0/Reflection.js";
import { Map_find } from "./Utils.js";
import { comparePrimitives, disposeSafe, getEnumerator } from "../../fable_modules/fable-library.4.3.0/Util.js";
import { map as map_1, delay, indexed, toList } from "../../fable_modules/fable-library.4.3.0/Seq.js";
import { defaultArg } from "../../fable_modules/fable-library.4.3.0/Option.js";
import { ofList, add } from "../../fable_modules/fable-library.4.3.0/Map.js";
import { length } from "../../fable_modules/fable-library.4.3.0/List.js";
import { toString } from "../../fable_modules/fable-library.4.3.0/Types.js";

export function renderTemplate(model, template) {
    let tree;
    const solution = Solver_solve(template);
    tree = ((solution.tag === 0) ? solution.fields[0].tree : toFail(printf("Template error: %A"))(solution.fields[0]));
    const sb = StringBuilder_$ctor();
    const render = (bindingContext, tree_1) => {
        const getIdentBoundValue = (exp) => {
            const matchValue = exp.value;
            if (matchValue.tag === 0) {
                return getValue(null, getIdentBoundValue(matchValue.fields[0].instanceExp));
            }
            else {
                return Map_find(matchValue.fields[0], "find ident in bindingContext", bindingContext);
            }
        };
        const enumerator = getEnumerator(tree_1);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const texp = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                switch (texp.tag) {
                    case 1: {
                        StringBuilder__Append_Z721C83C5(sb, getIdentBoundValue(texp.fields[0]));
                        break;
                    }
                    case 2: {
                        const objList = toList(indexed((() => {
                            throw 1;
                        })()));
                        const sep_1 = defaultArg(texp.fields[2].result, "");
                        const enumerator_1 = getEnumerator(objList);
                        try {
                            while (enumerator_1["System.Collections.IEnumerator.MoveNext"]()) {
                                const forLoopVar = enumerator_1["System.Collections.Generic.IEnumerator`1.get_Current"]();
                                render(add(texp.fields[0].value, forLoopVar[1], bindingContext), texp.fields[3]);
                                if (forLoopVar[0] < (length(objList) - 1)) {
                                    StringBuilder__Append_Z721C83C5(sb, sep_1);
                                }
                            }
                        }
                        finally {
                            disposeSafe(enumerator_1);
                        }
                        break;
                    }
                    case 3: {
                        if (getIdentBoundValue(texp.fields[0])) {
                            render(bindingContext, texp.fields[1]);
                        }
                        break;
                    }
                    case 4: {
                        if (!getIdentBoundValue(texp.fields[0])) {
                            render(bindingContext, texp.fields[1]);
                        }
                        break;
                    }
                    default:
                        StringBuilder__Append_Z721C83C5(sb, texp.fields[0]);
                }
            }
        }
        finally {
            disposeSafe(enumerator);
        }
    };
    render(ofList(toList(delay(() => map_1((p) => [name(p), getValue(p, model)], null))), {
        Compare: comparePrimitives,
    }), tree);
    return toString(sb);
}

