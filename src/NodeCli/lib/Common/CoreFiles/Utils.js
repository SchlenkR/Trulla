import { reverse, empty, cons, head, tail, isEmpty } from "../../fable_modules/fable-library.4.3.0/List.js";
import { defaultArgWith } from "../../fable_modules/fable-library.4.3.0/Option.js";
import { tryFind } from "../../fable_modules/fable-library.4.3.0/Map.js";
import { printf, toFail } from "../../fable_modules/fable-library.4.3.0/String.js";
import { exists } from "../../fable_modules/fable-library.4.3.0/Seq.js";
import { isLetterOrDigit } from "../../fable_modules/fable-library.4.3.0/Char.js";

export function List_partitionMap3(mapping, source) {
    const loop = (acc) => {
        const acc_1 = acc;
        const acc3 = acc_1[2];
        const acc2 = acc_1[1];
        const acc1 = acc_1[0];
        return (_arg) => {
            if (!isEmpty(_arg)) {
                const xs = tail(_arg);
                const matchValue = mapping(head(_arg));
                return (matchValue.tag === 1) ? loop([acc1, cons(matchValue.fields[0], acc2), acc3])(xs) : ((matchValue.tag === 2) ? loop([acc1, acc2, cons(matchValue.fields[0], acc3)])(xs) : loop([cons(matchValue.fields[0], acc1), acc2, acc3])(xs));
            }
            else {
                return acc_1;
            }
        };
    };
    return loop([empty(), empty(), empty()])(reverse(source));
}

export function Map_find(key, errorContext, map) {
    return defaultArgWith(tryFind(key, map), () => toFail(printf("Key not found: %A (context: %A)"))(key)(errorContext));
}

export function String_assertLetterDigitUnderscore(contextualMeaning, s) {
    if (exists((c) => !(isLetterOrDigit(c) ? true : (c === "_")), s.split(""))) {
        toFail(printf("The %s value \'%s\' is not a valid namespace value."))(contextualMeaning)(s);
    }
}

