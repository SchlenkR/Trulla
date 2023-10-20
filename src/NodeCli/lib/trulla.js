import { toConsole, join } from "./fable_modules/fable-library.4.3.0/String.js";
import { skip } from "./fable_modules/fable-library.4.3.0/Seq.js";

export const args = join("; ", skip(2, process.argv));

toConsole(`Args: ${args}`);

