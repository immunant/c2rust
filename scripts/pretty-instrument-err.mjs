#!/usr/bin/env node

import * as fs from "fs";

const [_nodePath, _scriptPath, ...args] = process.argv;
let stderr = fs.readFileSync("/dev/stdin").toString(); // jsonl
stderr = stderr.split("\n")
    .map(line => {
        try {
            return JSON.parse(line);
        } catch {
            // probably a stacktrace
            return {
                level: "error: internal compiler error",
                message: line,
                rendered: line,
            };
        }
    })
    .filter(e => e.level?.includes("error"))
    .map(e => e.rendered ?? e.message)
    .join("\n")
    ;
console.error(stderr);
