#!/usr/bin/env node

const { parseArgs } = require("../internal/parse-args");
const { runRequest } = require("./run-request");

const files = ["constructor", "init-governance", "init-tellorflex", "set-governance"]
const initModules = async () => {
    for (i = 0; i < files.length; i++) {
        let args = ["--send", files[i], "--signers", "tellor-admin-keyset"];
        request = await parseArgs(args).then(runRequest);

        if (request.status === "failure") {
            if (request.error.type === "DbError") {
                console.log(`DbError, ${files[i]} was probably executed previously`)
                continue
            }
            throw new Error(`Transaction failed: ${request.error.message}`)
        } else {
            console.log(request)
        }
    }
}

const main = async () => {
  await initModules();
};

main();

