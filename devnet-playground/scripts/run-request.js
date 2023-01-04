#!/usr/bin/env node

const { parseArgs } = require("../internal/parse-args.js");
const localhost = require("../internal/localhost.js");
const util = require("node:util");
const path = require("node:path");
const exec = util.promisify(require("child_process").exec);

const NETWORK_ID = "development";

const CHAIN = "0";

const apiPath = (endpoint) =>
  `/chainweb/0.0/${NETWORK_ID}/chain/${CHAIN}/pact/api/v1/${endpoint}`;

const yamlDir = path.join(__dirname, "..", "yaml");

const formatLocalRequest = async (request) => {
  const requestPath = path.join(yamlDir, "local", request.concat(".yaml"));
  const command = `pact --apireq ${requestPath} --local`;
  const { stdout, stderr } = await exec(command);
  if (stderr) console.error(`Error formatting local request: ${stderr}`);
  return stdout;
};

const formatExecRequest = async (request, signers) => {
  const requestPath = path.join(yamlDir, "send", request.concat(".yaml"));
  const command = `pact --unsigned ${requestPath}`;
  const getKey = (key) => path.join(yamlDir, "keys", key.concat(".yaml"));
  const sigs = signers.map((k) => `| pact add-sig ${getKey(k)}`).join(" ");
  const { stdout, stderr } = await exec(`${command} ${sigs}`);
  if (stderr) console.error(`Error formatting exec request: ${stderr}`);
  return stdout;
};

const runRequest = async (args) => {
  try {

    const formatted = args["send"]
        ? await formatExecRequest(args["send"], args["signers"])
        : await formatLocalRequest(args["local"]);

    try {
        JSON.parse(formatted);
    } catch (err) {
        throw new Error(
            `Command did not format to JSON. If the request file is valid, then you may have provided the wrong signer or too few signers.\n\nReceived:\n${formatted}`
        );
    }

    if (args["format-only"]) {
        return formatted;
    }

    if (args["local"]) {
        console.log(
            `\n-----\nexecuting 'local' request: ${args["local"].concat(
            ".yaml"
            )}\n-----`
        );
        const res = await localhost.post({
            body: formatted,
            path: apiPath("local"),
        });
        if (args["raw"]) {
            return JSON.stringify(res, null, 2);
        } else {
            return { ...res.result, gas: res.gas };
        }
    }

    else if (args["send"]) {
        console.log(
            `\n-----\nexecuting 'send' request: ${args["send"].concat(
            ".yaml"
            )}\n-----`
        );
      const sendResult = await localhost.post({
        body: formatted,
        path: apiPath("send"),
      });

      if (sendResult["requestKeys"]) {
        const requestKey = sendResult["requestKeys"][0];

        console.log(`Received request key: ${requestKey}`);
        console.log("Sending POST request with request key to /poll endpoint.");
        console.log(
          "May take up to 1 minute and 30 seconds to be mined into a block."
        );
        console.log(
          "Polling every 5 seconds until the transaction has been processed..."
        );

        let counter = 0;
        while (true) {
          counter++;
          if (counter % 3 === 0) {
            console.log(`Waiting (${5 * counter} seconds elapsed)...`);
          }

         const pollResult = await localhost.post({
            body: JSON.stringify({ requestKeys: [requestKey] }),
            path: apiPath("poll"),
        });

        if (typeof pollResult === "string") {
            throw new Error(
              `Transaction could not be processed: ${pollResult}`
            );
        }

        else if (Object.keys(pollResult).length === 0) {
            await (async () =>
              new Promise((resolve) => setTimeout(resolve, 5_000)))();
            continue;
        }

        else {
            if (args["raw"]) {
                return JSON.stringify(pollResult[requestKey], null, 2);
            } else {
                return {
                ...pollResult[requestKey].result,
                gas: pollResult[requestKey].gas,
                };
            }
        }
    }
    } else {
        throw new Error(
          `Expected to receive a request key, but received ${sendResult}`
        );
      }
    }
  } catch (err) {}
};

const main = async () => {
  if (process.argv[1].includes("run-request.js")) {
    try {
        const args = await parseArgs(process.argv.slice(2));
        const result = await runRequest(args);
        console.log(result);
    } catch (err) {
        console.log(err.message);
    }
  }
};

main();

module.exports = { runRequest };

