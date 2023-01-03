#!/usr/bin/env node

const { parseArgs } = require("../internal/parse-args");
const { runRequest } = require("./run-request");

const fundReporterIfNeeded = async () => {
  const detailsArgs = ["--local", "reporter-account"];
  const accountDetails = await parseArgs(detailsArgs).then(runRequest);

  if (
    accountDetails.status === "failure" &&
    accountDetails.error.message.includes("row not found: reporter1")
  ) {
    console.log(
      "Reporter account not found on local Chainweb node. Funding account..."
    );
    const fundArgs = ["--send", "fund-reporter1", "--signers", "sender00"];
    const result = await parseArgs(fundArgs).then(runRequest);
    if (result.status === "success") {
      console.log(`Funded! Cost ${result.gas} gas.`);
    } else {
      throw new Error(`Failed to fund account: ${result.error}`);
    }
  } else if (accountDetails.status === "failure") {
    throw new Error(
      `Unexpected error getting reporter account details: ${accountDetails.error.message}`
    );
  } else {
    console.log(
      `Reporter account found with ${accountDetails.data.balance} in funds.`
    );
  }
};

const depositStakeArgs = [
  "--send",
  "deposit-stake",
  "--signers",
  "reporter1",
];

const main = async () => {
  await fundReporterIfNeeded();
  const result = await parseArgs(depositStakeArgs).then(runRequest);
  console.log(result)
};

main();

