#!/usr/bin/env node

const { parseArgs } = require("../internal/parse-args");
const { runRequest } = require("./run-request");
const { deployModule } = require("./run-deploy-module.js");

const fundAdminIfNeeded = async () => {
  const detailsArgs = ["--local", "admin-account-details"];
  const accountDetails = await parseArgs(detailsArgs).then(runRequest);

  if (
    accountDetails.status === "failure" &&
    accountDetails.error.message.includes("row not found: tellor-admin-keyset")
  ) {
    console.log(
      "Admin account not found on local Chainweb node. Funding account..."
    );
    const fundArgs = ["--send", "admin-keys-init", "--signers", "sender00"];
    const result = await parseArgs(fundArgs).then(runRequest);
    if (result.status === "success") {
      console.log(`Funded! Cost ${result.gas} gas.`);
    } else {
      throw new Error(`Failed to fund account: ${result.error}`);
    }
  } else if (accountDetails.status === "failure") {
    throw new Error(
      `Unexpected error getting admin account details: ${accountDetails.error.message}`
    );
  } else {
    console.log(
      `Admin account found with ${accountDetails.data.balance} in funds.`
    );
  }
};

const iflexDetailArgs = ["--local", "iflex-details"];
const iflexArgs = [
  "--send",
  "iflex-deploy",
  "--signers",
  "tellor-admin-keyset",
];
const igovernanceDetailArgs = ["--local", "igovernance-details"];
const deployIGArgs = [
  "--send",
  "igovernance-deploy",
  "--signers",
  "tellor-admin-keyset",
];

const main = async () => {
  await fundAdminIfNeeded();
  await deployModule(iflexDetailArgs, iflexArgs)
  await deployModule(igovernanceDetailArgs, deployIGArgs)
};

main();

