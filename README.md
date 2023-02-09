<p align="center">
  <a href='https://www.tellor.io/'>
    <img src= 'https://raw.githubusercontent.com/tellor-io/TellorBrandMaterials/master/Swoosh%20and%20wordmark%20legacy/SwooshWordmarkLegacy.png' width="350" height="250" alt='tellor.io' />
    <img src= 'https://camo.githubusercontent.com/637425cb3d78e55411a0b3cb9ee5684b3a0cbd6d8dc8342c722268f5e4f8b346/68747470733a2f2f692e696d6775722e636f6d2f62415a464147462e706e67' width="250" height="200" alt='tellor.io' />

  </a>
</p>

<p align="center">
  <a href='https://twitter.com/WeAreTellor'>
    <img src= 'https://img.shields.io/twitter/url/http/shields.io.svg?style=social' alt='Twitter WeAreTellor' />
  </a>
</p>

## Tellorflex-kadena <a name="sample"> </a>

Follow instructions in the pact repository of the Kadena org github to install the pact interpeter [Here](https://github.com/kadena-io/pact#installing-pact). Although the atom code editor is no longer maintained, it has great tools for writing code in Pact [check here for instructions](https://github.com/kadena-io/pact#atom). Checkout the [devnet-playground](./devnet-playground) folder for a full installation and contract interaction using nix package manager on linux that includes a pact install among other packages.  Step by step instructions are provided for an easy install on ubuntu 22.04. 

## Tests
After cloning repo and installing the pact interpeter you can run tests in [tests](./tests) folder. 
```cli
# Example running a file in the interpeter
# enter the pact interpeter
pact

# run a test (if not set to true the test will persist causing it to fail on the second run)
(load "tests/tellorflex/e2e-1.repl" true)
```

## Modules

### Tellorflex
- #### Reporters
```lisp
(defun deposit-stake (staker:string guard:guard amount:integer) )
(defun submit-value (query-id:string value:string nonce:integer query-data:string staker:string) )
(defun request-staking-withdraw (staker:string amount:integer) )
(defun withdraw-stake (staker:string) )
```
- #### Users
```lisp
(defun get-data-before (query-id:string timestamp:integer) )
(defun retrieve-data (query-id:string timestamp:integer) )
```
### Governance
- #### Begin a dispute
```lisp
(defun begin-dispute (account:string query-id:string timestamp:integer) )
```
- #### Execute Vote
```lisp
(defun execute-vote (dispute-id:integer) )
```
- #### Tally Vote
```lisp
(defun tally-votes (dispute-id:integer) )
```
- #### Vote
```lisp
(defun vote (dispute-id:integer supports:bool invalid:bool voter-account:string) )
```
##### Currently deployed module-names on chain 1 
(updated on Feb 8, 2023)
```lisp
(namespace "n_728c22eb9b4ef4d2dd923d1c609e8536154c7fbf")
```
* [tellorflex](https://balance.chainweb.com/modules.html?server=api.testnet.chainweb.com&module=n_728c22eb9b4ef4d2dd923d1c609e8536154c7fbf.tellorflex&chain=1)
* [autopay](https://balance.chainweb.com/modules.html?server=api.testnet.chainweb.com&module=n_728c22eb9b4ef4d2dd923d1c609e8536154c7fbf.autopay&chain=1)
* [governance](https://balance.chainweb.com/modules.html?server=api.testnet.chainweb.com&module=n_728c22eb9b4ef4d2dd923d1c609e8536154c7fbf.governance&chain=1)
* [i-flex](https://balance.chainweb.com/modules.html?server=api.testnet.chainweb.com&module=n_728c22eb9b4ef4d2dd923d1c609e8536154c7fbf.i-flex&chain=1)
* [i-governance](https://balance.chainweb.com/modules.html?server=api.testnet.chainweb.com&module=n_728c22eb9b4ef4d2dd923d1c609e8536154c7fbf.i-governance&chain=1)

##### Tests
To run tests, execute:
```sh
./run-test.sh <module-name (ie tellorflex, autopay, or governance)> <Optional flag: --show-trace>
```
## Maintainers <a name="maintainers"> </a>
This repository is maintained by the [Tellor team](https://github.com/orgs/tellor-io/people)


## How to Contribute<a name="how2contribute"> </a>  
Join our Discord:
[<img src="https://github.com/tellor-io/tellorX/blob/main/public/discord.png" width="24" height="24">](https://discord.com/invite/n7drGjh)

Check out our issues log here on Github or feel free to reach out anytime [info@tellor.io](mailto:info@tellor.io)

## Copyright

Tellor Inc. 2023
