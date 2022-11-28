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

Follow instructions in the pact repository of the Kadena org github to install the pact interpeter [Pact](https://github.com/kadena-io/pact#installing-pact). Atom code editor has the best tools as far as code editors go [here](https://github.com/kadena-io/pact#atom).

## Usage
Once the pact interperter is installed you can clone this repo and cd into the tellorflex-kadena directory. Type pact on your terminal to enter the pact interpeter environment load the tellorflex.pact file to see the test in action.
```cli
git clone https://github.com/tellor-io/Tellorflex-kadena.git
cd tellorflex-kadena
pact
(load "tellorflex.repl" true)
```
## Oracle

A basic tellor implementation that is still a work in progress.  
Functions included currently: 

```cli
- depositStake
- submitValue
- retrieveValue
- requestStakeWithdrawal
- withdrawStake

```
